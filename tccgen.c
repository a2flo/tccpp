/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "tcc.h"

/* ------------------------------------------------------------------------- */
static void gen_cast(TCCState *s1, CType *type);
static inline CType *pointed_type(CType *type);
static void expr_eq(TCCState *s1);

ST_INLN int is_float(int t)
{
    int bt;
    bt = t & VT_BTYPE;
    return bt == VT_LDOUBLE || bt == VT_DOUBLE || bt == VT_FLOAT;
}

/* we use our own 'finite' function to avoid potential problems with
   non standard math libs */
/* XXX: endianness dependent */
ST_FUNC int ieee_finite(double d)
{
    int *p = (int *)&d;
    return ((unsigned)((p[1] | 0x800fffff) + 1)) >> 31;
}

ST_FUNC void test_lvalue(TCCState *s1)
{
    if (!(s1->vtop->r & VT_LVAL))
        expect("lvalue");
}

/* ------------------------------------------------------------------------- */
/* symbol allocator */
static Sym *__sym_malloc(TCCState *s1)
{
    Sym *sym_pool, *sym, *last_sym;

    sym_pool = tcc_malloc(SYM_POOL_NB * sizeof(Sym));
    dynarray_add(&s1->sym_pools, &s1->nb_sym_pools, sym_pool);

    last_sym = s1->sym_free_first;
    sym = sym_pool;
    for(unsigned long i = 0; i < SYM_POOL_NB; i++) {
        sym->next = last_sym;
        last_sym = sym;
        sym++;
    }
    s1->sym_free_first = last_sym;
    return last_sym;
}

static inline Sym *sym_malloc(TCCState *s1)
{
    Sym *sym;
    sym = s1->sym_free_first;
    if (!sym)
        sym = __sym_malloc(s1);
    s1->sym_free_first = sym->next;
    return sym;
}

ST_INLN void sym_free(TCCState *s1, Sym *sym)
{
    sym->next = s1->sym_free_first;
    s1->sym_free_first = sym;
}

/* push, without hashing */
ST_FUNC Sym *sym_push2(TCCState *s1, Sym **ps, int v, int t, long c)
{
    Sym *s;
    if (ps == &s1->local_stack) {
        for (s = *ps; s && s != s1->scope_stack_bottom; s = s->prev)
            if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM && s->v == v)
                tcc_error("incompatible types for redefinition of '%s'",
                          get_tok_str(s1, v, NULL));
    }
    s = sym_malloc(s1);
    s->v = v;
    s->type.t = t;
    s->type.ref = NULL;
    s->c = c;
    s->next = NULL;
    /* add in stack */
    s->prev = *ps;
    *ps = s;
    return s;
}

/* find a symbol and return its associated structure. 's' is the top
   of the symbol stack */
ST_FUNC Sym *sym_find2(Sym *s, int v)
{
    while (s) {
        if (s->v == v)
            return s;
        s = s->prev;
    }
    return NULL;
}

/* ------------------------------------------------------------------------- */

ST_FUNC void swap(int *p, int *q)
{
    int t;
    t = *p;
    *p = *q;
    *q = t;
}

static void vsetc(TCCState *s1, CType *type, int r, CValue *vc)
{
    if (s1->vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full");
    s1->vtop++;
    s1->vtop->type = *type;
    s1->vtop->r = r;
    s1->vtop->r2 = VT_CONST;
    s1->vtop->c = *vc;
}

/* push constant of type "type" with useless value */
void vpush(TCCState *s1, CType *type)
{
    CValue cval;
    vsetc(s1, type, VT_CONST, &cval);
}

/* push integer constant */
ST_FUNC void vpushi(TCCState *s1, int v)
{
    CValue cval;
    cval.i = v;
    vsetc(s1, &s1->int_type, VT_CONST, &cval);
}

/* push arbitrary 64bit constant */
void vpush64(TCCState *s1, int ty, unsigned long long v)
{
    CValue cval;
    CType ctype;
    ctype.t = ty;
    ctype.ref = NULL;
    cval.ull = v;
    vsetc(s1, &ctype, VT_CONST, &cval);
}

/* push long long constant */
static inline void vpushll(TCCState *s1, long long v)
{
    vpush64(s1, VT_LLONG, v);
}

ST_FUNC void vset(TCCState *s1, CType *type, int r, int v)
{
    CValue cval;

    cval.i = v;
    vsetc(s1, type, r, &cval);
}

ST_FUNC void vswap(TCCState *s1)
{
    SValue tmp;
    /* cannot let cpu flags if other instruction are generated. Also
       avoid leaving VT_JMP anywhere except on the top of the stack
       because it would complicate the code generator. */
    tmp = s1->vtop[0];
    s1->vtop[0] = s1->vtop[-1];
    s1->vtop[-1] = tmp;

/* XXX: +2% overall speed possible with optimized memswap
 *
 *  memswap(&vtop[0], &vtop[1], sizeof *vtop);
 */
}

ST_FUNC void vpushv(TCCState *s1, SValue *v)
{
    if (s1->vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full");
    s1->vtop++;
    *s1->vtop = *v;
}

static void vdup(TCCState *s1)
{
    vpushv(s1, s1->vtop);
}

/* get address of vtop (vtop MUST BE an lvalue) */
static void gaddrof(TCCState *s1)
{
    s1->vtop->r &= ~VT_LVAL;
    /* tricky: if saved lvalue, then we can go back to lvalue */
    if ((s1->vtop->r & VT_VALMASK) == VT_LLOCAL)
        s1->vtop->r = (s1->vtop->r & ~(VT_VALMASK | VT_LVAL_TYPE)) | VT_LOCAL | VT_LVAL;


}

/* rotate n first stack elements to the bottom 
   I1 ... In -> I2 ... In I1 [top is right]
*/
ST_FUNC void vrotb(TCCState *s1, int n)
{
    int i;
    SValue tmp;

    tmp = s1->vtop[-n + 1];
    for(i=-n+1;i!=0;i++)
        s1->vtop[i] = s1->vtop[i+1];
    s1->vtop[0] = tmp;
}

/* rotate the n elements before entry e towards the top
   I1 ... In ... -> In I1 ... I(n-1) ... [top is right]
 */
ST_FUNC void vrote(SValue *e, int n)
{
    int i;
    SValue tmp;

    tmp = *e;
    for(i = 0;i < n - 1; i++)
        e[-i] = e[-i - 1];
    e[-n + 1] = tmp;
}

/* rotate n first stack elements to the top
   I1 ... In -> In I1 ... I(n-1)  [top is right]
 */
ST_FUNC void vrott(TCCState *s1, int n)
{
    vrote(s1->vtop, n);
}

/* pop stack value */
ST_FUNC void vpop(TCCState *s1)
{
    s1->vtop--;
}

/* handle integer constant optimizations and various machine
   independent opt */
static void gen_opic(TCCState *s1, int op)
{
    int c1, c2, t1, t2, n;
    SValue *v1, *v2;
    long long l1, l2;
    typedef unsigned long long U;

    v1 = s1->vtop - 1;
    v2 = s1->vtop;
    t1 = v1->type.t & VT_BTYPE;
    t2 = v2->type.t & VT_BTYPE;

    if (t1 == VT_LLONG)
        l1 = v1->c.ll;
    else if (v1->type.t & VT_UNSIGNED)
        l1 = v1->c.ui;
    else
        l1 = v1->c.i;

    if (t2 == VT_LLONG)
        l2 = v2->c.ll;
    else if (v2->type.t & VT_UNSIGNED)
        l2 = v2->c.ui;
    else
        l2 = v2->c.i;

    /* currently, we cannot do computations with forward symbols */
    c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    if (c1 && c2) {
        switch(op) {
        case '+': l1 += l2; break;
        case '-': l1 -= l2; break;
        case '&': l1 &= l2; break;
        case '^': l1 ^= l2; break;
        case '|': l1 |= l2; break;
        case '*': l1 *= l2; break;

        case TOK_PDIV:
        case '/':
        case '%':
        case TOK_UDIV:
        case TOK_UMOD:
            /* if division by zero, generate explicit division */
            if (l2 == 0) {
                if (s1->const_wanted)
                    tcc_error("division by zero in constant");
                goto general_case;
            }
            switch(op) {
            default: l1 /= l2; break;
            case '%': l1 %= l2; break;
            case TOK_UDIV: l1 = (U)l1 / l2; break;
            case TOK_UMOD: l1 = (U)l1 % l2; break;
            }
            break;
        case TOK_SHL: l1 <<= l2; break;
        case TOK_SHR: l1 = (U)l1 >> l2; break;
        case TOK_SAR: l1 >>= l2; break;
            /* tests */
        case TOK_ULT: l1 = (U)l1 < (U)l2; break;
        case TOK_UGE: l1 = (U)l1 >= (U)l2; break;
        case TOK_EQ: l1 = l1 == l2; break;
        case TOK_NE: l1 = l1 != l2; break;
        case TOK_ULE: l1 = (U)l1 <= (U)l2; break;
        case TOK_UGT: l1 = (U)l1 > (U)l2; break;
        case TOK_LT: l1 = l1 < l2; break;
        case TOK_GE: l1 = l1 >= l2; break;
        case TOK_LE: l1 = l1 <= l2; break;
        case TOK_GT: l1 = l1 > l2; break;
            /* logical */
        case TOK_LAND: l1 = l1 && l2; break;
        case TOK_LOR: l1 = l1 || l2; break;
        default:
            goto general_case;
        }
        v1->c.ll = l1;
        s1->vtop--;
    } else {
        /* if commutative ops, put c2 as constant */
        if (c1 && (op == '+' || op == '&' || op == '^' || 
                   op == '|' || op == '*')) {
            vswap(s1);
            c2 = c1; //c = c1, c1 = c2, c2 = c;
            l2 = l1; //l = l1, l1 = l2, l2 = l;
        }
        /* Filter out NOP operations like x*1, x-0, x&-1... */
        if (c2 && (((op == '*' || op == '/' || op == TOK_UDIV || 
                     op == TOK_PDIV) && 
                    l2 == 1) ||
                   ((op == '+' || op == '-' || op == '|' || op == '^' || 
                     op == TOK_SHL || op == TOK_SHR || op == TOK_SAR) && 
                    l2 == 0) ||
                   (op == '&' && 
                    l2 == -1))) {
            /* nothing to do */
            s1->vtop--;
        } else if (c2 && (op == '*' || op == TOK_PDIV || op == TOK_UDIV)) {
            /* try to use shifts instead of muls or divs */
            if (l2 > 0 && (l2 & (l2 - 1)) == 0) {
                n = -1;
                while (l2) {
                    l2 >>= 1;
                    n++;
                }
                s1->vtop->c.ll = n;
                if (op == '*')
                    op = TOK_SHL;
                else if (op == TOK_PDIV)
                    op = TOK_SAR;
                else
                    op = TOK_SHR;
            }
            goto general_case;
        } else if (c2 && (op == '+' || op == '-') &&
                   (((s1->vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM))
                    || (s1->vtop[-1].r & (VT_VALMASK | VT_LVAL)) == VT_LOCAL)) {
            /* symbol + constant case */
            if (op == '-')
                l2 = -l2;
            s1->vtop--;
            s1->vtop->c.ll += l2;
        } else {
        general_case:
			s1->vtop--;
        }
    }
}

/* generate a floating point operation with constant propagation */
static void gen_opif(TCCState *s1, int op)
{
    int c1, c2;
    SValue *v1, *v2;
    long double f1, f2;

    v1 = s1->vtop - 1;
    v2 = s1->vtop;
    /* currently, we cannot do computations with forward symbols */
    c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    if (c1 && c2) {
        if (v1->type.t == VT_FLOAT) {
            f1 = v1->c.f;
            f2 = v2->c.f;
        } else if (v1->type.t == VT_DOUBLE) {
            f1 = v1->c.d;
            f2 = v2->c.d;
        } else {
            f1 = v1->c.ld;
            f2 = v2->c.ld;
        }

        /* NOTE: we only do constant propagation if finite number (not
           NaN or infinity) (ANSI spec) */
        if (!ieee_finite(f1) || !ieee_finite(f2))
            goto general_case;

        switch(op) {
        case '+': f1 += f2; break;
        case '-': f1 -= f2; break;
        case '*': f1 *= f2; break;
        case '/': 
            if (f2 == 0.0) {
                if (s1->const_wanted)
                    tcc_error("division by zero in constant");
                goto general_case;
            }
            f1 /= f2; 
            break;
            /* XXX: also handles tests ? */
        default:
            goto general_case;
        }
        /* XXX: overflow test ? */
        if (v1->type.t == VT_FLOAT) {
            v1->c.f = f1;
        } else if (v1->type.t == VT_DOUBLE) {
            v1->c.d = f1;
        } else {
            v1->c.ld = f1;
        }
        s1->vtop--;
    } else {
    general_case:
        s1->vtop--;
    }
}

static inline int is_null_pointer(SValue *p)
{
    if ((p->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        return 0;
    return ((p->type.t & VT_BTYPE) == VT_INT && p->c.i == 0) ||
        ((p->type.t & VT_BTYPE) == VT_LLONG && p->c.ll == 0) ||
	((p->type.t & VT_BTYPE) == VT_PTR && p->c.ptr == 0);
}

static inline int is_integer_btype(int bt)
{
    return (bt == VT_BYTE || bt == VT_SHORT || 
            bt == VT_INT || bt == VT_LLONG);
}

/* generic gen_op: handles types problems */
ST_FUNC void gen_op(TCCState *s1, int op)
{
    int t1, t2, bt1, bt2, t;
    CType type1;

    t1 = s1->vtop[-1].type.t;
    t2 = s1->vtop[0].type.t;
    bt1 = t1 & VT_BTYPE;
    bt2 = t2 & VT_BTYPE;
        
    if (bt1 == VT_PTR || bt2 == VT_PTR) {
        /* at least one operand is a pointer */
        /* relationnal op: must be both pointers */
        if (op >= TOK_ULT && op <= TOK_LOR) {
            //check_comparison_pointer_types(vtop - 1, vtop, op);
            /* pointers are handled are unsigned */
            t = VT_INT | VT_UNSIGNED;
            goto std_op;
        }
        /* if both pointers, then it must be the '-' op */
        if (bt1 == VT_PTR && bt2 == VT_PTR) {
            if (op != '-')
                tcc_error("cannot use pointers here");
            //check_comparison_pointer_types(vtop - 1, vtop, op);
            /* XXX: check that types are compatible */
            vrott(s1, 3);
            gen_opic(s1, op);
            /* set to integer type */
            s1->vtop->type.t = VT_INT; 
            vswap(s1);
            gen_op(s1, TOK_PDIV);
        } else {
            /* exactly one pointer : must be '+' or '-'. */
            if (op != '-' && op != '+')
                tcc_error("cannot use pointers here");
            /* Put pointer as first operand */
            if (bt2 == VT_PTR) {
                vswap(s1);
                swap(&t1, &t2);
            }
            type1 = s1->vtop[-1].type;
            type1.t &= ~VT_ARRAY;
            gen_op(s1, '*');
            {
                gen_opic(s1, op);
            }
            /* put again type if gen_opic() swaped operands */
            s1->vtop->type = type1;
        }
    } else if (is_float(bt1) || is_float(bt2)) {
        /* compute bigger type and do implicit casts */
        if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
            t = VT_LDOUBLE;
        } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
            t = VT_DOUBLE;
        } else {
            t = VT_FLOAT;
        }
        /* floats can only be used for a few operations */
        if (op != '+' && op != '-' && op != '*' && op != '/' &&
            (op < TOK_ULT || op > TOK_GT))
            tcc_error("invalid operands for binary operation");
        goto std_op;
    } else if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL) {
        t = bt1 == VT_LLONG ? VT_LLONG : VT_INT;
        if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (t | VT_UNSIGNED))
          t |= VT_UNSIGNED;
        goto std_op;
    } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
        /* cast to biggest op */
        t = VT_LLONG;
        /* convert to unsigned if it does not fit in a long long */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED))
            t |= VT_UNSIGNED;
        goto std_op;
    } else if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
        tcc_error("comparison of struct");
    } else {
        /* integer operations */
        t = VT_INT;
        /* convert to unsigned if it does not fit in an integer */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED))
            t |= VT_UNSIGNED;
    std_op:
        /* XXX: currently, some unsigned operations are explicit, so
           we modify them here */
        if (t & VT_UNSIGNED) {
            if (op == TOK_SAR)
                op = TOK_SHR;
            else if (op == '/')
                op = TOK_UDIV;
            else if (op == '%')
                op = TOK_UMOD;
            else if (op == TOK_LT)
                op = TOK_ULT;
            else if (op == TOK_GT)
                op = TOK_UGT;
            else if (op == TOK_LE)
                op = TOK_ULE;
            else if (op == TOK_GE)
                op = TOK_UGE;
        }
        vswap(s1);
        type1.t = t;
        gen_cast(s1, &type1);
        vswap(s1);
        /* special case for shifts and long long: we keep the shift as
           an integer */
        if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL)
            type1.t = VT_INT;
        gen_cast(s1, &type1);
        if (is_float(t))
            gen_opif(s1, op);
        else
            gen_opic(s1, op);
        if (op >= TOK_ULT && op <= TOK_GT) {
            /* relationnal op: the result is an int */
            s1->vtop->type.t = VT_INT;
        } else {
            s1->vtop->type.t = t;
        }
    }
}

/* force char or short cast */
static void force_charshort_cast(TCCState *s1, int t)
{
    int bits, dbt;
    dbt = t & VT_BTYPE;
    /* XXX: add optimization if lvalue : just change type and offset */
    if (dbt == VT_BYTE)
        bits = 8;
    else
        bits = 16;
    if (t & VT_UNSIGNED) {
        vpushi(s1, (1 << bits) - 1);
        gen_op(s1, '&');
    } else {
        bits = 32 - bits;
        vpushi(s1, bits);
        gen_op(s1, TOK_SHL);
        /* result must be signed or the SAR is converted to an SHL
           This was not the case when "t" was a signed short
           and the last value on the stack was an unsigned int */
        s1->vtop->type.t &= ~VT_UNSIGNED;
        vpushi(s1, bits);
        gen_op(s1, TOK_SAR);
    }
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
static void gen_cast(TCCState *s1, CType *type)
{
    int sbt, dbt, sf, df, c, p;

    /* special delayed cast for char/short */
    /* XXX: in some cases (multiple cascaded casts), it may still
       be incorrect */
    if (s1->vtop->r & VT_MUSTCAST) {
        s1->vtop->r &= ~VT_MUSTCAST;
        force_charshort_cast(s1, s1->vtop->type.t);
    }

    dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
    sbt = s1->vtop->type.t & (VT_BTYPE | VT_UNSIGNED);

    if (sbt != dbt) {
        sf = is_float(sbt);
        df = is_float(dbt);
        c = (s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        p = (s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM);
        if (c) {
            /* constant case: we can do it now */
            /* XXX: in ISOC, cannot do it if error in convert */
            if (sbt == VT_FLOAT)
                s1->vtop->c.ld = s1->vtop->c.f;
            else if (sbt == VT_DOUBLE)
                s1->vtop->c.ld = s1->vtop->c.d;

            if (df) {
                if ((sbt & VT_BTYPE) == VT_LLONG) {
                    if (sbt & VT_UNSIGNED)
                        s1->vtop->c.ld = s1->vtop->c.ull;
                    else
                        s1->vtop->c.ld = s1->vtop->c.ll;
                } else if(!sf) {
                    if (sbt & VT_UNSIGNED)
                        s1->vtop->c.ld = s1->vtop->c.ui;
                    else
                        s1->vtop->c.ld = s1->vtop->c.i;
                }

                if (dbt == VT_FLOAT)
                    s1->vtop->c.f = (float)s1->vtop->c.ld;
                else if (dbt == VT_DOUBLE)
                    s1->vtop->c.d = (double)s1->vtop->c.ld;
            } else if (sf && dbt == (VT_LLONG|VT_UNSIGNED)) {
                s1->vtop->c.ull = (unsigned long long)s1->vtop->c.ld;
            } else if (sf && dbt == VT_BOOL) {
                s1->vtop->c.i = (s1->vtop->c.ld != 0);
            } else {
                if(sf)
                    s1->vtop->c.ll = (long long)s1->vtop->c.ld;
                else if (sbt == (VT_LLONG|VT_UNSIGNED))
                    s1->vtop->c.ll = s1->vtop->c.ull;
                else if (sbt & VT_UNSIGNED)
                    s1->vtop->c.ll = s1->vtop->c.ui;
                else if (sbt != VT_LLONG)
                    s1->vtop->c.ll = s1->vtop->c.i;

                if (dbt == (VT_LLONG|VT_UNSIGNED))
                    s1->vtop->c.ull = s1->vtop->c.ll;
                else if (dbt == VT_BOOL)
                    s1->vtop->c.i = (s1->vtop->c.ll != 0);
                else if (dbt != VT_LLONG) {
                    int s = 0;
                    if ((dbt & VT_BTYPE) == VT_BYTE)
                        s = 24;
                    else if ((dbt & VT_BTYPE) == VT_SHORT)
                        s = 16;

                    if(dbt & VT_UNSIGNED)
                        s1->vtop->c.ui = ((unsigned int)s1->vtop->c.ll << s) >> s;
                    else
                        s1->vtop->c.i = ((int)s1->vtop->c.ll << s) >> s;
                }
            }
        } else if (p && dbt == VT_BOOL) {
            s1->vtop->r = VT_CONST;
            s1->vtop->c.i = 1;
        }
    } else if ((dbt & VT_BTYPE) == VT_PTR && !(s1->vtop->r & VT_LVAL)) {
        /* if we are casting between pointer types,
           we must update the VT_LVAL_xxx size */
        s1->vtop->r = (s1->vtop->r & ~VT_LVAL_TYPE) | (lvalue_type(type->ref->type.t) & VT_LVAL_TYPE);
    }
    s1->vtop->type = *type;
}

/* return the pointed type of t */
static inline CType *pointed_type(CType *type)
{
    return &type->ref->type;
}

/* modify type so that its it is a pointer to type. */
ST_FUNC void mk_pointer(CType *type)
{
    type->t = VT_PTR | (type->t & ~VT_TYPE);
}

/* store vtop in lvalue pushed on stack */
ST_FUNC void vstore(TCCState *s1)
{
    int sbt, dbt, ft, bit_size, bit_pos, delayed_cast;

    ft = s1->vtop[-1].type.t;
    sbt = s1->vtop->type.t & VT_BTYPE;
    dbt = ft & VT_BTYPE;
    if ((((sbt == VT_INT || sbt == VT_SHORT) && dbt == VT_BYTE) ||
         (sbt == VT_INT && dbt == VT_SHORT))
	&& !(s1->vtop->type.t & VT_BITFIELD)) {
        /* optimize char/short casts */
        delayed_cast = VT_MUSTCAST;
        s1->vtop->type.t = ft & (VT_TYPE & ~(VT_BITFIELD | (-1 << VT_STRUCT_SHIFT)));
        /* XXX: factorize */
        if (ft & VT_CONSTANT)
            tcc_error("assignment of read-only location");
    } else {
        delayed_cast = 0;
    }

    if (sbt == VT_STRUCT) {
        /* if structure, only generate pointer */
        /* structure assignment : generate memcpy */
        /* XXX: optimize if small size */
		vswap(s1);
		vpop(s1);
        /* leave source on stack */
    } else if (ft & VT_BITFIELD) {
        /* bitfield store handling */
        bit_pos = (ft >> VT_STRUCT_SHIFT) & 0x3f;
        bit_size = (ft >> (VT_STRUCT_SHIFT + 6)) & 0x3f;
        /* remove bit field info to avoid loops */
        s1->vtop[-1].type.t = ft & ~(VT_BITFIELD | (-1 << VT_STRUCT_SHIFT));

        /* duplicate source into other register */
        vswap(s1);
        vrott(s1, 3);

        if((ft & VT_BTYPE) == VT_BOOL) {
            gen_cast(s1, &s1->vtop[-1].type);
            s1->vtop[-1].type.t = (s1->vtop[-1].type.t & ~VT_BTYPE) | (VT_BYTE | VT_UNSIGNED);
        }

        /* duplicate destination */
        vdup(s1);
        s1->vtop[-1] = s1->vtop[-2];

        /* mask and shift source */
        if((ft & VT_BTYPE) != VT_BOOL) {
            if((ft & VT_BTYPE) == VT_LLONG) {
                vpushll(s1, (1ULL << bit_size) - 1ULL);
            } else {
                vpushi(s1, (1 << bit_size) - 1);
            }
            gen_op(s1, '&');
        }
        vpushi(s1, bit_pos);
        gen_op(s1, TOK_SHL);
        /* load destination, mask and or with source */
        vswap(s1);
        if((ft & VT_BTYPE) == VT_LLONG) {
            vpushll(s1, ~(((1ULL << bit_size) - 1ULL) << bit_pos));
        } else {
            vpushi(s1, ~(((1 << bit_size) - 1) << bit_pos));
        }
        gen_op(s1, '&');
        gen_op(s1, '|');
        /* store result */
        vstore(s1);

        /* pop off shifted source from "duplicate source..." above */
        vpop(s1);

    } else {
        vswap(s1);
        s1->vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        s1->vtop->r |= delayed_cast;
    }
}

/* post defines POST/PRE add. c is the token ++ or -- */
ST_FUNC void inc(TCCState *s1, int post, int c)
{
    test_lvalue(s1);
    vdup(s1); /* save lvalue */
    if (post) {
        vrotb(s1, 3);
        vrotb(s1, 3);
    }
    /* add constant */
    vpushi(s1, c - TOK_MID); 
    gen_op(s1, '+');
    vstore(s1); /* store value */
    if (post)
        vpop(s1); /* if post op, return saved value */
}

/* convert a function parameter type (array to pointer and function to
   function pointer) */
static inline void convert_parameter_type(CType *pt)
{
    /* remove const and volatile qualifiers (XXX: const could be used
       to indicate a const function parameter */
    pt->t &= ~(VT_CONSTANT | VT_VOLATILE);
    /* array must be transformed to pointer according to ANSI C */
    pt->t &= ~VT_ARRAY;
    if ((pt->t & VT_BTYPE) == VT_FUNC) {
        mk_pointer(pt);
    }
}

/* compute the lvalue VT_LVAL_xxx needed to match type t. */
ST_FUNC int lvalue_type(int t)
{
    int bt, r;
    r = VT_LVAL;
    bt = t & VT_BTYPE;
    if (bt == VT_BYTE || bt == VT_BOOL)
        r |= VT_LVAL_BYTE;
    else if (bt == VT_SHORT)
        r |= VT_LVAL_SHORT;
    else
        return r;
    if (t & VT_UNSIGNED)
        r |= VT_LVAL_UNSIGNED;
    return r;
}

/* indirection with full error checking and bound check */
ST_FUNC void indir(TCCState *s1)
{
    if ((s1->vtop->type.t & VT_BTYPE) != VT_PTR) {
        if ((s1->vtop->type.t & VT_BTYPE) == VT_FUNC)
            return;
        expect("pointer");
    }
    s1->vtop->type = *pointed_type(&s1->vtop->type);
    /* Arrays and functions are never lvalues */
    if (!(s1->vtop->type.t & VT_ARRAY) && !(s1->vtop->type.t & VT_VLA)
        && (s1->vtop->type.t & VT_BTYPE) != VT_FUNC) {
        s1->vtop->r |= lvalue_type(s1->vtop->type.t);
        /* if bound checking, the referenced pointer must be checked */
    }
}

static void vpush_tokc(TCCState *s1, int t)
{
    CType type;
    type.t = t;
    type.ref = 0;
    vsetc(s1, &type, VT_CONST, &s1->tokc);
}

ST_FUNC void unary(TCCState *s1)
{
    int t;
    CType type;
    Sym *s;
    switch(s1->tok) {
    case TOK_CINT:
    case TOK_CCHAR: 
    case TOK_LCHAR:
        vpushi(s1, s1->tokc.i);
        next(s1);
        break;
    case TOK_CUINT:
        vpush_tokc(s1, VT_INT | VT_UNSIGNED);
        next(s1);
        break;
    case TOK_CLLONG:
        vpush_tokc(s1, VT_LLONG);
        next(s1);
        break;
    case TOK_CULLONG:
        vpush_tokc(s1, VT_LLONG | VT_UNSIGNED);
        next(s1);
        break;
    case TOK_CFLOAT:
        vpush_tokc(s1, VT_FLOAT);
        next(s1);
        break;
    case TOK_CDOUBLE:
        vpush_tokc(s1, VT_DOUBLE);
        next(s1);
        break;
    case TOK___FUNC__:
        {
            /* special function name identifier */
            int len = (int)strlen(s1->funcname) + 1;
            /* generate char[len] type */
            type.t = VT_BYTE;
            mk_pointer(&type);
            type.t |= VT_ARRAY;
            type.ref->c = len;
            next(s1);
        }
        break;
    case '(':
        next(s1);
        if (s1->tok == '{') {
            /* statement expression : we do not accept break/continue
               inside as GCC does */
            skip(s1, ')');
        } else {
            gexpr(s1);
            skip(s1, ')');
        }
        break;
    case '*':
        next(s1);
        unary(s1);
        indir(s1);
        break;
    case '&':
        next(s1);
        unary(s1);
        /* functions names must be treated as function pointers,
           except for unary '&' and sizeof. Since we consider that
           functions are not lvalues, we only have to handle it
           there and in function calls. */
        /* arrays can also be used although they are not lvalues */
        if ((s1->vtop->type.t & VT_BTYPE) != VT_FUNC &&
            !(s1->vtop->type.t & VT_ARRAY) && !(s1->vtop->type.t & VT_LLOCAL))
            test_lvalue(s1);
        mk_pointer(&s1->vtop->type);
        gaddrof(s1);
        break;
    case '!':
        next(s1);
        unary(s1);
        if ((s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            CType boolean;
            boolean.t = VT_BOOL;
            gen_cast(s1, &boolean);
            s1->vtop->c.i = !s1->vtop->c.i;
        } else if ((s1->vtop->r & VT_VALMASK) == VT_CMP) {
            s1->vtop->c.i = s1->vtop->c.i ^ 1;
		}
        break;
    case '~':
        next(s1);
        unary(s1);
        vpushi(s1, -1);
        gen_op(s1, '^');
        break;
    case '+':
        next(s1);
        /* in order to force cast, we add zero */
        unary(s1);
        if ((s1->vtop->type.t & VT_BTYPE) == VT_PTR)
            tcc_error("pointer not accepted for unary plus");
        vpushi(s1, 0);
        gen_op(s1, '+');
        break;
    case TOK_INC:
    case TOK_DEC:
        t = s1->tok;
        next(s1);
        unary(s1);
        inc(s1, 0, t);
        break;
    case '-':
        next(s1);
        vpushi(s1, 0);
        unary(s1);
        gen_op(s1, '-');
        break;
    case TOK_LAND:
		goto tok_identifier;
    
    // special qnan , snan and infinity values
    case TOK___NAN__:
        vpush64(s1, VT_DOUBLE, 0x7ff8000000000000ULL);
        next(s1);
        break;
    case TOK___SNAN__:
        vpush64(s1, VT_DOUBLE, 0x7ff0000000000001ULL);
        next(s1);
        break;
    case TOK___INF__:
        vpush64(s1, VT_DOUBLE, 0x7ff0000000000000ULL);
        next(s1);
        break;

    default:
    tok_identifier:
        t = s1->tok;
        next(s1);
        if (t < TOK_UIDENT)
            expect("identifier");
        break;
    }
    
    /* post operations */
    while (1) {
        if (s1->tok == TOK_INC || s1->tok == TOK_DEC) {
            inc(s1, 1, s1->tok);
            next(s1);
        } else if (s1->tok == '.' || s1->tok == TOK_ARROW) {
            int qualifiers;
            /* field */ 
            if (s1->tok == TOK_ARROW) 
                indir(s1);
            qualifiers = s1->vtop->type.t & (VT_CONSTANT | VT_VOLATILE);
            test_lvalue(s1);
            gaddrof(s1);
            next(s1);
            /* expect pointer on structure */
            if ((s1->vtop->type.t & VT_BTYPE) != VT_STRUCT)
                expect("struct or union");
            s = s1->vtop->type.ref;
            /* find field */
            s1->tok |= SYM_FIELD;
            while ((s = s->next) != NULL) {
                if (s->v == s1->tok)
                    break;
            }
            if (!s)
                tcc_error("field not found: %s",  get_tok_str(s1, s1->tok & ~SYM_FIELD, NULL));
            /* add field offset to pointer */
            s1->vtop->type = s1->char_pointer_type; /* change type to 'char *' */
            vpushi(s1, (int)s->c);
            gen_op(s1, '+');
            /* change type to field type, and set to lvalue */
            s1->vtop->type = s->type;
            s1->vtop->type.t |= qualifiers;
            /* an array is never an lvalue */
            if (!(s1->vtop->type.t & VT_ARRAY)) {
                s1->vtop->r |= lvalue_type(s1->vtop->type.t);
            }
            next(s1);
        } else if (s1->tok == '[') {
            next(s1);
            gexpr(s1);
            gen_op(s1, '+');
            indir(s1);
            skip(s1, ']');
        } else if (s1->tok == '(') {
            next(s1);
            if (s1->tok != ')') {
                for(;;) {
                    expr_eq(s1);
                    if (s1->tok == ')')
                        break;
                    skip(s1, ',');
                }
            }
            skip(s1, ')');
        } else {
            break;
        }
    }
}

ST_FUNC void expr_prod(TCCState *s1)
{
    int t;

    unary(s1);
    while (s1->tok == '*' || s1->tok == '/' || s1->tok == '%') {
        t = s1->tok;
        next(s1);
        unary(s1);
        gen_op(s1, t);
    }
}

ST_FUNC void expr_sum(TCCState *s1)
{
    int t;

    expr_prod(s1);
    while (s1->tok == '+' || s1->tok == '-') {
        t = s1->tok;
        next(s1);
        expr_prod(s1);
        gen_op(s1, t);
    }
}

static void expr_shift(TCCState *s1)
{
    int t;

    expr_sum(s1);
    while (s1->tok == TOK_SHL || s1->tok == TOK_SAR) {
        t = s1->tok;
        next(s1);
        expr_sum(s1);
        gen_op(s1, t);
    }
}

static void expr_cmp(TCCState *s1)
{
    int t;

    expr_shift(s1);
    while ((s1->tok >= TOK_ULE && s1->tok <= TOK_GT) ||
           s1->tok == TOK_ULT || s1->tok == TOK_UGE) {
        t = s1->tok;
        next(s1);
        expr_shift(s1);
        gen_op(s1, t);
    }
}

static void expr_cmpeq(TCCState *s1)
{
    int t;

    expr_cmp(s1);
    while (s1->tok == TOK_EQ || s1->tok == TOK_NE) {
        t = s1->tok;
        next(s1);
        expr_cmp(s1);
        gen_op(s1, t);
    }
}

static void expr_and(TCCState *s1)
{
    expr_cmpeq(s1);
    while (s1->tok == '&') {
        next(s1);
        expr_cmpeq(s1);
        gen_op(s1, '&');
    }
}

static void expr_xor(TCCState *s1)
{
    expr_and(s1);
    while (s1->tok == '^') {
        next(s1);
        expr_and(s1);
        gen_op(s1, '^');
    }
}

static void expr_or(TCCState *s1)
{
    expr_xor(s1);
    while (s1->tok == '|') {
        next(s1);
        expr_xor(s1);
        gen_op(s1, '|');
    }
}

/* XXX: fix this mess */
static void expr_land_const(TCCState *s1)
{
    expr_or(s1);
    while (s1->tok == TOK_LAND) {
        next(s1);
        expr_or(s1);
        gen_op(s1, TOK_LAND);
    }
}

/* XXX: fix this mess */
static void expr_lor_const(TCCState *s1)
{
    expr_land_const(s1);
    while (s1->tok == TOK_LOR) {
        next(s1);
        expr_land_const(s1);
        gen_op(s1, TOK_LOR);
    }
}

/* only used if non constant */
static void expr_land(TCCState *s1)
{
    expr_or(s1);
    if (s1->tok == TOK_LAND) {
        for(;;) {
            if (s1->tok != TOK_LAND) {
                break;
            }
            next(s1);
            expr_or(s1);
        }
    }
}

static void expr_lor(TCCState *s1)
{
    expr_land(s1);
    if (s1->tok == TOK_LOR) {
        for(;;) {
            if (s1->tok != TOK_LOR) {
                break;
            }
            next(s1);
            expr_land(s1);
        }
    }
}

/* XXX: better constant handling */
static void expr_cond(TCCState *s1)
{
    int t1, t2, bt1, bt2;
    SValue sv;
    CType type, type1, type2;

    if (s1->const_wanted) {
        expr_lor_const(s1);
        if (s1->tok == '?') {
            CType boolean;
            int c;
            boolean.t = VT_BOOL;
            vdup(s1);
            gen_cast(s1, &boolean);
            c = s1->vtop->c.i;
            vpop(s1);
            next(s1);
			
			vpop(s1);
			gexpr(s1);
			
            if (!c)
                vpop(s1);
            skip(s1, ':');
            expr_cond(s1);
            if (c)
                vpop(s1);
        }
    } else {
        expr_lor(s1);
        if (s1->tok == '?') {
            next(s1);
			gexpr(s1);
            type1 = s1->vtop->type;
            sv = *s1->vtop; /* save value to handle it later */
            s1->vtop--; /* no vpop so that FP stack is not flushed */
            skip(s1, ':');
            expr_cond(s1);
            type2 = s1->vtop->type;

            t1 = type1.t;
            bt1 = t1 & VT_BTYPE;
            t2 = type2.t;
            bt2 = t2 & VT_BTYPE;
            /* cast operands to correct type according to ISOC rules */
            if (is_float(bt1) || is_float(bt2)) {
                if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
                    type.t = VT_LDOUBLE;
                } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
                    type.t = VT_DOUBLE;
                } else {
                    type.t = VT_FLOAT;
                }
            } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
                /* cast to biggest op */
                type.t = VT_LLONG;
                /* convert to unsigned if it does not fit in a long long */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED) ||
                    (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
		/* If one is a null ptr constant the result type
		   is the other.  */
		if (is_null_pointer (s1->vtop))
		  type = type1;
		else if (is_null_pointer (&sv))
		  type = type2;
                /* XXX: test pointer compatibility, C99 has more elaborate
		   rules here.  */
		else
		  type = type1;
            } else if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
                /* XXX: test function pointer compatibility */
                type = bt1 == VT_FUNC ? type1 : type2;
            } else if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
                /* XXX: test structure compatibility */
                type = bt1 == VT_STRUCT ? type1 : type2;
            } else if (bt1 == VT_VOID || bt2 == VT_VOID) {
                /* NOTE: as an extension, we accept void on only one side */
                type.t = VT_VOID;
            } else {
                /* integer operations */
                type.t = VT_INT;
                /* convert to unsigned if it does not fit in an integer */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED) ||
                    (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            }
                
            /* now we convert second operand */
            gen_cast(s1, &type);
            if (VT_STRUCT == (s1->vtop->type.t & VT_BTYPE))
                gaddrof(s1);
            
            /* put again first value and cast it */
            *s1->vtop = sv;
            gen_cast(s1, &type);
            if (VT_STRUCT == (s1->vtop->type.t & VT_BTYPE))
                gaddrof(s1);
        }
    }
}

static void expr_eq(TCCState *s1)
{
    int t;
    
    expr_cond(s1);
    if (s1->tok == '=' ||
        (s1->tok >= TOK_A_MOD && s1->tok <= TOK_A_DIV) ||
        s1->tok == TOK_A_XOR || s1->tok == TOK_A_OR ||
        s1->tok == TOK_A_SHL || s1->tok == TOK_A_SAR) {
        test_lvalue(s1);
        t = s1->tok;
        next(s1);
        if (t == '=') {
            expr_eq(s1);
        } else {
            vdup(s1);
            expr_eq(s1);
            gen_op(s1, t & 0x7f);
        }
        vstore(s1);
    }
}

ST_FUNC void gexpr(TCCState *s1)
{
    while (1) {
        expr_eq(s1);
        if (s1->tok != ',')
            break;
        vpop(s1);
        next(s1);
    }
}

/* parse a constant expression and return value in vtop.  */
static void expr_const1(TCCState *s1)
{
    int a;
    a = s1->const_wanted;
    s1->const_wanted = 1;
    expr_cond(s1);
    s1->const_wanted = a;
}

/* parse an integer constant and return its value. */
ST_FUNC int expr_const(TCCState *s1)
{
    int c;
    expr_const1(s1);
    if ((s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect("constant expression");
    c = s1->vtop->c.i;
    vpop(s1);
    return c;
}
