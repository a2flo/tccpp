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

/********************************************************/
/* global variables */

/* loc : local variable index
   ind : output code index
   rsym: return symbol
   anon_sym: anonymous symbol index
*/
ST_DATA int rsym, anon_sym, ind, loc;

ST_DATA Section *text_section, *data_section, *bss_section; /* predefined sections */
ST_DATA Section *cur_text_section; /* current section where function code is generated */
/* symbol sections */
ST_DATA Section *symtab_section, *strtab_section;
/* debug sections */
ST_DATA Section *stab_section, *stabstr_section;
ST_DATA Sym *sym_free_first;
ST_DATA void **sym_pools;
ST_DATA int nb_sym_pools;

ST_DATA Sym *global_stack;
ST_DATA Sym *local_stack;
ST_DATA Sym *scope_stack_bottom;
ST_DATA Sym *define_stack;

ST_DATA SValue __vstack[1+VSTACK_SIZE], *vtop;

ST_DATA int const_wanted; /* true if constant wanted */
ST_DATA int global_expr;  /* true if compound literals must be allocated globally (used during initializers parsing */
ST_DATA CType func_vt; /* current function return type (used by return instruction) */
ST_DATA int func_vc;
ST_DATA int last_line_num, last_ind, func_ind; /* debug last line number and pc */
ST_DATA char *funcname;

ST_DATA CType char_pointer_type, func_old_type, int_type, size_type;

/* ------------------------------------------------------------------------- */
static void gen_cast(CType *type);
static inline CType *pointed_type(CType *type);
static void expr_eq(void);

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

ST_FUNC void test_lvalue(void)
{
    if (!(vtop->r & VT_LVAL))
        expect("lvalue");
}

/* ------------------------------------------------------------------------- */
/* symbol allocator */
static Sym *__sym_malloc(void)
{
    Sym *sym_pool, *sym, *last_sym;

    sym_pool = tcc_malloc(SYM_POOL_NB * sizeof(Sym));
    dynarray_add(&sym_pools, &nb_sym_pools, sym_pool);

    last_sym = sym_free_first;
    sym = sym_pool;
    for(unsigned long i = 0; i < SYM_POOL_NB; i++) {
        sym->next = last_sym;
        last_sym = sym;
        sym++;
    }
    sym_free_first = last_sym;
    return last_sym;
}

static inline Sym *sym_malloc(void)
{
    Sym *sym;
    sym = sym_free_first;
    if (!sym)
        sym = __sym_malloc();
    sym_free_first = sym->next;
    return sym;
}

ST_INLN void sym_free(Sym *sym)
{
    sym->next = sym_free_first;
    sym_free_first = sym;
}

/* push, without hashing */
ST_FUNC Sym *sym_push2(Sym **ps, int v, int t, long c)
{
    Sym *s;
    if (ps == &local_stack) {
        for (s = *ps; s && s != scope_stack_bottom; s = s->prev)
            if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM && s->v == v)
                tcc_error("incompatible types for redefinition of '%s'",
                          get_tok_str(v, NULL));
    }
    s = sym_malloc();
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

static void vsetc(CType *type, int r, CValue *vc)
{
    if (vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full");
    vtop++;
    vtop->type = *type;
    vtop->r = r;
    vtop->r2 = VT_CONST;
    vtop->c = *vc;
}

/* push constant of type "type" with useless value */
void vpush(CType *type)
{
    CValue cval;
    vsetc(type, VT_CONST, &cval);
}

/* push integer constant */
ST_FUNC void vpushi(int v)
{
    CValue cval;
    cval.i = v;
    vsetc(&int_type, VT_CONST, &cval);
}

/* push arbitrary 64bit constant */
void vpush64(int ty, unsigned long long v)
{
    CValue cval;
    CType ctype;
    ctype.t = ty;
    ctype.ref = NULL;
    cval.ull = v;
    vsetc(&ctype, VT_CONST, &cval);
}

/* push long long constant */
static inline void vpushll(long long v)
{
    vpush64(VT_LLONG, v);
}

/* push a reference to a section offset by adding a dummy symbol */
static void vpush_ref(CType *type, Section *sec tcc_unused, unsigned long offset tcc_unused, unsigned long size tcc_unused)
{
    CValue cval;

    cval.ul = 0;
    vsetc(type, VT_CONST | VT_SYM, &cval);
}

ST_FUNC void vset(CType *type, int r, int v)
{
    CValue cval;

    cval.i = v;
    vsetc(type, r, &cval);
}

static void vseti(int r, int v)
{
    CType type;
    type.t = VT_INT;
    type.ref = 0;
    vset(&type, r, v);
}

ST_FUNC void vswap(void)
{
    SValue tmp;
    /* cannot let cpu flags if other instruction are generated. Also
       avoid leaving VT_JMP anywhere except on the top of the stack
       because it would complicate the code generator. */
    tmp = vtop[0];
    vtop[0] = vtop[-1];
    vtop[-1] = tmp;

/* XXX: +2% overall speed possible with optimized memswap
 *
 *  memswap(&vtop[0], &vtop[1], sizeof *vtop);
 */
}

ST_FUNC void vpushv(SValue *v)
{
    if (vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full");
    vtop++;
    *vtop = *v;
}

static void vdup(void)
{
    vpushv(vtop);
}

/* get address of vtop (vtop MUST BE an lvalue) */
static void gaddrof(void)
{
    vtop->r &= ~VT_LVAL;
    /* tricky: if saved lvalue, then we can go back to lvalue */
    if ((vtop->r & VT_VALMASK) == VT_LLOCAL)
        vtop->r = (vtop->r & ~(VT_VALMASK | VT_LVAL_TYPE)) | VT_LOCAL | VT_LVAL;


}

/* rotate n first stack elements to the bottom 
   I1 ... In -> I2 ... In I1 [top is right]
*/
ST_FUNC void vrotb(int n)
{
    int i;
    SValue tmp;

    tmp = vtop[-n + 1];
    for(i=-n+1;i!=0;i++)
        vtop[i] = vtop[i+1];
    vtop[0] = tmp;
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
ST_FUNC void vrott(int n)
{
    vrote(vtop, n);
}

/* pop stack value */
ST_FUNC void vpop(void)
{
    vtop--;
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

/* force char or short cast */
static void force_charshort_cast(int t)
{
    int bits, dbt;
    dbt = t & VT_BTYPE;
    /* XXX: add optimization if lvalue : just change type and offset */
    if (dbt == VT_BYTE)
        bits = 8;
    else
        bits = 16;
    if (t & VT_UNSIGNED) {
        vpushi((1 << bits) - 1);
    } else {
        bits = 32 - bits;
        vpushi(bits);
        /* result must be signed or the SAR is converted to an SHL
           This was not the case when "t" was a signed short
           and the last value on the stack was an unsigned int */
        vtop->type.t &= ~VT_UNSIGNED;
        vpushi(bits);
    }
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
static void gen_cast(CType *type)
{
    int sbt, dbt, sf, df, c, p;

    /* special delayed cast for char/short */
    /* XXX: in some cases (multiple cascaded casts), it may still
       be incorrect */
    if (vtop->r & VT_MUSTCAST) {
        vtop->r &= ~VT_MUSTCAST;
        force_charshort_cast(vtop->type.t);
    }

    dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
    sbt = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);

    if (sbt != dbt) {
        sf = is_float(sbt);
        df = is_float(dbt);
        c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        p = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM);
        if (c) {
            /* constant case: we can do it now */
            /* XXX: in ISOC, cannot do it if error in convert */
            if (sbt == VT_FLOAT)
                vtop->c.ld = vtop->c.f;
            else if (sbt == VT_DOUBLE)
                vtop->c.ld = vtop->c.d;

            if (df) {
                if ((sbt & VT_BTYPE) == VT_LLONG) {
                    if (sbt & VT_UNSIGNED)
                        vtop->c.ld = vtop->c.ull;
                    else
                        vtop->c.ld = vtop->c.ll;
                } else if(!sf) {
                    if (sbt & VT_UNSIGNED)
                        vtop->c.ld = vtop->c.ui;
                    else
                        vtop->c.ld = vtop->c.i;
                }

                if (dbt == VT_FLOAT)
                    vtop->c.f = (float)vtop->c.ld;
                else if (dbt == VT_DOUBLE)
                    vtop->c.d = (double)vtop->c.ld;
            } else if (sf && dbt == (VT_LLONG|VT_UNSIGNED)) {
                vtop->c.ull = (unsigned long long)vtop->c.ld;
            } else if (sf && dbt == VT_BOOL) {
                vtop->c.i = (vtop->c.ld != 0);
            } else {
                if(sf)
                    vtop->c.ll = (long long)vtop->c.ld;
                else if (sbt == (VT_LLONG|VT_UNSIGNED))
                    vtop->c.ll = vtop->c.ull;
                else if (sbt & VT_UNSIGNED)
                    vtop->c.ll = vtop->c.ui;
                else if (sbt != VT_LLONG)
                    vtop->c.ll = vtop->c.i;

                if (dbt == (VT_LLONG|VT_UNSIGNED))
                    vtop->c.ull = vtop->c.ll;
                else if (dbt == VT_BOOL)
                    vtop->c.i = (vtop->c.ll != 0);
                else if (dbt != VT_LLONG) {
                    int s = 0;
                    if ((dbt & VT_BTYPE) == VT_BYTE)
                        s = 24;
                    else if ((dbt & VT_BTYPE) == VT_SHORT)
                        s = 16;

                    if(dbt & VT_UNSIGNED)
                        vtop->c.ui = ((unsigned int)vtop->c.ll << s) >> s;
                    else
                        vtop->c.i = ((int)vtop->c.ll << s) >> s;
                }
            }
        } else if (p && dbt == VT_BOOL) {
            vtop->r = VT_CONST;
            vtop->c.i = 1;
        }
    } else if ((dbt & VT_BTYPE) == VT_PTR && !(vtop->r & VT_LVAL)) {
        /* if we are casting between pointer types,
           we must update the VT_LVAL_xxx size */
        vtop->r = (vtop->r & ~VT_LVAL_TYPE)
                  | (lvalue_type(type->ref->type.t) & VT_LVAL_TYPE);
    }
    vtop->type = *type;
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
ST_FUNC void vstore(void)
{
    int sbt, dbt, ft, bit_size, bit_pos, delayed_cast;

    ft = vtop[-1].type.t;
    sbt = vtop->type.t & VT_BTYPE;
    dbt = ft & VT_BTYPE;
    if ((((sbt == VT_INT || sbt == VT_SHORT) && dbt == VT_BYTE) ||
         (sbt == VT_INT && dbt == VT_SHORT))
	&& !(vtop->type.t & VT_BITFIELD)) {
        /* optimize char/short casts */
        delayed_cast = VT_MUSTCAST;
        vtop->type.t = ft & (VT_TYPE & ~(VT_BITFIELD | (-1 << VT_STRUCT_SHIFT)));
        /* XXX: factorize */
        if (ft & VT_CONSTANT)
            tcc_warning("assignment of read-only location");
    } else {
        delayed_cast = 0;
    }

    if (sbt == VT_STRUCT) {
        /* if structure, only generate pointer */
        /* structure assignment : generate memcpy */
        /* XXX: optimize if small size */
		vswap();
		vpop();
        /* leave source on stack */
    } else if (ft & VT_BITFIELD) {
        /* bitfield store handling */
        bit_pos = (ft >> VT_STRUCT_SHIFT) & 0x3f;
        bit_size = (ft >> (VT_STRUCT_SHIFT + 6)) & 0x3f;
        /* remove bit field info to avoid loops */
        vtop[-1].type.t = ft & ~(VT_BITFIELD | (-1 << VT_STRUCT_SHIFT));

        /* duplicate source into other register */
        vswap();
        vrott(3);

        if((ft & VT_BTYPE) == VT_BOOL) {
            gen_cast(&vtop[-1].type);
            vtop[-1].type.t = (vtop[-1].type.t & ~VT_BTYPE) | (VT_BYTE | VT_UNSIGNED);
        }

        /* duplicate destination */
        vdup();
        vtop[-1] = vtop[-2];

        /* mask and shift source */
        if((ft & VT_BTYPE) != VT_BOOL) {
            if((ft & VT_BTYPE) == VT_LLONG) {
                vpushll((1ULL << bit_size) - 1ULL);
            } else {
                vpushi((1 << bit_size) - 1);
            }
        }
        vpushi(bit_pos);
        /* load destination, mask and or with source */
        vswap();
        if((ft & VT_BTYPE) == VT_LLONG) {
            vpushll(~(((1ULL << bit_size) - 1ULL) << bit_pos));
        } else {
            vpushi(~(((1 << bit_size) - 1) << bit_pos));
        }
        /* store result */
        vstore();

        /* pop off shifted source from "duplicate source..." above */
        vpop();

    } else {
        vswap();
        vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        vtop->r |= delayed_cast;
    }
}

/* post defines POST/PRE add. c is the token ++ or -- */
ST_FUNC void inc(int post, int c)
{
    test_lvalue();
    vdup(); /* save lvalue */
    if (post) {
        vrotb(3);
        vrotb(3);
    }
    /* add constant */
    vpushi(c - TOK_MID); 
    vstore(); /* store value */
    if (post)
        vpop(); /* if post op, return saved value */
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
ST_FUNC void indir(void)
{
    if ((vtop->type.t & VT_BTYPE) != VT_PTR) {
        if ((vtop->type.t & VT_BTYPE) == VT_FUNC)
            return;
        expect("pointer");
    }
    vtop->type = *pointed_type(&vtop->type);
    /* Arrays and functions are never lvalues */
    if (!(vtop->type.t & VT_ARRAY) && !(vtop->type.t & VT_VLA)
        && (vtop->type.t & VT_BTYPE) != VT_FUNC) {
        vtop->r |= lvalue_type(vtop->type.t);
        /* if bound checking, the referenced pointer must be checked */
    }
}

static void vpush_tokc(int t)
{
    CType type;
    type.t = t;
    type.ref = 0;
    vsetc(&type, VT_CONST, &tokc);
}

ST_FUNC void unary(void)
{
    int t;
    CType type;
    Sym *s;
    static int in_sizeof = 0;

    in_sizeof = 0;
    switch(tok) {
    case TOK_CINT:
    case TOK_CCHAR: 
    case TOK_LCHAR:
        vpushi(tokc.i);
        next();
        break;
    case TOK_CUINT:
        vpush_tokc(VT_INT | VT_UNSIGNED);
        next();
        break;
    case TOK_CLLONG:
        vpush_tokc(VT_LLONG);
        next();
        break;
    case TOK_CULLONG:
        vpush_tokc(VT_LLONG | VT_UNSIGNED);
        next();
        break;
    case TOK_CFLOAT:
        vpush_tokc(VT_FLOAT);
        next();
        break;
    case TOK_CDOUBLE:
        vpush_tokc(VT_DOUBLE);
        next();
        break;
    case TOK___FUNC__:
        {
            /* special function name identifier */
            int len = (int)strlen(funcname) + 1;
            /* generate char[len] type */
            type.t = VT_BYTE;
            mk_pointer(&type);
            type.t |= VT_ARRAY;
            type.ref->c = len;
            vpush_ref(&type, data_section, data_section->data_offset, len);
            next();
        }
        break;
    case '(':
        next();
        if (tok == '{') {
            /* statement expression : we do not accept break/continue
               inside as GCC does */
            skip(')');
        } else {
            gexpr();
            skip(')');
        }
        break;
    case '*':
        next();
        unary();
        indir();
        break;
    case '&':
        next();
        unary();
        /* functions names must be treated as function pointers,
           except for unary '&' and sizeof. Since we consider that
           functions are not lvalues, we only have to handle it
           there and in function calls. */
        /* arrays can also be used although they are not lvalues */
        if ((vtop->type.t & VT_BTYPE) != VT_FUNC &&
            !(vtop->type.t & VT_ARRAY) && !(vtop->type.t & VT_LLOCAL))
            test_lvalue();
        mk_pointer(&vtop->type);
        gaddrof();
        break;
    case '!':
        next();
        unary();
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            CType boolean;
            boolean.t = VT_BOOL;
            gen_cast(&boolean);
            vtop->c.i = !vtop->c.i;
        } else if ((vtop->r & VT_VALMASK) == VT_CMP) {
            vtop->c.i = vtop->c.i ^ 1;
		}
        break;
    case '~':
        next();
        unary();
        vpushi(-1);
        break;
    case '+':
        next();
        /* in order to force cast, we add zero */
        unary();
        if ((vtop->type.t & VT_BTYPE) == VT_PTR)
            tcc_error("pointer not accepted for unary plus");
        vpushi(0);
        break;
    case TOK_INC:
    case TOK_DEC:
        t = tok;
        next();
        unary();
        inc(0, t);
        break;
    case '-':
        next();
        vpushi(0);
        unary();
        break;
    case TOK_LAND:
		goto tok_identifier;
    
    // special qnan , snan and infinity values
    case TOK___NAN__:
        vpush64(VT_DOUBLE, 0x7ff8000000000000ULL);
        next();
        break;
    case TOK___SNAN__:
        vpush64(VT_DOUBLE, 0x7ff0000000000001ULL);
        next();
        break;
    case TOK___INF__:
        vpush64(VT_DOUBLE, 0x7ff0000000000000ULL);
        next();
        break;

    default:
    tok_identifier:
        t = tok;
        next();
        if (t < TOK_UIDENT)
            expect("identifier");
        break;
    }
    
    /* post operations */
    while (1) {
        if (tok == TOK_INC || tok == TOK_DEC) {
            inc(1, tok);
            next();
        } else if (tok == '.' || tok == TOK_ARROW) {
            int qualifiers;
            /* field */ 
            if (tok == TOK_ARROW) 
                indir();
            qualifiers = vtop->type.t & (VT_CONSTANT | VT_VOLATILE);
            test_lvalue();
            gaddrof();
            next();
            /* expect pointer on structure */
            if ((vtop->type.t & VT_BTYPE) != VT_STRUCT)
                expect("struct or union");
            s = vtop->type.ref;
            /* find field */
            tok |= SYM_FIELD;
            while ((s = s->next) != NULL) {
                if (s->v == tok)
                    break;
            }
            if (!s)
                tcc_error("field not found: %s",  get_tok_str(tok & ~SYM_FIELD, NULL));
            /* add field offset to pointer */
            vtop->type = char_pointer_type; /* change type to 'char *' */
            vpushi((int)s->c);
            /* change type to field type, and set to lvalue */
            vtop->type = s->type;
            vtop->type.t |= qualifiers;
            /* an array is never an lvalue */
            if (!(vtop->type.t & VT_ARRAY)) {
                vtop->r |= lvalue_type(vtop->type.t);
            }
            next();
        } else if (tok == '[') {
            next();
            gexpr();
            indir();
            skip(']');
        } else if (tok == '(') {
            next();
            if (tok != ')') {
                for(;;) {
                    expr_eq();
                    if (tok == ')')
                        break;
                    skip(',');
                }
            }
            skip(')');
        } else {
            break;
        }
    }
}

ST_FUNC void expr_prod(void)
{
    unary();
    while (tok == '*' || tok == '/' || tok == '%') {
        next();
        unary();
    }
}

ST_FUNC void expr_sum(void)
{
    expr_prod();
    while (tok == '+' || tok == '-') {
        next();
        expr_prod();
    }
}

static void expr_shift(void)
{
    expr_sum();
    while (tok == TOK_SHL || tok == TOK_SAR) {
        next();
        expr_sum();
    }
}

static void expr_cmp(void)
{
    expr_shift();
    while ((tok >= TOK_ULE && tok <= TOK_GT) ||
           tok == TOK_ULT || tok == TOK_UGE) {
        next();
        expr_shift();
    }
}

static void expr_cmpeq(void)
{
    expr_cmp();
    while (tok == TOK_EQ || tok == TOK_NE) {
        next();
        expr_cmp();
    }
}

static void expr_and(void)
{
    expr_cmpeq();
    while (tok == '&') {
        next();
        expr_cmpeq();
    }
}

static void expr_xor(void)
{
    expr_and();
    while (tok == '^') {
        next();
        expr_and();
    }
}

static void expr_or(void)
{
    expr_xor();
    while (tok == '|') {
        next();
        expr_xor();
    }
}

/* XXX: fix this mess */
static void expr_land_const(void)
{
    expr_or();
    while (tok == TOK_LAND) {
        next();
        expr_or();
    }
}

/* XXX: fix this mess */
static void expr_lor_const(void)
{
    expr_land_const();
    while (tok == TOK_LOR) {
        next();
        expr_land_const();
    }
}

/* only used if non constant */
static void expr_land(void)
{
    int t;

    expr_or();
    if (tok == TOK_LAND) {
        t = 0;
        for(;;) {
            if (tok != TOK_LAND) {
                vseti(VT_JMPI, t);
                break;
            }
            next();
            expr_or();
        }
    }
}

static void expr_lor(void)
{
    int t;

    expr_land();
    if (tok == TOK_LOR) {
        t = 0;
        for(;;) {
            if (tok != TOK_LOR) {
                vseti(VT_JMP, t);
                break;
            }
            next();
            expr_land();
        }
    }
}

/* XXX: better constant handling */
static void expr_cond(void)
{
    int t1, t2, bt1, bt2;
    SValue sv;
    CType type, type1, type2;

    if (const_wanted) {
        expr_lor_const();
        if (tok == '?') {
            CType boolean;
            int c;
            boolean.t = VT_BOOL;
            vdup();
            gen_cast(&boolean);
            c = vtop->c.i;
            vpop();
            next();
			
			vpop();
			gexpr();
			
            if (!c)
                vpop();
            skip(':');
            expr_cond();
            if (c)
                vpop();
        }
    } else {
        expr_lor();
        if (tok == '?') {
            next();
			gexpr();
            type1 = vtop->type;
            sv = *vtop; /* save value to handle it later */
            vtop--; /* no vpop so that FP stack is not flushed */
            skip(':');
            expr_cond();
            type2 = vtop->type;

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
		if (is_null_pointer (vtop))
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
            gen_cast(&type);
            if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                gaddrof();
            
            /* put again first value and cast it */
            *vtop = sv;
            gen_cast(&type);
            if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                gaddrof();
        }
    }
}

static void expr_eq(void)
{
    int t;
    
    expr_cond();
    if (tok == '=' ||
        (tok >= TOK_A_MOD && tok <= TOK_A_DIV) ||
        tok == TOK_A_XOR || tok == TOK_A_OR ||
        tok == TOK_A_SHL || tok == TOK_A_SAR) {
        test_lvalue();
        t = tok;
        next();
        if (t == '=') {
            expr_eq();
        } else {
            vdup();
            expr_eq();
        }
        vstore();
    }
}

ST_FUNC void gexpr(void)
{
    while (1) {
        expr_eq();
        if (tok != ',')
            break;
        vpop();
        next();
    }
}

/* parse a constant expression and return value in vtop.  */
static void expr_const1(void)
{
    int a;
    a = const_wanted;
    const_wanted = 1;
    expr_cond();
    const_wanted = a;
}

/* parse an integer constant and return its value. */
ST_FUNC int expr_const(void)
{
    int c;
    expr_const1();
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect("constant expression");
    c = vtop->c.i;
    vpop();
    return c;
}
