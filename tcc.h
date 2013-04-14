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

#ifndef _TCC_H
#define _TCC_H

#define TCC_VERSION "0.9.26-cl-preprocessor"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <fcntl.h>
#include <setjmp.h>
#include <time.h>

#ifndef _WIN32
# include <unistd.h>
# include <sys/time.h>
# include <sys/mman.h>
# ifndef CONFIG_TCC_STATIC
#  include <dlfcn.h>
# endif
#else
# include <windows.h>
# include <sys/timeb.h>
# include <io.h> /* open, close etc. */
# include <direct.h> /* getcwd */
# ifdef __GNUC__
#  include <stdint.h>
# else
   typedef UINT_PTR uintptr_t;
# endif
# define inline __inline
# define inp next_inp
# ifdef LIBTCC_AS_DLL
#  define LIBTCCAPI __declspec(dllexport)
#  define PUB_FUNC LIBTCCAPI
# endif
#endif

#ifndef O_BINARY
# define O_BINARY 0
#endif

typedef unsigned char           uint8_t;
typedef unsigned short int      uint16_t;

#include "libtcc.h"

//
#if defined(__clang__) || defined(__GNUC__)
#define tcc_unused __attribute__((unused))
#else
#define tcc_unused
#endif

/* -------------------------------------------- */

#define INCLUDE_STACK_SIZE  32
#define IFDEF_STACK_SIZE    64
#define VSTACK_SIZE         256
#define STRING_MAX_SIZE     1024
#define PACK_STACK_SIZE     8

#define TOK_HASH_SIZE       8192 /* must be a power of two */
#define TOK_ALLOC_INCR      512  /* must be a power of two */
#define TOK_MAX_SIZE        4 /* token max size in int unit when stored in string */

/* token symbol management */
typedef struct TokenSym {
    struct TokenSym *hash_next;
    struct Sym *sym_define; /* direct pointer to define */
    struct Sym *sym_label; /* direct pointer to label */
    struct Sym *sym_struct; /* direct pointer to structure */
    struct Sym *sym_identifier; /* direct pointer to identifier */
    int tok; /* token number */
    int len;
    char str[1];
} TokenSym;

typedef int nwchar_t;

typedef struct CString {
    int size; /* size in bytes */
    void *data; /* either 'char *' or 'nwchar_t *' */
    int size_allocated;
    void *data_allocated; /* if non NULL, data has been malloced */
} CString;

/* type definition */
typedef struct CType {
    int t;
    struct Sym *ref;
} CType;

/* constant value */
typedef union CValue {
    long double ld;
    double d;
    float f;
    int i;
    unsigned int ui;
    unsigned int ul; /* address (should be unsigned long on 64 bit cpu) */
    long long ll;
    unsigned long long ull;
    struct CString *cstr;
    void *ptr;
    int tab[8/4];
} CValue;

/* value on stack */
typedef struct SValue {
    CType type;      /* type */
    unsigned short r;      /* register + flags */
    unsigned short r2;     /* second register, used for 'long long'
                              type. If not used, set to VT_CONST */
    CValue c;              /* constant, if VT_CONST */
    struct Sym *sym;       /* symbol, if (VT_SYM | VT_CONST) */
} SValue;

/* symbol management */
typedef struct Sym {
    int v;    /* symbol token */
    union {
        long c;    /* associated number */
        int *d;   /* define token stream */
    };
    CType type;    /* associated type */
    union {
        struct Sym *next; /* next related symbol */
        long jnext; /* next jump label */
    };
    struct Sym *prev; /* prev symbol in stack */
    struct Sym *prev_tok; /* previous symbol for this token */
} Sym;

/* section definition */
/* XXX: use directly ELF structure for parameters ? */
/* special flag to indicate that the section should not be linked to
   the other ones */
#define SHF_PRIVATE 0x80000000

/* special flag, too */
#define SECTION_ABS ((void *)1)

typedef struct Section {
    unsigned long data_offset; /* current data offset */
    unsigned char *data;       /* section data */
    unsigned long data_allocated; /* used for realloc() handling */
    int sh_name;             /* elf section name (only used during output) */
    int sh_num;              /* elf section number */
    int sh_type;             /* elf section type */
    int sh_flags;            /* elf section flags */
    int sh_info;             /* elf section info */
    int sh_addralign;        /* elf section alignment */
    int sh_entsize;          /* elf entry size */
    unsigned long sh_size;   /* section size (only used during output) */
    unsigned long sh_offset; /* file offset */
    int nb_hashed_syms;      /* used to resize the hash table */
    struct Section *link;    /* link to another section */
    struct Section *reloc;   /* corresponding section for relocation, if any */
    struct Section *hash;     /* hash table for symbols */
    struct Section *next;
    char name[1];           /* section name */
} Section;

typedef struct DLLReference {
    int level;
    void *handle;
    char name[1];
} DLLReference;

/* GNUC attribute definition */
typedef struct AttributeDef {
    unsigned
      func_call     : 3, /* calling convention (0..5), see below */
      aligned       : 5, /* alignement (0..16) */
      packed        : 1,
      func_export   : 1,
      func_import   : 1,
      func_args     : 5,
      mode          : 4,
      weak          : 1,
      fill          : 11;
    struct Section *section;
    int alias_target;    /* token */
} AttributeDef;

/* gr: wrappers for casting sym->r for other purposes */
#define FUNC_CALL(r) (((AttributeDef*)&(r))->func_call)
#define FUNC_EXPORT(r) (((AttributeDef*)&(r))->func_export)
#define FUNC_IMPORT(r) (((AttributeDef*)&(r))->func_import)
#define FUNC_ARGS(r) (((AttributeDef*)&(r))->func_args)
#define FUNC_ALIGN(r) (((AttributeDef*)&(r))->aligned)
#define FUNC_PACKED(r) (((AttributeDef*)&(r))->packed)
#define ATTR_MODE(r)  (((AttributeDef*)&(r))->mode)
#define INT_ATTR(ad) (*(int*)(ad))

/* -------------------------------------------------- */

#define SYM_STRUCT     0x40000000 /* struct/union/enum symbol space */
#define SYM_FIELD      0x20000000 /* struct/union field symbol space */
#define SYM_FIRST_ANOM 0x10000000 /* first anonymous sym */

/* stored in 'Sym.c' field */
#define FUNC_NEW       1 /* ansi function prototype */
#define FUNC_OLD       2 /* old function prototype */
#define FUNC_ELLIPSIS  3 /* ansi function prototype with ... */

/* stored in 'Sym.r' field */
#define FUNC_CDECL     0 /* standard c call */
#define FUNC_STDCALL   1 /* pascal c call */
#define FUNC_FASTCALL1 2 /* first param in %eax */
#define FUNC_FASTCALL2 3 /* first parameters in %eax, %edx */
#define FUNC_FASTCALL3 4 /* first parameter in %eax, %edx, %ecx */
#define FUNC_FASTCALLW 5 /* first parameter in %ecx, %edx */

/* field 'Sym.t' for macros */
#define MACRO_OBJ      0 /* object like macro */
#define MACRO_FUNC     1 /* function like macro */

/* type_decl() types */
#define TYPE_ABSTRACT  1 /* type without variable */
#define TYPE_DIRECT    2 /* type with variable */

#define IO_BUF_SIZE 8192

typedef struct BufferedFile {
    uint8_t *buf_ptr;
    uint8_t *buf_end;
    int fd;
    struct BufferedFile *prev;
    int line_num;    /* current line number - here to simplify code */
    int ifndef_macro;  /* #ifndef macro / #endif search */
    int ifndef_macro_saved; /* saved ifndef_macro */
    int *ifdef_stack_ptr; /* ifdef_stack value at the start of the file */
    char filename[1024];    /* filename */
    unsigned char buffer[IO_BUF_SIZE + 1]; /* extra size for CH_EOB char */
} BufferedFile;

#define CH_EOB   '\\'       /* end of buffer or '\0' char in file */
#define CH_EOF   (-1)   /* end of file */

/* parsing state (used to save parser state to reparse part of the
   source several times) */
typedef struct ParseState {
    const int *macro_ptr;
    int line_num;
    int tok;
    CValue tokc;
} ParseState;

/* used to record tokens */
typedef struct TokenString {
    int *str;
    int len;
    int allocated_len;
    int last_line_num;
} TokenString;

/* inline functions */
typedef struct InlineFunc {
    int *token_str;
    Sym *sym;
    char filename[1];
} InlineFunc;

/* include file cache, used to find files faster and also to eliminate
   inclusion if the include file is protected by #ifndef ... #endif */
typedef struct CachedInclude {
    int ifndef_macro;
    int hash_next; /* -1 if none */
    char filename[1]; /* path specified in #include */
} CachedInclude;

#define CACHED_INCLUDES_HASH_SIZE 512

struct sym_attr {
    unsigned long got_offset;
};

struct TCCState {

    int verbose; /* if true, display some information during compilation */

    char *soname; /* as specified on the command line (-soname) */
    char *rpath; /* as specified on the command line (-Wl,-rpath=) */

    /* output type, see TCC_OUTPUT_XXX */
    int output_type;
    /* output format, see TCC_OUTPUT_FORMAT_xxx */
    int output_format;

    /* C language options */
    int char_is_unsigned;
    int leading_underscore;
    
    /* warning switches */
    int warn_write_strings;
    int warn_unsupported;
    int warn_error;
    int warn_none;
    int warn_implicit_function_declaration;

    char *init_symbol; /* symbols to call at load-time (not used currently) */
    char *fini_symbol; /* symbols to call at unload-time (not used currently) */

    /* array of all loaded dlls (including those referenced by loaded dlls) */
    DLLReference **loaded_dlls;
    int nb_loaded_dlls;

    /* include paths */
    char **include_paths;
    int nb_include_paths;

    char **sysinclude_paths;
    int nb_sysinclude_paths;

    /* library paths */
    char **library_paths;
    int nb_library_paths;

    /* crt?.o object path */
    char **crt_paths;
    int nb_crt_paths;

    /* error handling */
    void *error_opaque;
    void (*error_func)(void *opaque, const char *msg);
    int error_set_jmp_enabled;
    jmp_buf error_jmp_buf;
    int nb_errors;

    /* output file for preprocessing (-E) */
    FILE *ppfp;

    /* for -MD/-MF: collected dependencies for this compilation */
    char **target_deps;
    int nb_target_deps;

    /* compilation */
    BufferedFile *include_stack[INCLUDE_STACK_SIZE];
    BufferedFile **include_stack_ptr;

    int ifdef_stack[IFDEF_STACK_SIZE];
    int *ifdef_stack_ptr;

    /* included files enclosed with #ifndef MACRO */
    int cached_includes_hash[CACHED_INCLUDES_HASH_SIZE];
    CachedInclude **cached_includes;
    int nb_cached_includes;

    /* #pragma pack stack */
    int pack_stack[PACK_STACK_SIZE];
    int *pack_stack_ptr;

    /* inline functions are stored as token lists and compiled last
       only if referenced */
    struct InlineFunc **inline_fns;
    int nb_inline_fns;

    /* sections */
    Section **sections;
    int nb_sections; /* number of sections, including first dummy section */

    Section **priv_sections;
    int nb_priv_sections; /* number of private sections */

    /* got & plt handling */
    Section *got;
    Section *plt;
    struct sym_attr *sym_attrs;
    int nb_sym_attrs;
    /* give the correspondance from symtab indexes to dynsym indexes */
    int *symtab_to_dynsym;

    /* temporary dynamic symbol sections (for dll loading) */
    Section *dynsymtab_section;
    /* exported dynamic symbol section */
    Section *dynsym;
    /* copy of the gobal symtab_section variable */
    Section *symtab;
    /* tiny assembler state */
    Sym *asm_labels;

    /* used by main and tcc_parse_args only */
    char **files; /* files seen on command line */
    int nb_files; /* number thereof */
    int nb_libraries; /* number of libs thereof */
	
	/* additional state */
	
	/*** tccpp.c ***/
	int tok_flags;
	int parse_flags;
	
	struct BufferedFile *file;
	int ch, tok;
	CValue tokc;
	const int *macro_ptr;
	CString tokcstr; /* current parsed string, if any */
	
	/* display benchmark infos */
	int tok_ident;
	TokenSym **table_ident;
	
	/* get_tok_str buffer */
	char buf[STRING_MAX_SIZE + 1];
	
	/* more */
	int *macro_ptr_allocated;
	const int *unget_saved_macro_ptr;
	int unget_saved_buffer[TOK_MAX_SIZE + 1];
	int unget_buffer_enabled;
	TokenSym *hash_ident[TOK_HASH_SIZE];
	char token_buf[STRING_MAX_SIZE + 1];
	/* true if isid(c) || isnum(c) */
	unsigned char isidnum_table[256-CH_EOF];
	
	/*** tccgen.c ***/
    /* loc : local variable index
       ind : output code index
       rsym: return symbol
       anon_sym: anonymous symbol index
    */
	int rsym, anon_sym, ind, loc;
	
	Sym *sym_free_first;
	void **sym_pools;
	int nb_sym_pools;
	
	Sym *global_stack;
	Sym *local_stack;
	Sym *scope_stack_bottom;
	Sym *define_stack;
	
	SValue __vstack[1+VSTACK_SIZE], *vtop;
	
	int const_wanted; /* true if constant wanted */
	int global_expr;  /* true if compound literals must be allocated globally (used during initializers parsing */
	CType func_vt; /* current function return type (used by return instruction) */
	int func_vc;
	int last_line_num, last_ind, func_ind; /* debug last line number and pc */
	char *funcname;
	
	CType char_pointer_type, func_old_type, int_type, size_type;
};

/* The current value can be: */
#define VT_VALMASK   0x003f  /* mask for value location, register or: */
#define VT_CONST     0x0030  /* constant in vc (must be first non register value) */
#define VT_LLOCAL    0x0031  /* lvalue, offset on stack */
#define VT_LOCAL     0x0032  /* offset on stack */
#define VT_CMP       0x0033  /* the value is stored in processor flags (in vc) */
#define VT_JMP       0x0034  /* value is the consequence of jmp true (even) */
#define VT_JMPI      0x0035  /* value is the consequence of jmp false (odd) */
#define VT_REF       0x0040  /* value is pointer to structure rather than address */
#define VT_LVAL      0x0100  /* var is an lvalue */
#define VT_SYM       0x0200  /* a symbol value is added */
#define VT_MUSTCAST  0x0400  /* value must be casted to be correct (used for
                                char/short stored in integer registers) */
#define VT_MUSTBOUND 0x0800  /* bound checking must be done before
                                dereferencing value */
#define VT_BOUNDED   0x8000  /* value is bounded. The address of the
                                bounding function call point is in vc */
#define VT_LVAL_BYTE     0x1000  /* lvalue is a byte */
#define VT_LVAL_SHORT    0x2000  /* lvalue is a short */
#define VT_LVAL_UNSIGNED 0x4000  /* lvalue is unsigned */
#define VT_LVAL_TYPE     (VT_LVAL_BYTE | VT_LVAL_SHORT | VT_LVAL_UNSIGNED)

/* types */
#define VT_BTYPE       0x000f  /* mask for basic type */
#define VT_INT              0  /* integer type */
#define VT_BYTE             1  /* signed byte type */
#define VT_SHORT            2  /* short type */
#define VT_VOID             3  /* void type */
#define VT_PTR              4  /* pointer */
#define VT_ENUM             5  /* enum definition */
#define VT_FUNC             6  /* function type */
#define VT_STRUCT           7  /* struct/union definition */
#define VT_FLOAT            8  /* IEEE float */
#define VT_DOUBLE           9  /* IEEE double */
#define VT_LDOUBLE         10  /* IEEE long double */
#define VT_BOOL            11  /* ISOC99 boolean type */
#define VT_LLONG           12  /* 64 bit integer */
#define VT_LONG            13  /* long integer (NEVER USED as type, only
                                  during parsing) */
#define VT_UNSIGNED    0x0010  /* unsigned type */
#define VT_ARRAY       0x0020  /* array type (also has VT_PTR) */
#define VT_BITFIELD    0x0040  /* bitfield modifier */
#define VT_CONSTANT    0x0800  /* const modifier */
#define VT_VOLATILE    0x1000  /* volatile modifier */
#define VT_SIGNED      0x2000  /* signed type */
#define VT_VLA     0x00020000  /* VLA type (also has VT_PTR and VT_ARRAY) */

/* storage */
#define VT_EXTERN  0x00000080  /* extern definition */
#define VT_STATIC  0x00000100  /* static variable */
#define VT_TYPEDEF 0x00000200  /* typedef definition */
#define VT_INLINE  0x00000400  /* inline definition */
#define VT_IMPORT  0x00004000  /* win32: extern data imported from dll */
#define VT_EXPORT  0x00008000  /* win32: data exported from dll */
#define VT_WEAK    0x00010000  /* weak symbol */

#define VT_STRUCT_SHIFT 18     /* shift for bitfield shift values (max: 32 - 2*6) */

/* type mask (except storage) */
#define VT_STORAGE (VT_EXTERN | VT_STATIC | VT_TYPEDEF | VT_INLINE | VT_IMPORT | VT_EXPORT | VT_WEAK)
#define VT_TYPE (~(VT_STORAGE))

/* token values */

/* warning: the following compare tokens depend on i386 asm code */
#define TOK_ULT 0x92
#define TOK_UGE 0x93
#define TOK_EQ  0x94
#define TOK_NE  0x95
#define TOK_ULE 0x96
#define TOK_UGT 0x97
#define TOK_Nset 0x98
#define TOK_Nclear 0x99
#define TOK_LT  0x9c
#define TOK_GE  0x9d
#define TOK_LE  0x9e
#define TOK_GT  0x9f

#define TOK_LAND  0xa0
#define TOK_LOR   0xa1

#define TOK_DEC   0xa2
#define TOK_MID   0xa3 /* inc/dec, to void constant */
#define TOK_INC   0xa4
#define TOK_UDIV  0xb0 /* unsigned division */
#define TOK_UMOD  0xb1 /* unsigned modulo */
#define TOK_PDIV  0xb2 /* fast division with undefined rounding for pointers */
#define TOK_CINT   0xb3 /* number in tokc */
#define TOK_CCHAR 0xb4 /* char constant in tokc */
#define TOK_STR   0xb5 /* pointer to string in tokc */
#define TOK_TWOSHARPS 0xb6 /* ## preprocessing token */
#define TOK_LCHAR    0xb7
#define TOK_LSTR     0xb8
#define TOK_CFLOAT   0xb9 /* float constant */
#define TOK_LINENUM  0xba /* line number info */
#define TOK_CDOUBLE  0xc0 /* double constant */
#define TOK_UMULL    0xc2 /* unsigned 32x32 -> 64 mul */
#define TOK_ADDC1    0xc3 /* add with carry generation */
#define TOK_ADDC2    0xc4 /* add with carry use */
#define TOK_SUBC1    0xc5 /* add with carry generation */
#define TOK_SUBC2    0xc6 /* add with carry use */
#define TOK_CUINT    0xc8 /* unsigned int constant */
#define TOK_CLLONG   0xc9 /* long long constant */
#define TOK_CULLONG  0xca /* unsigned long long constant */
#define TOK_ARROW    0xcb
#define TOK_DOTS     0xcc /* three dots */
#define TOK_SHR      0xcd /* unsigned shift right */
#define TOK_PPNUM    0xce /* preprocessor number */
#define TOK_NOSUBST  0xcf /* means following token has already been pp'd */

#define TOK_SHL   0x01 /* shift left */
#define TOK_SAR   0x02 /* signed shift right */
  
/* assignement operators : normal operator or 0x80 */
#define TOK_A_MOD 0xa5
#define TOK_A_AND 0xa6
#define TOK_A_MUL 0xaa
#define TOK_A_ADD 0xab
#define TOK_A_SUB 0xad
#define TOK_A_DIV 0xaf
#define TOK_A_XOR 0xde
#define TOK_A_OR  0xfc
#define TOK_A_SHL 0x81
#define TOK_A_SAR 0x82

#ifndef offsetof
#define offsetof(type, field) ((size_t) &((type *)0)->field)
#endif

#ifndef countof
#define countof(tab) (sizeof(tab) / sizeof((tab)[0]))
#endif

#define TOK_EOF       (-1)  /* end of file */
#define TOK_LINEFEED  10    /* line feed */

/* all identificators and strings have token above that */
#define TOK_IDENT 256

#define TCC_TOKEN_LIST_APPLY(F) \
	/* keywords */ \
	F(TOK_INT, "int") \
	F(TOK_VOID, "void") \
	F(TOK_CHAR, "char") \
	F(TOK_IF, "if") \
	F(TOK_ELSE, "else") \
	F(TOK_WHILE, "while") \
	F(TOK_BREAK, "break") \
	F(TOK_RETURN, "return") \
	F(TOK_FOR, "for") \
	F(TOK_EXTERN, "extern") \
	F(TOK_STATIC, "static") \
	F(TOK_UNSIGNED, "unsigned") \
	F(TOK_GOTO, "goto") \
	F(TOK_DO, "do") \
	F(TOK_CONTINUE, "continue") \
	F(TOK_SWITCH, "switch") \
	F(TOK_CASE, "case") \
	F(TOK_CONST1, "const") \
	F(TOK_VOLATILE1, "volatile") \
	F(TOK_LONG, "long") \
	F(TOK_REGISTER, "register") \
	F(TOK_SIGNED1, "signed") \
	F(TOK_AUTO, "auto") \
	F(TOK_INLINE1, "inline") \
	F(TOK_RESTRICT1, "restrict") \
	F(TOK_RESTRICT2, "__restrict") \
	F(TOK_RESTRICT3, "__restrict__") \
	/*********************************************************************/ \
	/* the following are not keywords. They are included to ease parsing */ \
	/* preprocessor only */ \
	F(TOK_DEFINE, "define") \
	F(TOK_INCLUDE, "include") \
	F(TOK_IFDEF, "ifdef") \
	F(TOK_IFNDEF, "ifndef") \
	F(TOK_ELIF, "elif") \
	F(TOK_ENDIF, "endif") \
	F(TOK_DEFINED, "defined") \
	F(TOK_UNDEF, "undef") \
	F(TOK_ERROR, "error") \
	F(TOK_WARNING, "warning") \
	F(TOK_LINE, "line") \
	F(TOK_PRAGMA, "pragma") \
	F(TOK___LINE__, "__LINE__") \
	F(TOK___FILE__, "__FILE__") \
	F(TOK___DATE__, "__DATE__") \
	F(TOK___TIME__, "__TIME__") \
	F(TOK___VA_ARGS__, "__VA_ARGS__") \
	/* special identifiers */ \
	F(TOK___FUNC__, "__func__") \
	/* special floating point values */ \
	F(TOK___NAN__, "__nan__") \
	F(TOK___SNAN__, "__snan__") \
	F(TOK___INF__, "__inf__")

enum tcc_token {
    TOK_LAST = TOK_IDENT - 1,
#define DEF(id, str) id,
	TCC_TOKEN_LIST_APPLY(DEF)
#undef DEF
};

#define TOK_UIDENT TOK_DEFINE

#ifdef _WIN32
#define snprintf _snprintf
#define vsnprintf _vsnprintf
#ifndef __GNUC__
  #define strtold (long double)strtod
  #define strtof (float)strtod
  #define strtoll _strtoi64
  #define strtoull _strtoui64
#endif
#else
/* XXX: need to define this to use them in non ISOC99 context */
extern float strtof (const char *__nptr, char **__endptr);
extern long double strtold (const char *__nptr, char **__endptr);
#endif

#ifdef _WIN32
#define IS_DIRSEP(c) (c == '/' || c == '\\')
#define IS_ABSPATH(p) (IS_DIRSEP(p[0]) || (p[0] && p[1] == ':' && IS_DIRSEP(p[2])))
#define PATHCMP stricmp
#else
#define IS_DIRSEP(c) (c == '/')
#define IS_ABSPATH(p) IS_DIRSEP(p[0])
#define PATHCMP strcmp
#endif

#define PATHSEP ':'

/* space exlcuding newline */
static inline int is_space(int ch)
{
    return ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f' || ch == '\r';
}

static inline int isid(int c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static inline int isnum(int c)
{
    return c >= '0' && c <= '9';
}

static inline int isoct(int c)
{
    return c >= '0' && c <= '7';
}

static inline int toup(int c)
{
    return (c >= 'a' && c <= 'z') ? c - 'a' + 'A' : c;
}

#ifndef PUB_FUNC
# define PUB_FUNC
#endif

#define ST_INLN
#define ST_FUNC

/* ------------ libtcc.c ------------ */

#define AFF_PRINT_ERROR     0x0001 /* print error if file not found */
#define AFF_REFERENCED_DLL  0x0002 /* load a referenced dll from another dll */
#define AFF_PREPROCESS      0x0004 /* preprocess file */

/* public functions currently used by the tcc main function */
PUB_FUNC char *pstrcpy(char *buf, int buf_size, const char *s);
PUB_FUNC char *pstrcat(char *buf, int buf_size, const char *s);
PUB_FUNC char *pstrncpy(char *out, const char *in, size_t num);
PUB_FUNC char *tcc_basename(const char *name);
PUB_FUNC char *tcc_fileextension (const char *name);
PUB_FUNC void tcc_free(void *ptr);
PUB_FUNC void *tcc_malloc(unsigned long size);
PUB_FUNC void *tcc_mallocz(unsigned long size);
PUB_FUNC void *tcc_realloc(void *ptr, unsigned long size);
PUB_FUNC char *tcc_strdup(const char *str);
PUB_FUNC void tcc_memstats(void);
PUB_FUNC void tcc_error_noabort(TCCState *s1, const char *fmt, ...);
PUB_FUNC void tcc_error(const char *fmt, ...);
PUB_FUNC void tcc_warning(TCCState *s1, const char *fmt, ...);

/* other utilities */
ST_FUNC void dynarray_add(void ***ptab, int *nb_ptr, void *data);
ST_FUNC void dynarray_reset(void *pp, int *n);
ST_FUNC void cstr_ccat(CString *cstr, int ch);
ST_FUNC void cstr_cat(CString *cstr, const char *str);
ST_FUNC void cstr_wccat(CString *cstr, int ch);
ST_FUNC void cstr_new(CString *cstr);
ST_FUNC void cstr_free(CString *cstr);
ST_FUNC void cstr_reset(CString *cstr);

ST_FUNC void greloc(Section *s, Sym *sym, unsigned long offset, int type);

ST_INLN void sym_free(TCCState *s1, Sym *sym);
ST_FUNC Sym *sym_push2(TCCState *s1, Sym **ps, int v, int t, long c);
ST_FUNC Sym *sym_find2(Sym *s, int v);

ST_FUNC void tcc_open_bf(TCCState *s1, const char *filename, int initlen);
ST_FUNC int tcc_open(TCCState *s1, const char *filename);
ST_FUNC void tcc_close(TCCState *s1);

ST_FUNC int tcc_add_file_internal(TCCState *s1, const char *filename, int flags);

PUB_FUNC int tcc_parse_args(TCCState *s, int argc, const char **argv);

/* ------------ tccpp.c ------------ */

#define TOK_FLAG_BOL   0x0001 /* beginning of line before */
#define TOK_FLAG_BOF   0x0002 /* beginning of file before */
#define TOK_FLAG_ENDIF 0x0004 /* a endif was found matching starting #ifdef */
#define TOK_FLAG_EOF   0x0008 /* end of file */

#define PARSE_FLAG_PREPROCESS 0x0001 /* activate preprocessing */
#define PARSE_FLAG_TOK_NUM    0x0002 /* return numbers instead of TOK_PPNUM */
#define PARSE_FLAG_LINEFEED   0x0004 /* line feed is returned as a
                                        token. line feed is also
                                        returned at eof */
#define PARSE_FLAG_ASM_COMMENTS 0x0008 /* '#' can be used for line comment */
#define PARSE_FLAG_SPACES     0x0010 /* next() returns space tokens (for -E) */

ST_FUNC TokenSym *tok_alloc(TCCState *s1, const char *str, int len);
ST_FUNC char *get_tok_str(TCCState *s1, int v, CValue *cv);
ST_FUNC void save_parse_state(TCCState *s1, ParseState *s);
ST_FUNC void restore_parse_state(TCCState *s1, ParseState *s);
ST_INLN void tok_str_new(TokenString *s);
ST_FUNC void tok_str_free(int *str);
ST_FUNC void tok_str_add(TokenString *s, int t);
ST_FUNC void tok_str_add_tok(TCCState *s1, TokenString *s);
ST_INLN void define_push(TCCState *s1, int v, int macro_type, int *str, Sym *first_arg);
ST_FUNC void define_undef(TCCState *s1, Sym *s);
ST_INLN Sym *define_find(TCCState *s1, int v);
ST_FUNC void free_defines(TCCState *s1, Sym *b);
ST_FUNC void parse_define(TCCState *s1);
ST_FUNC void preprocess(TCCState *s1, int is_bof);
ST_FUNC void next_nomacro(TCCState *s1);
ST_FUNC void next(TCCState *s1);
ST_INLN void unget_tok(TCCState *s1, int last_tok);
ST_FUNC void preprocess_init(TCCState *s1);
ST_FUNC void preprocess_new(TCCState *s1);
ST_FUNC int tcc_preprocess(TCCState *s1);
ST_FUNC void skip(TCCState *s1, int c);
ST_FUNC void expect(const char *msg);

/* input_buf_ptr must be \0 terminated! */
ST_FUNC int tcc_in_memory_preprocess(TCCState *s1,
									 const uint8_t* input_buf_ptr, const size_t input_length,
									 void* user_state,
									 void (*output_write_func)(const char* str, void* user_state));

/* ------------ tccgen.c ------------ */

#define SYM_POOL_NB (8192 / sizeof(Sym))
#define vstack  (s1->__vstack + 1)

ST_INLN int is_float(int t);
ST_FUNC int ieee_finite(double d);
ST_FUNC void test_lvalue(TCCState *s1);
ST_FUNC void swap(int *p, int *q);
ST_FUNC void vpushi(TCCState *s1, int v);
ST_FUNC void vset(TCCState *s1, CType *type, int r, int v);
ST_FUNC void vswap(TCCState *s1);
ST_FUNC void vrote(SValue *e, int n);
ST_FUNC void vrott(TCCState *s1, int n);
ST_FUNC void vrotb(TCCState *s1, int n);
ST_FUNC void vpushv(TCCState *s1, SValue *v);
ST_FUNC void vpop(TCCState *s1);
ST_FUNC void gen_op(TCCState *s1, int op);
ST_FUNC void mk_pointer(CType *type);
ST_FUNC void vstore(TCCState *s1);
ST_FUNC void inc(TCCState *s1, int post, int c);
ST_FUNC void parse_asm_str(CString *astr);
ST_FUNC int lvalue_type(int t);
ST_FUNC void indir(TCCState *s1);
ST_FUNC void unary(TCCState *s1);
ST_FUNC void expr_prod(TCCState *s1);
ST_FUNC void expr_sum(TCCState *s1);
ST_FUNC void gexpr(TCCState *s1);
ST_FUNC int expr_const(TCCState *s1);
ST_FUNC void decl(int l);
ST_FUNC void vpush(TCCState *s1, CType *type);
ST_FUNC void vpush64(TCCState *s1, int ty, unsigned long long v);

/********************************************************/
#endif /* _TCC_H */
