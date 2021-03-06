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
#include <stdlib.h>

/********************************************************/
/* global variables */

/********************************************************/
/* copy a string and truncate it. */
PUB_FUNC char *pstrcpy(char *buf, int buf_size, const char *s)
{
    char *q, *q_end;
    int c;

    if (buf_size > 0) {
        q = buf;
        q_end = buf + buf_size - 1;
        while (q < q_end) {
            c = *s++;
            if (c == '\0')
                break;
            *q++ = (char)c;
        }
        *q = '\0';
    }
    return buf;
}

/* strcat and truncate. */
PUB_FUNC char *pstrcat(char *buf, int buf_size, const char *s)
{
    int len = (int)strlen(buf);
    if (len < buf_size) 
        pstrcpy(buf + len, buf_size - len, s);
    return buf;
}

PUB_FUNC char *pstrncpy(char *out, const char *in, size_t num)
{
    memcpy(out, in, num);
    out[num] = '\0';
    return out;
}

/* extract the basename of a file */
PUB_FUNC char *tcc_basename(const char *name)
{
    char *p = strchr(name, 0);
    while (p > name && !IS_DIRSEP(p[-1]))
        --p;
    return p;
}

/* extract extension part of a file
 *
 * (if no extension, return pointer to end-of-string)
 */
PUB_FUNC char *tcc_fileextension (const char *name)
{
    char *b = tcc_basename(name);
    char *e = strrchr(b, '.');
    return e ? e : strchr(b, 0);
}

/********************************************************/
/* memory management */

PUB_FUNC void tcc_free(void *ptr)
{
    free(ptr);
}

PUB_FUNC void *tcc_malloc(unsigned long size)
{
    return malloc(size);
}

PUB_FUNC void *tcc_mallocz(unsigned long size)
{
    void *ptr;
    ptr = tcc_malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

PUB_FUNC void *tcc_realloc(void *ptr, unsigned long size)
{
    void *ptr1;
    ptr1 = realloc(ptr, size);
    return ptr1;
}

PUB_FUNC char *tcc_strdup(const char *str)
{
    char *ptr;
	const size_t size = strlen(str) + 1;
    ptr = tcc_malloc(size);
	strncpy(ptr, str, size);
    return ptr;
}

/********************************************************/
/* dynarrays */

ST_FUNC void dynarray_add(void ***ptab, int *nb_ptr, void *data)
{
	int nb, nb_alloc;
    void **pp;
    
    nb = *nb_ptr;
    pp = *ptab;
    /* every power of two we double array size */
    if ((nb & (nb - 1)) == 0) {
        if (!nb)
            nb_alloc = 1;
        else
            nb_alloc = nb * 2;
        pp = tcc_realloc(pp, (size_t)nb_alloc * sizeof(void *));
        *ptab = pp;
    }
	pp[nb] = data;
	*nb_ptr = ++nb;
}

ST_FUNC void dynarray_reset(void *pp, int *n)
{
    void **p;
    for (p = *(void***)pp; *n; ++p, --*n)
        if (*p)
            tcc_free(*p);
    tcc_free(*(void**)pp);
    *(void**)pp = NULL;
}

static void tcc_split_path(TCCState *s tcc_unused, void ***p_ary, int *p_nb_ary, const char *in)
{
    const char *p;
    do {
        int c;
        CString str;

        cstr_new(&str);
        for (p = in; c = *p, c != '\0' && c != PATHSEP; ++p) {
            if (c == '{' && p[1] && p[2] == '}') {
                p += 2;
            } else {
                cstr_ccat(&str, c);
            }
        }
        cstr_ccat(&str, '\0');
        dynarray_add(p_ary, p_nb_ary, str.data);
        in = p+1;
    } while (*p);
}


/********************************************************/

static void strcat_vprintf(char *buf, int buf_size, const char *fmt, va_list ap)
{
    int len;
    len = (int)strlen(buf);
	
#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wformat-nonliteral"
#endif
    vsnprintf(buf + len, (size_t)(buf_size - len), fmt, ap);
#if defined(__clang__)
#pragma clang diagnostic pop
#endif
}

static void strcat_printf(char *buf, int buf_size, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    strcat_vprintf(buf, buf_size, fmt, ap);
    va_end(ap);
}

static void error1(TCCState *s1, int is_warning, const char *fmt, va_list ap)
{
    char buf[2048];
    BufferedFile **pf, *f = NULL;
    
    buf[0] = '\0';
    /* use upper file if inline ":asm:" or token ":paste:" */
	if(s1 != NULL) {
		for (f = s1->file; f && f->filename[0] == ':'; f = f->prev);
	}
    if (f && s1) {
        for(pf = s1->include_stack; pf < s1->include_stack_ptr; pf++)
            strcat_printf(buf, sizeof(buf), "In file included from %s:%d:\n",
                (*pf)->filename, (*pf)->line_num);
        if (f->line_num > 0) {
            strcat_printf(buf, sizeof(buf), "%s:%d: ",
                f->filename, f->line_num);
        } else {
            strcat_printf(buf, sizeof(buf), "%s: ",
                f->filename);
        }
    } else {
        strcat_printf(buf, sizeof(buf), "tcc: ");
    }
    if (is_warning)
        strcat_printf(buf, sizeof(buf), "warning: ");
    else
        strcat_printf(buf, sizeof(buf), "error: ");
    strcat_vprintf(buf, sizeof(buf), fmt, ap);

    if (s1 == NULL || !s1->error_func) {
        /* default case: stderr */
#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdisabled-macro-expansion"
#endif
        fprintf(stderr, "%s\n", buf);
#if defined(__clang__)
#pragma clang diagnostic pop
#endif
    } else {
        s1->error_func(s1->error_opaque, buf);
    }
	if (s1 != NULL && (!is_warning || s1->warn_error)) {
        s1->nb_errors++;
	}
}

LIBTCCAPI void tcc_set_error_func(TCCState *s, void *error_opaque,
                        void (*error_func)(void *opaque, const char *msg))
{
    s->error_opaque = error_opaque;
    s->error_func = error_func;
}

/* error without aborting current compilation */
PUB_FUNC void tcc_error_noabort(TCCState *s1, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
}

PUB_FUNC void __attribute__((noreturn)) tcc_error(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    error1(NULL, 0, fmt, ap);
    va_end(ap);
	
	/* XXX: eliminate this someday */
	exit(1);
}

PUB_FUNC void tcc_warning(TCCState *s1, const char *fmt, ...)
{
    va_list ap;

    if (s1->warn_none)
        return;

    va_start(ap, fmt);
    error1(s1, 1, fmt, ap);
    va_end(ap);
}

/********************************************************/
/* I/O layer */

ST_FUNC void tcc_open_bf(TCCState *s1, const char *filename, int initlen)
{
    BufferedFile *bf;
    int buflen = initlen ? initlen : IO_BUF_SIZE;

    bf = tcc_malloc(sizeof(BufferedFile) + (size_t)buflen);
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer + initlen;
    bf->buf_end[0] = CH_EOB; /* put eob symbol */
    pstrcpy(bf->filename, sizeof(bf->filename), filename);
    bf->line_num = 1;
    bf->ifndef_macro = 0;
    bf->ifdef_stack_ptr = s1->ifdef_stack_ptr;
    bf->fd = -1;
    bf->prev = s1->file;
    s1->file = bf;
}

ST_FUNC void tcc_close(TCCState *s1)
{
    BufferedFile *bf = s1->file;
    if (bf->fd > 0) {
        close(bf->fd);
    }
    s1->file = bf->prev;
    tcc_free(bf);
}

ST_FUNC int tcc_open(TCCState *s1, const char *filename)
{
    int fd;
    if (strcmp(filename, "-") == 0)
        fd = 0, filename = "stdin";
    else
        fd = open(filename, O_RDONLY | O_BINARY);
    if ((s1->verbose == 2 && fd >= 0) || s1->verbose == 3)
        printf("%s %*s%s\n", fd < 0 ? "nf":"->",
               (int)(s1->include_stack_ptr - s1->include_stack), "", filename);
    if (fd < 0)
        return -1;

    tcc_open_bf(s1, filename, 0);
    s1->file->fd = fd;
    return fd;
}

/* define a preprocessor symbol. A value can also be provided with the '=' operator */
LIBTCCAPI void tcc_define_symbol(TCCState *s1, const char *sym, const char *value)
{
    int len1, len2;
    /* default value */
    if (!value)
        value = "1";
    len1 = (int)strlen(sym);
    len2 = (int)strlen(value);

    /* init file structure */
    tcc_open_bf(s1, "<define>", len1 + len2 + 1);
    memcpy(s1->file->buffer, sym, (size_t)len1);
    s1->file->buffer[len1] = ' ';
    memcpy(s1->file->buffer + len1 + 1, value, (size_t)len2);

    /* parse with define parser */
    s1->ch = s1->file->buf_ptr[0];
    next_nomacro(s1);
    parse_define(s1);

    tcc_close(s1);
}

/* undefine a preprocessor symbol */
LIBTCCAPI void tcc_undefine_symbol(TCCState *s1, const char *sym)
{
    TokenSym *ts;
    Sym *s;
    ts = tok_alloc(s1, sym, (int)strlen(sym));
    s = define_find(s1, ts->tok);
    /* undefine symbol by putting an invalid name */
    if (s)
        define_undef(s1, s);
}

/* cleanup all static data used during compilation */
static void tcc_cleanup(TCCState *s1)
{
    int i, n;
    if(s1 == NULL) return;

    /* free -D defines */
    free_defines(s1, NULL);

    /* free tokens */
    n = s1->tok_ident - TOK_IDENT;
    for(i = 0; i < n; i++)
        tcc_free(s1->table_ident[i]);
    tcc_free(s1->table_ident);

    /* free sym_pools */
    dynarray_reset(&s1->sym_pools, &s1->nb_sym_pools);
    /* string buffer */
    cstr_free(&s1->tokcstr);
    /* reset symbol stack */
    s1->sym_free_first = NULL;
    /* cleanup from error/setjmp */
    s1->macro_ptr = NULL;
}

LIBTCCAPI TCCState *tcc_new(void)
{
    TCCState *s = NULL;
    char buffer[100];
    int a,b,c;

    tcc_cleanup(s);

    s = tcc_mallocz(sizeof(TCCState));
    s->output_type = TCC_OUTPUT_MEMORY;
    preprocess_new(s);
    s->include_stack_ptr = s->include_stack;

    /* we add dummy defines for some special macros to speed up tests
       and to have working defined() */
    define_push(s, TOK___LINE__, MACRO_OBJ, NULL, NULL);
    define_push(s, TOK___FILE__, MACRO_OBJ, NULL, NULL);
    define_push(s, TOK___DATE__, MACRO_OBJ, NULL, NULL);
    define_push(s, TOK___TIME__, MACRO_OBJ, NULL, NULL);

    /* define __TINYC__ 92X  */
    sscanf(TCC_VERSION, "%d.%d.%d", &a, &b, &c);
    sprintf(buffer, "%d", a*10000 + b*100 + c);
    tcc_define_symbol(s, "__TINYC__", buffer);

    /* standard defines */
    tcc_define_symbol(s, "__STDC__", NULL);
    tcc_define_symbol(s, "__STDC_VERSION__", "199901L");
	
	/* OpenCL */
    tcc_define_symbol(s, "CL_VERSION_1_0", "100");
    tcc_define_symbol(s, "CL_VERSION_1_1", "110");
    tcc_define_symbol(s, "CL_VERSION_1_2", "120");
    tcc_define_symbol(s, "__OPENCL_VERSION__", "120");
    tcc_define_symbol(s, "__IMAGE_SUPPORT__", "1");

    /* no section zero */
    dynarray_add((void ***)&s->sections, &s->nb_sections, NULL);

#ifdef CHAR_IS_UNSIGNED
    s->char_is_unsigned = 1;
#endif
    return s;
}

LIBTCCAPI void tcc_delete(TCCState *s1)
{
    tcc_cleanup(s1);
    
    /* free loaded dlls array */
    dynarray_reset(&s1->loaded_dlls, &s1->nb_loaded_dlls);

    /* free library paths */
    dynarray_reset(&s1->library_paths, &s1->nb_library_paths);
    dynarray_reset(&s1->crt_paths, &s1->nb_crt_paths);

    /* free include paths */
    dynarray_reset(&s1->cached_includes, &s1->nb_cached_includes);
    dynarray_reset(&s1->include_paths, &s1->nb_include_paths);
    dynarray_reset(&s1->sysinclude_paths, &s1->nb_sysinclude_paths);

    tcc_free(s1->soname);
    tcc_free(s1->rpath);
    tcc_free(s1->init_symbol);
    tcc_free(s1->fini_symbol);
    dynarray_reset(&s1->files, &s1->nb_files);
    dynarray_reset(&s1->target_deps, &s1->nb_target_deps);

    tcc_free(s1);
}

LIBTCCAPI int tcc_add_include_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, (void ***)&s->include_paths, &s->nb_include_paths, pathname);
    return 0;
}

LIBTCCAPI int tcc_add_sysinclude_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, (void ***)&s->sysinclude_paths, &s->nb_sysinclude_paths, pathname);
    return 0;
}

ST_FUNC int tcc_add_file_internal(TCCState *s1, const char *filename, int flags)
{
    const char *ext;
    int ret;

    /* find source file type with extension */
    ext = tcc_fileextension(filename);
    if (ext[0])
        ext++;

    /* open the file */
    ret = tcc_open(s1, filename);
    if (ret < 0) {
        if (flags & AFF_PRINT_ERROR)
            tcc_error_noabort(s1, "file '%s' not found", filename);
        return ret;
    }

    /* update target deps */
    dynarray_add((void ***)&s1->target_deps, &s1->nb_target_deps,
				 tcc_strdup(filename));

    if (flags & AFF_PREPROCESS) {
        ret = tcc_preprocess(s1);
        goto the_end;
    }

    ret = -1;
    if (ret < 0)
        tcc_error_noabort(s1, "unrecognized file type");

the_end:
    tcc_close(s1);
    return ret;
}

LIBTCCAPI int tcc_add_file(TCCState *s, const char *filename)
{
	return tcc_add_file_internal(s, filename, AFF_PRINT_ERROR | AFF_PREPROCESS);
}

static int strstart(const char *val, const char **str)
{
    const char *p, *q;
    p = *str;
    q = val;
    while (*q) {
        if (*p != *q)
            return 0;
        p++;
        q++;
    }
    *str = p;
    return 1;
}

typedef struct TCCOption {
    const char *name;
    uint16_t index;
    uint16_t flags;
} TCCOption;

enum {
    TCC_OPTION_I,
    TCC_OPTION_D,
    TCC_OPTION_U,
    TCC_OPTION_v,
    TCC_OPTION_E,
};

#define TCC_OPTION_HAS_ARG 0x0001
#define TCC_OPTION_NOSEP   0x0002 /* cannot have space before option and arg */

static const TCCOption tcc_options[] = {
    { "I", TCC_OPTION_I, TCC_OPTION_HAS_ARG },
    { "D", TCC_OPTION_D, TCC_OPTION_HAS_ARG },
    { "U", TCC_OPTION_U, TCC_OPTION_HAS_ARG },
    { "v", TCC_OPTION_v, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "E", TCC_OPTION_E, 0},
    { NULL, 0, 0 },
};

static void parse_option_D(TCCState *s1, const char *optarg_)
{
    char *sym = tcc_strdup(optarg_);
    char *value = strchr(sym, '=');
    if (value)
        *value++ = '\0';
    tcc_define_symbol(s1, sym, value);
    tcc_free(sym);
}

PUB_FUNC int tcc_parse_args(TCCState *s, int argc, const char **argv)
{
    const TCCOption *popt;
    const char *optarg_, *r;
    int optind_ = 0;
	
	s->warn_none = 1;
    while (optind_ < argc) {

        r = argv[optind_++];
        if (r[0] != '-' || r[1] == '\0') {
            /* add a new file */
            dynarray_add((void ***)&s->files, &s->nb_files, tcc_strdup(r));
            continue;
        }

        /* find option in table */
        for(popt = tcc_options; ; ++popt) {
            const char *p1 = popt->name;
            const char *r1 = r + 1;
            if (p1 == NULL)
                tcc_error("invalid option -- '%s'", r);
            if (!strstart(p1, &r1))
                continue;
            optarg_ = r1;
            if (popt->flags & TCC_OPTION_HAS_ARG) {
                if (*r1 == '\0' && !(popt->flags & TCC_OPTION_NOSEP)) {
                    if (optind_ >= argc)
                        tcc_error("argument to '%s' is missing", r);
                    optarg_ = argv[optind_++];
                }
            } else if (*r1 != '\0')
                continue;
            break;
        }

        switch(popt->index) {
			case TCC_OPTION_I:
				if (tcc_add_include_path(s, optarg_) < 0)
					tcc_error("too many include paths");
				break;
			case TCC_OPTION_D:
				parse_option_D(s, optarg_);
				break;
			case TCC_OPTION_U:
				tcc_undefine_symbol(s, optarg_);
				break;
			case TCC_OPTION_v:
				do ++s->verbose; while (*optarg_++ == 'v');
				break;
			case TCC_OPTION_E:
				s->output_type = TCC_OUTPUT_PREPROCESS;
				break;
			default:
				break;
        }
    }

    return optind_;
}

LIBTCCAPI int tcc_set_options(TCCState *s, const char *str)
{
    const char *s1;
    const char **argv;
	char *arg;
    int argc;
    int ret;
	size_t len;

    argc = 0, argv = NULL;
    for(;;) {
        while (is_space(*str))
            str++;
        if (*str == '\0')
            break;
        s1 = str;
        while (*str != '\0' && !is_space(*str))
            str++;
        len = (size_t)(str - s1);
        arg = tcc_malloc((len + 1));
        pstrncpy(arg, s1, len);
        dynarray_add((void ***)&argv, &argc, arg);
    }
    ret = tcc_parse_args(s, argc, argv);
    dynarray_reset(&argv, &argc);
    return ret;
}

/********************************************************/
/* standalone binary */

#if !defined(TCC_LIB_ONLY)

static void help(void)
{
    printf("tcc version " TCC_VERSION " - Tiny C Compiler - Copyright (C) 2001-2006 Fabrice Bellard\n"
           "Usage: tcc [options...] infile(s)...\n"
           "General options:\n"
           "  -v          show version\n"
           "Preprocessor options:\n"
           "  -E          preprocess only\n"
           "  -Idir       add include path 'dir'\n"
           "  -Dsym[=val] define 'sym' with value 'val'\n"
           "  -Usym       undefine 'sym'\n"
           );
}

int main(int argc, const char* argv[]) {
    TCCState *s;
    int ret, optind_, i;
    const char *first_file = NULL;
	
    s = tcc_new();
    s->output_type = TCC_OUTPUT_EXE;
	
    optind_ = tcc_parse_args(s, argc - 1, argv + 1);
	
    if (optind_ == 0) {
        help();
        return 1;
    }
	
    if (s->verbose)
        printf("tcc version %s\n", TCC_VERSION);
	
    if (s->verbose && optind_ == 1)
        return 0;
	
    if (s->nb_files == 0)
        tcc_error("no input files\n");
    
    if (s->output_type == TCC_OUTPUT_PREPROCESS) {
#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdisabled-macro-expansion"
#endif
        s->ppfp = stdout;
#if defined(__clang__)
#pragma clang diagnostic pop
#endif
    }
	
    /* compile or add each files or library */
    for(i = ret = 0; i < s->nb_files && ret == 0; i++) {
        const char *filename;
		
        filename = s->files[i];
		if (1 == s->verbose)
			printf("-> %s\n", filename);
		if (tcc_add_file(s, filename) < 0)
			ret = 1;
		if (!first_file)
			first_file = filename;
    }
	
    if (0 == ret) {
        if (s->output_type == TCC_OUTPUT_MEMORY) {
            tcc_error_noabort(s, "-run is not available");
            ret = 1;
        }
    }
	
    tcc_delete(s);
    return ret;
}

#endif
