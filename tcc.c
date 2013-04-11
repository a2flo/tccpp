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

#ifdef ONE_SOURCE
#include "libtcc.c"
#else
#include "tcc.h"
#endif

static void help(void)
{
    printf("tcc version " TCC_VERSION " - Tiny C Compiler - Copyright (C) 2001-2006 Fabrice Bellard\n"
           "Usage: tcc [options...] [-o outfile] [-c] infile(s)...\n"
           "       tcc [options...] -run infile [arguments...]\n"
           "General options:\n"
           "  -c          compile only - generate an object file\n"
           "  -o outfile  set output filename\n"
           "  -run        run compiled source\n"
           "  -fflag      set or reset (with 'no-' prefix) 'flag' (see man page)\n"
           "  -Wwarning   set or reset (with 'no-' prefix) 'warning' (see man page)\n"
           "  -w          disable all warnings\n"
           "  -v          show version\n"
           "  -vv         show included files (as sole argument: show search paths)\n"
           "  -dumpversion\n"
           "  -bench      show compilation statistics\n"
           "Preprocessor options:\n"
           "  -E          preprocess only\n"
           "  -Idir       add include path 'dir'\n"
           "  -Dsym[=val] define 'sym' with value 'val'\n"
           "  -Usym       undefine 'sym'\n"
           "Linker options:\n"
           "  -Ldir       add library path 'dir'\n"
           "  -llib       link with dynamic or static library 'lib'\n"
           "  -pthread    link with -lpthread and -D_REENTRANT (POSIX Linux)\n"
           "  -r          generate (relocatable) object file\n"
           "  -rdynamic   export all global symbols to dynamic linker\n"
           "  -shared     generate a shared library\n"
           "  -soname     set name for shared library to be used at runtime\n"
           "  -static     static linking\n"
           "  -Wl,-opt[=val]  set linker option (see manual)\n"
           "Debugger options:\n"
           "  -g          generate runtime debug info\n"
           "Misc options:\n"
           "  -nostdinc   do not use standard system include paths\n"
           "  -nostdlib   do not link with standard crt and libraries\n"
           "  -Bdir       use 'dir' as tcc internal library and include path\n"
           "  -MD         generate target dependencies for make\n"
           "  -MF depfile put generated dependencies here\n"
           );
}

static void print_paths(const char *msg, char **paths, int nb_paths)
{
    int i;
    printf("%s:\n%s", msg, nb_paths ? "" : "  -\n");
    for(i = 0; i < nb_paths; i++)
        printf("  %s\n", paths[i]);
}

static void display_info(TCCState *s, int what)
{
    switch (what) {
    case 0:
        printf("tcc version %s\n", TCC_VERSION);
        break;
    case 1:
        printf("install: %s/\n", s->tcc_lib_path);
        /* print_paths("programs", NULL, 0); */
        print_paths("crt", s->crt_paths, s->nb_crt_paths);
        print_paths("libraries", s->library_paths, s->nb_library_paths);
        print_paths("include", s->sysinclude_paths, s->nb_sysinclude_paths);
        break;
    }
}

int main(int argc, char **argv)
{
    TCCState *s;
    int ret, optind, i;
    const char *first_file = NULL;

    s = tcc_new();
    s->output_type = TCC_OUTPUT_EXE;

    optind = tcc_parse_args(s, argc - 1, argv + 1);

    if (optind == 0) {
        help();
        return 1;
    }

    if (s->verbose)
        display_info(s, 0);

    if (s->verbose && optind == 1)
        return 0;

    if (s->nb_files == 0)
        tcc_error("no input files\n");
    
    if (s->output_type == TCC_OUTPUT_PREPROCESS) {
        s->ppfp = stdout;
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
            tcc_error_noabort("-run is not available");
            ret = 1;
        }
    }

    tcc_delete(s);
    return ret;
}
