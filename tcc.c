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

#if !defined(TCC_LIB_ONLY)

#include "tcc.h"

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
        printf("tcc version %s\n", TCC_VERSION);

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

#endif
