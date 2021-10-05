/************************************************************************/
/*              putpatch.c              */
/*                                                                      */
/* Public-domain code by Jeff Prothero (jsp@glia.biostr.washington.edu) */
/*                                                                      */
/* This program expands special comments in the xlisp-plus source code  */
/* into #include sequences.  It is useful when using xlisp-plus as an   */
/* application language, the includes allow a complete application to   */
/* be built around xlisp-plus without hacking the regular xlisp-plus    */
/* source files.  Aside from simple cleanliness, the big advantage of   */
/* this is that you will be able to effortlessly plug new releases      */
/* of xlisp-plus into your application as they become available.        */
/*                                                                      */
/* This program looks for comments in the source of the form            */
/*                                                                      */
/*     /* $putpatch.c$: "MODULE_XLISP_C_WRAPUP" */
/*                                                                      */
/* and expands them into text looking like:                             */
/*                                                                      */
/*     #define MODULE_XLISP_C_WRAPUP                                    */
/*     #include "../../xmodules.h"                                      */
/*     #undef MODULE_XLISP_C_WRAPUP                                     */
/*                                                                      */
/* where the #included file is UNIVERSAL_INCLUDE, below.                */
/*                                                                      */
/* You should be able to compile with simply:                           */
/*     cc putpatch.c -o putpatch                                        */
/*                                                                      */
/* Normal use is                                                        */
/*     putpatch *.h *.c                                                 */
/* in the main xlisp-plus source directory.                             */
/*                                                                      */
/* Docs and examples of using xlisp-plus as an application engine       */
/* are available, but are not part of the standard xlisp-plus           */
/* distribution.  Contact jsp@glia.biostr.washington.edu.               */
/************************************************************************/

#define UNIVERSAL_INCLUDE "../../xmodules.h"
#define MAX_UNIVERSAL_INCLUDE (256)
char universal_include[ MAX_UNIVERSAL_INCLUDE ];

/************************************************************************/
/*              contents                */
/*. do_prehacks                                                     */
/*. do_posthacks                                                    */
/*. patch_a_file                                                    */
/*. usage                                                           */
/*. main                                                            */
/*                                                                      */
/*      (generated via "grep '/\*\.' putpatch.c")       */
/************************************************************************/

/************************************************************************/
/*              history                 */
/*                                                                      */
/* 92Jan27 CrT:  Created.  Operational.                                 */
/************************************************************************/

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#define PATCH_COOKIE " /* $putpatch.c$: \"%[^\"]\" */"

/* Following line is just so we can selftest */
/* $putpatch.c$: "MODULE_PUTPATCH_C_GLOBALS" */

/************************************************************************/
/*. do_prehacks                                                     */
/************************************************************************/
do_prehacks( fout, patch_name )
FILE*        fout;
char*          patch_name;
{
    /*************************************************************/
    /* A couple of patchpoints need to define a special macro.   */
    /* Haven't thought of a nicer mechanism, so it's hacked into */
    /* putpatch.c here.  Yeah, kinda ugly.           */
    /*************************************************************/
    if (!strcmp( patch_name, "MODULE_XLFTAB_C_FUNTAB_S")) {
    fprintf( fout, "#define DEFINE_SUBR(a,b) {a, S, b},\n" );
    }
    if (!strcmp( patch_name, "MODULE_XLFTAB_C_FUNTAB_F")) {
    fprintf( fout, "#define DEFINE_FSUBR(a,b) {a, F, b},\n" );
    }
}

/************************************************************************/
/*. do_posthacks                                                    */
/************************************************************************/
do_posthacks( fout, patch_name )
FILE*         fout;
char*           patch_name;
{
    if (!strcmp( patch_name, "MODULE_XLFTAB_C_FUNTAB_S")) {
    fprintf( fout, "#undef DEFINE_SUBR\n" );
    }
    if (!strcmp( patch_name, "MODULE_XLFTAB_C_FUNTAB_F")) {
    fprintf( fout, "#undef DEFINE_FSUBR\n" );
    }
}

/************************************************************************/
/*. patch_a_file                                                    */
/************************************************************************/
#define MAX_PATCH_BUF (10000)
patch_a_file( filename )
char*         filename; 
{
    FILE* fout;
    char buf[        MAX_PATCH_BUF ];
    char patch_name[ MAX_PATCH_BUF ];
    int  patches_done = 0;

    /* Open the file for input: */
    FILE* f_in = fopen( filename, "r" );
    if (f_in == NULL) {printf("Could not read '%s'\n",filename);exit(1);}

    /* Open a file "filename.N" for output: */
    strcpy( buf, filename );
    strcat( buf, ".N" );
    if (fopen(buf,"r") != NULL) {printf("'%s' already exists!",buf);exit(1);}
    fout = fopen( buf, "w" );
    if (fout == NULL) {printf("Could not create '%s'\n",buf);exit(1);}

    /* Over all lines in input file: */
    while (fgets( buf, MAX_PATCH_BUF, f_in ) != NULL) {

    /* Check for $putpatch.c$: */
    if (sscanf( buf, PATCH_COOKIE, patch_name ) != 1) {

        /* No match, just copy line to output: */
        fputs( buf, fout );

    } else {

        /* Found a patchpoint, replace comment by real patch: */
        fprintf( fout, "#define %s\n", patch_name );
        do_prehacks(  fout, patch_name );
        fprintf( fout, "#include %s\n", UNIVERSAL_INCLUDE );
        do_posthacks( fout, patch_name );
        fprintf( fout, "#undef %s\n", patch_name );
            ++patches_done;
    }   
    }
    fclose( f_in );
    fclose( fout );

    /* If no patches made, no point in keeping output file: */
    strcpy( buf, filename );
    strcat( buf, ".N" );
    if (!patches_done) {
        unlink( buf );
    } else {
        printf(
        "%d patches for %s, result in %s\n",
        patches_done,
        filename,
        buf
    );
    }
    return patches_done != 0;
}

/************************************************************************/
/*. usage                                                           */
/************************************************************************/
usage() {
    printf("usage: putpatch [-u myinclude.h] filename ...\n");
    exit(1);
}

/************************************************************************/
/*. main                                                            */
/************************************************************************/
main( argc, argv )
int   argc;
char**      argv;
{
    int i;
    int count = 0;

    /* Initialize stuff: */
    strcpy( universal_include, UNIVERSAL_INCLUDE );

    for (i = 1;   i < argc;   ++i) {
    char* t = argv[i];
        if (*t == '-') {
        switch (*++t) {
        case 'u':
        strcpy( universal_include, argv[ ++i ] );
        break;
        default:
        usage();
        }
    } else {
            count += patch_a_file( t );
    }
    }

    printf( "%d files patched.\n", count );
    exit(0);
}

