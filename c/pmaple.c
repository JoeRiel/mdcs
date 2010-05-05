/* *********************************************************************
 * pmaple.c --- source file for pmaple
 *
 * Author:     Joseph S. Riel <jriel@maplesoft.com>
 * Created:    May 2009
 * Keywords:   maple
 * Repository: //wmi/groups/scripts/share/emacs/emaple/pmaple.c
 *
 * This is the source code for pmaple, which enhances the pipe-line
 * communication with the Maple engine.  The enhancement is trivial
 * (and really should be incorporated into cmaple); it ensures that
 * prompts are flushed to standard output when they are generated.
 * This makes it possible to communicate with the Maple engine in a
 * reasonable way.  With cmaple, the prompts are not flushed until new
 * input arrives, which makes it difficult to determine when a
 * computation is complete.
 *
 ************************************************************************ */

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#include "maplec.h"

#ifdef _MSC_VER
#  define CDECL __cdecl
#else
#  define CDECL
#endif

/* The prompts are hard-coded.  It might be useful to provide an
   option for changing them. */

#define PROMPT "(**) "
#define DBG_PROMPT "(*DBG*) "

/* global variable used by queryInterrupt() */
static int Interrupted = 0;


/* global variable used for redirection */
static FILE *outfile = NULL;

/* {{{ catch_intr */

/* interrupt signal handler: sets global variable when user hits Ctrl-C */
void CDECL catch_intr( int signo )
{
  Interrupted = TRUE;
  signal(SIGINT,catch_intr);
#ifdef _MSC_VER
  signal(SIGBREAK,catch_intr);
#endif
}

/* }}} */
/* {{{ queryInterrupt */

/* interrupt callback: stops computation when `Interrupted' is true */
static M_BOOL M_DECL queryInterrupt( void *data )
{
  if( Interrupted ) {
    Interrupted = FALSE;
    return( TRUE );
  }
  return( FALSE );
}

/* }}} */
/* {{{ readLineCallBack */

/* callback used for capturing user input */

static char* M_DECL readLineCallBack( void *data, M_BOOL debug )
{
    static char input[1024];  /* static so it can be returned */
    static M_BOOL seen_debugger = FALSE;
    if( debug ) {
        if( !seen_debugger ) {
            printf("Entering the debugger.  Type help for list of possible commands\n");
            seen_debugger = TRUE;
        }
	printf("\n%s", DBG_PROMPT);
    }
    else
      printf("\n%s", PROMPT );

    fflush(NULL);
    fgets(input,1024,stdin);
    
    return( input );
}

/* }}} */
/* {{{ redirectCallBack */

static M_BOOL M_DECL redirectCallBack( void *data, char *name, char *mode )
{
    if( !name ) { 
        if( outfile != stdout ) {
	    fclose(outfile);
	    outfile = stdout;
        }
    }
    else {
        if( outfile != stdout ) {
	    fclose(outfile);
        }
        if( !strcmp(name,"default") || !strcmp(name,"terminal") ) {
            outfile = stdout;
            return( TRUE );
        }
	printf("Opening %s\n",name);
        if( (outfile=fopen(name,mode)) == NULL ) {
	    printf("Could not open %s\n",name);
            outfile = stdout;
	    return( FALSE );
        }
    }

    return( TRUE );
}

/* }}} */
/* {{{ streamCallBack */

static char* M_DECL streamCallBack( void *data, char *name,
				     M_INT nargs, char **args )
{
  if( strcmp(name,"app_version") == 0 ) {
    return( "1.1;" );
  }
  printf("unrecognized stream %s\n",name);
  printf("nargs=%d\n", nargs);
  printf("args=%s\n", args[0]);
  return( NULL );
}

/* }}} */
/* {{{ writeHelpChar */

/* callback used for directing help output */
static M_BOOL M_DECL writeHelpChar( void *data, int c )
{
  putchar(c);
  return( FALSE );
}

/* }}} */
/* {{{ textCallBack */

/* callback used for directing result output */
static void M_DECL textCallBack( void *data, int tag, char *output )
{
  fprintf(outfile, "%s\n", output);
}

/* }}} */

/* {{{ main */

/* simple program to print a prompt, get input, evaluate it, 
   and display results 
*/
int main( int argc, char *argv[] )
{
  char expr[1000], err[2048];  /* command input and error string buffers */
  MKernelVector kv;  /* Maple kernel handle */
  MCallBackVectorDesc cb = {  textCallBack, 
			      0,   /* errorCallBack not used */
			      0,   /* statusCallBack not used */
			      readLineCallBack,   
			      redirectCallBack,
			      streamCallBack,  /* streamCallBack not used */
			      queryInterrupt, 
			      0    /* callBackCallBack not used */
  };
  ALGEB dag;  /* eval result (Maple data-structure) */
  int len;

  /* set up default output stream */
  outfile = stdout;

  /* initialize Maple */
  if( (kv=StartMaple(argc,argv,&cb,NULL,NULL,err)) == NULL ) {
    printf("Fatal error, %s\n",err);
    return( 1 );
  }
 
  /* catch ^C 
     Alas, this does not work
  */
  signal(SIGINT,catch_intr);

  printf("    |\\^/|     eMaple\n");
  printf("._|\\|   |/|_. Copyright (c) Maplesoft, a division of Waterloo Maple Inc. 2004\n");
  printf(" \\ eMaple  /  All rights reserved. Maple and OpenMaple are trademarks of\n");
  printf(" <____ ____>  Waterloo Maple Inc.\n");
  printf("      |       Type ? for help.\n");

  /* Print a prompt, get Maple input, evaluate it, ... */

  for( ;; ) {

    printf( PROMPT );
    fflush(NULL); 

    if( !fgets(expr,sizeof(expr),stdin) ) break;

    /* Strip off trailing whitespace (including CR and/or LF). */
    for( len = strlen(expr); len > 0 && isspace(expr[len-1]); --len )
      ;
    expr[len] = '\0'; /* insert terminating null character */

    
    if( expr[0] == '?' ) {
      MapleHelp(kv,expr+1,NULL,writeHelpChar,NULL,80,NULL);
    }
    else {
      dag = EvalMapleStatement(kv,expr);
      if( dag && IsMapleStop(kv,dag) ) 
	break;
    }

  }


  if( outfile != stdout )
    fclose(outfile);

  printf("That's all, folks.\n");
  fflush(NULL);
  return( 0 );
}

/* }}} */

/* Local Variables: */
/* mode: folding */
/* End: */
