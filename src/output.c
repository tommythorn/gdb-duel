/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/output.c,v 1.1 93/03/12 04:26:16 mg Exp $*/

/* This module supports i/o for duel. Normally output goes to stdout,
 * but it could be piped out instead.
 * this module support minimal duel output w/o pipes. output2.c which
 * support piped i/o is still an alpha version.
 */

#include <varargs.h>  /* for duel_printf */
#include "duel.h"

/* output functions:
 * duel_printf    all duel output goes thru this - like printf()
 * duel_flush     used by duel to flush the output.
 */

PROC duel_printf(va_alist)      /* like printf, but for duel output */
va_dcl
{
  va_list args;
  char *fmt ;
  va_start(args);
  fmt=va_arg(args,char *);
  vfprintf(stdout,fmt,args);
}

PROC duel_flush(void)   /* flush out output from duel */
{
   fflush(stdout);
}

/* tells us the output is "directable" now. s is the expression
 * being evaluated for this */

PROC duel_redirectable_output_start(char *s)
{
   if(s) duel_flush();  /* if(s) is always true. prevents warnings ...*/
}

PROC duel_redirectable_output_end(void)
{
   duel_flush();
}

PROC duel_redirectable_output_abort(void)
{
    duel_flush();
}

PROC duel_redirectable_output_init(void)
{
}
