/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code			           */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/duel.h,v 1.4 93/01/12 21:28:47 mg Exp $*/

/* duel.h - include file to be used with all duel source code.
 * it defines important global constants & data types, as well as some
 * global variables. prototypes should go in the duelprot.h file
 */


/* common duel include files. I dunno why people don't like recursive
 * includes, as long as it is consistent. Note only one level of recursion.
 * the debugger module itself dont use this file, instead it includes what
 * it needs directly.
 * as for including "everything", unless one redefine some standard lib func,
 * I feel safer including most standard lib files. It seems to prevent errors
 * even if it garble the name space a bit.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
/* global data and types */

#include "global.h"

/* prototypes for all global functions follows */

#include "proto.h"
