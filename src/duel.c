/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/duel.c,v 1.11 93/03/13 04:03:07 mg Exp $*/

/* this module contains the entery point to duel - duel_eval_and_pasre.
 */

/*
 * $Log:        duel.c,v $
 * Revision 1.11  93/03/13  04:03:07  mg
 * moved VERSION to patchlevel.h
 *
 * Revision 1.10  93/03/12  05:41:54  mg
 * Version 1.10 - support (x)y cast, piped output.
 *
 * Revision 1.9  93/02/23  19:09:35  mg
 * new version 1.02 release (support gdb4.8)
 *
 * Revision 1.8  93/02/03  21:56:33  mg
 * version 1.01
 *
 * Revision 1.7  93/01/12  21:28:44  mg
 * cleanup and set for release
 *
 * Revision 1.6  93/01/06  23:57:10  mg
 * added alias, clear commands, new memory alloc/release
 *
 * Revision 1.5  93/01/03  07:26:47  mg
 * new printing setup
 *
 * Revision 1.4  92/12/24  23:32:38  mg
 * better struct support, misc changes
 *
 * Revision 1.3  92/10/19  15:02:04  mg
 * lcc happy, size zero arrays
 *
 * Revision 1.2  92/10/14  02:03:15  mg
 * misc
 *
 */

#include <setjmp.h>
#define DEF             /* define global variables */
#include "duel.h"
#include "patchlevel.h"

static jmp_buf duel_abort_jmp ; /* abort current execution */
static tnode *root ;            /* root of current eval node */


/* abort evaluation of current expression */

PROC duel_abort(void)
{
    longjmp(duel_abort_jmp,1);
}

PROC duel_cleanup(void) /* cleanup malloc, etc, when duel eval ends*/
{
    duel_redirectable_output_abort();
    duel_free_nodes(root);
    root=NULL ;
}

LPROC help(void)
{
        duel_printf(
"Duel - Debugging U (might) Even Like -- A high level debugging language\n\n\
Duel was designed to overcome problems with traditional debuggers' print\n\
statement. It supports the C operators, many C constructs, and many new\n\
operators for easy exploration of the program's space, e.g.\n\
x[..100] >? 0                 show positive x[i] for i=0 to 99\n\
y[10..20].code !=? 0          show non-zero y[i].code for i=10 to 20\n\
h-->next->code                expand linked list h->next, h->next->next ...\n\
head-->next.if(code>0) name   show name for each element with code>0\n\
x[i:=..100]=y[i];             array copy. i is an alias to vals 0..99\n\
head-->next[[10..15]]         the 10th to 15th element of a linked list\n\
#/(head-->next->val==?4)      count elements with val==4\n\
head-->next->if(next) val >? next->val    check if list is sorted by val\n\
\n\
Duel was written by Michael Golan at Princeton University. Send email to\n\
mg@cs.princeton.edu. Duel is public domain code. No copy left or right.\n\
all but 500 lines are independent of gdb. Port it! Make it Commercial!\n\
\n\
Try \"dl ops\" for op summary; \"dl\" alone lists all commands\n");
}

LPROC examples(void)
{
    duel_printf("\
x[10..20,22,24,40..60]    display x[i] for the selected indexes\n\
x[9..0]                   display x[i] backwards\n\
x[..100] >? 5 <? 10       display x[i] if 5<x[i]<10\n\
x[0..99]=>if(_>5 && _<10) _     same\n\
val[..50].if(is_dx) x else y   \
val[i].x or val[i].y depending on val[i].is_dx\n\
emp[..50].if(is_m) _      return emp[i] if emp[i].is_m.\n\
x[i:=..100]=y[i] ;        assign y[i] to x[i]\n\
x[i:=..100] >? x[i+1]     check if x[i] is not sorted\n\
(x[..100] >? 0)[[2]]      return the 3rd positive x[i]\n\
argv[0..]@0               argv[0] argv[1] .. until first null\n\
emp[0..]@(code==0)        emp[0]..emp[n-1] where emp[n].code==0\n\
head-->next->val          val of each element in a linked list\n\
*head-->next[[20]]        element 20 of list, '*' display struct w/fields\n\
#/head-->next             count elements on a linked list\n\
#/(head-->next-val>?5)    count those over 5\n\
head-->(next!=?head)      expand cyclic linked list (tail->head)\n\
T mytype x ;              declare var for user defined type (need 'T')\n\
int i ; for(i=0 ;i<5 ..   declare variable, use C construct.\n");
}

LPROC operators(void)
{
    duel_printf("\
DUEL operators in decreasing precedence. All C operators are accepted!\n\
note: precede typedefs by 'T', eg \"sizeof(T uint)\"\n\n\
{x}     same as (x) but x's value is used for symbol                emp[{i}]\n\
x-->y   expands data structure from x, using y      root-->(left,right)->key\n\
x.y     eval y under x's scope, like pascal \"with\". x is accesible as '_'\n\
x->y    same as (*x).y.                             hash[..50]->if(_!=0) key\n\
x[[y]]  select the y'th elements of x                     head-->next[[..6]]\n\
x@y     eval x, stop as soon as y true (_==y if y const).        argv[0..]@0\n\
x#y     eval x, set alias y as values counter        (x-->next#n)->level==?n");
    duel_printf("\n\
#/x     count number of values from x                          #/head-->next\n\
frame(n) use with '.' to reference stack frames.                frame(3).val\n\
x..y    x, x+1, x+2 .. y. (if x>y, return them backwards)          x[10..20]\n\
..y     like 0..y-1                                                x[..1024]\n\
x..     like x..maxint. Caution: use only with x@y or x[[y]]     name[0..]@0\n\
x>?y    x if x>y else nothing. also <? <=? >=? ==? and !=?        x[..10]<?0\n\
&&/x    1 or 0 if x!=0 for all x values. ||/x similar         &&/x[..100]>=0\n\
x,y     x, then y.                                    head-->next->(val,key)\n\
x=>y    eval y for each x, setting '_' to x's values            x[..50]=>_*_\n\
if(x) y C statements are operators. Also for(), while(), if() else\n\
x;y     Evaluate and ignore x's value, then return y\n");
}

/* entry point into duel. s is the expression to evaluate */

void duel_parse_and_eval(char *s)
{
   static int first = 1 ;
   if(first)  {                 /* init stuff */
      duel_init_basic_ctypes();
      duel_printf("%s.%d, public domain debugging language. \"dl\" for help\n",
               VERSION,PATCHLEVEL);
      duel_redirectable_output_init();
      first=0 ;
   }
   if(!s || *s==0) {  /* no input, give some help */
       duel_printf("\
Supported DUEL commands: (see man page duel(1))\n\
duel help     - give basic help\n\
duel examples - show useful usage examples\n\
duel ops      - operators summary\n\
duel alias    - show current aliases\n\
duel clear    - clear all aliases\n\n");
       return ;
   }
   if(strcmp(s,"?")==0 || strcmp(s,"help")==0) {
       help();
       return ;
   }
   if(strcmp(s,"examples")==0 || strcmp(s,"ex")==0 ) {
       examples();
       return ;
   }
   if(strcmp(s,"operators")==0 || strcmp(s,"ops")==0) {
       operators();
       return ;
   }
   if(strcmp(s,"debug")==0) {           /* turn debugging of duel itself */
       duel_debug= !duel_debug ;
       duel_printf("duel debug mode %d\n",duel_debug);
       return ;
   }
   if(strcmp(s,"clear")==0) {
       duel_clear_aliases();
       duel_printf("Aliases table cleared\n");
       return ;
   }
   if(strcmp(s,"alias")==0 || strcmp(s,"aliases")==0) {
       duel_show_aliases();
       return ;
   }
   if(setjmp(duel_abort_jmp)==0) {              /* set abort point */
     if((root=duel_parse(s))!=NULL) {
       tvalue v ;
       duel_set_input_string(s);          /* for src-location err management */
       duel_reset_eval();
       duel_redirectable_output_start(s); /* allow eval output to go to pipe */
       while(duel_eval(root,&v)) {
           duel_print_value(&v);
           duel_flush();
       }
       duel_redirectable_output_end();
     }
   }
   duel_cleanup();
}
