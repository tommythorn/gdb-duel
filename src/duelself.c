/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/duelself.c,v 1.6 93/03/19 16:55:35 mg Exp $*/

/* self-debugger module, it contains all of duel's access to
 * the outside world (like duelgdb.c), but is intended to work when duel
 * is linked with the debuggee directly (not with the debugger!)
 * if you link this with duel.a, you get a test suite of duel
 * for the C program: (tsuite.c)
 *
 * int gint ;
 * typedef unsigned int uint ;
 * int main() {
 *      printf ; malloc ;       / * include them in bin * /
 *      char *s="main string" ;
 * }
 *
 * You could link this module with your own program and call it at
 * "appropriate places", then ship the program with this code, and
 * use duel at client's site when a crash occurs. However, reading in
 * the symbol table and figuring out the active frames remains a problem.
 */

/*
 * $Log:        duelself.c,v $
 * Revision 1.6  93/03/19  16:55:35  mg
 * allow execution from within make
 *
 * Revision 1.5  93/03/12  05:47:29  mg
 * *** empty log message ***
 *
 * Revision 1.4  93/02/04  00:05:10  mg
 * avoid Header problems in self.out
 *
 * Revision 1.3  93/01/13  16:21:40  mg
 * made sure printf is declared
 *
 * Revision 1.2  93/01/12  21:31:01  mg
 * brought uptodate with duelgdb.c
 * cleanup and set for release
 *
 */
/* include system dependent stuff here */

#include "duel.h"

int main(int argc,char **argv);

FUNC void* duel_malloc(size_t size)
{
  void *p=malloc(size);
  if(p==0) duel_fatal("out of memory.");
  return p;
}

PROC duel_free(void *p)
{
   free(p);
}
/* fetch n bytes from the target at the given memory address.
 * the address to fetch from is given by (from).
 * the value is stored at the 'to' location, which points to space for
 * n bytes in the debugger.
 * if the address can not be accessed, false is returned (if all ok, ret true)
 */

FUNC bool duel_get_target_bytes(ttarget_ptr from,void *to,size_t n)
{
  duel_bcopy(to,from,n);
  return TRUE ;
}

/* store n bytes to the debuggee. reverse parms from above */
FUNC bool duel_put_target_bytes(ttarget_ptr to,void *from,size_t n)
{
   duel_bcopy(to,from,n);
   return TRUE ;
}

/* fetch the value of a bitfield of a given structure. */

FUNC bool duel_get_target_bitfield(ttarget_ptr struct_at,int bitpos,
                                    int bitlen,void *to,tctype_kind tkind)
{
    return FALSE ; /* not supported in duelself */
}

/* make a function call to the target. */
/* support only passing/returing sizeof(int) upto 5 parameters */
PROC duel_target_func_call(tvalue *func, tvalue *parms[],
                            int parms_no,tvalue *rval)
{
    if (parms_no > 5)
        duel_fatal("too many parms");

    if (sizeof func->ctype->u.kid != sizeof(intptr_t) && func->ctype->u.kid != ctype_void)
        duel_fatal("unsupported func parm");

    for (int i = 0; i < parms_no; ++i) {
        if (parms[i]->val_kind != VK_RVALUE || parms[i]->ctype->size != sizeof(int))
            duel_fatal("unsupported parameter");
    }

    int (*f)() = (int (*)()) func->u.lvalue;
    switch (parms_no) {
      case 0:
        rval->u.rval_int= (*f)();
      break ;
      case 1:
        rval->u.rval_int= (*f)(parms[0]->u.rval_int);
      break ;
      case 2:
        rval->u.rval_int= (*f)(parms[0]->u.rval_int,parms[1]->u.rval_int);
      break ;
      case 3:
        rval->u.rval_int= (*f)(parms[0]->u.rval_int,parms[1]->u.rval_int,
              parms[2]->u.rval_int);
      break ;
      case 4:
        rval->u.rval_int= (*f)(parms[0]->u.rval_int,parms[1]->u.rval_int,
              parms[2]->u.rval_int,parms[3]->u.rval_int);
      break ;
      case 5:
        rval->u.rval_int= (*f)(parms[0]->u.rval_int,parms[1]->u.rval_int,
              parms[2]->u.rval_int,parms[3]->u.rval_int,parms[4]->u.rval_int);
      break ;
    }
    rval->val_kind=VK_RVALUE ;
    rval->ctype=func->ctype->u.kid ;
}

/* find debuggee variable.
 * recognize main.s, gint, malloc, printf
 */

int gint ;      /* global variable recognized */
char **main_s ; /* point to main's 's' */

FUNC bool duel_get_target_variable(char *name, int frame_no, tvalue *v)
{
   v->val_kind=VK_LVALUE ;
   if(frame_no<=0 && strcmp(name,"s")==0) {
       v->ctype=duel_mkctype_ptr(ctype_char);
       v->u.lvalue = (void*) main_s ;
       return TRUE ;
   }
   if(frame_no != -1) return FALSE ;
   if(strcmp(name,"main")==0) {
       v->ctype=duel_mkctype_func(ctype_void);
       v->u.lvalue = (void*) main ;
       return TRUE ;
   }
   if(strcmp(name,"malloc")==0) {
       v->ctype=duel_mkctype_func(duel_mkctype_ptr(ctype_void));
       v->u.lvalue = (void*) malloc ;
       return TRUE ;
   }
   if(strcmp(name,"printf")==0) {
       v->ctype=duel_mkctype_func(ctype_int);
       v->u.lvalue = (void*) printf ;
       return TRUE ;
   }
   if(strcmp(name,"gint")!=0) return FALSE ;
   v->ctype= ctype_int ;  /* type of this variable */
   v->u.lvalue= (void*) &gint ;   /* address of variable   */
   return TRUE ;
}


FUNC int duel_get_frames_number(void)
{
    return 1 ; /* main */
}


FUNC ttarget_ptr duel_get_function_for_frame(int frame_no)
{
    if(frame_no==0) return (void*) main ;
    else duel_fatal("bad frame number - internal err");
}

FUNC tctype* duel_get_target_typedef(char *name)
{
    if(strcmp(name,"uint")==0) return ctype_uint ;
    return NULL ;
}

FUNC tctype* duel_get_target_struct_tag(char *name) { return 0 ; }
FUNC tctype* duel_get_target_union_tag(char *name)  { return 0 ; }
FUNC tctype* duel_get_target_enum_tag(char *name)   { return 0 ; }

int main(int argc,char **argv)
{
   char *s="main string" ;
   main_s = &s ;                /* put s into the "symbol tbl" */

   while(1) {
       char in[120] ;
       fgets(in, sizeof in, stdin);
       // Code seems to assume trailing \n gets stripped
       if (in[strlen(in)-1] == '\n')
           in[strlen(in)-1] = 0;

       if(feof(stdin)) break ;
        /* output & input are RCS'ed. avoid $Header in self.out, which is
         * the input header, which RCS fix... which create mis matches */
       if(strncmp(in,"## $Header",10)==0) continue ;
       printf("dl> %s\n",in);
       if(in[0]==0 || in[0]=='#' && in[1]=='#') continue ;  /* comments */
       duel_parse_and_eval(in);
   }
   return 0 ; /* to run in make */
}
