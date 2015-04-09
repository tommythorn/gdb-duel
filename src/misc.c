/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/misc.c,v 1.5 93/03/12 05:51:00 mg Exp $*/

/* misc function/library-like */

/*
 * $Log:	misc.c,v $
 * Revision 1.5  93/03/12  05:51:00  mg
 * support output redirection
 * 
 * Revision 1.4  93/01/13  16:22:09  mg
 * allow malloc to return int (is a mini symbol on SUN)
 * 
 * Revision 1.3  93/01/12  21:52:43  mg
 * moved aliases mgmt here
 * cleanup and set for release
 * 
 */

#include "duel.h"

/* like strncpy, but put a zero at to[len] anyway.
 * are copied (hence taking len+1 bytes
 */

FUNC char* strncpyz(char *to,char *from,size_t len)
{
   strncpy(to,from,len);
   to[len]=0 ;
   return to ;
}

/* allocate n bytes in the target's space. Used for storing constant
 * strings when they are parsed, and for variables declaration
 * note: on SUN (and others?) gdb claims malloc to return an integer. we
 * accept this (silly but probably no harm done)
 */

FUNC ttarget_ptr duel_alloc_target_space(size_t n)
{
  tvalue p,f,r,*parms[1] ;

  p.val_kind=VK_RVALUE ;        /* mk n into a value, to pass as parm */
  p.ctype=ctype_int ;
  p.u.rval_int=n ;
  parms[0]= &p ;

  if(!duel_get_target_variable("malloc",-1,&f) || 
     f.ctype->type_kind!=CTK_FUNC || 
     f.ctype->u.kid->type_kind!=CTK_INT && f.ctype->u.kid->type_kind!=CTK_PTR) 
      duel_fatal("malloc() function returning a pointer required in target");

  duel_target_func_call(&f,parms,1,&r);
  if(r.u.rval_ptr==NULL) duel_fatal("target's malloc() failed");
  return r.u.rval_ptr ;
}

PROC duel_free_val_list(tval_list *l)  /*free a val list and mark list empty */
{
  tval_lcell *cell,*next ;
  for(cell=l->head ; cell!=NULL ; cell=next ) {
      next=cell->next ;
      duel_free(cell);
  }
  l->head=l->tail=NULL ;
}

/* free all the memory allocated in the parse tree from node */

PROC duel_free_nodes(tnode *n)
{
    int i;
    if(!n) return ;
    for(i=0 ; i<NODE_MAX_KIDS ; i++) 
        duel_free_nodes(n->kids[i]);
    duel_free_val_list(&n->eval.vlist);
    duel_free(n);
}



/* Aliases management */

typedef struct sduel_var {      /* an alias */
    char *name ;
    tvalue val ;
    struct sduel_var *next ;
 } tduel_var ;
tduel_var *duel_vars_head ;

/* find the value of a duel variable, if exist (else return null */

FUNC tvalue* duel_find_alias(char *name)
{
   tduel_var *p ;
   for(p=duel_vars_head ; p ; p=p->next) {
        if(strcmp(name,p->name)==0) return(&p->val);
   }
   return NULL ;
}

/* set a value of a duel alias. Create var as needed */

PROC duel_set_alias(char *name,tvalue *v)
{
   tduel_var *p ;
   tvalue *found = duel_find_alias(name) ;
   if(found!=NULL) *found= *v ;
   else {
     p=duel_malloc(sizeof(tduel_var)) ;
     p->next=duel_vars_head ;
     p->val= *v ;
     p->name=name ;
     duel_vars_head=p ;
  }
}

PROC duel_clear_aliases(void)   /* clear all defined aliases */
{
   tduel_var *p=duel_vars_head ;
   while(p) {
        tduel_var *q=p ;
        p=p->next ;
        duel_free(q);
    }
   duel_vars_head=0 ;
}

PROC duel_show_aliases(void)
{
   tduel_var *p ;
   if(!duel_vars_head) duel_printf("No aliases defined\n");
   else {
     duel_printf("Aliases table:\n");
     for(p=duel_vars_head ; p ; p=p->next) {
        duel_printf("%s:  ",p->name);
        duel_print_value(&p->val);
     }
   }
}



