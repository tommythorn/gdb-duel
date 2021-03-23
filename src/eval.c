/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/eval.c,v 1.13 93/03/19 15:39:34 mg Exp $*/

/* this module is the most critical code, the recursive evaluation
 */

/*
 * $Log:        eval.c,v $
 * Revision 1.13  93/03/19  15:39:34  mg
 * fixed bug in left->left symbolic, and long symbolics which caused crashes
 *
 * Revision 1.12  93/03/12  05:48:45  mg
 * fixed sizeof(type) symbolic
 *
 * Revision 1.11  93/02/26  04:59:51  mg
 * improved symbolic debugging of #/
 *
 * Revision 1.10  93/02/03  21:47:41  mg
 * fixed dot stack bug; fixed func-call last parm eval bug
 *
 * Revision 1.9  93/01/12  21:50:26  mg
 * cleanup and set for release
 *
 * Revision 1.8  93/01/07  00:09:34  mg
 * scope stack changes a bit.
 * clear/aliases commands.
 * func.x support, x.y etc force existance if y field of x., x=>y w/'_'
 * allow fields in y for x@y, x pointer.
 * fixed eval_node setup
 * added &&/ ||/
 *
 *
 * Revision 1.7  93/01/03  07:29:23  mg
 * function calls, error reporting, printing.
 *
 * Revision 1.6  92/12/24  23:33:48  mg
 * frames support
 *
 * Revision 1.5  92/10/19  15:06:35  mg
 * *** empty log message ***
 *
 * Revision 1.4  92/10/14  02:04:35  mg
 * misc
 *
 * Revision 1.3  92/09/16  11:04:16  mg
 * *** empty log message ***
 *
 * Revision 1.2  92/09/15  05:54:56  mg
 * cosmetics and new ops:
 * generic '.' and '_'  support. x@y. '..x' and 'x..'.  while(), for(), ?:
 *
 */

#include "duel.h"



#define DOT_STACK_SIZE 100    /* dot stack size, maximum no. of dots in stmt */

/* the scope eval stack.
 * used for several purposes: to push _'s values (=>), to push structs
 * and unions for x.y and x->y, and to push func/frame values fro frame(i).x
 * and func.x/
 * realv is the real value for the left side operand. It is used for '_'.
 * fieldv is the one to be used to fetch actual names. either:
 * fieldv=realv (for x.y, frame(i).y), fieldv=*realv (for x->y),
 * fieldv=func_frame(realv) (for func.x), or fieldv=0  (x=>y).
 */
struct {
    tvalue *realv ;     /* actual value for x in x.y or x->y etc. used for _ */
    tvalue *fieldv;     /* value to use for fetching fields, *realv for x->y */
  } dot_stack[DOT_STACK_SIZE] ;
int dot_stack_top= -1 ;

PROC duel_reset_eval(void)   /* reset evaluation states from previous eval */
{
  dot_stack_top= -1 ;   /* reset stack */
}

/* try to find the given name on the dot_stack of structures.
 * if found, return true and value in(v) else return false.
 * the symbolic value is not set.
 * if top_only, looks only at the top of the stack.
 */
LFUNC bool find_dot_name(char *name,tvalue *v,bool top_only)
{
    int i,j ;
    tctype_field *f ;
    tctype *t ;
    tvalue *x;

    for(i=dot_stack_top ; i>=0 ; i--) {           /* look at the stack[i]    */
        if(top_only && i!=dot_stack_top) break ;  /* consider only top of stk*/
        x=dot_stack[i].realv ;
        if(name[0]=='_' && name[1]==0 ||                           /* _ */
           name[1]=='_' && i==dot_stack_top-1 && name[2]==0 ||     /* __ */
           name[1]=='0'+dot_stack_top-i && name[2]==0) {           /* _[0-9] */
            *v = *x ;
            return TRUE ;
        }
        x=dot_stack[i].fieldv;
        if(!x) continue ;
        if(x->val_kind==VK_FVALUE) {
            if(!duel_get_target_variable(name,x->u.fvalue,v)) continue ;
            strcpy(v->symb_val,name);
            return TRUE ;
        }
        t=x->ctype ;
        if(!ctype_kind_struct_like(t)) continue ;
        duel_assert(x->val_kind==VK_LVALUE);
        for(j=0 ; j<t->u.f.fields_no ; j++) {  /* look at field[j] of struct*/
            f= &t->u.f.fields[j] ;
            if(strcmp(f->name,name)==0) goto found ;
        }
    }
    return FALSE ;
found:
    v->ctype=f->ctype ;
    strcpy(v->symb_val,name);
    if(f->bitlen==0) {
       v->u.lvalue=x->u.lvalue+f->bitpos/BITS_PER_BYTE ;
       v->val_kind=VK_LVALUE ;
       return TRUE ;
   }
   /* special care for a bitfield */
   if(f->ctype->type_kind!=CTK_INT && f->ctype->type_kind!=CTK_UINT)
       duel_gen_error("bitfield '%s' must be int or unsigned",name);
    v->val_kind=VK_BVALUE ;
    v->u.bvalue.lvalue=x->u.lvalue ;
    v->u.bvalue.bitpos=f->bitpos ;
    v->u.bvalue.bitlen=f->bitlen ;
    return TRUE ;
}

/* find the value of a given symbolic name and
 * return in in v
 * if name is not found, aborts with an error
 */

LPROC duel_eval_name(char *name,tvalue *v)
{
   tvalue *aval=duel_find_alias(name) ; /* find internal alias */
   if(aval!=NULL) *v= *aval ;
   else
   if(find_dot_name(name,v,FALSE)) return ;   /* setup name itself */
   else
   if(duel_get_target_variable(name,-1,v));
   else
   if(strcmp(name,"frames_no")==0) {            /* check special variables */
       v->val_kind=VK_RVALUE ;
       v->ctype=ctype_int ;
       v->u.rval_int=duel_get_frames_number();
   }
   else duel_gen_error("variable '%s' not found",name);
   strcpy(v->symb_val,name);
}


LPROC push_val(tval_list *l,tvalue *v)
{
  tval_lcell *e = duel_malloc(sizeof(tval_lcell));
  e->val = *v ;
  e->next=l->head ;
  l->head=e ;
  if(l->tail==0) l->tail=e ;
}

LPROC append_val(tval_list *l,tvalue *v)
{
  tval_lcell *e=duel_malloc(sizeof(tval_lcell)) ;
  e->val = *v ;
  e->next=0 ;
  if(l->head==0) l->head=l->tail=e ;
  else     l->tail=l->tail->next=e ;
}

/* push a whole val-list(ins) into (l) i.e. insert (ins) at the head of(l)
 */

LPROC push_val_list(tval_list *l,tval_list *ins)
{
  if(ins->head==NULL) return ;              /* inserted list is empty */
  if(l->head==NULL) { *l= *ins ; return ; }  /* insert-into is empty */
  ins->tail->next=l->head ;
  l->head=ins->head ;
}


/* remove first(top) element */
LFUNC bool pop_val(tval_list *l,tvalue *v)
{
  tval_lcell *e=l->head ;
  if(e==0) return(FALSE);
  *v=e->val ;
  l->head=e->next ;
  return(TRUE);
}
/* compute the symbolic value of one iteration of a search (eg -->) operator.
 * Normally, for (x) and (y) the result x->y  is returned.
 * But, if x === a-->next[[n]] and y === next, then we
 * return a-->next[[m]] where m=n+1.
 * Also, instead of returning x->y we return x-->y[[1]]
 *
 * note: this was not written for speed! I don't know if it takes
 * a significant amount of time or not.
 */

LPROC set_search_symb_val(char *opcode,tvalue *xval,tvalue *yval)
{
    char *x = xval->symb_val ;
    char *y = yval->symb_val ;
    int i,opl=strlen(opcode),xl=strlen(x),yl=strlen(y) ;
    char s[3*VALUE_MAX_SYMBOLIC_SIZE];

    if(yl+2<xl && strcmp(x+xl-yl,y)==0 && x[xl-yl-2]=='-' && x[xl-yl-1]=='>')
        sprintf(s,"%-.*s%s%s[[2]]",xl-yl-2,x,opcode,y);
    else {
    for(i=xl-1 ; i>0  ; i--)            /* see if we have op in x */
        if(strncmp(&x[i],opcode,opl)==0) break ;
    if(i>0 && strncmp(&x[i+opl],y,yl)==0 &&
       x[i+opl+yl]=='[' && x[i+opl+yl+1]=='[' && x[xl-2]==']' && x[xl-1]==']'){
        /* x seems to be something like  head-->next[1] */
        int j,val=0 ;
        for(j=i+opl+yl+2 ; j<xl-2 ; j++) {
            if(x[j]<'0' || x[j]>'9') goto simple ;
            val=10*val+x[j]-'0' ;       /* compute index */
        }
        sprintf(s,"%-.*s[[%d]]",i+opl+yl,x,val+1);   /* and re-create it */
    }
    else {
simple:         /* we failed to find a x-->y[z] pattern, make new one */
        sprintf(s,"%s->%s",x,y);
    }
   }
   s[VALUE_MAX_SYMBOLIC_SIZE-1]=0 ; /* chop as needed */
    strcpy(y,s);
}

LPROC push_dot_stack(tvalue *realv,tvalue *fieldv)
{
   if(dot_stack_top==DOT_STACK_SIZE)
       duel_gen_error("expression too complex ('.' and '->' levels)",0);
   dot_stack[++dot_stack_top].realv=realv ;
   dot_stack[dot_stack_top].fieldv=fieldv ;
}

LPROC pop_dot_stack(void)
{
   duel_assert(dot_stack_top>=0);
   dot_stack_top-- ;
}

/* a simple fetch of a field. used mainly when printing
 * v must be a struct with the given name field. value is returned in ret.
 * return false if name not found.
 */

FUNC bool duel_get_dot_name(tvalue *v,char *name,tvalue *ret)
{
    bool ok ;
    push_dot_stack(v,v);
    ok=find_dot_name(name,ret,TRUE);
    pop_dot_stack();
    return ok ;
}

/* evaluate x.y and similar "with" operators. Special care when y is a
 * name and not an expression -- force y to be a direct field of x.
 * y is the 'y' node. v is value to return. op is opcode for error reports
 * (rv,fv) are values to push on the dot stack. rv is the real 'x' value,
 *         fv is the value of x to use for field lookup (fv= *rv for '->')
 */

LFUNC bool eval_dot(tnode *y,tvalue *v,char *op,tvalue *rv,tvalue *fv)
{
   bool ok ;
   push_dot_stack(rv,fv);
   if(y->node_kind!=NK_NAME) ok=duel_eval(y,v);  /* "with" style */
   else {                                        /* x.y y simply name */
      if(++y->eval.level>1) { y->eval.level=0 ; ok=FALSE ; }
      else {
        ok=find_dot_name(y->name,v,TRUE);
        if(!ok) duel_op_error("field not found in operator '%s'",op,rv,0);
      }
   }
   pop_dot_stack();
   return ok ;
}


/* evaluate special operators like '-->' '?:' etc */

/* get the next result of an sop val:
 * DFS:  init by pushing(x)
 * Iterate: pop x, compute all x->y, push (reversed)
 * out: x

 * POS: init by pushing(x), unmarked.
 * Iterate: pop x, if marked return it. else push back, marked.
 *          compute all x->y, push (reversed)
 *          repeat until marked x is popped.
 * out: poped x which is marked.

 * BFS: init by pushing(x)
 * Iterate: get first(x),
 *      compute all x->y and put into queue.
 *      return x.
 */

/* fetch the next value in a DFS search on x-->y. y is given as a node
 * and x is popped of the given list. Value is returned in v.
 * note: the results from x->y are reversed when pushed on the list,
 * this is so x-->(left,right) would return the left first (put last on
 * the stack, even though it is computed first!)
 * malloc problem: newl is kept locally, so in case of ^C while here, mem
 * it points to will be lost. Normally only a few values
 */

LFUNC bool get_next_dfs_val(tval_list *l, tnode *y,tvalue *v)
{
   tvalue child,x ;
   tval_list newl ;
   newl.head=0 ;
   do {
       if(!pop_val(l,v)) return(FALSE) ;
       x= *v ;
       duel_get_struct_ptr_val(&x,"x-->y");
   } while(x.u.lvalue==0) ;     /* ignore null pointers */
   while(eval_dot(y,&child,"-->",v,&x)) {
       set_search_symb_val("-->",&x,&child);  /* makes x-->y[n] neatly */
       append_val(&newl,&child);                /* append to childs list */
   }
   push_val_list(l,&newl);              /* append new childs to stack */
   return(TRUE);                        /* returns the popped value in v */
}


/* stop the evaluation of the expression at node n.
 * useful with operators like first().
 * each node keeps an internal state allowing it to produce the next value.
 * this function resets those states.
 *
 * How: the internal state is kept in n->eval.level. we reset level to zero
 *      for the node and the subnodes. if the level is already zero,
 *      the node has already gone to the 'initial state', so the subnodes
 *      are not visited.
 */

LPROC stop_eval(tnode *n)
{
   int i;
   if(n==NULL || n->eval.level==0) return ; /* done! subnodes are also ok */
   n->eval.level=0 ;
   for(i=0 ; i<NODE_MAX_KIDS ; i++) stop_eval(n->kids[i]);
}

/* evaluate function paramaters. This recursive function should be called
 * with the top node for the parms. Parms are parsed as "," operators.
 * the function leaves the computed values "hanging" on v1 of each "," node.
 * the last paramater is left at v2 of the function call itself (there isnt
 * any other reasonable place!).
 * 2nd paramater is the function call node, used for the last paramater.
 */

LFUNC bool eval_func_parms(tnode *n,tnode *fn)
{
    tvalue *p= &n->eval.v1 ;
    if(n->node_kind==NK_OP && n->op_kind==OPK_SBIN && n->op==',') {
       while(n->eval.level==2 || duel_eval(n->kids[0],p)) {
          n->eval.level=2 ;  /* left side active parm in p */
          if(eval_func_parms(n->kids[1],fn)) goto ok;
          n->eval.level=1 ;   /*re-eval */
       }
       return FALSE ;
   }
   else if(!duel_eval(n,p = &fn->eval.v2)) return FALSE;  /* last paramater */
ok:
   duel_standardize_func_parm(p);
   return TRUE ;
}

LFUNC bool eval_func_call(tnode *n,tvalue *v)
{

   tvalue *f= &n->eval.v1 ;
   tvalue *parms[21];
   int i,parms_no ;
   tnode *p ;

again:
   if(n->kids[1]==NULL) {       /* no parms */
       n->eval.level=1 ;
       if(!duel_eval(n->kids[0],f)) return FALSE ;
   }
   else {
      while(n->eval.level==2 || duel_eval(n->kids[0],f)) {
        n->eval.level=2 ;  /* function in f */
        if(eval_func_parms(n->kids[1],n)) goto ok;
        n->eval.level=1 ;   /*re-eval func */
      }
      return FALSE ;
    ok: ;
   }

   if(f->ctype->type_kind!=CTK_FUNC) duel_op_error("bad function call",0,f,0);

   p=n->kids[1] ;                       /* collect paramaters now */
   parms_no=0 ;
   while(p && p->node_kind==NK_OP && p->op_kind==OPK_SBIN && p->op==',') {
     parms[parms_no++]= &p->eval.v1 ;
     p=p->kids[1] ;
     if(parms_no>=20) duel_op_error("too many paramaters",0,0,0);
   }
   if(p) parms[parms_no++]= &n->eval.v2 ;       /* last paramater */

   duel_target_func_call(f,parms,parms_no,v);
   if(f->ctype->u.kid->type_kind==CTK_VOID) goto again ; /* no return vals */
   duel_set_symb_val(v,"%s(",f,0);
   for(i=0 ; i<parms_no ; i++)
      duel_set_symb_val(v,"%s%s,",v,parms[i]);
   if(parms_no>0) v->symb_val[strlen(v->symb_val)-1]='\0' ; /*chop ',' tail*/
   strcat(v->symb_val,")");
   return TRUE ;
}


/* evaluate for special operators: those the produce more than one value,
 * binary ones. ',' '..' etc
 */

LFUNC bool duel_eval_sbin(tnode *n,tvalue *v)
{
   tval_list *vl = &n->eval.vlist ;
   tvalue y,*v1= &n->eval.v1, *v2= &n->eval.v2 ;
   tnode *kid0 = n->kids[0], *kid1 = n->kids[1] ;
   int vi ;
   bool ok ;
#define lev n->eval.level

   duel_assert(n->node_kind==NK_OP && n->op_kind==OPK_SBIN);
   switch ((unsigned) n->op) {
   case OP_DECL:
           duel_assert(kid0->node_kind==NK_NAME && kid1->node_kind==NK_CTYPE);
           if(kid1->ctype->size<=0) duel_gen_error("illegal type size",0);
           v->val_kind=VK_LVALUE ;
           v->ctype=kid1->ctype ;
           v->u.lvalue=duel_alloc_target_space(kid1->ctype->size);
           strcpy(v->symb_val,kid0->name);
           duel_set_alias(kid0->name,v);
   break ;
   case OP_DEF:
           if(kid0->node_kind!=NK_NAME)
                duel_gen_error("left side of := must be a simple var",0);
           if(!duel_eval(kid1,v)) return FALSE ;
           duel_set_alias(kid0->name,v);
           return TRUE ;
   case ',':
         if(lev==1 && duel_eval(kid0,v)) return TRUE ;
         lev=2 ;
         return duel_eval(kid1,v);
   case ';':
         /*note: (x;) is not allowed in syntax, but is allowed here and
          *means eval x, return nothing. used by parser, e.g. terminating ';'
          *produces no side effects
          */

         if(lev==1) while(duel_eval(kid0,v)) ; /* eval all left size */
         lev=2 ;
         return duel_eval(kid1,v);
   break ;
   case OP_IMP:  /* a=>b  for each _=eval(a) return eval(b) (with _ set) */
             if(lev>1) goto im2 ;
             for(;;) {
                  if(!duel_eval(kid0,v1)) return FALSE ;
                  lev=2 ;
             im2: push_dot_stack(v1,0);
                  ok=duel_eval(kid1,v);
                  pop_dot_stack();
                  if(ok) return TRUE ;
             }
   case OP_IF:  /* if(a) b  return eval(b) for each eval(a)!=0 */
             if(lev>1) goto if2 ;
             for(;;) {
                  if(!duel_eval(kid0,v)) return FALSE ;
                  if(!duel_mk_logical(v,"if(x)y")) continue ;
                  lev=2 ;
             if2: if(duel_eval(kid1,v)) return TRUE ;
             }
   case OP_OR:  /* a||b normal 'C' logical or */
             if(lev>1) goto or2 ;
             for(;;) {
                  if(!duel_eval(kid0,v)) return FALSE ;
                  if(duel_mk_logical(v,"x||y")) {lev=1 ; return TRUE ;}
             or2: if(duel_eval(kid1,v)) {
                    lev=2 ;
                    duel_mk_logical(v,"y||x");
                    return TRUE ;
                  }
             }
   case OP_AND:  /* a&&b normal 'C' logical and */
             if(lev>1) goto an2 ;
             for(;;) {
                  if(!duel_eval(kid0,v)) return FALSE ;
                  if(!duel_mk_logical(v,"x&&y")) {lev=1 ; return TRUE ;}
             an2: if(duel_eval(kid1,v)) {
                    lev=2 ;
                    duel_mk_logical(v,"y&&x");
                    return TRUE ;
                  }
             }
   case '.':
             if(lev>1) goto dt2 ;
             for(;;) {
                  if(!duel_eval(kid0,v1)) return FALSE ;
                  *v2 = * v1 ;  /* copy value for the lookup */
                  if(ctype_kind_func_ptr_like(v1->ctype))  /* func.x */
                      duel_find_func_frame(v2,"x.y");
                  else
                  if(v1->val_kind!=VK_FVALUE)  /* type check frame or struct*/
                      duel_get_struct_val(v1,"x.y");
                  lev=2 ;
             dt2: if(!eval_dot(kid1,v,".",v1,v2)) continue ;
                  if(v->ctype!=v1->ctype || v->val_kind!=v1->val_kind ||
                     v->u.lvalue != v1->u.lvalue ||
                     strcmp(v->symb_val,v1->symb_val)!=0) /* check for x._ */
                      duel_set_symb_val(v,"%s.%s",v1,v);
                  return TRUE ;
             }
   case OP_ARR:
             if(lev>1) goto ar2 ;
             for(;;) {
                  if(!duel_eval(kid0,v1)) return FALSE ;
                  *v2 = *v1 ;           /* copy value for dereferencing */
                  duel_get_struct_ptr_val(v2,"x->y");
                  lev=2 ;
             ar2: if(!eval_dot(kid1,v,"->",v1,v2)) continue ;
                  if(v->ctype!=v1->ctype || v->val_kind!=v1->val_kind ||
                     v->u.lvalue != v1->u.lvalue ||
                     strcmp(v->symb_val,v1->symb_val)!=0) /* check for x->_ */
                      duel_set_symb_val(v,"%s->%s",v1,v);
                  return TRUE ;
             }
   case OP_TO:  /* a..b  Is it legal to have 1..(5,6) sure! */
         if(lev>1) goto to2 ;
         do {
             if(kid0 && !duel_eval(kid0,v1)) break ;
             do {
               if(kid1 && !duel_eval(kid1,v2)) break ;
         to2:  if(duel_do_op_to(kid0? v1:0,kid1? v2:0,++lev-2,v)) return TRUE;
             } while(kid1);
         } while(kid0) ;  /* either one (kid0 null) or infinite iterations*/
   break ;
   case OP_SEL:          /* x[[y]] */
           if(lev==1) { lev=2 ; n->eval.counter= -1 ; }
           if(!duel_eval(kid1,v1)) {
               stop_eval(kid0);
               return FALSE ;
           }
           vi=duel_get_posint_val(v1,"y[[x]]");
           if(vi<=n->eval.counter) {
                   /* v is smaller than previous v value, so reset x and
                    * start over. Example: \x[1,5,3] after \x[1],
                    * we continue to get [5]. but to get [3] we reset
                    * Alternatively, we could have kept a list of old
                    * generated values.
                    */
               stop_eval(kid0) ;
               n->eval.counter= -1 ;
           }
           for( ; n->eval.counter<vi ; n->eval.counter++)
               if(!duel_eval(kid0,v))
                   duel_op_error("operator x of y[[x]] too large",0,v1,0);
           return TRUE ; /* value is the last (v) computed */
   break ;
   case '@':             /* x@y - generate x stops when y true */
           if(!duel_eval(kid0,v)) return FALSE ;
           if(kid1->node_kind==NK_CONST) {      /* special case y constant */
               *v2=kid1->cnst ;
               *v1= *v ;        /* because 'apply_bin_op' destroy its args */
               duel_apply_bin_op(OP_EQ,v1,v2,&y);
               if(y.u.rval_int) { stop_eval(kid0); return FALSE ; }
               return TRUE ;
           }
           *v1 = *v ;           /* allow fields in y of x@y for x struct ptr */
           if(ctype_kind_ptr_like(v->ctype) &&
              ctype_kind_struct_like(v->ctype->u.kid))
               duel_get_struct_ptr_val(v1,"x@y");

           while(eval_dot(kid1,v2,"@",v,v1)) /* check &&/y */
               if(!duel_mk_logical(v2,"y@x")) {  /* y==0, so dont stop x */
                   stop_eval(kid1);
                   return TRUE ;
               }
           stop_eval(kid0);
    break ;
    case '#':            /* x#i define variable i as counter for gen. x*/
       if(kid1->node_kind!=NK_NAME)
               duel_gen_error("x#y 2rd operand must be a name",0);
       if(!duel_eval(kid0,v)) return FALSE ;
       if(lev==1) { lev=2 ; n->eval.counter= -1 ; } /* first time */
       y.val_kind=VK_RVALUE ;
       y.ctype=ctype_int ;
       y.u.rval_int= ++n->eval.counter ;
       sprintf(y.symb_val,"%d",n->eval.counter);
       duel_set_alias(kid1->name,&y);
       return TRUE ;
   break ;
   case OP_DFS:          /* x-->y */
       if(lev>1) goto df2 ;
       for(;;) {
            if(!duel_eval(kid0,v)) return FALSE ;
            duel_free_val_list(vl);
            push_val(vl,v);
            lev=2 ;
       df2: if(get_next_dfs_val(vl,kid1,v)) return TRUE ;
       }
   break ;
   case OP_WHILE:       /* while(a) b  */
             if(lev==2) goto wh2 ;
             for(;;) {
                  while(duel_eval(kid0,v)) /* check &&/a */
                    if(!duel_mk_logical(v,"while(x)y")) {
                       stop_eval(kid0);
                       return FALSE ;
                    }
                  lev=2 ;
             wh2: if(duel_eval(kid1,v)) return TRUE ;
             }
   default: duel_assert(0);
   }
   return FALSE ;
#undef lev
}



LFUNC bool duel_eval_tri(tnode *n,tvalue *v)
{
#define lev n->eval.level
   duel_assert(n->node_kind==NK_OP && n->op_kind==OPK_TRI);
   switch((unsigned)n->op) {
   case OP_IF:  /* if(a) b else c return eval(b) for each eval(a)!=0
                 * and eval(c) for each eval(a)==0 (usu. (a) is one result*/
             if(lev>1) goto if2;
             for(;;) {
                   if(!duel_eval(n->kids[0],v)) return FALSE ;
                   lev=(duel_mk_logical(v,"if(x) y else z")? 2:3) ;
              if2: if(duel_eval(n->kids[lev-1],v)) return TRUE ;
             }
   case '?':    /* a? b:c has the same semantics as if(a) b else c */
             if(lev>1) goto qm2;
             for(;;) {
                   if(!duel_eval(n->kids[0],v)) return FALSE ;
                   lev=(duel_mk_logical(v,"x? y:z")? 2:3) ;
              qm2: if(duel_eval(n->kids[lev-1],v)) return TRUE ;
             }
   default: duel_assert(0);
   }
   return FALSE ;
#undef lev
}

LFUNC bool duel_eval_quad(tnode *n,tvalue *v)
{
#define lev n->eval.level
   duel_assert(n->node_kind==NK_OP && n->op_kind==OPK_QUAD);
   switch(n->op) {
   case OP_FOR: /* for(a;b;c) d ;  */
             if(lev==1) { lev=2 ; while(duel_eval(n->kids[0],v)); }
             if(lev==3) goto fr3 ;
             for(;;) {
                  while(duel_eval(n->kids[1],v)) /* check &&/b */
                    if(!duel_mk_logical(v,"for(a;x;y)z")) {
                       stop_eval(n->kids[1]);
                       return FALSE ;
                    }
                  lev=3 ;
             fr3: if(duel_eval(n->kids[3],v)) return TRUE ;
                  while(duel_eval(n->kids[2],v));
             }
   default: duel_assert(0);
   }
   return FALSE ;
#undef lev
}

FUNC bool duel_eval(tnode *n,tvalue *v)
{
   tvalue u,tmp ;
   bool ok=FALSE ;
   tnode *prev_loc ;

   if(!n) return FALSE ;
   prev_loc=duel_set_eval_loc(n);        /* set current eval node, save prev */
   if(n->eval.level==0) n->eval.level=1 ; /* indicate node is 'active' */

   switch(n->node_kind) {
      case NK_CONST: /* return a 'value' node made of this constant */
         if(n->eval.level==1) {
            n->eval.level=2 ;
            *v=n->cnst ;
            ok=TRUE ;
         }
      break ;
      case NK_NAME:
         if(n->eval.level==1) {
            n->eval.level=2 ;
            duel_eval_name(n->name,v);
            ok=TRUE ;
         }
      break ;
      case NK_OP:
         switch(n->op_kind) {
          case OPK_SUNARY:              /* special unary ops */
             if(n->op=='#') {
                 int count=0 ;
                 if(n->eval.level==1) {
                     while(duel_eval(n->kids[0],&u))
                       if(count++ == 0) duel_set_symb_val(v,"#/(%s ...)",&u,0);
                     if(count == 0) duel_set_symb_val(v,"#/(empty)",0,0);
                     v->val_kind=VK_RVALUE ;
                     v->ctype=ctype_int ;
                     v->u.rval_int=count ;
                     n->eval.level=2 ;
                     ok=TRUE ;
                 }
             }
             else
             if(n->op==OP_AND) {
                 if(n->eval.level==1) { int result=1 ;
                     while(duel_eval(n->kids[0],v)) {
                         if(!duel_mk_logical(v,"&&/x")) {
                             stop_eval(n->kids[0]);
                             result=0 ;
                             break ;
                         }
                     }
                     v->val_kind=VK_RVALUE ;
                     v->ctype=ctype_int ;
                     v->u.rval_int=result ;
                     sprintf(v->symb_val,"%d",result);
                     n->eval.level=2 ;
                     ok=TRUE ;
                 }
             }
             else
             if(n->op==OP_OR) {
                 if(n->eval.level==1) { int result=0 ;
                     while(duel_eval(n->kids[0],v)) {
                         if(duel_mk_logical(v,"||/x")) {
                             stop_eval(n->kids[0]);
                             result=1 ;
                             break ;
                         }
                     }
                     v->val_kind=VK_RVALUE ;
                     v->ctype=ctype_int ;
                     v->u.rval_int=result ;
                     sprintf(v->symb_val,"%d",result);
                     n->eval.level=2 ;
                     ok=TRUE ;
                 }
             }
             else
             if(n->op==OP_SIZ) {
                 if(n->eval.level==1) {
                     char *tname=n->kids[0]->ctype->name ;
                     duel_assert(n->kids[0]->node_kind==NK_CTYPE);
                     v->val_kind=VK_RVALUE ;
                     v->ctype=ctype_size_t ;
                     v->u.rval_size_t=n->kids[0]->ctype->size ;
                     n->eval.level=2 ;
                     if(tname==NULL || *tname=='\0') tname="T" ; /* cheating */
                     sprintf(v->symb_val,"sizeof(%s)",tname);
                     ok=TRUE ;
                 }
             }
             else duel_assert(0);
          break ;
          case OPK_UNARY:
            if(!duel_eval(n->kids[0],v)) break ;
            duel_apply_unary_op(n->op,v);
            ok=TRUE ;
          break ;
          case OPK_POST_UNARY:
            if(!duel_eval(n->kids[0],v)) break ;
            duel_apply_post_unary_op(n->op,v);
            ok=TRUE ;
          break ;
          case OPK_BIN:    /* a+b, compute and hold a, iterate on b, redo a */
             while(n->eval.level==2 || duel_eval(n->kids[0],&n->eval.v1)) {
                n->eval.level=2 ;  /* left side active op in vals[0] */
                while(duel_eval(n->kids[1],&u)) {
                  tmp= n->eval.v1 ;  /* copy left val, it is destoryed*/
                  ok=duel_apply_bin_op(n->op,&tmp,&u,v);
                  if(ok) goto done;
                }
                n->eval.level=1 ;   /*left side val no longer valid, re-eval*/
             }
          break ;
          case OPK_SBIN:   /* a,b etc, special ops */
             ok=duel_eval_sbin(n,v) ;
          break ;
          case OPK_TRI:
             ok=duel_eval_tri(n,v) ;
          break ;
          case OPK_QUAD:
             ok=duel_eval_quad(n,v) ;
          break ;
          case OPK_CAST:
             duel_assert(n->kids[0]->node_kind==NK_CTYPE);
             if(!duel_eval(n->kids[1],v)) break ;
             duel_do_cast(n->kids[0]->ctype,v);
             ok=TRUE ;
          break ;
          case OPK_ASSIGN:
             duel_gen_error("modified assignment is not supported yet",0);
          case OPK_FUNC:
             ok=eval_func_call(n,v) ;
          break ;
          default: duel_assert(0);
         }
      break ;
      default: duel_assert(0);
   }
done:
   if(!ok) n->eval.level=0 ;  /* no other val available */
   duel_set_eval_loc(prev_loc);
   return ok ;
}
