/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/evalops.c,v 1.14 93/03/19 15:40:17 mg Exp $*/

/* this module contains evalauation code for many standard operators, eg '+'
 */

/*
 * $Log:	evalops.c,v $
 * Revision 1.14  93/03/19  15:40:17  mg
 * fixed bug long symbolics handling
 * 
 * Revision 1.13  93/03/12  05:50:01  mg
 * uses tuint instead of uint, etc.
 * 
 * Revision 1.12  93/02/26  05:00:13  mg
 * fixed void pointers compare. ctype_voidptr is not unique!
 * 
 * Revision 1.11  93/02/04  02:09:55  mg
 * typo
 * 
 * Revision 1.10  93/02/04  01:24:51  mg
 * better error reports for "="
 * 
 * Revision 1.9  93/02/03  21:47:43  mg
 * support "signed char"
 * 
 * Revision 1.8  93/01/12  21:51:29  mg
 * cleanup and set for release
 * 
 * Revision 1.7  93/01/07  00:10:51  mg
 * auto convert func to &func
 * find a frame for a func
 * 
 * 
 * Revision 1.6  93/01/03  07:30:02  mg
 * function calls, error reporting, printing.
 * 
 * Revision 1.5  92/12/24  23:34:47  mg
 * frames support
 * 
 * Revision 1.4  92/10/19  15:07:46  mg
 * fvalue added (not ready yet), svalues dropped
 * 
 * Revision 1.3  92/10/14  02:05:10  mg
 * add print/{x} support
 * 
 * Revision 1.2  92/09/15  05:48:57  mg
 * support '..' new formats
 * 
 */

#include "duel.h"

/*
 * This file is made up of three parts:
 * (1) low-level functions that interact with the debugger/type system directly
 * (2) mid-level functions that compute the result of simple operators like '+'
 * (3) high-level functions that compute any binary/unary op.
 * Only some of the functions in (1) are global, and all of the (3) are.
 * this collection is in one file to allow a minimal of global symbols
 * (for minimum collision with the debugger)
 */



/****************************************************************************
 A low-level set of functions follows. They interact with the type system
 and the debugger/target's space directly:
 get_storage_type_kind - retrieve ctype_kind, with special conversion for enums
 get_rvalue - retrieve the rvalue of a variable/lvalue.
 set_symb_val - set the symbolic value of a tvalue.
 upgrade_small_int_types - figure out the type to upgrade to from char etc.
 find_numeric_result_type - figure type of x+y where + is generic C op
 convert_scalar_type - convert one scalar type to another
 get_numeric_val - retrieve rvalue, making sure it is numeric
 get_scalar_val  - retrieve rvalue, making sure it is numeric or pointer
 get_integral_val- retrieve rvalue, making sure it is an integer
 get_int_val     - retrieve rvalue, make sure it's an integer, return int val
 get_pointer_val - retrieve rvalue, make sure it's a pointer.
 ****************************************************************************/ 

/* give the storage-type kind of a given type. 
 * this is the same type-kind as the type itslef, except in the case of enums
 * where the type-kind of the storage will be CTK_INT etc (integral type)
 * the storage type kind is set when the enum is created.
 */

LFUNC tctype_kind get_storage_type_kind(tctype *t)
{
    if(t->type_kind!=CTK_ENUM) return t->type_kind ;
    return t->u.e.real_type_kind ;
}

/* try_get_rvalue -- make an rvalue of v. if v is already an rvalue, 
 * nothing is done. Else v is an b/lvalue, so its rvalue is fetched.
 * special care:
 * (1) Enums are fetched as int of same size. type stay enum!
 * (2) Arrays and functions are made into pointers
 * (3) Bitfields are converted to ints (debugger dependent)
 *     there are no rvalues of 'bitfield' type!
 * return succ/fail for bad mem ref. the "real" function everyone calls is
 * get_rvalue (this function is used only by printing functions to avoid
 * chicken&egg problem of error reporting.)
 */

FUNC duel_try_get_rvalue(tvalue *v,char *op)
{
   void *p ;
   int n ;
   bool ok;
   if(v->val_kind == VK_RVALUE) return TRUE;
   if(v->val_kind == VK_FVALUE) 
       duel_op_error("illegal type 'frame' for operand x of '%s'",op,v,0);
   switch(get_storage_type_kind(v->ctype)) {
      case CTK_CHAR:    p= &v->u.rval_char   ; n=sizeof(char)         ;break ;
      case CTK_SCHAR:   p= &v->u.rval_schar  ; n=sizeof(tschar)       ;break ;
      case CTK_UCHAR:   p= &v->u.rval_uchar  ; n=sizeof(tuchar)       ;break ;
      case CTK_USHORT:  p= &v->u.rval_ushort ; n=sizeof(tushort)      ;break ;
      case CTK_SHORT:   p= &v->u.rval_short  ; n=sizeof(short)        ;break ;
      case CTK_INT:     p= &v->u.rval_int    ; n=sizeof(int)          ;break ;
      case CTK_UINT:    p= &v->u.rval_uint   ; n=sizeof(tuint)        ;break ;
      case CTK_LONG:    p= &v->u.rval_long   ; n=sizeof(long)         ;break ;
      case CTK_ULONG:   p= &v->u.rval_ulong  ; n=sizeof(tulong)       ;break ;
      case CTK_FLOAT:   p= &v->u.rval_float  ; n=sizeof(float)        ;break ;
      case CTK_DOUBLE:  p= &v->u.rval_double ; n=sizeof(double)       ;break ;
      case CTK_PTR:     p= &v->u.rval_ptr    ; n=sizeof(ttarget_ptr) ;break ;
      case CTK_ARRAY:   /* the lvalue becomes an rvalue, a real pointer! */
                 duel_assert(v->val_kind==VK_LVALUE); /* duel exp rules */
                 v->val_kind=VK_RVALUE ;
                 v->ctype=duel_mkctype_ptr(v->ctype->u.kid) ;
                 v->u.rval_ptr=v->u.lvalue ;
                 return TRUE;   
      case CTK_FUNC:   /* makes it a pointer to a func*/
                 duel_assert(v->val_kind==VK_LVALUE); /* duel exp rules */
                 v->val_kind=VK_RVALUE ;
                 v->ctype=duel_mkctype_ptr(v->ctype) ;
                 v->u.rval_ptr=v->u.lvalue ;
                 return TRUE;   

      case CTK_STRUCT: /* can't have an rval from struct? */
      case CTK_UNION:  
      /* enums were eliminated above */
      default: duel_assert(0);
   }
   if(v->val_kind == VK_BVALUE) {        /* bitfield: lvalue+bitpos/len */
       tbvalue_info bv; 
       bv=v->u.bvalue ;
       ok=duel_get_target_bitfield(bv.lvalue, bv.bitpos, bv.bitlen, p,
                                    v->ctype->type_kind);
       if(!ok) { v->u.bvalue=bv ; return FALSE ; } 
   }
   else {
       ttarget_ptr lv=v->u.lvalue ;
       ok=duel_get_target_bytes(lv,p,n); /*fetch n debuggee bytes*/
       if(!ok) { v->u.lvalue=lv ; return FALSE ; }
   }

   v->val_kind=VK_RVALUE ;
   /* in remote debugging, one might need to swap byte order at this
    * point. [remote debugging is not supported by duel v1.0]
    */
   return TRUE ;
}


/*
 * safe get_rvalue - produce error messages on memory access failer.
 */

LPROC get_rvalue(tvalue *v,char *op)
{
   bool ok=duel_try_get_rvalue(v,op);
   if(!ok) duel_op_error("illegal address for operand x of '%s'",op,v,0);
}

/* put_rvalue: put the rvalue of v2 into the location in v1.
 * types are assumed to be the same.
 */

LPROC put_rvalue(tvalue *v1,tvalue *v2,char *op)
{
   void *p ;
   int n ;
   duel_assert(v1->val_kind!=VK_RVALUE);
   duel_assert(v1->val_kind!=VK_FVALUE);
   duel_assert(v2->val_kind==VK_RVALUE);
   duel_assert(v1->ctype->size == v2->ctype->size);
   switch(get_storage_type_kind(v2->ctype)) {
      case CTK_CHAR:    p= &v2->u.rval_char   ; n=sizeof(char)         ;break ;
      case CTK_SCHAR:   p= &v2->u.rval_schar  ; n=sizeof(tschar)       ;break ;
      case CTK_UCHAR:   p= &v2->u.rval_uchar  ; n=sizeof(tuchar)       ;break ;
      case CTK_USHORT:  p= &v2->u.rval_ushort ; n=sizeof(tushort)      ;break ;
      case CTK_SHORT:   p= &v2->u.rval_short  ; n=sizeof(short)        ;break ;
      case CTK_INT:     p= &v2->u.rval_int    ; n=sizeof(int)          ;break ;
      case CTK_UINT:    p= &v2->u.rval_uint   ; n=sizeof(tuint)        ;break ;
      case CTK_LONG:    p= &v2->u.rval_long   ; n=sizeof(long)         ;break ;
      case CTK_ULONG:   p= &v2->u.rval_ulong  ; n=sizeof(tulong)       ;break ;
      case CTK_FLOAT:   p= &v2->u.rval_float  ; n=sizeof(float)        ;break ;
      case CTK_DOUBLE:  p= &v2->u.rval_double ; n=sizeof(double)       ;break ;
      case CTK_PTR:     p= &v2->u.rval_ptr    ; n=sizeof(ttarget_ptr) ;break ;
      default: duel_assert(0);  /* other types not supported as rvalues */
   }
   if(v1->val_kind == VK_BVALUE) 
       duel_gen_error("assignment to bitfields is not yet supported",0);
   else
   if(!duel_put_target_bytes(v1->u.lvalue,p,n)) /*store n debuggee bytes*/
       duel_op_error("cant write memory for operand x of '%s'",op,v1,0);
}

/* set the symbolic val for tvalue. input format is a sprintf,
 * with v1,v2 being other values that show as '%s' in the format.
 * v1,v2 can be zero if they are unused by the format.
 */
PROC duel_set_symb_val(tvalue *r,char *format,tvalue *v1,tvalue *v2)
{
   char s[3*VALUE_MAX_SYMBOLIC_SIZE];
   sprintf(s,format,v1->symb_val,v2->symb_val);
   s[VALUE_MAX_SYMBOLIC_SIZE-1]=0 ; /* chop as needed */
   strcpy(r->symb_val,s);
}

/* given a small int type (short,char,enum) return the upgraded (int or 
 * uint) type. Else return the original type
 */
LFUNC tctype* upgrade_small_int_types(tctype *t)
{
   switch(t->type_kind) {
      case CTK_ENUM:
      case CTK_CHAR:
      case CTK_SCHAR:
      case CTK_UCHAR:
      case CTK_SHORT:
                     return ctype_int ;
      case CTK_USHORT:
                     if(sizeof(tushort)==sizeof(int)) return ctype_uint ;
                     else return ctype_int ;
        default:
                      return t ;    
   }
}

/* find the type of the result of a generic numeric operation on
 * v1,v2. This applies the standard C type upgrade rules.
 * The type of the result is returned.
 * r is setup so it can receive the result: an RVALUE of the specified
 * type. Its symbolic value is also setup based on the symbolic value
 * of v1 v2 and the operation op.
 * Note: op is not used to figure out the numeric result, only
 * the types of v1 and v2. As a side effect, the answer for x|y where
 * y is a double will be given as double. it is up to the caller to
 * verify that v1,v2 have meaningful types of this operation
 * 
 */

LFUNC tctype* find_numeric_result_type(tvalue *v1,tvalue *v2,
                                          tvalue *r,char *op)
{
    tctype *t1=upgrade_small_int_types(v1->ctype);   /* upgrade to int etc */
    tctype *t2=upgrade_small_int_types(v2->ctype);
    char s[80] ;
    r->val_kind=VK_RVALUE ;
    sprintf(s,"%%s%s%%s",op) ;  /* eg, if op=">>" then s becomes "%s>>%s" */
    duel_set_symb_val(r,s,v1,v2);

    if(t1==ctype_double || t2==ctype_double) return r->ctype=ctype_double ;
    if(t1==ctype_float  || t2==ctype_float)  return r->ctype=ctype_float ;
    if(t1==ctype_ulong  || t2==ctype_ulong)  return r->ctype=ctype_ulong ;
    if(sizeof(unsigned)==sizeof(long) &&
       (t1==ctype_long && t2==ctype_uint  ||
        t1==ctype_uint && t2==ctype_long))    return r->ctype=ctype_ulong ;
    if(t1==ctype_long  || t2==ctype_long)     return r->ctype=ctype_long ;
    if(t1==ctype_uint  || t2==ctype_uint)     return r->ctype=ctype_uint ;
    return r->ctype=ctype_int ;
}

/* convert_to_fix assisting-macro: takes the val stored in v and put it
 * into w. w is an lvalue with a 'fixed' type (t).
 */

#define convert_to_fix(v,w)                          \
     switch(get_storage_type_kind(v->ctype)) {         \
      case CTK_CHAR:    w v->u.rval_char     ; break ; \
      case CTK_SCHAR:   w v->u.rval_schar    ; break ; \
      case CTK_UCHAR:   w v->u.rval_uchar    ; break ; \
      case CTK_USHORT:  w v->u.rval_ushort   ; break ; \
      case CTK_SHORT:   w v->u.rval_short    ; break ; \
      case CTK_INT:     w v->u.rval_int      ; break ; \
      case CTK_UINT:    w v->u.rval_uint     ; break ; \
      case CTK_LONG:    w v->u.rval_long     ; break ; \
      case CTK_ULONG:   w v->u.rval_ulong    ; break ; \
      case CTK_FLOAT:   w v->u.rval_float    ; break ; \
      case CTK_DOUBLE:  w v->u.rval_double   ; break ; \
      case CTK_PTR:     w (tptrsize_int) v->u.rval_ptr ; break ; \
      default: duel_assert(0);                         \
   }


/* convert_scalar_type -- convert rvalue v to type t.
 * uses convert_to_fix macro. In effect, this is a huge switch for
 * all possible combinations of basic C types.
 */

LPROC convert_scalar_type(tvalue *v,tctype *t,char *op)
{
   get_rvalue(v,op);
   switch(get_storage_type_kind(t)) {
      case CTK_CHAR:   convert_to_fix(v,v->u.rval_char=(char))      ; break ;
      case CTK_SCHAR:  convert_to_fix(v,v->u.rval_schar=(tschar))   ; break ;
      case CTK_UCHAR:  convert_to_fix(v,v->u.rval_uchar=(tuchar))   ; break ;
      case CTK_SHORT:  convert_to_fix(v,v->u.rval_short=(short))    ; break ;
      case CTK_USHORT: convert_to_fix(v,v->u.rval_ushort=(tushort)) ; break ;
      case CTK_INT:    convert_to_fix(v,v->u.rval_int=(int))        ; break ;
      case CTK_UINT:   convert_to_fix(v,v->u.rval_uint=(tuint))     ; break ;
      case CTK_LONG:   convert_to_fix(v,v->u.rval_long=(long))      ; break ;
      case CTK_ULONG:  convert_to_fix(v,v->u.rval_ulong=(tulong))   ; break ;
      case CTK_FLOAT:  convert_to_fix(v,v->u.rval_float=(float))    ; break ;
      case CTK_DOUBLE: convert_to_fix(v,v->u.rval_double=(double))  ; break ;
      case CTK_PTR:    convert_to_fix(v,
                        v->u.rval_ptr=(ttarget_ptr)(tptrsize_int)) ; break ;
      default: duel_assert(0);
   }
   v->ctype=t ;
}


/* verify v is numeric, get its rvalue converted to type tout or at least int*/
LPROC get_numeric_val(tvalue *v,char *op,tctype *tout)
{
   if(!ctype_kind_numeric(v->ctype)) 
       duel_op_error("operand x of '%s' is not numeric",op,v,0);
   if(!tout) tout=upgrade_small_int_types(v->ctype);   /* upgrade to int etc */
   convert_scalar_type(v,tout,op);
}

/*verify v is integral, get its rvalue converted to type tout or at least int*/
LPROC get_integral_val(tvalue *v,char *op,tctype *tout)
{
   if(!ctype_kind_integral(v->ctype)) 
       duel_op_error("operand x of '%s' is not integral",op,v,0);
   if(!tout) tout=upgrade_small_int_types(v->ctype);   /* upgrade to int etc */
   convert_scalar_type(v,tout,op);
}

/* verify v is integral, return its actual value as 'int' */
FUNC int duel_get_int_val(tvalue *v,char *op)
{
   get_integral_val(v,op,ctype_int);
   return v->u.rval_int ;
}

/* verify v is numeric or pointer/array, upgrade type to at least int or ptr
   and get the rvalue */
LPROC get_scalar_val(tvalue *v,char *op)
{
   if(ctype_kind_ptr_like(v->ctype)) get_rvalue(v,op);
   else {
       tctype *t=upgrade_small_int_types(v->ctype);   /* upgrade to int */
       if(!ctype_kind_numeric(v->ctype)) 
           duel_op_error("operand x of '%s' is not a scalar",op,v,0);
       convert_scalar_type(v,t,op);
   }
}

LPROC get_pointer_val(tvalue *v,char *op,bool zero_ok)
{
   if(ctype_kind_ptr_like(v->ctype)) get_rvalue(v,op);
   else
   if(zero_ok && v->ctype->type_kind==CTK_INT && 
      v->val_kind==VK_RVALUE && v->u.rval_int==0) {
           v->ctype=ctype_voidptr ;
           v->u.rval_ptr=0 ;
   }
   else duel_op_error("operand x of '%s' is not a pointer",op,v,0);
}

/* copy one lvalue over the other. This copy is used for assignment,
 * including the assignment of structures and unions.
 * supports unlimited size and error reports when memory access fails.
 */

LPROC copy_lvalues(tvalue *v1,tvalue *v2,char *op)
{
    size_t size ;
    ttarget_ptr to=v1->u.lvalue,from=v2->u.lvalue ;
    char buf[BUFSIZ] ;
    duel_assert(v1->val_kind==VK_LVALUE && v2->val_kind==VK_LVALUE);
    size=v1->ctype->size ;
    duel_assert(v2->ctype->size==size);
    while(size!=0) {
        size_t chunk_size=((size>BUFSIZ)? BUFSIZ:size) ;
        if(!duel_get_target_bytes(from,buf,chunk_size))
            duel_op_error("error reading memory (copy) in '%s'",op,v1,v2);
        if(!duel_put_target_bytes(to,buf,chunk_size))
            duel_op_error("error writing memory (copy) in '%s'",op,v1,v2);
        size-=chunk_size ;
        to+=chunk_size ;
        from+=chunk_size ;
    }
}

/*
 * check that two values have "compatible" types.
 * since structs compiled in different modules are each unique,
 * we settle for comparing the number of references (array/ptr)
 * and then  make sure the same type-kind is used, with the same
 * physical size. 
 * this allows, e.g. struct {short x,y }  and struct {int x}
 * to be considered equal. Possibly one could compare struct/union
 * for member sizes (but not names?!). this however requires to keep
 * track of self references and is not implemented here.
 */

LPROC duel_check_type_eq(tvalue *v1,tvalue *v2,char *op)
{
    tctype *t1=v1->ctype, *t2=v2->ctype ;
    if(ctype_kind_ptr_like(t1) && ctype_kind_ptr_like(t2) &&  /*(void*) match*/
       (t1->u.kid==ctype_void || t2->u.kid==ctype_void)) return; 

    while(ctype_kind_ptr_like(t1) && ctype_kind_ptr_like(t2)) 
        t1=t1->u.kid, t2=t2->u.kid ;
    if(t1==t2) return ; /* exact same type */
    if(t1->type_kind != t2->type_kind || t1->size != t2->size) 
        duel_op_error("incompatible types for op %s",op,v1,v2);
}


/**************************************************************************
 a set of mid-level functions follow. These actually apply duel/C
 operators to values 
 **************************************************************************/

/* these do pointer+int addition/subtraction of v1,v2 and store result in r.
 * NOTE: r's symbolic value is not set.
 */

LPROC add_offset_to_ptr(tvalue *v1,tvalue *v2,tvalue *r)
{
   size_t len ;
   get_pointer_val(v1,"x+y (ptr add)",FALSE);
   get_integral_val(v2,"y+x (ptr add)",NULL);
   r->val_kind=VK_RVALUE ;
   r->ctype=v1->ctype ;
   len=v1->ctype->u.kid->size ;
   if(len==0) duel_op_error("unknown pointer object size for '+' op",0,v1,0);
   switch(v2->ctype->type_kind) {
    case CTK_INT:   r->u.rval_ptr =v1->u.rval_ptr +len*v2->u.rval_int  ;break ;
    case CTK_UINT:  r->u.rval_ptr =v1->u.rval_ptr +len*v2->u.rval_uint ;break ;
    case CTK_LONG:  r->u.rval_ptr =v1->u.rval_ptr +len*v2->u.rval_long ;break ;
    case CTK_ULONG: r->u.rval_ptr =v1->u.rval_ptr +len*v2->u.rval_ulong;break ;
    default: duel_assert(0);
   }
}

LPROC sub_offset_from_ptr(tvalue *v1,tvalue *v2,tvalue *r)
{
   size_t len ;
   get_pointer_val(v1,"x-y (ptr sub)",FALSE);
   get_integral_val(v2,"y-x (ptr sub)",NULL);
   r->val_kind=VK_RVALUE ;
   r->ctype=v1->ctype ;
   len=v1->ctype->u.kid->size ;
   if(len==0) duel_op_error("unknown pointer object size for '-' op",0,v1,0);
   switch(v2->ctype->type_kind) {
    case CTK_INT:   r->u.rval_ptr =v1->u.rval_ptr -len*v2->u.rval_int  ;break ;
    case CTK_UINT:  r->u.rval_ptr =v1->u.rval_ptr -len*v2->u.rval_uint ;break ;
    case CTK_LONG:  r->u.rval_ptr =v1->u.rval_ptr -len*v2->u.rval_long ;break ;
    case CTK_ULONG: r->u.rval_ptr =v1->u.rval_ptr -len*v2->u.rval_ulong;break ;
    default: duel_assert(0);
   }
}


/* do addition of v1,v2 and store result in r.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_add(tvalue *v1,tvalue *v2,tvalue *r)
{
   tctype *t=find_numeric_result_type(v1,v2,r,"+");
   if(ctype_kind_ptr_like(v1->ctype)) {
       get_integral_val(v2,"pointer+x",NULL);
       add_offset_to_ptr(v1,v2,r);
       return ;
   }
   if(ctype_kind_ptr_like(v2->ctype)) {
       get_integral_val(v1,"x+pointer",NULL);
       add_offset_to_ptr(v2,v1,r);
       return ;
   }
   get_numeric_val(v1,"x+y",t);
   get_numeric_val(v2,"y+x",t);
   r->val_kind=VK_RVALUE ;
   r->ctype=t ;
   duel_set_symb_val(r,"%s+%s",v1,v2);
   switch(t->type_kind) {
    case CTK_INT:   r->u.rval_int  =v1->u.rval_int   +v2->u.rval_int   ;break ;
    case CTK_UINT:  r->u.rval_uint =v1->u.rval_uint  +v2->u.rval_uint  ;break ;
    case CTK_LONG:  r->u.rval_long =v1->u.rval_long  +v2->u.rval_long  ;break ;
    case CTK_ULONG: r->u.rval_ulong=v1->u.rval_ulong +v2->u.rval_ulong ;break ;
    case CTK_FLOAT: r->u.rval_float=v1->u.rval_float +v2->u.rval_float ;break ;
    case CTK_DOUBLE:r->u.rval_double=v1->u.rval_double+v2->u.rval_double;break;
    default: duel_assert(0);
   }
}

/* do arithmeric subtraction of v1,v2 and store result in r.
 * v1 and v2 should be of numeric type to begin with.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_subtract(tvalue *v1,tvalue *v2,tvalue *r)
{
   tctype *t=find_numeric_result_type(v1,v2,r,"-");
   if(ctype_kind_ptr_like(v1->ctype)) {
       if(ctype_kind_ptr_like(v2->ctype)) {
           long len ;   /* length must be signed to allow signed p-q result*/
           get_pointer_val(v1,"x-y",FALSE);
           get_pointer_val(v2,"x-y",FALSE);
           duel_check_type_eq(v1,v2,"- (ptr)");
           /* should compare pointer types */
           len=v1->ctype->u.kid->size ;
           if(len<=0) 
              duel_op_error("illegal object size for op %s","- (ptr)",v1,v2);
           r->ctype=ctype_ptrdiff_t ;
           r->u.rval_ptrdiff_t= (v1->u.rval_ptr - v2->u.rval_ptr)/len ;
           return ;
       }
       get_integral_val(v2,"pointer-x",NULL);
       sub_offset_from_ptr(v1,v2,r);
       return ;
   }
   get_numeric_val(v1,"x-y",t);
   get_numeric_val(v2,"y-x",t);
   switch(t->type_kind) {
    case CTK_INT:   r->u.rval_int  =v1->u.rval_int   - v2->u.rval_int   ;break;
    case CTK_UINT:  r->u.rval_uint =v1->u.rval_uint  - v2->u.rval_uint  ;break;
    case CTK_LONG:  r->u.rval_long =v1->u.rval_long  - v2->u.rval_long  ;break;
    case CTK_ULONG: r->u.rval_ulong=v1->u.rval_ulong - v2->u.rval_ulong ;break;
    case CTK_FLOAT: r->u.rval_float=v1->u.rval_float - v2->u.rval_float ;break;
    case CTK_DOUBLE:r->u.rval_double=v1->u.rval_double-v2->u.rval_double;break;
    default: duel_assert(0);
   }
}

/* compare values of v1 and v2, knowing that at least one is a frame-value
 * type. Allows two fvals to be compared, or an fval to be compared
 * to a func (this compares the func at the frame to the given func)
 */

LFUNC bool comp_bin_op_eq_fvals(tvalue *v1,tvalue *v2)
{
   bool v1f=v1->val_kind == VK_FVALUE ;
   bool v2f=v2->val_kind == VK_FVALUE ;
   int frame_no ;
   ttarget_ptr frame_func,p ;
   if(v1f && v2f) return v1->u.fvalue == v2->u.fvalue ; /*cmp frames */
   if(v1f) { 
       frame_no = v1->u.fvalue ;
       get_pointer_val(v2,"frame==x",FALSE) ;
       if(v2->ctype->u.kid->type_kind!=CTK_FUNC) 
          duel_op_error("operand x of 'frame=x' not a func pointer",0,v2,0);
       p=v2->u.rval_ptr ;
   }
   else {
       frame_no = v2->u.fvalue ;
       get_pointer_val(v1,"x==frame",FALSE) ;
       if(v1->ctype->u.kid->type_kind!=CTK_FUNC) 
          duel_op_error("operand x of 'x==frame' not a func pointer",0,v1,0);
       p=v1->u.rval_ptr ;
   }
   frame_func = duel_get_function_for_frame(frame_no);
   return frame_func == p ; 
}


/* compares of v1,v2 and store result in r.
 * v1 and v2 should be of numeric/pointer type to begin with.
 * NOTE: v1, v2 are destroyed!
 */

LPROC do_op_eq(tvalue *v1,tvalue *v2,tvalue *r)
{                                                              
   tctype *t=find_numeric_result_type(v1,v2,r,"==");            
   r->ctype=ctype_int ;                                        
   if(v1->val_kind==VK_FVALUE || v2->val_kind==VK_FVALUE) {
       r->u.rval_int = comp_bin_op_eq_fvals(v1,v2);
       return ;
   }
   if(ctype_kind_ptr_like(v1->ctype) || ctype_kind_ptr_like(v2->ctype)) {
       get_pointer_val(v1,"x==y",TRUE); 
       get_pointer_val(v2,"y==x",TRUE);       
       duel_check_type_eq(v1,v2,"==");          
       r->u.rval_int = v1->u.rval_ptr == v2->u.rval_ptr ;
       return ;                                 
   }                                            
   get_numeric_val(v1,"x==y",t);                   
   get_numeric_val(v2,"y==x",t);                   
   switch(t->type_kind) {                       
   case CTK_INT:   r->u.rval_int=v1->u.rval_int    == v2->u.rval_int   ;break;
   case CTK_UINT:  r->u.rval_int=v1->u.rval_uint   == v2->u.rval_uint  ;break;
   case CTK_LONG:  r->u.rval_int=v1->u.rval_long   == v2->u.rval_long  ;break;
   case CTK_ULONG: r->u.rval_int=v1->u.rval_ulong  == v2->u.rval_ulong ;break;
   case CTK_FLOAT: r->u.rval_int=v1->u.rval_float  == v2->u.rval_float ;break;
   case CTK_DOUBLE:r->u.rval_int=v1->u.rval_double == v2->u.rval_double;break;
   default: duel_assert(0);                                     
   }                                                            
}


/* compares of v1,v2 and store result in r.
 * v1 and v2 should be of numeric/pointer type to begin with.
 * NOTE: v1, v2 are destroyed!
 */
#define mk_func_compare(func,op,sop,xysop,yxsop,nullok) \
LPROC func(tvalue *v1,tvalue *v2,tvalue *r)                    \
{                                                              \
   tctype *t=find_numeric_result_type(v1,v2,r,sop);             \
   r->ctype=ctype_int ;                                         \
                                                                \
   if(ctype_kind_ptr_like(v1->ctype) || ctype_kind_ptr_like(v2->ctype)) { \
       get_pointer_val(v1,xysop,nullok);                        \
       get_pointer_val(v2,yxsop,nullok);                        \
       duel_check_type_eq(v1,v2,sop);                           \
       r->u.rval_int = v1->u.rval_ptr op v2->u.rval_ptr ;       \
       return ;                                                 \
   }                                                            \
   get_numeric_val(v1,xysop,t);                                 \
   get_numeric_val(v2,yxsop,t);                                 \
   switch(t->type_kind) {                                       \
   case CTK_INT:   r->u.rval_int=v1->u.rval_int    op v2->u.rval_int   ;break;\
   case CTK_UINT:  r->u.rval_int=v1->u.rval_uint   op v2->u.rval_uint  ;break;\
   case CTK_LONG:  r->u.rval_int=v1->u.rval_long   op v2->u.rval_long  ;break;\
   case CTK_ULONG: r->u.rval_int=v1->u.rval_ulong  op v2->u.rval_ulong ;break;\
   case CTK_FLOAT: r->u.rval_int=v1->u.rval_float  op v2->u.rval_float ;break;\
   case CTK_DOUBLE:r->u.rval_int=v1->u.rval_double op v2->u.rval_double;break;\
   default: duel_assert(0);                                     \
   }                                                            \
}

mk_func_compare(do_op_ne,!=,"!=","x!=y","y!=x",TRUE)
mk_func_compare(do_op_ge,>=,">=","x>=y","y>=x",FALSE)
mk_func_compare(do_op_le,<=,"<=","x<=y","y<=x",FALSE)
mk_func_compare(do_op_ls,<, "<", "x<y", "y<x",FALSE)
mk_func_compare(do_op_gt,>, ">", "x>y", "y>x",FALSE)
#undef mk_func_compare 


/* do_compare_questionmark -- handle the <? >? etc ops */

LFUNC bool do_compare_questionmark(topcode op,tvalue *v1,tvalue *v2,tvalue *r)
{
   tvalue tmp ;
   tmp= *v1 ; 
   switch(op) {
     case OP_EQQ: do_op_eq(v1,v2,r); break ;
     case OP_NEQ: do_op_ne(v1,v2,r); break ;
     case OP_GEQ: do_op_ge(v1,v2,r); break ;
     case OP_LEQ: do_op_le(v1,v2,r); break ;
     case OP_LSQ: do_op_ls(v1,v2,r); break ;
     case OP_GTQ: do_op_gt(v1,v2,r); break ;
   }
   if(r->u.rval_int==0) return FALSE ;
   *r=tmp ;
   return TRUE ;
}



/* apply indirection of a pointer.
 * this  is easy, you just force the value to be an rvalue pointer, then 
 * make it into an lvalue with the pointed-to type.
 * useful for (*x x[y] x->y etc)
 * does not setup a symbolic value!
 */


LPROC follow_pointer(tvalue *v,char *op,bool nonull)
{
   get_pointer_val(v,op,FALSE);
   if(nonull && v->u.rval_ptr == NULL)
        duel_op_error("dereference NULL pointer x in '%s'",op,v,0);
   v->val_kind=VK_LVALUE ;
   v->u.lvalue=v->u.rval_ptr ;
   v->ctype=v->ctype->u.kid ;
}

PROC duel_get_struct_val(tvalue *v,char *op)
{
   if(!ctype_kind_struct_like(v->ctype)) 
       duel_op_error("operand x of '%s' not a sturct/union",op,v,0);
   duel_assert(v->val_kind==VK_LVALUE);
}

PROC duel_get_struct_ptr_val(tvalue *v,char *op)
{
   follow_pointer(v,op,FALSE);
   if(!ctype_kind_struct_like(v->ctype)) 
     duel_op_error("operand x of '%s' not a pointer to sturct/union",op,v,0);
}

FUNC int duel_get_posint_val(tvalue *v,char *op)    
{
    int x ;
    x=duel_get_int_val(v,op);
    if(x<0) duel_op_error("operand x of '%s' can not be negative",op,v,0);
   return x ;
}

/* indirection operator (*x) */

LPROC do_op_indirection(tvalue *v)
{
   follow_pointer(v,"*x",TRUE);
   duel_set_symb_val(v,"*%s",v,0);
}

/* address operator (&x) 
 * x must be an lvalue. it is converted into a pointer to the given type,
 * an rvalue.
 */

LPROC do_op_address(tvalue *v)
{
   if(v->val_kind != VK_LVALUE) 
       duel_op_error("operand x of '&x' is not a lvalue",0,v,0);
   v->val_kind=VK_RVALUE ;
   v->u.rval_ptr=v->u.lvalue ;
   v->ctype=duel_mkctype_ptr(v->ctype);
   duel_set_symb_val(v,"&%s",v,0);
}

LPROC do_op_index(tvalue *v1,tvalue *v2,tvalue *r)
{
    get_pointer_val(v1,"x[y]",FALSE);
    get_integral_val(v2,"y[x]",NULL);
    add_offset_to_ptr(v1,v2,r);
    follow_pointer(r,"[]",TRUE);
    duel_set_symb_val(r,"%s[%s]",v1,v2);
}


/* do arithmeric multiply of v1,v2 and store result in r.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_multiply(tvalue *v1,tvalue *v2,tvalue *r)
{
   tctype *t=find_numeric_result_type(v1,v2,r,"*");
   get_numeric_val(v1,"x*y",t);
   get_numeric_val(v2,"y*x",t);
   r->val_kind=VK_RVALUE ;
   r->ctype=t ;
   duel_set_symb_val(r,"%s*%s",v1,v2);
   switch(t->type_kind) {
    case CTK_INT:   r->u.rval_int   =v1->u.rval_int   * v2->u.rval_int  ;break;
    case CTK_UINT:  r->u.rval_uint  =v1->u.rval_uint  * v2->u.rval_uint ;break;
    case CTK_LONG:  r->u.rval_long  =v1->u.rval_long  * v2->u.rval_long ;break;
    case CTK_ULONG: r->u.rval_ulong =v1->u.rval_ulong * v2->u.rval_ulong;break;
    case CTK_FLOAT: r->u.rval_float =v1->u.rval_float * v2->u.rval_float;break;
    case CTK_DOUBLE:r->u.rval_double=v1->u.rval_double*v2->u.rval_double;break;
    default: duel_assert(0);
   }
}

/* do numeric divide of v1,v2 and store result in r.
 * v1 and v2 should be of numeric type to begin with.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_divide(tvalue *v1,tvalue *v2,tvalue *r)
{
   tctype *t=find_numeric_result_type(v1,v2,r,"/");
   get_numeric_val(v1,"x/y",t);
   get_numeric_val(v2,"y/x",t);
   switch(t->type_kind) {
    case CTK_INT:   if(v2->u.rval_int==0) goto div_err;
                    r->u.rval_int   =v1->u.rval_int  / v2->u.rval_int   ;break;
    case CTK_UINT:  if(v2->u.rval_uint==0) goto div_err;
                    r->u.rval_uint  =v1->u.rval_uint / v2->u.rval_uint  ;break;
    case CTK_LONG:  if(v2->u.rval_long==0) goto div_err;
                    r->u.rval_long  =v1->u.rval_long / v2->u.rval_long  ;break;
    case CTK_ULONG: if(v2->u.rval_ulong==0) goto div_err;
                    r->u.rval_ulong =v1->u.rval_ulong/ v2->u.rval_ulong ;break;
    case CTK_FLOAT: if(v2->u.rval_float==0.0) goto div_err;
                    r->u.rval_float =v1->u.rval_float/ v2->u.rval_float ;break;
    case CTK_DOUBLE:if(v2->u.rval_double==0.0) goto div_err;
                    r->u.rval_double=v1->u.rval_double/v2->u.rval_double;break;
    default: duel_assert(0);
   }
   return ;
div_err: duel_op_error("division by zero",0,v1,v2);
}


/* do arithmeric reminder of v1,v2 and store result in r.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_reminder(tvalue *v1,tvalue *v2,tvalue *r)
{
   tctype *t=find_numeric_result_type(v1,v2,r,"%");
   get_integral_val(v1,"x%y",t);
   get_integral_val(v2,"y%x",t);
   switch(t->type_kind) {
    case CTK_INT:    if(v2->u.rval_int==0) goto div_err;
               r->u.rval_int   = v1->u.rval_int   % v2->u.rval_int    ; break ;
    case CTK_UINT:   if(v2->u.rval_uint==0) goto div_err;
               r->u.rval_uint  = v1->u.rval_uint  % v2->u.rval_uint   ; break ;
    case CTK_LONG:   if(v2->u.rval_long==0) goto div_err;
               r->u.rval_long  = v1->u.rval_long  % v2->u.rval_long   ; break ;
    case CTK_ULONG:  if(v2->u.rval_ulong==0) goto div_err;
               r->u.rval_ulong = v1->u.rval_ulong % v2->u.rval_ulong  ; break ;
    default: duel_assert(0);
   }
   return ;
div_err: duel_op_error("reminder modulo zero",0,v1,v2);
}

/* do arithmeric or (bitwise) of v1,v2 and store result in r.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_or(tvalue *v1,tvalue *v2,tvalue *r)
{
   tctype *t=find_numeric_result_type(v1,v2,r,"|");
   get_integral_val(v1,"x|y",t);
   get_integral_val(v2,"y|x",t);
   switch(t->type_kind) {
    case CTK_INT:  r->u.rval_int  = v1->u.rval_int   | v2->u.rval_int  ; break;
    case CTK_UINT: r->u.rval_uint = v1->u.rval_uint  | v2->u.rval_uint ; break;
    case CTK_LONG: r->u.rval_long = v1->u.rval_long  | v2->u.rval_long ; break;
    case CTK_ULONG:r->u.rval_ulong= v1->u.rval_ulong | v2->u.rval_ulong; break;
    default: duel_assert(0);
   }
   return ;
}

/* do arithmeric and (bitwise) of v1,v2 and store result in r.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_and(tvalue *v1,tvalue *v2,tvalue *r)
{
   tctype *t=find_numeric_result_type(v1,v2,r,"&");
   get_integral_val(v1,"x&y",t);
   get_integral_val(v2,"y&x",t);
   switch(t->type_kind) {
    case CTK_INT:  r->u.rval_int  = v1->u.rval_int   & v2->u.rval_int  ; break;
    case CTK_UINT: r->u.rval_uint = v1->u.rval_uint  & v2->u.rval_uint ; break;
    case CTK_LONG: r->u.rval_long = v1->u.rval_long  & v2->u.rval_long ; break;
    case CTK_ULONG:r->u.rval_ulong= v1->u.rval_ulong & v2->u.rval_ulong; break;
    default: duel_assert(0);
   }
}

/* do arithmeric xor (bitwise) of v1,v2 and store result in r.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_xor(tvalue *v1,tvalue *v2,tvalue *r)
{
   
   tctype *t=find_numeric_result_type(v1,v2,r,"^");
   get_integral_val(v1,"x^y",t);
   get_integral_val(v2,"y^x",t);
   switch(t->type_kind) {
    case CTK_INT:  r->u.rval_int  = v1->u.rval_int  ^ v2->u.rval_int  ; break ;
    case CTK_UINT: r->u.rval_uint = v1->u.rval_uint ^ v2->u.rval_uint ; break ;
    case CTK_LONG: r->u.rval_long = v1->u.rval_long ^ v2->u.rval_long ; break ;
    case CTK_ULONG:r->u.rval_ulong= v1->u.rval_ulong^ v2->u.rval_ulong; break ;
    default: duel_assert(0);
   }
   return ;
}

/* do arithmeric leftshift of v1,v2 and store result in r.
 * v1 and v2 should be of numeric type to begin with.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_leftshift(tvalue *v1,tvalue *v2,tvalue *r)
{
   int by;
   get_integral_val(v1,"x<<y",NULL);
   by=duel_get_int_val(v2,"y<<x");
   r->val_kind=VK_RVALUE ;
   r->ctype=v1->ctype ;
   duel_set_symb_val(r,"%s<<%s",v1,v2);
   switch(v1->ctype->type_kind) {
    case CTK_INT:    r->u.rval_int   = v1->u.rval_int   << by ; break ;
    case CTK_UINT:   r->u.rval_uint  = v1->u.rval_uint  << by ; break ;
    case CTK_LONG:   r->u.rval_long  = v1->u.rval_long  << by ; break ;
    case CTK_ULONG:  r->u.rval_ulong = v1->u.rval_ulong << by ; break ;
    default: duel_assert(0);
   }
}

/* do arithmeric rightshift of v1,v2 and store result in r.
 * v1 and v2 should be of numeric type to begin with.
 * NOTE: v1, v2 are destroyed!
 */
LPROC do_op_rightshift(tvalue *v1,tvalue *v2,tvalue *r)
{
   int by ;
   get_integral_val(v1,"x>>y",NULL);
   by=duel_get_int_val(v2,"y>>x");
   r->val_kind=VK_RVALUE ;
   r->ctype=v1->ctype ;
   duel_set_symb_val(r,"%s>>%s",v1,v2);
   switch(v1->ctype->type_kind) {
    case CTK_INT:    r->u.rval_int   = v1->u.rval_int   >> by ; break ;
    case CTK_UINT:   r->u.rval_uint  = v1->u.rval_uint  >> by ; break ;
    case CTK_LONG:   r->u.rval_long  = v1->u.rval_long  >> by ; break ;
    case CTK_ULONG:  r->u.rval_ulong = v1->u.rval_ulong >> by ; break ;
    default: duel_assert(0);
   }
}

/* do arithmeric not of v (!v) */
LPROC do_op_not(tvalue *v)
{
   get_scalar_val(v,"!x");
   duel_set_symb_val(v,"!%s",v,0);
   switch(v->ctype->type_kind) {
    case CTK_PTR:    v->u.rval_int   =  ! v->u.rval_ptr    ; break ;
    case CTK_INT:    v->u.rval_int   =  ! v->u.rval_int    ; break ;
    case CTK_UINT:   v->u.rval_int   =  ! v->u.rval_uint   ; break ;
    case CTK_LONG:   v->u.rval_int   =  ! v->u.rval_long   ; break ;
    case CTK_ULONG:  v->u.rval_int   =  ! v->u.rval_ulong  ; break ;
    case CTK_FLOAT:  v->u.rval_int   =  ! v->u.rval_float  ; break ;
    case CTK_DOUBLE: v->u.rval_int   =  ! v->u.rval_double ; break ;
    default: duel_assert(0);
   }
   v->ctype=ctype_int ;         /* always returns an int */
}

/* do bit-complement of v (~v) */
LPROC do_op_complement(tvalue *v)
{
   get_integral_val(v,"~x",NULL);
   duel_set_symb_val(v,"~%s",v,0);
   switch(v->ctype->type_kind) {
    case CTK_INT:    v->u.rval_int   =  ~ v->u.rval_int    ; break ;
    case CTK_UINT:   v->u.rval_uint  =  ~ v->u.rval_uint   ; break ;
    case CTK_LONG:   v->u.rval_long  =  ~ v->u.rval_long   ; break ;
    case CTK_ULONG:  v->u.rval_ulong =  ~ v->u.rval_ulong  ; break ;
    default: duel_assert(0);
   }
}

 /* do arithmeric minus of v (-v) */
LPROC do_op_minus(tvalue *v)
{
   get_numeric_val(v,"-x",NULL);
   duel_set_symb_val(v,"-%s",v,0);
   switch(v->ctype->type_kind) {
    case CTK_INT:    v->u.rval_int   =  - v->u.rval_int    ; break ;
    case CTK_UINT:   v->u.rval_uint  =  - v->u.rval_uint   ; break ;
    case CTK_LONG:   v->u.rval_long  =  - v->u.rval_long   ; break ;
    case CTK_ULONG:  v->u.rval_ulong =  - v->u.rval_ulong  ; break ;
    case CTK_FLOAT:  v->u.rval_float =  - v->u.rval_float  ; break ;
    case CTK_DOUBLE: v->u.rval_double=  - v->u.rval_double ; break ;
    default: duel_assert(0);
   }
}

/* do sizeif(exp) - simply return the size of the exp's type */
LPROC do_op_sizeofexp(tvalue *v)
{
    v->val_kind=VK_RVALUE ;
    v->u.rval_size_t=v->ctype->size ;
    v->ctype=ctype_size_t ;
    duel_set_symb_val(v,"sizeof(%s)",v,0);
}

LPROC do_op_assignment(tvalue *v1,tvalue *v2,tvalue *r,char *op)
{
    char xopy[10],yopx[10] ;
    sprintf(xopy,"x%sy",op);
    sprintf(yopx,"y%sx",op);
    if(v1->val_kind!=VK_LVALUE && v1->val_kind!=VK_BVALUE)
       duel_op_error("operand x is not an lvalue for operator '%s'",xopy,v1,0);

    if(ctype_kind_struct_like(v1->ctype)) {
        duel_check_type_eq(v1,v2,xopy);
        copy_lvalues(v1,v2,xopy);
        *r= *v2 ;  /* return the result as an lvalue. this means (x1=x2)=x3
                      is legal for struct, unlike ansi-c. */
        return ;
    }
    if(ctype_kind_numeric(v1->ctype)) get_numeric_val(v2,yopx,v1->ctype);
    else
    if(v1->ctype->type_kind==CTK_PTR) {
      get_pointer_val(v2,yopx,TRUE);
      duel_check_type_eq(v1,v2,xopy);
    }
    else duel_op_error("bad operand x type for operator '%s'",xopy,v1,0);
    put_rvalue(v1,v2,xopy);
    *r= *v2 ;
}

LPROC do_op_increment(tvalue *v,char *op,int inc,bool postfix)
{
    tvalue lvalue_v,oldv ;
    char s[80] ;
    if(v->val_kind!=VK_LVALUE) 
      duel_op_error("operand of '%s' must be an lvalue",op,v,0);
    lvalue_v= *v ;
    if(v->ctype->type_kind==CTK_PTR) get_pointer_val(v,op,FALSE);
    else get_integral_val(v,op,0);
    oldv= *v ;

   switch(v->ctype->type_kind) {
    case CTK_INT:   v->u.rval_int   +=  inc ; break ;
    case CTK_UINT:  v->u.rval_uint  +=  inc ; break ;
    case CTK_LONG:  v->u.rval_long  +=  inc ; break ;
    case CTK_ULONG: v->u.rval_ulong +=  inc ; break ;
    case CTK_PTR:   v->u.rval_ptr   +=  inc*v->ctype->u.kid->size ; break;
    default: duel_assert(0);
   }
   convert_scalar_type(v,lvalue_v.ctype,op);  /* back to the original type */
   put_rvalue(&lvalue_v,v,op);
   if(postfix) {                        /* if prefix, keep v and its sym val*/
       *v= oldv ;
       convert_scalar_type(v,lvalue_v.ctype,op);  /* back to original type */
       sprintf(s,"%%s%s",op);
       duel_set_symb_val(v,s,v,0);
   }
}

/* unary op 'frame(n)' converts int n to a "frame type" */
LPROC do_op_frame(tvalue *v)
{
   int f=duel_get_int_val(v,"frame(x)");
   if(f<0 || f>=duel_get_frames_number()) 
       duel_gen_error("Frame number too big",0);
   v->val_kind=VK_FVALUE ;
   v->ctype=ctype_int ;
   v->u.fvalue=f ;
   duel_set_symb_val(v,"frame(%s)",v,0);
}

/* unary op '(x)'. this only add parenthesis to the symbolic value, if needed*/

LPROC do_op_parenthesis(tvalue *v)
{
    char *s=v->symb_val ;
    int l=strlen(s);
    if(s[0]=='(' && s[l-1]==')') return ; /* val is (x) dont make it ((x)) */
    while(*s!=0 && (isalnum(*s) || *s=='_')) s++ ; /* find first non alnum*/
    if(*s==0) return ;  /* no need for (x) if x is a name or constant number */
    duel_set_symb_val(v,"(%s)",v,0); 
}

/***********************************************************************
 High-level functions, major entries to this module, evaluate a node
 with a standard (single value) result
 ***********************************************************************/


/* standardize a paramater to a function call according to the standard rules.
 * we currently don't support union/struct paramater passing
 */

PROC duel_standardize_func_parm(tvalue *p)
{
   /* convert paramater into "standard" function calling */   
   if(ctype_kind_integral(p->ctype)) get_integral_val(p,"f()",NULL);
   else 
   if(ctype_kind_numeric(p->ctype))     /* float, double to double */
       convert_scalar_type(p,ctype_double,"f()");
   else
   if(ctype_kind_ptr_like(p->ctype)) get_pointer_val(p,"f()",FALSE);
   else
       duel_op_error("unsupported paramater type",0,p,0);
}


/* given a function (or pointer), find the first (top-most) frame that
 * the function is active in, and return the FVALUE  for it
 */
PROC duel_find_func_frame(tvalue *v,char *op)
{
    int i, frames_no=duel_get_frames_number();
    get_pointer_val(v,op,FALSE);

    for(i=0 ; i<frames_no ; i++) {
        ttarget_ptr frame_func = duel_get_function_for_frame(i);
        if(frame_func==v->u.rval_ptr) {
            v->val_kind=VK_FVALUE ;
            v->ctype=ctype_int ;
            v->u.rval_int=i ;
            return ;
        }
    }
    duel_op_error("func x is not on the call stack for operator '%s'",op,v,0);
}


/* compute the n'th result of v1..v2 operator.
 * result is returned in r, returns false if there isnt an nth result
 * v1,v2 are converted to integer values (but can be used to call this 
 * function again. errors are reported if they arenot integral.
 */

FUNC bool duel_do_op_to(tvalue *v1,tvalue *v2,int n,tvalue *r)
{
    int a,b,inc,x ;
    char *p,*fmt ;

    if(!v1) { a=0 ; b=duel_get_int_val(v2,"..x")-1 ; }
    else if(!v2) { a=duel_get_int_val(v1,"x.."); b=INT_MAX ; }
    else { a=duel_get_int_val(v1,"x..y"); b=duel_get_int_val(v2,"x..y"); }
    if(a<=b) inc=1 ;
    else inc= -1 ;
    x=a+n*inc ;
    if(inc>0 && x>b || inc<0 && x<b) return FALSE ;

    r->val_kind=VK_RVALUE ;
    r->ctype=ctype_int ;
    r->u.rval_int=x ;
    if(v1) p=v1->symb_val ;
    else   p=v2->symb_val ;
    if(     *p=='0'  && p[1]=='x'  || p[1]=='X')  fmt="0x%x" ;
    else if(*p=='0'  && p[1]>='0'  && p[1]<='7')  fmt="0%o"  ;
    else if(*p=='\'' && isascii(x) && isprint(x)) fmt="'%c'" ;
    else                                          fmt="%d"   ;
    sprintf(r->symb_val,fmt,x);
    return TRUE ;
}

/* convert value to True/False and return it 
 * in case of an illegal operand, indicateds the operator is (op)
 * main use: in operators '&&' '||' and 'if'
 */

FUNC bool duel_mk_logical(tvalue *v,char *op)
{
   get_scalar_val(v,op);                /* verify v is scalar */
   do_op_not(v);                        /* convert and force 0 or 1 */
   v->u.rval_int = !v->u.rval_int ;
   sprintf(v->symb_val,"%d",v->u.rval_int);
   return(v->u.rval_int);
}

/* cast the value v into type t. */

PROC duel_do_cast(tctype *tout,tvalue *v)
{
    tctype *t=v->ctype ;
    if(ctype_kind_scalar(t) && ctype_kind_scalar(tout)) {
        if(tout->type_kind==CTK_ARRAY) 
            duel_gen_error("casting to an array type is illegal",0);
        else convert_scalar_type(v,tout,"(cast) x");
    }
    else duel_op_error("illegal type conversion in cast op",0,v,0);
}


/* apply unary oprand op to the given value. The original value is 
 * destoryed (of course)
 */

PROC duel_apply_unary_op(topcode op,tvalue *v)
{
   switch(op) {
      case '(':  do_op_parenthesis(v);            break ;
      case '{':  duel_sprint_scalar_value(v->symb_val,v);break ;
      case '-':  do_op_minus(v);                  break ;
      case '!':  do_op_not(v);                    break ;
      case '*':  do_op_indirection(v);            break ;
      case '~':  do_op_complement(v);             break ;
      case '&':  do_op_address(v);                break ;
      case OP_SIZ: do_op_sizeofexp(v);            break ;
      case OP_INC: do_op_increment(v,"++",1,FALSE); break ;
      case OP_DEC: do_op_increment(v,"--",-1,FALSE); break ;
      case OP_FRAME: do_op_frame(v);    break ;
      default: duel_assert(0);
   }
}

PROC duel_apply_post_unary_op(topcode op,tvalue *v)
{
   switch(op) {
      case OP_INC: do_op_increment(v,"++",1,TRUE); break ;
      case OP_DEC: do_op_increment(v,"--",-1,TRUE); break ;
      default: duel_assert(0);
   }
}


/* apply_bin_op -- apply the operator op to the values v1 v2 and return
 *                 the result in r. the bin op is a 'regular' one, like
 *                 '+', '*', etc. in the C language.
 * note: v1,v2 are destroyed
 * returns: true if a value has been produced, false otherwise. 
 *       (ops like '+' always return true. ops like '<=?' also return false)
 */

FUNC bool duel_apply_bin_op(topcode op,tvalue *v1,tvalue *v2,tvalue *r)
{
  switch(op) {
   case '[': do_op_index(v1,v2,r); break ;
   case '+':  do_op_add(v1,v2,r);      break ;
   case '-':  do_op_subtract(v1,v2,r); break ;
   case '*':  do_op_multiply(v1,v2,r); break ;
   case '/':  do_op_divide(v1,v2,r);   break ;
   case '%':  do_op_reminder(v1,v2,r); break ;
   case '|':  do_op_or(v1,v2,r);       break;
   case '&':  do_op_and(v1,v2,r);      break ;
   case '^':  do_op_xor(v1,v2,r); break ;
   case OP_LSH: do_op_leftshift(v1,v2,r); break ;
   case OP_RSH: do_op_rightshift(v1,v2,r); break ;
   case OP_EQ:  do_op_eq(v1,v2,r); break ;
   case OP_NE:  do_op_ne(v1,v2,r); break ;
   case OP_GE:  do_op_ge(v1,v2,r); break ;
   case OP_LE:  do_op_le(v1,v2,r); break ;
   case '<':    do_op_ls(v1,v2,r); break ;
   case '>':    do_op_gt(v1,v2,r); break ;
   case OP_EQQ: 
   case OP_NEQ:
   case OP_GEQ:
   case OP_LEQ:
   case OP_LSQ:
   case OP_GTQ: return do_compare_questionmark(op,v1,v2,r); 
   case '=':    do_op_assignment(v1,v2,r,"="); break ;
   default: duel_assert(0);
  }
  return TRUE ;
}
