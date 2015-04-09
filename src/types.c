/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code			           */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/types.c,v 1.8 93/03/12 06:01:50 mg Exp $*/

/* this module contains the duel type system management
 */

/*
 * $Log:	types.c,v $
 * Revision 1.8  93/03/12  06:01:50  mg
 * use tuint for uint etc
 * 
 * Revision 1.7  93/02/03  21:55:26  mg
 * support "signed char"
 * 
 * Revision 1.6  93/01/12  21:54:25  mg
 * cleanup and set for release
 * 
 * Revision 1.5  93/01/03  07:31:24  mg
 * *** empty log message ***
 * 
 * Revision 1.4  92/10/19  15:09:28  mg
 * support zero fields/enumerators because gdb have them sometimes.
 * 
 * Revision 1.3  92/10/14  02:07:00  mg
 * misc
 * 
 * Revision 1.2  92/09/16  11:11:43  mg
 * added builtin charptr type
 * 
 */

#include "duel.h"

FUNC tctype* duel_mkctype_ptr(tctype *t)
{
  tctype *n ;
  n=(tctype *) duel_malloc(sizeof(tctype));
  duel_bzero((char*) n,sizeof(tctype));
  n->type_kind=CTK_PTR ;
  n->size=sizeof(void*);
  n->u.kid=t ;
  return n ;
}

FUNC tctype* duel_mkctype_func(tctype *t)
{
  tctype *n ;
  n=(tctype *) duel_malloc(sizeof(tctype));
  duel_bzero((char*) n,sizeof(tctype));
  n->size=0 ;
  n->type_kind=CTK_FUNC ;
  n->u.kid=t ;
  return n ;
}

/* create a struct or union type. The fields are not set here, but
 * filled individually with the next function
 * note the having zero fields is supported. This shouldnt be legal in C
 * but compilers allow pointer to sturct w/o every specifying the struct.
 * this is especially true for gdb itself!
 */

FUNC tctype* duel_mkctype_struct(char *name,size_t size,int fields_no,
				 bool is_union)
{
  tctype *n ;
  n=(tctype *) duel_malloc(sizeof(tctype));
  duel_bzero((char*) n,sizeof(tctype));
  n->name=name ;
  n->size=size ;
  if(is_union) n->type_kind=CTK_UNION ;
  else n->type_kind=CTK_STRUCT ;
  n->u.f.fields_no=fields_no ;
  if(fields_no==0) n->u.f.fields=NULL ;
  else {
    n->u.f.fields=(tctype_field *) duel_malloc(fields_no*sizeof(tctype_field));
    duel_bzero((char*) n->u.f.fields,fields_no*sizeof(tctype_field));
  }
  return n ;
}

/* insert field (field_no) into sturct/union (t), with type fctype 
 */
PROC duel_mkctype_struct_field(tctype *t,int field_no,char *name,
			       int bitpos,int bitlen, tctype *fctype)
{
   tctype_field *f ;
   duel_assert(t->type_kind==CTK_STRUCT || t->type_kind==CTK_UNION);
   duel_assert(field_no>=0 && field_no <= t->u.f.fields_no);
   f= &t->u.f.fields[field_no] ;
   f->name=name ;
   f->bitpos=bitpos ;
   f->bitlen=bitlen ;
   f->ctype=fctype ;
}

/* create an enum type. The enumerators are not set here, but
 * filled individually with the next function
 * again like sturct, we support zero enums. I am not sure its needed,
 * but better safe than sorry.
 */

FUNC tctype* duel_mkctype_enum(char *name,tctype_kind real_type_kind,
			       size_t size,int enumerators_no)
{
  tctype *n ;
  n=(tctype *) duel_malloc(sizeof(tctype));
  duel_bzero((char*) n,sizeof(tctype));
  n->name=name ;
  n->size=size ;
  n->type_kind=CTK_ENUM ;
  n->u.e.real_type_kind=real_type_kind ;
  n->u.e.enumerators_no=enumerators_no ;
  if(enumerators_no==0) n->u.e.enumerators=NULL ;
  else {
      n->u.e.enumerators= (tctype_enumerator *) 
                   duel_malloc(enumerators_no*sizeof(tctype_enumerator));
      duel_bzero((char*) n->u.e.enumerators,
	           enumerators_no*sizeof(tctype_enumerator));
  }
  return n ;
}


/* insert enumerator (enumerator_no_ into an enum type (t), with given name/val
 */
PROC duel_mkctype_enumerator(tctype *t,int enumerator_no,char *name,int val)
{
   tctype_enumerator *e ;
   duel_assert(t->type_kind==CTK_ENUM);
   duel_assert(enumerator_no>=0 && enumerator_no <= t->u.e.enumerators_no);
   e= &t->u.e.enumerators[enumerator_no] ;
   e->name=name ;
   e->val=val ;
}

FUNC tctype* duel_mkctype_array(tctype *t,int size)
{
  tctype *n ;
  if(t->size==0) duel_gen_error("array of a type of zero size is illegal",0);
  if(size<=0) 	 duel_gen_error("array of size zero or negative is illegal",0);
  n=(tctype *) duel_malloc(sizeof(tctype));
  duel_bzero((char*) n,sizeof(tctype));
  n->type_kind=CTK_ARRAY ;
  n->size=size*t->size ;
  n->u.kid=t ;
  return n ;
}


LFUNC tctype* init_basic_ctype(tctype_kind tk,char *name,size_t size)
{
    tctype *p = duel_malloc(sizeof(tctype));
    p->type_kind=tk ;
    p->name=name ;
    p->size=size ;
    return p ;
}

PROC duel_init_basic_ctypes(void)
{
  ctype_void= init_basic_ctype(CTK_VOID, "void", 0);

  ctype_char =init_basic_ctype(CTK_CHAR,    "char", sizeof(char));
  ctype_short=init_basic_ctype(CTK_SHORT,   "short",sizeof(short));
  ctype_int  =init_basic_ctype(CTK_INT,     "int",  sizeof(int));
  ctype_long =init_basic_ctype(CTK_LONG,    "long", sizeof(long));

  ctype_schar =init_basic_ctype(CTK_SCHAR,  "signed char", sizeof(tschar));
  ctype_uchar =init_basic_ctype(CTK_UCHAR,  "unsigned char", sizeof(tuchar));
  ctype_ushort=init_basic_ctype(CTK_USHORT, "unsigned short",sizeof(tushort));
  ctype_uint  =init_basic_ctype(CTK_UINT,   "unsigned int",  sizeof(tuint));
  ctype_ulong =init_basic_ctype(CTK_ULONG,  "unsigned long", sizeof(tulong));

  ctype_double=init_basic_ctype(CTK_DOUBLE, "double", sizeof(double));
  ctype_float =init_basic_ctype(CTK_FLOAT,  "float",  sizeof(float));

  ctype_voidptr=duel_mkctype_ptr(ctype_void);
  ctype_charptr=duel_mkctype_ptr(ctype_char);
  /* find and set the special types.
   * support only a signed ptrdiff; size_t/ptrdiff must both be int or long
   */
  { ptrdiff_t p ; size_t s ;
    p= -1 ; s= -1 ;
    if(p>0)  duel_gen_error("bad ptrdiff_t - unsigned",0);
  
    if(sizeof(p)==sizeof(int)) 	     ctype_ptrdiff_t=ctype_int ;
    else if(sizeof(p)==sizeof(long)) ctype_ptrdiff_t=ctype_long ;
    else duel_gen_error("bad ptrdiff_t size",0);

    if(sizeof(s)==sizeof(int)) 	ctype_size_t= (s<0)? ctype_int:ctype_uint ;
    else 
    if(sizeof(s)==sizeof(long)) ctype_size_t= (s<0)? ctype_long:ctype_ulong ;
    else duel_gen_error("bad size_t size",0);
   }
}


