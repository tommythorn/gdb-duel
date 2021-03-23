/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/print.c,v 1.9 93/03/12 06:01:00 mg Exp $*/

/* handle value/type printing */

/*
 * $Log:        print.c,v $
 * Revision 1.9  93/03/12  06:01:00  mg
 * cosmetics: tuint for uint, etc.
 * display union like structs
 * suport piped output
 * note for all pointers if illegal, better handling of string bad pointers
 *
 * Revision 1.8  93/02/27  06:06:50  mg
 * removed usage of fabs() so -lm is not required in gdb's linking (HP9000)
 *
 * Revision 1.7  93/02/26  05:00:42  mg
 * fixed display of *p for void *p
 *
 * Revision 1.6  93/02/23  20:57:56  mg
 * *** empty log message ***
 *
 * Revision 1.5  93/02/23  19:16:00  mg
 * improved escaped char support
 *
 * Revision 1.4  93/02/03  21:54:49  mg
 * support "signed char"
 *
 * Revision 1.3  93/01/12  21:53:45  mg
 * cleanup and set for release
 *
 * Revision 1.2  93/01/07  00:14:34  mg
 * print union name
 *
 * Revision 1.1  93/01/03  07:31:55  mg
 * Initial revision
 *
 */

#include "duel.h"

/* print type information (no new lines)
 * parm 'expand' controls expansion level for structs & unions. normally 1.
 */

PROC duel_print_type(tctype *t,int expand)
{
   int i ;
   tctype *kid=t->u.kid ;

   switch(t->type_kind) {
      case CTK_VOID:    duel_printf("void ")   ; break ;
      case CTK_CHAR:    duel_printf("char ")   ; break ;
      case CTK_SCHAR:   duel_printf("schar ")  ; break ;
      case CTK_UCHAR:   duel_printf("uchar ")  ; break ;
      case CTK_USHORT:  duel_printf("ushort ") ; break ;
      case CTK_SHORT:   duel_printf("short ")  ; break ;
      case CTK_INT:     duel_printf("int ")    ; break ;
      case CTK_UINT:    duel_printf("uint ")   ; break ;
      case CTK_LONG:    duel_printf("long ")   ; break ;
      case CTK_ULONG:   duel_printf("ulong ")  ; break ;
      case CTK_FLOAT:   duel_printf("float ")  ; break ;
      case CTK_DOUBLE:  duel_printf("double ") ; break ;
      case CTK_ENUM:    duel_printf("enum %s ",t->name); break ;

      case CTK_PTR:
                  if(ctype_kind_base(kid)) {
                      duel_print_type(kid,0);
                      duel_printf("* ");
                  }
                  else {
                      duel_printf("ptr to ");
                      duel_print_type(kid,expand-1) ;
                  }
      break ;
      case CTK_ARRAY: {
                  int n=t->size ;
                  if(kid->size>0) n/=kid->size ;

                  if(ctype_kind_base(kid)) {
                      duel_print_type(kid,0);
                      duel_printf("[%d] ",n);
                  }
                  else {
                      duel_printf("array [%d] of ",n);
                      duel_print_type(kid,expand) ;
                  }
               }
      break ;
      case CTK_FUNC:  duel_printf("func returning ");
                      duel_print_type(kid,expand);
      break ;
      case CTK_STRUCT:
        if(expand <= 0) {
            duel_printf("struct %s ",t->name);
            break ;
        }
        duel_printf("struct %s { ",t->name) ;
         for(i=0 ; i<t->u.f.fields_no ; i++) {
            tctype_field *f= &t->u.f.fields[i] ;
            duel_print_type(f->ctype,expand-1);
            duel_printf("%s ",f->name);
            if(f->bitlen != 0) duel_printf(":%d ",f->bitlen);
            duel_printf("; ");
        }
        duel_printf("} ; ");
      break ;
      case CTK_UNION:   duel_printf("union %s",t->name)  ; break ;
      default: duel_assert(0);
   }
}

/* display char 'c' in a "neat" way, e.g. c='\n' is displayed as such,
 * etc. Special case for c==quote (normally ' or "), we add '\\'
 * return a pointer to a static string which is overriden each call etc
 */
char *neat_char(char c,char quote)
{
   static char s[8] ;
   switch(c) {
     case '\0': strcpy(s,"\\0");  break ;
     case '\n': strcpy(s,"\\n");  break ;
     case '\r': strcpy(s,"\\r");  break ;
     case '\t': strcpy(s,"\\t");  break ;
     case '\\': strcpy(s,"\\\\"); break ;
     default:
      if(!isprint(c) || !isascii(c)) sprintf(s,"\\%3.3o",c & 0377);
      else
      if(c==quote) sprintf(s,"\\%c",c);
      else sprintf(s,"%c",c);
   }
   return s ;
}

/* print the given scalar value into string s.
 * for none-scalar values, print the lval's address.
 * note: v's value is not modified.
 */

PROC duel_sprint_scalar_value(char *s,tvalue *v)
{
   bool ok ;
   tvalue rval ;  /* copy of v, but as an rval */
   rval = *v ;
   if(v->val_kind==VK_FVALUE) {
        sprintf(s,"frame(%d)",v->u.fvalue);
        return ;
   }
   if(v->val_kind==VK_LVALUE) {
        char tmpstr[256],*m="" ; /* tmpstr= temporary for check, m= message */
        int sz = v->ctype->size ; /* size of object ref. */
        if(sz<1) sz=1 ;
        if(sz>256) sz=256 ;
        /* check for null and bad (try read sz bytes at addr ) references */
        if(v->u.lvalue==NULL ||
           !duel_get_target_bytes(v->u.lvalue,tmpstr,sz)) m=" [ILLEGAL]" ;

        switch(v->ctype->type_kind) {
          case CTK_VOID:   sprintf(s,"void @%p%s",v->u.lvalue,m); return;
          case CTK_STRUCT: sprintf(s,"struct @%p%s",v->u.lvalue,m); return;
          case CTK_UNION:  sprintf(s,"union @%p%s",v->u.lvalue,m); return;
          case CTK_FUNC:   sprintf(s,"func @%p%s",v->u.lvalue,m)  ; return;
          case CTK_ARRAY:
             if(*m!='\0' || v->ctype->u.kid!=ctype_char) {
                sprintf(s,"array @%p%s",v->u.lvalue,m);
                return;
             }
        }
        /* convert scalar type to rvalue */
        if(!duel_try_get_rvalue(&rval,"")) {
           sprintf(s,"ref @%p [ILLEGAL]",v->u.lvalue,m);
           return ;
        }
   }
   else
   if(v->val_kind==VK_BVALUE) {
        if(!duel_try_get_rvalue(&rval,"")) {
           sprintf(s,"ref @%p [ILLEGAL]",v->u.bvalue.lvalue);
           return ;
        }
   }

   switch(rval.ctype->type_kind) {                      /* handle rvalues */
      case CTK_VOID:   sprintf(s,"void") ; break ;
      case CTK_CHAR: sprintf(s,"'%s'",neat_char(rval.u.rval_char,'\''));break;
      case CTK_SHORT:   sprintf(s,"%d",     rval.u.rval_short)  ; break ;
      case CTK_INT:     sprintf(s,"%d",     rval.u.rval_int)    ; break ;
      case CTK_LONG:    sprintf(s,"%ldL",   rval.u.rval_long)   ; break ;
      case CTK_SCHAR:   if(rval.u.rval_schar==0) sprintf(s,"'\\0'");
                        else sprintf(s,"%d", rval.u.rval_schar)  ; break ;
      case CTK_UCHAR:   if(rval.u.rval_uchar==0) sprintf(s,"'\\0'");
                        else sprintf(s,"'\\x%d'", rval.u.rval_uchar)  ; break ;
      case CTK_USHORT:  if(rval.u.rval_ushort==0) sprintf(s,"0");
                        else sprintf(s,"0x%x",   rval.u.rval_ushort) ; break ;
      case CTK_UINT:    if(rval.u.rval_uint==0) sprintf(s,"0");
                        else sprintf(s,"0x%x",   rval.u.rval_uint)   ; break ;
      case CTK_ULONG:   if(rval.u.rval_ulong==0L) sprintf(s,"0");
                        else sprintf(s,"0x%lxL", rval.u.rval_ulong)  ; break ;
      case CTK_FLOAT:   rval.u.rval_double=rval.u.rval_float ;
      case CTK_DOUBLE:  { double x=rval.u.rval_double ;
                          if(x >= -1e-6 && x <= 1e-6 || x >= 1e8 || x <= -1e8)
                             sprintf(s,"%.4le",x);      /* standard 'e' fmt */
                          else {        /* fixed point removing trailing '0'*/
                             int l;
                             sprintf(s,"%.8lf",x);
                             l=strlen(s)-1;
                             while(s[l]=='0' && s[l-1]!='.') s[l--]=0 ;
                         }
                        }
      break ;
      case CTK_PTR:
          if(rval.u.rval_ptr==NULL) sprintf(s,"NULL");
          else {
              char sval[41];
              bool ok=duel_get_target_bytes(rval.u.lvalue,sval,1);
              if(ok && rval.ctype->u.kid==ctype_char) {
                 int i ;
                 duel_get_target_bytes(rval.u.lvalue,sval,41);
                 strcpy(s,"\"");
                 for(i=0 ; i<40 && sval[i]!='\0' ; i++)
                    strcat(s,neat_char(sval[i],'"'));
                 if(sval[i]=='\0') strcat(s,"\"");
                 else strcat(s,"...\"");
             }
             else sprintf(s,"@%p%s",rval.u.rval_ptr,ok? "":" [ILLEGAL]");
          }
      break ;
      case CTK_ENUM:    {
          int i, n=rval.ctype->u.e.enumerators_no ;
          tctype_enumerator *e=rval.ctype->u.e.enumerators ;
          int val=duel_get_int_val(v,"");
          for(i=0 ; i<n ; i++)
              if(e[i].val == val) { strcpy(s,e[i].name); return ;}
          sprintf(s,"%d",val);
      }
      break ;
      default: duel_assert(0);
   }
}

/* print the given value, symbolic+val. Handles structures */

PROC duel_print_value(tvalue *v)
{
   tctype *t=v->ctype ;
   int i ;
   bool topipe=duel_output_pipe_style ;
   char s[160];

   duel_printf(topipe? "$$$SYM: %s\n":"%s",v->symb_val);

   if(duel_debug && !topipe) {
       duel_printf("`` %s '' ",v->symb_val);
       duel_printf("{ ");
       duel_print_type(v->ctype,2);
       duel_printf("} ");
       if(v->val_kind==VK_LVALUE) {
          duel_printf("lval @%p",v->u.lvalue);
       }
       else
       if(v->val_kind==VK_BVALUE) {
          duel_printf("bval @%p [%d,%d]",v->u.bvalue.lvalue,v->u.bvalue.bitpos,
                                    v->u.bvalue.bitlen);
       }
       duel_printf("\n");
   }

   duel_sprint_scalar_value(s,v);
   if(v->val_kind==VK_LVALUE && strstr(s,"ILLEGAL")==NULL) {
      switch(t->type_kind) {
        case CTK_UNION:
        case CTK_STRUCT:
             duel_printf(topipe? "$$$VAL: { " : " = { ");
             for(i=0 ; i<t->u.f.fields_no ; i++) {
               tvalue u ;
               char *name=t->u.f.fields[i].name ;
               duel_get_dot_name(v,name,&u);
               duel_sprint_scalar_value(s,&u);
               duel_printf("%s = %s",name,s) ;
               if(i < t->u.f.fields_no-1) duel_printf(", ");
             }
             duel_printf(" }\n");
        return ;
        case CTK_ARRAY: ;  /* not handled except for char[] as scalar*/
      }
   }

   if(topipe) duel_printf("$$$VAL: %s",s);
   else
   if(strcmp(v->symb_val,s)!=0) duel_printf(" = %s",s);
   duel_printf("\n");
}
