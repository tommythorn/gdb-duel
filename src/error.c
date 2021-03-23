/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/error.c,v 1.5 93/03/12 05:48:27 mg Exp $*/

/* display errors in a neat way */

/*
 * $Log:        error.c,v $
 * Revision 1.5  93/03/12  05:48:27  mg
 * support output redirection
 *
 * Revision 1.4  93/01/12  21:35:31  mg
 * cleanup and set for release
 *
 */

#include "duel.h"

static tnode *curr_eval_node ;  /* current node being evaluated */
static  char *curr_inputstr ;   /* current input string being eval */

/* indicate the active node where an operator is now evaluated.
 * if an error occurs, this marker is used to tell the user where
 * the error is located.
 * return previous setup to caller, so it can be restored.
 */

FUNC tnode* duel_set_eval_loc(tnode *n)
{
    tnode *prev=curr_eval_node ;
    curr_eval_node=n ;
    return prev ;
}

/* indicate the current input string which is evaluated
 * (intended for future versions with multiple input strings)
 */

FUNC char* duel_set_input_string(char *s)
{
   char *prev=curr_inputstr ;
   curr_inputstr=s ;
   return prev ;
}
/* display source position for errors, based on current node being eval'ed */

LPROC print_src_pos(void)
{
    int src_pos=0 ;
    int i ;
    if(curr_eval_node) src_pos=curr_eval_node->src_pos ;
    duel_printf("Error:   %s\n",curr_inputstr) ;
    duel_printf("         ") ;
    for(i=0 ; i<src_pos ; i++) duel_printf("-");
    duel_printf("^-- ");
}

/* called for errors that are results of bad user input (syntax/sematics),
 * e.g. an illegal variable name, etc
 * the message is printed as a format string for 'op'.
 * the error location in the source is printed based on the current eval node,
 * and the value of the given operands are displayed.
 */

PROC duel_op_error(char *mesg,char *op,tvalue *v1,tvalue *v2)
{
    char s[160] ;

    duel_redirectable_output_abort();
    print_src_pos();
    duel_printf(mesg,op);
    duel_printf("\n");
    if(v1) {
      duel_printf("operand%s ``%s'' ",(v2!=0)? "1":"",v1->symb_val);
      duel_printf("\t-- type: ");
      duel_print_type(v1->ctype,1);
      duel_sprint_scalar_value(s,v1);
      duel_printf("\n\t\t-- value: %s\n",s);
    }
    if(v2) {
      duel_printf("operand%s ``%s'' ",(v1!=0)? "2":"",v2->symb_val);
      duel_printf("\t-- type: ");
      duel_print_type(v2->ctype,1);
      duel_sprint_scalar_value(s,v2);
      duel_printf("\n\t\t-- value: %s\n",s);
    }

    duel_abort();
}

/* handle a genral error, no value (operand) is involved.
 * location (node) is still displayed
 */

PROC duel_gen_error(char *mesg,char *arg1)
{
    duel_redirectable_output_abort();
    print_src_pos();
    duel_printf(mesg,arg1);
    duel_printf("\n");
    duel_abort();
}

/* handle fatal messages */

PROC duel_fatal(char *msg)
{
   duel_redirectable_output_abort();
   duel_printf("Fatal Duel error: %s\n",msg);
   duel_abort();
}
