/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/proto.h,v 1.10 93/03/12 06:01:28 mg Exp $*/

/* prototypes for all of duel's global functions */

FUNC tctype* duel_mkctype_ptr(tctype *t);
FUNC tctype* duel_mkctype_func(tctype *t);
FUNC tctype* duel_mkctype_array(tctype *t,int size);
FUNC tctype* duel_mkctype_struct(char *name,size_t size,int fields_no,
                                 bool is_union);
PROC duel_mkctype_struct_field(tctype *t,int field_no,char *name,
                               int bitpos,int bitlen, tctype *fctype);
FUNC tctype* duel_mkctype_enum(char *name,tctype_kind real_type_kind,
                               size_t size,int enumerators_no);
PROC duel_mkctype_enumerator(tctype *t,int enumerator_no,char *name,int val);

PROC duel_init_basic_ctypes(void);

PROC duel_print_value(tvalue *v);
PROC duel_print_type(tctype *t,int expand);
PROC duel_sprint_scalar_value(char *s,tvalue *v);


PROC duel_fatal(char *msg);
PROC duel_abort(void);
PROC duel_cleanup(void);

FUNC tnode* duel_parse(char *s);


PROC duel_reset_eval(void);
FUNC bool duel_eval(tnode *n,tvalue *v);
FUNC bool duel_get_dot_name(tvalue *v,char *name,tvalue *ret);

FUNC tnode* duel_set_eval_loc(tnode *n);
FUNC char* duel_set_input_string(char *s);
PROC duel_op_error(char *mesg,char *op,tvalue *v1,tvalue *v2);
PROC duel_gen_error(char *mesg,char *arg1);


FUNC bool duel_try_get_rvalue(tvalue *v,char *op);
PROC duel_standardize_func_parm(tvalue *p);
FUNC bool duel_do_op_to(tvalue *v1,tvalue *v2,int n,tvalue *r);
PROC duel_do_cast(tctype *tout,tvalue *v);
FUNC bool duel_mk_logical(tvalue *v,char *op);
PROC duel_set_symb_val(tvalue *r,char *format,tvalue *v1,tvalue *v2);
PROC duel_get_struct_val(tvalue *v,char *op);
PROC duel_get_struct_ptr_val(tvalue *v,char *op);
FUNC int duel_get_int_val(tvalue *v,char *op);
FUNC int duel_get_posint_val(tvalue *v,char *op);
PROC duel_apply_unary_op(topcode op,tvalue *v);
PROC duel_apply_post_unary_op(topcode op,tvalue *v);
FUNC bool duel_apply_bin_op(topcode op,tvalue *v1,tvalue *v2,tvalue *r);
PROC duel_find_func_frame(tvalue *v,char *op);

/* output management */

PROC duel_printf(char *fmt, ...);
PROC duel_flush(void);
PROC duel_redirectable_output_start(char *);
PROC duel_redirectable_output_end(void);
PROC duel_redirectable_output_abort(void);
PROC duel_redirectable_output_init(void);

/* debugger dependent functions */

FUNC void* duel_malloc(size_t size);
PROC duel_free(void *);

FUNC bool duel_get_target_bytes(ttarget_ptr from,void *to,size_t n);
FUNC bool duel_put_target_bytes(ttarget_ptr to,void *from,size_t n);

FUNC bool duel_get_target_bitfield(ttarget_ptr struct_at,int bitpos,
                                    int bitlen,void *to,tctype_kind tkind);
FUNC bool duel_get_target_variable(char *name, int frame_no, tvalue *v);
FUNC tctype* duel_get_target_typedef(char *name);
FUNC tctype* duel_get_target_struct_tag(char *name);
FUNC tctype* duel_get_target_union_tag(char *name);
FUNC tctype* duel_get_target_enum_tag(char *name);
FUNC ttarget_ptr duel_alloc_target_space(size_t n);

FUNC int duel_get_frames_number(void);
FUNC ttarget_ptr duel_get_function_for_frame(int frame_no);
PROC duel_target_func_call(tvalue *func, tvalue *parms[],
                            int parms_no,tvalue *ret);

/* prototypes for misc functions */

FUNC char* strncpyz(char *to,char *from,size_t len);

PROC duel_free_val_list(tval_list *l);
PROC duel_free_nodes(tnode *);

FUNC tvalue* duel_find_alias(char *name);
PROC duel_set_alias(char *name,tvalue *v);
PROC duel_clear_aliases(void);
PROC duel_show_aliases(void);
