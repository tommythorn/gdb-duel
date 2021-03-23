/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/global.h,v 1.12 93/03/12 05:50:35 mg Exp $*/

/* this include file contains global definitions for duel. All global info
 * is shared thru this single file.
 */

/*
 * $Log:        global.h,v $
 * Revision 1.12  93/03/12  05:50:35  mg
 * uses tuint instead of uint, etc.
 *
 * Revision 1.11  93/02/27  06:03:26  mg
 * added HP9000 support by not defining uint etc
 * clean annoying enum {x,y,} warnings
 *
 * Revision 1.10  93/02/03  21:49:32  mg
 * support "signed char"
 *
 * Revision 1.9  93/01/12  21:52:11  mg
 * cleanup and set for release
 *
 * Revision 1.8  93/01/07  00:10:53  mg
 * macros for func ptr auto-convert
 *
 * Revision 1.7  93/01/03  07:30:23  mg
 * *** empty log message ***
 *
 * Revision 1.6  92/12/24  23:49:25  mg
 * *** empty log message ***
 *
 * Revision 1.5  92/10/19  15:07:58  mg
 * fvalue added (not ready yet), svalues dropped
 *
 * Revision 1.4  92/10/14  02:05:37  mg
 * misc
 *
 * Revision 1.3  92/09/16  11:11:54  mg
 * add builtin ctype_charptr
 *
 * Revision 1.2  92/09/15  05:47:49  mg
 * some ops added
 *
 */


/* The following definitions are common programming tricks that I use
 * in every program I write. I hope they are obvious. the FUNC/PROC
 * constants are useful mainly when editing (to find a function def.)
 * or when grep'ing on source files. DEF is set to "extern" by default,
 * allowing declaration and definition of globals in one source. only
 * duel.c should have #define DEF before including duel.h
 */

#define FUNC
#define PROC void
#define LPROC static void
#define LFUNC static

#ifndef DEF
#define DEF extern            /* declare only, 'int x' become 'extern int x'*/
#endif

/* use simpler names for unsigned types, very common,
 * too common, indeed, so I forced 't' as a prefix.
 */

typedef unsigned int   tuint ;
typedef unsigned char  tuchar ;
typedef unsigned short tushort ;
typedef unsigned long  tulong ;

typedef signed char tschar ;   /* explicit signed char */
typedef int bool ;             /* suppose to be just 0,1 */
#define FALSE 0
#define TRUE  1

 /* these should be inline functions, they replace common lib func which
  * change from system to system.
  * duel_assert can be modified to call duel_fatal, so gdb itself isn't
  * aborted because on an internal duel error.
  */

#define duel_assert(x)    assert(x)
#define duel_bzero(x,y)   memset(x,0,y)
#define duel_bcopy(x,y,s) memcpy(x,y,s)



/*************
 Parsing: the expression is converted into a tree.
 Each node of this tree is of 'tnode' type.
 Nodes can be:
 An operator (like '+', or ':=' or 'for')
 A  constant (numbers. Strings are pointers to space!)
 A  symbol.
 A  regular expression (really a symbol extention)

 The evaluation of any node results in:
 1. 'Environment': duel variables and their values
 2. symbolic expression which describe the computation
    meaning: keep the parentethis. in the expression a<<(3+b) (even if not needed!)
 3. A type & value

 Values:
 Values are always a fixed length area of bytes; Any C type is like that.
**************/


/****** TYPES *******
  A type is described in the typical compiler's way, which is also gdb's
  way. That is, a type is made of 'atomic' types, like 'pointer to',
  'array of', etc. Each atomic type is described by a 'ctype' node. Ctype
  node points to other ctype nodes as required. For example, a ctype node
  of 'array of' will point to the node 'int' for the 'C' type array of int.
  [The name ctype is used to indicate the C language type; 'type' is too
   generic a term]
  Example:
   char *(*f[])() ;
  Here, f is an array of pointers to functions returning a pointer to char.
  The type is described with the top node being of CTK_ARRAY type. It
  points to a 'CTK_PTR' node, which points to a 'CTK_FUNC' node which
  points to a 'CTK_PTR' node which points to a 'CTK_CHAR' node which has,
  as size, 1.

  The size of a type is the number of bytes occupied by it. it is zero
  for types like 'function'. An array of 10 integers will have as size
  10*sizeof(int)

  A note on UCHAR vs CHAR vs SCHAR:
  Internally, duel supports all three forums (for aritmetic).
  For output, only "char" types are considered ASCII, and will be displayed
  as 'A' instead of "65", etc. The same is true for char[] and char*.
  GDB doesn't really support "signed char", but some compilers do;
  we convert "signed char" into just a "char" when the default char is signed.
  This is only significant for output.
  Maybe duel shouldn't support "char" at all, and force everything to be
  "schar" or "uchar". I'm not sure. I prefer "char" asa distnct  type
  which implies "string" and uchar/schar as "integral".
 ********************/

typedef enum {       /* 'C' Type atomic primitive Kinds (CTK_) */
   CTK_VOID=0,       /* void type   'void'     */
   CTK_CHAR=1,       /* primitive types        */
   CTK_SCHAR=2,      /* signed char (ansi c) */
   CTK_UCHAR=3,      /* note order is used in some macros */
   CTK_SHORT=4,
   CTK_USHORT=5,
   CTK_INT=6,
   CTK_UINT=7,
   CTK_LONG=8,
   CTK_ULONG=9,
   CTK_FLOAT=10,
   CTK_DOUBLE=11,
   CTK_PTR=12,          /* pointer '*'            */
   CTK_ARRAY=13,        /* array   '[]'           */
   CTK_FUNC=14,         /* a function '()'        */
   CTK_STRUCT=15,       /* a sturcture 'struct{}' */
   CTK_UNION=16,        /* a union     'union{}'  */
   CTK_ENUM=17          /* enum type   'enum{}'   */
 } tctype_kind ;

typedef struct {                /* a field (of struct) info */
  char *name ;                  /* field name */
  struct sctype *ctype ;        /* field type */
  int bitpos, bitlen ;          /* for bit fields only */
} tctype_field ;

typedef struct {                /* an enumerator (constant of an enum)  info */
  char *name ;                  /* enumerator name */
  int val ;                     /* value assigned to the name */
                                /* question: ansi-C say cosnt fit in int?*/
} tctype_enumerator ;


typedef struct sctype {       /* a 'C' type description  */
   tctype_kind type_kind ;
   char *name ;               /* named of this type, if any       */
   size_t size ;              /* size (total bytes) for this type */
   union {
    struct sctype *kid ;       /* pointer to next atomic type               */
    struct {
      int fields_no ;
      tctype_field *fields ;     /* union/struct fields */
    } f ;
    struct {
       tctype_kind real_type_kind ; /* the type used for storage of the enum */
       int enumerators_no ;
       tctype_enumerator *enumerators ; /* enum's type enumerators list */
    } e;
   } u ;
 } tctype ;


/**** BASIC C TYPES
  these are initialized at run type to point to the basic C types.
  basic types are never created again, e.g. pointer comparison with
  these is sufficient to check type equality.
  the voidptr is (void*) which is a basic type (what zero is converted
  to when it is a pointer).
  ptrdiff_t and size_t are actually pointing to their actualy types,
  ie. normally point to ctype_int.
 ****/

DEF tctype *ctype_int,  *ctype_uint, *ctype_short, *ctype_ushort,
           *ctype_char, *ctype_schar, *ctype_uchar,
           *ctype_long, *ctype_ulong,
           *ctype_float, *ctype_double,
           *ctype_void, *ctype_charptr,
           *ctype_voidptr,*ctype_ptrdiff_t,*ctype_size_t ;

/* type checking macros */

/** tell if type is a struct or union */
#define ctype_kind_struct_like(t) ((t)->type_kind==CTK_STRUCT || \
                                   (t)->type_kind==CTK_UNION)

/** tell if type is a pointer or can be made one (array, func) */
#define ctype_kind_ptr_like(t)   ((t)->type_kind==CTK_PTR || \
                         (t)->type_kind==CTK_ARRAY || (t)->type_kind==CTK_FUNC)
/* tell if type is numeric */
#define ctype_kind_numeric(t) ((t)->type_kind>=CTK_CHAR && \
                     (t)->type_kind<=CTK_DOUBLE || (t)->type_kind==CTK_ENUM)

/* tell if type is integral */
#define ctype_kind_integral(t) ((t)->type_kind>=CTK_CHAR && \
                     (t)->type_kind<=CTK_ULONG || (t)->type_kind==CTK_ENUM)

/* tell if type fits into fixed memory size (void not included) */
#define ctype_kind_scalar(t) (ctype_kind_numeric(t) || ctype_kind_ptr_like(t))

/* tell if type is a "base" ie can't have kids */

#define ctype_kind_base(t) ((t)->type_kind>=CTK_VOID && \
                            (t)->type_kind<=CTK_DOUBLE)

/* tell if a type is a function pointer or like a func ptr (ie a func!) */
#define ctype_kind_func_ptr_like(t) ((t)->type_kind==CTK_FUNC || \
                    (t)->type_kind==CTK_PTR && t->u.kid->type_kind==CTK_FUNC)


/***** VALUES
   used to keep information when evaluating an expression. A value is
   either:
   1) a right-value, which is represented in a fixed number of bytes that
      actually contain its value. Example: result of '+' operation.
   2) a left-value. This is a reference to the actual value, which
      resides in the debuggee's address space.
   3) a special-value. This contains a long value specifying a special
      target location (e.g. register) from which values can be fetched
      or written to. However, special-values can not be manipulated (i.e
      have an offset added to them, as done to an lvalue struct).

   Note that the operation '*x' starts with x as an lvalue - a pointer to
   x is kept. Then, '*x' is executed, fetching the value of x and creating
   another lvalue, which points to where x has pointed to! This allows
   &*x to be executed, as well as *x=1 or &x[1] ...
   When the value of an lvalue is actually needed, it must be fetched
   from the debugge's space.
 *****/

#define VALUE_MAX_CONST_SIZE        8
#define VALUE_MAX_SYMBOLIC_SIZE     128

typedef int tptrsize_int ;      /* type so that
                                   tptrsize x; char *p ;
                                   p == (char*) (tptrsize) p
                                 */

  /* type that represent a target address space location.  */

typedef char* ttarget_ptr ; /* pointer to target's address */

typedef struct {
    ttarget_ptr lvalue ;        /* pointer to target's location of struct*/
    int bitpos,bitlen ;         /* as specified in the field info of stuct*/
 } tbvalue_info ;

typedef enum {
  VK_RVALUE,      /* this value represent a constant, in the rval_ union */
  VK_LVALUE,      /* this value represent a left-value. address in lvalue*/
  VK_BVALUE,      /* a bit-field lvalue, given as point and bitpos/bitlen*/
  VK_FVALUE       /* a frame, fvalue contains it number */
 } tval_kind ;

typedef struct {
   tval_kind val_kind ;
   tctype *ctype ;       /* type of this value */
   union {
      ttarget_ptr lvalue ; /* location in target of the value */
      tbvalue_info bvalue ; /* location in target of a bitfield */
      int          fvalue ; /* a frame number (0 = top of stack) */
      char    rval_char ;
      tschar  rval_schar ;
      tuchar  rval_uchar ;
      short   rval_short ;
      tushort rval_ushort ;
      int     rval_int ;
      tuint   rval_uint ;
      long    rval_long ;
      tulong  rval_ulong ;
      float   rval_float ;
      double  rval_double ;
      ttarget_ptr rval_ptr ;
      ptrdiff_t rval_ptrdiff_t ; /* Synonyms to one of the above fields! */
      size_t    rval_size_t ;    /* normally these are simply = rval_int */
   } u ;
   char symb_val[VALUE_MAX_SYMBOLIC_SIZE];
 }  tvalue ;


typedef struct sval_lcell {    /* a cell on a linked-list containing a tvalue*/
    tvalue val ;
    struct sval_lcell *next ;
 } tval_lcell ;

typedef struct {               /* a linked list of tvalue's */
    tval_lcell *head ;
    tval_lcell *tail ;
} tval_list ;

/**** OP CODES ****
 Opcodes are divided into groups, or 'kinds'. For example, all the
 C regular unary operators, binary operators, etc.
 An opcode is defined by its 'kind' and actual value. The value is,
 in most cases, a single char. For example, the C plus operator is
 defined as op_kind=OPK_BIN and op='+'. The C operator '+=' is
 defined as op_kind=OPK_ASSIGN and op='+'.
 some operators are uniquely identified by type alone (e.g. func call).
 In the lexer there is a table of actual operators as tokens, and
 the kind & value assigned to each.
 Several opertors use more than one char, and are defined below.
 the OPK_SBIN, etc kinds are special in the sense that A op B isn't
 computed in the normal way of the C language (i.e. compute both sides,
 then apply the operator). Such operators include ',' '&&', etc.
 **********/
typedef enum {
    /* '+' '-' etc are used directly and have their ascii value */
    OP_LSH = 300,     /* '<<' */
    OP_RSH = 301,     /* '>>' */
    OP_INC = 302,     /* '++' */
    OP_DEC = 303,     /* '--' */
    OP_ARR = 304,     /* '->' */
    OP_AND = 305,     /* '&&' */
    OP_OR  = 306,     /* '||' */
    OP_EQ  = 307,     /* '==' */
    OP_GE  = 308,     /* '>=' */
    OP_LE  = 309,     /* '<=' */
    OP_NE  = 310,     /* '!=' */
    OP_SIZ = 311,     /* 'sizeof' */
    OP_TO  = 312,     /* '..' */
    OP_EQQ  = 313,     /* '==?' */
    OP_NEQ  = 314,     /* '!=?' */
    OP_GEQ  = 315,     /* '>=?' */
    OP_LEQ  = 316,     /* '<=?' */
    OP_GTQ  = 317,     /* '>?'  */
    OP_LSQ  = 318,     /* '<?'  */
    OP_IMP  = 319,     /* '=>'  */
    OP_IF   = 320,     /* if()  */
    OP_DEF  = 321,     /* ':='  */
    OP_DFS  = 322,     /* '-->' */
    OP_BFS  = 323,     /* '->>' */
    OP_POS  = 324,     /* '>--' */
    OP_IOS  = 325,     /* '->-' */
    OP_SEL  = 326,     /*'x[[y]]'*/
    OP_FOR  = 327,     /*for(;;)'*/
    OP_WHILE= 328,     /*while() */
    OP_DECL = 329,     /* var decl*/
    OP_FRAME= 330      /* frame(i)*/
   } topcode ;

/**** OPCODE KINDS ****/

typedef enum {          /* the kind of the opcode, 'OPK_' = OPcode Kind */
   OPK_UNARY      =0,   /* regular unary operator                       */
   OPK_SUNARY     =1,   /* special unary operator (sizeof)              */
   OPK_POST_UNARY =2,   /* post unary operator (x++ etc)                */
   OPK_BIN        =3,   /* regular binary operator                      */
   OPK_SBIN       =4,   /* special binary operator                      */
   OPK_TRI        =5,   /* trinary (eg ?:) operator                     */
   OPK_QUAD       =6,   /* quad (e.g.for(;;)) operator                  */
   OPK_ASSIGN     =7,   /* assignment ie =, +=, -= etc                  */
   OPK_FUNC       =8,   /* function call. 2nd kid made of ',' kids for parms */
   OPK_CAST       =9,   /* type cast.  2nd kid convert to type in 1st kid */
   OPK_NUM_OF     =10   /* number of OPK_* op kinds                       */
 } top_kind ;


/**** NODE KINDS ****/

typedef enum {           /* the kind of node        */
   NK_OP,               /* contain an operator     */
   NK_CONST,            /* contains a constant     */
   NK_NAME,             /* contains a name/symbol  */
   NK_CTYPE             /* contain a 'C' type      */
 } tnode_kind ;

#define NODE_MAX_KIDS      4     /* maximum no of kids per node         */
#define NODE_MAX_SYM_LEN  50     /* maximum length for a symbol         */

/* the following structure is used to store one node of the expression tree.
 * It contains two parts: (a) information to describe the node and (b)
 * information used during the evaluation of the node.
 *
 * This structure could/should be optimized with ugly unions (e.g., for
 * a node describing a constant(node_kind==NK_CONST) there is no need for
 * the op, kids or any eval stuff!). However, only few nodes (less than 100?)
 * are expected to be in memory at any time (how complex can a user
 * expression be?). Hence, for clarity, no unions where used.
 *
 * All nodes have kids in the typical way. the opcode OPK_FUNC node is tricky:
 * see parsing - the commas are handled as operators and not as part of the
 * syntax. when a func call is evaluated, the top comma nodes are used to
 * separate the arguments (this hack prevents complex parsing and complex tnode
 * with variable num of kids, which would force the kids[] to be malloc etc.)
 */

typedef struct snode {          /* a single node on an expression tree     */
   tnode_kind node_kind ;       /* what kind of note this is               */
                                /* NK_OP node info: op_kind,op,kids        */
   int src_pos ;           /* starting source code location (op etc)  */
   top_kind op_kind ;           /* opcode type (unary,bin etc)   */
   topcode op ;                 /* actual opcode: '-' for both unary&bin '-'*/
   struct snode *kids[NODE_MAX_KIDS] ;   /* pointers to node's kids     */
   tvalue cnst ;                /* NK_CONST info - constant value       */
   char *name ;                 /* NK_NAME  info - variable name        */
   tctype *ctype ;              /* NK_CTYPE info - a type               */

   struct {                     /* information used during node evaluation   */
       int level ;              /* evaluation level (0=none, 1=left in x etc)*/
       tvalue  v1,v2 ;          /* keep last computed operands, etc          */
       tval_list vlist ;        /* value list for node (used in --> etc ops) */
       int counter ;            /* used for counting            */
   } eval ;
 } tnode ;


DEF bool duel_debug ;                   /* debugging duel-itself mode   */
DEF bool duel_output_pipe_style ;       /* pipe-style output of values  */

/* this things are machine dependent stuff */

#define BITS_PER_BYTE 8  /* no of bits in (char) (right. try '9' :-) */
