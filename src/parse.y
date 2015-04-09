%{
/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code			           */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/parse.y,v 1.14 93/03/17 11:04:12 mg Exp $*/

/* this module contains the duel parser, in yacc, plus a simple lexer.
 * the lexer is slow, but duel expressions are tiny.
 * the parsing generate an AST with essentially no type checking.
 * names are only looked up when the refer explicitly to types. This forces
 * the use of "T" before user types. You can't parse (x)(y) correctly, if
 * you want the node to contain "cast" or "func", without knowing the x is 
 * not a type. (It is interesting to note that (x *)(y) is clearly a cast,
 * but it can not be parsed without a context sensitive grammer!). 
 *
 * Version 1.1 now accept (x) as a type cast, so (print)("hi") fails, but
 * (uint)z is ok. Also accepted is (uint*)z. T is still required in sizeof
 * and in variable declarations. A side effect was making "sizeof x" illegal
 * (since then sizeof(x)-1 was parsed sizeof((x)-1) with (x) a cast), so
 * now sizeof(x) must be used. Note that in C, sizoef(x)++ is acceptable,
 * and the '++' operate on x (which is optimized out!) This can be confusing.
 *
 * yacc is also not smart enough to recognize e.g. "if(e) e ; else e" as
 * a special case (redundent ';'). I hacked this in the lexer. It should
 * reduce the trouble with C->duel coding. (It can also be done for {e1}e2,
 * in some speical cases, e.g. if e2 is a keyword, or a name or a unary op,
 * but this can confuse some people, e.g. in {i}[5], so I left it alone.)
 * Finally, the %/ operator is accepted as "#/" and "%%" as "#", to those
 * who wish to keep gdb with # comments.
 * memory: nodes are alloc'ed dynamically. a parsing error loose so-far
 * allocated nodes, which is normally acceptable (yyerror can probably hack
 * into the yacc stack to release them.)
 */

/*
 * $Log:	parse.y,v $
 * Revision 1.14  93/03/17  11:04:12  mg
 * fixed (t*)x bug, was parsed as (t**)x 
 * 
 * Revision 1.13  93/03/12  06:15:09  mg
 * modified unary's a bit - cosmetics
 * support (x)y as type cast
 * support (x*)y as type cast
 * replace sizeof exp  with sizeof(exp) to prevent clash with above
 * more cosmetics, including yyerror abort, tuint for uint.
 * takes anything after |> to be comment (pipe command really)
 * 
 * 
 * Revision 1.12  93/02/27  06:06:09  mg
 * added signed char parsing.
 * 
 * Revision 1.11  93/02/23  19:15:38  mg
 * improved escaped char support
 * 
 * Revision 1.10  93/02/03  21:49:34  mg
 * bug fix - yyerror calls now abort parsing (eg called from lex)
 * 
 * Revision 1.9  93/01/12  21:53:07  mg
 * cleanup and set for release
 * 
 * Revision 1.8  93/01/07  00:14:33  mg
 * add &&/ ||/
 * fixed parsing of trailing ';' was a mess.
 * ignore ';' before 'else' and '}' w/warning.
 * 
 * Revision 1.7  93/01/03  07:31:01  mg
 * error reporting
 * 
 * Revision 1.6  92/12/24  23:35:50  mg
 * began src pos support
 * 
 * Revision 1.5  92/10/19  15:08:02  mg
 * frames() added; bug fixed
 * 
 * Revision 1.4  92/10/14  02:06:32  mg
 * misc/change casting parsing/variable def.
 * 
 * Revision 1.3  92/09/16  11:09:39  mg
 * add typedef/struct support, const strings 
 * cleanup s/r conflict by setting ELSE to a token. explained some stuff in
 * comments.
 * 
 * Revision 1.2  92/09/15  06:10:46  mg
 * cosmetics and new ops: x@y, for() while() ..x and x..
 * generic '.' and '_'  support. x@y. '..x' and 'x..'.  while(), for(), ?:
 * 
 */

#include "duel.h"

static char *inputstr ;		/* pointer to string being parsed */
static char *lexptr ;           /* current lexer pointer into input str */
static tnode *root ;		/* result of parsing stored here */

/* pick unique names for globals of yacc. gdb has other parsers! */
#define	yyparse	duel_yyparse
#define	yylex	duel_yylex
#define	yyerror	duel_yyerror
#define	yylval	duel_yylval
#define	yychar	duel_yychar
#define	yydebug	duel_yydebug
#define	yypact	duel_yypact	
#define	yyr1	duel_yyr1			
#define	yyr2	duel_yyr2			
#define	yydef	duel_yydef		
#define	yychk	duel_yychk		
#define	yypgo	duel_yypgo		
#define	yyact	duel_yyact		
#define	yyexca	duel_yyexca
#define yyerrflag duel_yyerrflag
#define yynerrs	duel_yynerrs
#define	yyps	duel_yyps
#define	yypv	duel_yypv
#define	yys	duel_yys
#define	yystate	duel_yystate
#define	yytmp	duel_yytmp
#define	yyv	duel_yyv
#define	yyval	duel_yyval
#define	yylloc	duel_yylloc

typedef struct {                /* token info for operators */
        int src_pos ;            /* source position */
        topcode opcode ;        /* opcode          */
       } topinfo ;

typedef struct {                /* token info for symbols */
        int src_pos ;            /* source position */
        char *name ;             /* symbol          */
       } tnameinfo ;

/* these are used as operators to mknode_... when source location is unknown*/
static topinfo seq_op  = { -1,';' } ; /* sequencing operator, src pos unkown */
static topinfo decl_op = { -1,OP_DECL } ; /* declare var op, src pos unkown */

/* local prototypes. */
LPROC  yyerror(char *msg);
LFUNC  int yylex (void);

LPROC push_type(char desc) ;
LPROC push_type_int(char desc,tnode *n)  ;
LFUNC bool pop_type(char *desc,int *size);

LFUNC tnode* mknode_op(top_kind,topinfo opinfo,tnode*,tnode*,tnode*,tnode*);
LFUNC tnode* mknode_const(int src_pos,tctype *ctype);
LFUNC tnode* mknode_ctype(tctype *ctype);
LFUNC tnode* mknode_name(tnameinfo nameinfo);
LFUNC tnode* mknode_modified_ctype(tctype *base);

#define mknode_post_unary(op,n) (mknode_op(OPK_POST_UNARY,op,n, 0, 0,0))
#define mknode_unary(op,n)      (mknode_op(OPK_UNARY,     op,n, 0, 0,0))
#define mknode_sunary(op,n)     (mknode_op(OPK_SUNARY,    op,n, 0, 0,0))
#define mknode_bin(op,n1,n2)    (mknode_op(OPK_BIN,       op,n1,n2,0,0))
#define mknode_sbin(op,n1,n2)   (mknode_op(OPK_SBIN,      op,n1,n2,0,0))
#define mknode_tri(op,n1,n2,n3) (mknode_op(OPK_TRI,       op,n1,n2,n3,0))

static tctype *decl_tbase ; /* used for variables decl */

/* #define	YYDEBUG	1 */

%}

%union
  {
    tnode   *node ;                 /* node pointer for constructed exp tree */
    tctype  *ctype;                 /* type for type nodes                   */
    tnameinfo nameinfo ;            /* a name/symbol + src position */
    topinfo opinfo;                 /* keyword/operator + source position    */
  }

%type  <node>   start duel_inp duel_exp exp type nameexp sm_exp oexp  
%type  <node>   all_decls vars_decl var_decl name_decl1 name_decl
%type  <ctype>  typebase 
%type  <nameinfo>   name

%token <node>   T_CONST
%token <nameinfo>   T_SYM
%token <opinfo> T_ASSIGN T_DEFVAR


%token <opinfo> T_CHAR T_INT T_SHORT T_LONG  T_UNSIGNED T_FLOAT T_DOUBLE T_VOID
%token <opinfo> T_STRUCT T_UNION T_ENUM T_SIZEOF T_TYPEDEF_INDICATOR T_SIGNED

%token <opinfo> T_IF T_ELSE T_FOR T_WHILE 
%token <opinfo> ';' ',' '=' '?' '|' '^' '&' '<' '>' '+' '-' '*' '/' '%'
%token <opinfo> '.' '[' ']' '(' ')' '{' '}' '#' '@' '!' '~' 
%token <opinfo> T_OR T_AND T_RSH T_LSH T_INC T_DEC T_COUNT T_FRAME T_TO
%token <opinfo> T_DFS T_BFS T_ARROW T_OSEL T_CSEL T_IMP T_ANDL T_ORL
%token <opinfo> T_EQ T_NE T_EQQ T_NEQ T_LE T_GE T_LSQ T_GTQ T_LEQ T_GEQ

%left  ';'
%right STMT T_ELSE
%right T_IMP
%right ','
%right '=' T_ASSIGN T_DEFVAR
%right '?'
%left  T_OR  T_ORL
%left  T_AND T_ANDL
%left  '|'
%left  '^'
%left  '&'
%left  T_EQ T_NE T_EQQ T_NEQ
%left  '<' '>' T_LE T_GE T_LSQ T_GTQ T_LEQ T_GEQ
%nonassoc T_TO
%left  T_LSH T_RSH
%left  '+' '-'
%left  '*' '/' '%'
%right UNARY '!' '~' T_INC T_DEC T_COUNT T_FRAME
%left T_DFS T_BFS T_POS T_ARROW '.' '[' ']' '(' ')' '{' '}' '#' '@' T_OSEL T_CSEL 
%%

start : duel_inp     { root=$1 ; }
      ;

duel_inp : all_decls 
         | all_decls ';'
         | all_decls ';' duel_exp	{ $$=mknode_sbin($2,$1,$3);}
         | duel_exp
         ;
duel_exp : sm_exp
         | sm_exp ';' { $$=mknode_sbin($2,$1,0); }
         ;
all_decls:  vars_decl			      
          | all_decls ';' vars_decl  	{ $$=mknode_sbin($2,$1,$3); }
          ;

vars_decl: typebase { decl_tbase=$1 ; } var_decl { $$=$3 ; }
         ;
var_decl : name_decl1
         | var_decl ',' name_decl1  { $$=mknode_sbin(seq_op,$1,$3); }
         ;

name_decl1: name_decl	    { $$=mknode_sbin(decl_op,$1,
 				   mknode_modified_ctype(decl_tbase)); }
          ;

name_decl : '(' name_decl ')'		   { $$=$2 ; }
	  | '(' name_decl ')' '(' ')'	   { $$=$2 ; push_type('('); }
	  | '*' name_decl	           { $$=$2 ; push_type('*'); }
	  | name_decl '[' T_CONST ']' 	   { $$=$1 ; push_type_int('[',$3); }
	  | nameexp
	  ;

/* Statements   - not really, these are expressions too!
   Notes: for(;;) oexp - will create lots of shift/reduce conflicts, 
                        'for(;;;)' and 'for(;;) exp' are specified
			instead and yacc handle this as a "standard" s/r.
			the only diff is yacc dont complain on these!
	   if() - same comments as above, plus, we prevent meaningless
	          if's like in C: ' if(x); else;' - a useless statement.
 */
exp   :   T_IF '(' exp ')' exp %prec STMT { $$=mknode_tri($1,$3,$5,0); }
      |   T_IF '(' exp ')' exp T_ELSE %prec STMT 
                                          { $$=mknode_tri($1,$3,$5,0); } 
      |   T_IF '(' exp ')' T_ELSE exp %prec STMT 
                                          { $$=mknode_tri($1,$3,0,$6); } 
      |   T_IF '(' exp ')' exp T_ELSE exp %prec STMT 
                                          { $$=mknode_tri($1,$3,$5,$7); } 

      |   T_FOR '(' oexp ';' exp ';' oexp ')' exp %prec STMT
                              {  $$=mknode_op(OPK_QUAD,$1,$3,$5,$7,$9); } 
      |   T_FOR '(' oexp ';' exp ';' oexp ')' %prec STMT
                              {  $$=mknode_op(OPK_QUAD,$1,$3,$5,$7,0); } 
      |   T_WHILE '(' exp ')' exp %prec STMT
                              {  $$=mknode_sbin($1,$3,$5); } 
      |   T_WHILE '(' exp ')' %prec STMT
                              {  $$=mknode_sbin($1,$3,0); } 
      ;

/* Expressions  */

exp   :      '*' exp          %prec UNARY  { $$=mknode_unary( $1,$2); }
      |      '&' exp          %prec UNARY  { $$=mknode_unary( $1,$2); }
      |      '-' exp          %prec UNARY  { $$=mknode_unary( $1,$2); }
      |      '!' exp            	   { $$=mknode_unary( $1,$2); }
      |      '~' exp                       { $$=mknode_unary( $1,$2); }
      |  T_COUNT exp                 	   { $$=mknode_sunary($1,$2); }
      |   T_ANDL exp	      		   { $$=mknode_sunary($1,$2); }
      |   T_ORL  exp	      		   { $$=mknode_sunary($1,$2); }
      |    T_INC exp                  	   { $$=mknode_unary( $1,$2); }
      |    T_DEC exp                       { $$=mknode_unary( $1,$2); }
      |    exp T_INC                       { $$=mknode_post_unary($2,$1); }
      |    exp T_DEC                  	   { $$=mknode_post_unary($2,$1); }
      |	T_SIZEOF '(' type ')'   	   { $$=mknode_sunary($1,$3); }
      | T_SIZEOF '(' exp  ')'   	   { $$=mknode_unary($1,$3); }
      | T_FRAME '(' exp ')'   %prec UNARY  { $$=mknode_unary( $1,$3); }
      ;

exp  :  exp T_DFS exp                     { $$=mknode_sbin($2,$1,$3); }
      |  exp T_BFS exp                    { $$=mknode_sbin($2,$1,$3); }

      |  exp '#' nameexp                  { $$=mknode_sbin($2,$1,$3); }
      |  exp '@'     exp            { $$=mknode_sbin($2,$1,$3); }
      |  exp T_ARROW exp            { $$=mknode_sbin($2,$1,$3); }
      |  exp '.'     exp            { $$=mknode_sbin($2,$1,$3); }
      |  exp '[' exp ']'            { $$=mknode_bin( $2,$1,$3); }
      |  exp T_OSEL exp T_CSEL      { $$=mknode_sbin($2,$1,$3); }
      |  exp '(' oexp ')' %prec '.' { $$=mknode_op(OPK_FUNC,$2,$1,$3,0,0); }
      |  '(' sm_exp ')'             { $$=mknode_unary($1,$2); }
      |  '{' sm_exp '}'             { $$=mknode_unary($1,$2); }
      ;

exp  :  '(' type ')' exp  %prec UNARY
                                   { $$=mknode_op(OPK_CAST,$1,$2,$4,0,0); }
/* HACKS to handle the most common cast cases with a typedef, without
 * requiring a 'T'. This code breaks (printf)("hi"), which returns the
 * error "printf not a typedef", but otherwise it works ok. 
 * It might be confusing since "(uint *)p" works but "(uint (*)())p" wont,
 * but it seems that (uint*)p, (uint)x are the most common, and users get
 * confused w/o them. 
 * The code below works essentially with context-sensitive parsing!
 * see the hacked %prec for nameexp which prevents yacc s/r warning!
 */
     |  '(' name ')' exp %prec UNARY { 
                  tctype *t=duel_get_target_typedef($2.name);
                  if(t==NULL) yyerror("not a typedef name"); 
                  $$=mknode_op(OPK_CAST,$1,mknode_ctype(t),$4,0,0); }
     |  '(' name '*' type_mod ')' exp %prec UNARY { 
                 tctype *t=duel_get_target_typedef($2.name);
                 if(t==NULL) yyerror("not a typedef name"); 
                 push_type('*');
                 $$=mknode_op(OPK_CAST,$1,mknode_modified_ctype(t),$6,0,0); }
     ;

 /* Bin ops in decreasing precedence order:  */

exp	:      exp  '*'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '/'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '%'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '+'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '-'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_LSH exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_RSH exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_EQ  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_NE  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_EQQ exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_NEQ exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_LE  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_GE  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_LEQ exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_GEQ exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '<'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '>'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_LSQ exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_GTQ exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '&'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '|'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp  '^'  exp	{ $$=mknode_bin($2,$1,$3); }
   	|      exp T_AND exp	{ $$=mknode_sbin($2,$1,$3); }
   	|      exp T_OR  exp	{ $$=mknode_sbin($2,$1,$3); }
        ;

exp	:    exp '?' exp ':' exp  %prec '?'
                	{ $$=mknode_tri($2,$1,$3,$5); }
	;
			  
exp	:    exp   '='    exp  { $$=mknode_bin($2,$1,$3); }
   	|    exp T_ASSIGN exp  { $$=mknode_op(OPK_ASSIGN,$2, $1,$3,0,0);  }
   	|nameexp T_DEFVAR exp  { $$=mknode_sbin($2,$1,$3);  }
	;

     /* generating expressions */

exp	:      exp T_TO  exp   { $$=mknode_sbin($2,$1,$3); }
        |      T_TO exp	       { $$=mknode_sbin($1, 0,$2); }
        |      exp T_TO        { $$=mknode_sbin($2,$1, 0); }
        |	exp ',' exp    { $$=mknode_sbin($2,$1,$3); }
        |      exp T_IMP exp   { $$=mknode_sbin($2,$1,$3); }
        ;

sm_exp  :	sm_exp ';' exp { $$=mknode_sbin($2,$1,$3); }
        |       exp 
	;

oexp	:	exp		/* optional expression, eg in for() */
        |			{ $$=0 ; }
        ;

exp	:	T_CONST  ;
exp	:	nameexp  ;
               /* convert a string input (name) into an expression. 
		* precedence of '+' is a hack to make the special case
		* of (x*)(y) parsed as a cast, without shift/reduce conflict
		*(this would work the same w/o the '+' but gives warning)
		*/
nameexp	:	name %prec '+'    { $$=mknode_name($1) ; } ;

type	:	typebase type_mod { $$=mknode_modified_ctype($1); }
        ;
/* type_mod has no value. bison warning is meaningless. I cant find a way
 * to shut it up
 */
type_mod: '(' type_mod ')'
	| '(' type_mod ')' '(' ')'	   {  push_type('('); }
	| '*' type_mod	 	           {  push_type('*'); }
	| type_mod '[' T_CONST ']' 	   {  push_type_int('[',$3); }
	| 
	;


/* note that names are evaluated at runtime. hence (name)(x) is ambigious
 * as either a function call or a cast.
 * We could identify a typedef 'name' as such and return a special token from
 * the lexr, but this will make 'x.(5+y)' illegal if y is both a field and
 * a typedef (Note that gdb's own code include such things).
 *
 * there is a complex solution, that keeps the the casting as a syntax tree,
 * and compute ctype at runtime, too. However, we want to compute all types at
 * parse time. Out solution forces the reserved word T_TYPEDEF_INDICATOR to
 * appear before any typedef name. (the reserved word is normally just 'T')
 * example: instead of '(list *) x'  use in duel:  '(T list *) x'
 */

typebase:	T_TYPEDEF_INDICATOR name           {
                    $$=duel_get_target_typedef($2.name);
		    if($$==NULL) {
		       tvalue v;
		       if(duel_get_target_variable($2.name,-1,&v)) $$=v.ctype;
		       else  yyerror("not a typedef name"); 
		    }
		} 
        ;

typebase:	T_CHAR                   { $$ = ctype_char;  }
	|	T_SIGNED T_CHAR          { $$ = ctype_schar; }
	|	T_UNSIGNED T_CHAR        { $$ = ctype_uchar; }
	|	T_INT                    { $$ = ctype_int;   }
	|	T_UNSIGNED               { $$ = ctype_uint;  }
	|	T_UNSIGNED T_INT         { $$ = ctype_uint;  }
	|	T_LONG                   { $$ = ctype_long;  }
	|	T_LONG T_INT             { $$ = ctype_long;  }
	|	T_UNSIGNED T_LONG        { $$ = ctype_ulong; }
	|	T_UNSIGNED T_LONG T_INT  { $$ = ctype_ulong; }
	|	T_SHORT                  { $$ = ctype_short; }
        |	T_SHORT T_INT		 { $$ = ctype_short; }
	|	T_UNSIGNED T_SHORT    	 { $$ = ctype_ushort; }
	|	T_UNSIGNED T_SHORT T_INT { $$ = ctype_ushort; }
        |       T_FLOAT                  { $$ = ctype_float ; }
        |       T_DOUBLE                 { $$ = ctype_double; }
        |	T_VOID			 { $$ = ctype_void;   }
	|	T_STRUCT name
		   { $$ = duel_get_target_struct_tag($2.name);
		     if($$==NULL) yyerror("not a struct tag"); }
	|	T_UNION name
		   { $$ = duel_get_target_union_tag($2.name);
		     if($$==NULL) yyerror("not a union tag"); }
	|	T_ENUM name
		   { $$ = duel_get_target_enum_tag($2.name);
		     if($$==NULL) yyerror("not an enum tag"); }
        ;

name    : T_SYM ;
%%

static struct stoken {    /* all opcodes we recognize */
  char *opstr ;                 /* op code as a string      */
  int token ;                   /* token to return to yacc  */
  int opcode ;                  /* opcode value associated with the token */
 } tokens[] =  {                /* the special tokens, longer ones 1st! */
    {">>=",T_ASSIGN, OP_RSH},
    {"<<=",T_ASSIGN, OP_LSH},
    {"-->",T_DFS,    OP_DFS},
    {"->>",T_BFS,    OP_BFS},
    {"==?", T_EQQ,   OP_EQQ},
    {"!=?", T_NEQ,   OP_NEQ},
    {"<=?", T_LEQ,   OP_LEQ},
    {">=?", T_GEQ,   OP_GEQ},
    {"&&/", T_ANDL,  OP_AND},
    {"||/", T_ORL,   OP_OR},

    {"<?", T_LSQ,    OP_LSQ},
    {">?", T_GTQ,    OP_GTQ},
    {"#/", T_COUNT,  '#' },
    {"%/", T_COUNT,  '#' }, /* gdb insists to recognize # as start of comma!*/
    {"%%", '#',      '#' }, /* same. so %/ for #/ and %% for #. not doc!*/
    {"+=", T_ASSIGN,  '+'},
    {"-=", T_ASSIGN,  '-'},
    {"*=", T_ASSIGN,  '*'},
    {"/=", T_ASSIGN,  '/'},
    {"%=", T_ASSIGN,  '%'},
    {"|=", T_ASSIGN,  '|'},
    {"&=", T_ASSIGN,  '&'},
    {"^=", T_ASSIGN,  '^'},
    {":=", T_DEFVAR,OP_DEF},
    {"++", T_INC,   OP_INC },
    {"--", T_DEC,   OP_DEC },
    {"->", T_ARROW, OP_ARR },
    {"&&", T_AND,   OP_AND },
    {"||", T_OR,    OP_OR  },
    {"<<", T_LSH,   OP_LSH },
    {">>", T_RSH,   OP_RSH },
    {"==", T_EQ,    OP_EQ  },
    {"!=", T_NE,    OP_NE  },
    {"<=", T_LE,    OP_LE  },
    {">=", T_GE,    OP_GE  },
    {"..", T_TO,    OP_TO  },
    {"=>", T_IMP,   OP_IMP },
    {"[[", T_OSEL,  OP_SEL },
    {"]]", T_CSEL,  OP_SEL },
  };

static struct skeyword {  /* all keywords we recognize */
  char *keyword_str ;           /* keyword as a string       */
  int token ;                   /* token to return to yacc   */
  topcode opcode ;              /* opcode associated w/keyword */
 } keywords[] = {
    {"if",	T_IF	   , OP_IF},
    {"else",	T_ELSE	   },
    {"for",	T_FOR	   , OP_FOR},
    {"while",	T_WHILE	   , OP_WHILE},
    {"sizeof",  T_SIZEOF   , OP_SIZ},
    {"frame",	T_FRAME	   , OP_FRAME},

    {"T", 	T_TYPEDEF_INDICATOR  },
    {"struct",  T_STRUCT   },
    {"union",   T_UNION    },
    {"enum",    T_ENUM     },

    {"unsigned",T_UNSIGNED },
    {"signed",  T_SIGNED   },
    {"short",   T_SHORT    },
    {"long",    T_LONG     },
    {"char",    T_CHAR     },
    {"int",     T_INT      },
    {"double",  T_DOUBLE   },
    {"float",   T_FLOAT    },
    {"void",	T_VOID	   },
   } ;


LFUNC tnode* duel_lex_int(void)    /* parse next token as integer num */
{
   tnode *n ;
   tulong val=0 ;
   char *p=lexptr ;
   bool is_l=0,is_u=0 ;
   int base=10 ;
   int src_pos=lexptr-inputstr ;
   
   if(*p=='0') {                        /* figure out the base */
      p++ ;
      if(*p=='x' || *p=='X') base=16,p++ ;
      else 
      if(isdigit(*p)) base=8 ; /* avoid having '0' as a base 8 (uint) */
   }

   while(isdigit(*p) || base==16 && isxdigit(*p)) {  /* get the value */
      val*=base ;
      if(isupper(*p)) val+= *p-'A'+10 ;
      else if(islower(*p)) val+= *p-'a'+10 ;
      else val+= *p-'0' ;
      p++ ;
   }
   if(*p=='l' || *p=='L') is_l=1,p++ ;          /* yuk. figure 0L etc */
   if(*p=='u' || *p=='U') is_u=1,p++ ;
   if(!is_l && (*p=='l' || *p=='L')) is_l=1,p++ ;
   is_u=is_u || base!=10 ;

   if(is_l && is_u || (long) val < 0 || ((tuint) val != val && is_u)) {
        n=mknode_const(src_pos,ctype_ulong);
        n->cnst.u.rval_ulong=val ;
   }
   else
   if(is_l || (tuint) val != val) {
        n=mknode_const(src_pos,ctype_long) ; 
        n->cnst.u.rval_long=(long) val ; 
   }
   else
   if(is_u || (int) val < 0) {
        n=mknode_const(src_pos,ctype_uint) ; 
        n->cnst.u.rval_uint=(tuint) val ; 
   }
   else {
        n=mknode_const(src_pos,ctype_int) ; 
        n->cnst.u.rval_int=(int) val ; 
   }
   strncpyz(n->cnst.symb_val,lexptr,p-lexptr); /* save the symbolic val*/
   lexptr=p ;
   return n ;
}

LFUNC tnode* duel_lex_float(void)    /* parse next token as float num */
{
  tnode *n=0 ;
  char *p=lexptr ;
  double val ;
  char c,tmpc ;
  bool ok=TRUE;
  int src_pos = lexptr - inputstr ;

  /* this is disgusting.. why isnt there a lib call to recognize floats?! */
  while(isdigit(*p)) p++ ;
  if(*p=='.') p++ ;
  while(isdigit(*p)) p++ ;
  if(*p=='e' || *p=='E') {
     p++ ;
     if(*p=='+' || *p=='-') p++ ;
     if(!isdigit(*p)) ok=FALSE ;     /* force digit (scanf allows 1e-.2 ?!) */
     while(isdigit(*p)) p++ ;
  }
  tmpc= *p ; *p=0 ;
  ok=ok && sscanf(lexptr,"%lf%c",&val,&c)==1 ;
  *p=tmpc ;
  if(!ok) yyerror("Invalid float constant.");

  n=mknode_const(src_pos,ctype_double); 
  n->cnst.u.rval_double=val ; 
  strncpyz(n->cnst.symb_val,lexptr,p-lexptr); /* save the symbolic val*/
  lexptr=p ;
  return(n);
}

/* parse_escaped_char -- parse an escaped char (e.g. '\n'). 
 * lexptr expected to point to text right after the '\'. 
 * return: actual char value (e.g. 012 if 'n' or '012' is found.)
 *         lexptr is advanced after the espaced char.
 */

LFUNC char parse_escaped_char(void)
{
  char retc ;
  switch(lexptr[0]) { 
   /*case 'a': retc='\a' ; break ;	/* some compilers don't support it. */
   case 'b': retc='\b' ; break ;
   case 'f': retc='\f' ; break ;
   case 'n': retc='\n' ; break ;
   case 'r': retc='\r' ; break ;
   case 't': retc='\t' ; break ;
   case 'v': retc='\v' ; break ;
   case 'x': yyerror("hex char const not yet suppported");
   case '0': case '1': case '2': case '3': 
	     retc= lexptr[0] - '0' ;
	     if(lexptr[1]>='0' && lexptr[1]<='7') 
		retc= retc* 010 +  *++lexptr - '0' ;
	     if(lexptr[1]>='0' && lexptr[1]<='7') 
		retc= retc* 010 +  *++lexptr - '0' ;
             break ;
   default:  retc=lexptr[0] ;     /* default also takes care of '\'' '\\' */
  }
  lexptr++ ;
  return retc ;
}

/* FUNC yylex -- return the next token to yacc. 
 * GLOBALS: lexptr point to the string we are parsing next. it is updated.
 */

LFUNC int yylex (void)
{
  int c,i,src_pos ;
  char *p ;

  for(c= *lexptr; c==' ' || c=='\t' || c=='\n' ; c= *++lexptr); /* skip blank*/

  src_pos = lexptr - inputstr ;	/* current char being parsed */
  yylval.opinfo.src_pos = src_pos ;

  if(*lexptr=='\0' || strncmp(lexptr,"|>",2)==0) return 0 ; /* end of expr */

  for (i = 0;  i < sizeof(tokens)/sizeof(struct stoken) ; i++) { 
    int l=strlen(tokens[i].opstr) ;             /* check next token vs table */
    if(strncmp(lexptr,tokens[i].opstr,l)==0) {
	lexptr+=l ;
	yylval.opinfo.opcode = tokens[i].opcode;
	return tokens[i].token ;
    }
  }

  switch (c = *lexptr) {
    case '\'':                /* char constant, but stored as int (ansi-c) */
      p=lexptr++ ;
      c = *lexptr++ ;
      if (c == '\\') c=parse_escaped_char();
      if( *lexptr++ != '\'') yyerror("Invalid character constant.");
      yylval.node=mknode_const(src_pos,ctype_int) ;
      yylval.node->cnst.u.rval_int=c ;
      strncpyz(yylval.node->cnst.symb_val,p,lexptr-p); /*save the symbol. val*/
      return T_CONST ;
    
    case '0':                           /* chk hex  */
        if(lexptr[1]=='x' || lexptr[1]=='X') {
           yylval.node=duel_lex_int(); 
           return T_CONST ;
        }
        /* fall thru for other numbers */
    case '1': case '2': case '3':      /* decimal or floating point number */
    case '4': case '5': case '6': case '7': case '8': case '9':
          for(p=lexptr ; *p>='0' && *p<='9' ; p++ ) ;  /*find next non digit*/
          if(*p=='.' && p[1]!='.' || *p=='e' || *p=='E') 
               yylval.node=duel_lex_float();
          else yylval.node=duel_lex_int();
          return T_CONST ;

    case '(':  case ')':
    case '<':  case '>':
    case '[':  case ']':
    case '{':  case '}':
    case '+':  case '-':  case '*':  case '/':  case '%':
    case '|':  case '&':  case '^':  case '~':  case '!':
    case ',':  case '?':  case ':':  case '=':  
    case '.':  case '@':  case '$':  case '#':  case '`': case '\\': 
      lexptr++;
      yylval.opinfo.opcode=c ;
      return c;
    case ';': { /* hack, ignore ';' before '}' and else. for C compatability*/
	        char *save_lexptr= ++lexptr ;
		int tok=yylex()	;	/* hack, call myself for next token */
		if(tok=='}' || tok==T_ELSE) {
		    duel_printf("warning: useless ';' ignored\n");
		    return tok ;
		}
		/* else restore position and return the ';' */
		lexptr=save_lexptr ;
		yylval.opinfo.opcode=';' ;
		yylval.opinfo.src_pos = src_pos ;
		return ';';
    }
    case '"': {
          char s[512] ;
	  size_t len=0 ;
	  ttarget_ptr dptr ;
	  tnode *n ;

	  p=lexptr++ ; 
	  while((c= *lexptr++)!='"') {
	       if (c == '\\') c=parse_escaped_char();
	       s[len++]=c ;
	  }
	  s[len++]=0 ;
	  dptr=duel_alloc_target_space(len);
	  duel_put_target_bytes(dptr,s,len);
	  
	  n=mknode_const(src_pos,ctype_charptr); 
	  n->cnst.u.rval_ptr=dptr ; 
	  len=lexptr-p ;
	  if(len>60) len=60 ;
	  strncpyz(n->cnst.symb_val,p,len); /* save the symbolic val*/
          yylval.node=n ;
          return T_CONST ;
      }
    }

  if(c != '_' && !isalpha(c))
     yyerror ("Invalid character in expression.");

  p=lexptr ;
  do { c= *++lexptr ; } while(c=='_' || isalnum(c));
  
  for (i = 0;  i < sizeof(keywords)/sizeof(struct skeyword) ; i++) { 
    int l=strlen(keywords[i].keyword_str) ;   /* check next token vs keywords*/
    if(l==lexptr-p && strncmp(p,keywords[i].keyword_str,l)==0) {
        yylval.opinfo.opcode=keywords[i].opcode ;
	return keywords[i].token ;
    }
  }

  /* the symbol/name found is not a reserved word, so return it as a T_SYM
   */
    
  i=lexptr-p ;          /* length of string found (symbol/name) */
  yylval.nameinfo.src_pos=src_pos ;
  yylval.nameinfo.name=duel_malloc(i+1);
  strncpyz(yylval.nameinfo.name,p,i);
  return T_SYM;
}

LPROC yyerror(char *msg)
{
  int i,n=lexptr-inputstr ;
  duel_printf("%s\n",inputstr);
  for(i=0 ; i<n ; i++) duel_printf("-");
  duel_printf("^ %s\n",msg);
  duel_abort();		/* terminate parsing. some callers depend on this*/
}

/*************************************************************************/
/* utility functions used to parse the expression and build it as a tree */
/*************************************************************************/

/* mknode_op -- make a tree node of type op with given opcode and kids
 */

LFUNC tnode* mknode_op(top_kind op_kind,topinfo opinfo,
                       tnode *k1,tnode *k2,tnode *k3,tnode *k4)
{
   tnode *n ;
   duel_assert(opinfo.opcode>' ');
   n=(tnode *) duel_malloc(sizeof(tnode));
   duel_bzero((char*) n,sizeof(tnode));
   n->node_kind=NK_OP ;
   n->op_kind=op_kind ;
   n->op=opinfo.opcode ;
   n->src_pos=opinfo.src_pos ;
   n->kids[0]=k1 ;  n->kids[1]=k2 ;  n->kids[2]=k3 ; n->kids[3]=k4 ;
   return n ;
}


 /* mknode_const -- make a constant node for the given type. 
  */

LFUNC tnode* mknode_const(int src_pos,tctype *ctype)
{
   tnode *n ;
   n=(tnode *) duel_malloc(sizeof(tnode));
   duel_bzero((char*) n,sizeof(tnode));
   n->node_kind=NK_CONST ;
   n->src_pos=src_pos ;
   n->cnst.val_kind=VK_RVALUE ;
   n->cnst.ctype=ctype ;
   return n ;
}

 /* mknode_ctype -- make a node of the given c-type. 
  */

LFUNC tnode* mknode_ctype(tctype *ctype)
{
   tnode *n ;
   n=(tnode *) duel_malloc(sizeof(tnode));
   duel_bzero((char*) n,sizeof(tnode));
   n->node_kind=NK_CTYPE ;
   n->ctype=ctype ;
   return n ;
}

 /* mknode_name -- make a node of the given name/symbol.
  * input is pointer to the saved name (on heap)
  */

LFUNC tnode* mknode_name(tnameinfo nameinfo)
{
   tnode *n ;
   n=(tnode *) duel_malloc(sizeof(tnode));
   duel_bzero((char*) n,sizeof(tnode));
   n->node_kind=NK_NAME ;
   n->name=nameinfo.name ;
   n->src_pos=nameinfo.src_pos ;
   return n ;
}

/* In order to parse C types, which are 'reversed' in the parser, a stack
 * is used to push abstract declarators, e.g. in (*)() we first push a func
 * indicator '(' and then push a pointer indicator '*'. for arrays we push
 * a '[' and the array size. 
 * This stack is popped and a ctype is constructed at the end of the 
 * abstract type parsing. The following functions implement the stack
 */

typedef struct stype_desc {  /* stack of type descriptors is made of these */
        char desc ; 
        int size ;
        struct stype_desc *next ;       /* next on stack */
      } ttype_desc ;

ttype_desc *top = 0 ;


LPROC push_type(char desc)     /* put desc on the types stack */
{
    ttype_desc *p = (ttype_desc* ) duel_malloc(sizeof(ttype_desc));
    p->desc=desc ;
    p->size=0 ;
    p->next=top ;
    top=p ;
}

/* push_type_int -- same as push_type but also set the size parameter, which
 *                  is given as a constant node (which is expected to be int)
 */

LPROC push_type_int(char desc,tnode *n)  
{
   duel_assert(n->node_kind==NK_CONST);
   if(n->cnst.ctype != ctype_int || 
      n->cnst.u.rval_int <=0 ) duel_gen_error("Illegal array size",0);
   push_type(desc);
   top->size=n->cnst.u.rval_int ;
}

LFUNC bool pop_type(char *desc,int *size)  /* pop item from stack. */
{
   ttype_desc *p = top ;
   if(p==0) return FALSE ;
   *desc=p->desc ;
   *size=p->size ;
   top=p->next ;
   duel_free(p) ;
   return TRUE ;
}


/* abstract type-modifiers were pushed on a stack. Retrieve
 * them (reversed) creating type nodes as we go 
 * input: base type (e.g. 'long'). 
 * returns: node of the modified type. 
 * modification is based on the stack of things pushed while parsing.
 */

LFUNC tnode* mknode_modified_ctype(tctype *base)
{  
    int size;
    char tdesc ;           /* descriptor of abs decl eg '*' */
    tctype *t=base ;       /* type under construction       */
    
    while(pop_type(&tdesc,&size))    /* pop next abs decl    */
	switch (tdesc) {
	  case '*':  t=duel_mkctype_ptr(t);         break ;
	  case '(':  t=duel_mkctype_func(t);        break ;
	  case '[':  t=duel_mkctype_array(t,size);  break ;
	}	
    return mknode_ctype(t) ;
}

/* entry point for parsing. the given expression is parsed into the given
 * node as root.
 */

FUNC tnode* duel_parse(char *s)
{
  lexptr=inputstr=s ;
  top=0 ; 				/* reset the types stack */
  if(duel_yyparse()) root=NULL ;
  return root ;
}
