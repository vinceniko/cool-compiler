/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* memory */
#define YYINITDEPTH 10000
#define YYMAXDEPTH 10000

extern char *curr_filename;

void yyerror(const char *s);        /*  defined below; called for each parse error */
extern int yylex();           /*  the entry point to the lexer  */

#define YYLTYPE int              /* the type of locations */
#define cool_yylloc curr_lineno
extern int node_lineno;          /* set before constructing a tree node
                                    to whatever you want the line number
                                    for the tree node to be */

/* The default action for locations.  Use the location of the first
   terminal/non-terminal and set the node_lineno to that value. */
#define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current = Rhs[1];                             \
  node_lineno = Current;

#define SET_NODELOC(Current)  \
  node_lineno = Current;


/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

Program ast_root;             /* the result of the parse  */
Classes parse_results;        /* for use in semantic analysis */
int omerrs = 0;               /* number of errors in lexing and parsing */
%}

/* A union of all the types that can be the result of parsing actions. Don't change this.*/
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class

/* You will want to change the following line. */
%type <features> feature_list
%type <feature> feature
%type <expression> optional_assign
%type <formals> formal_list_w_empty
%type <formals> formal_list
%type <formal> formal
%type <expression> expression
%type <expressions> expression_list_block
%type <expressions> expression_list_dispatch_w_empty
%type <expressions> expression_list_dispatch
%type <expression> let
%type <case_> case_branch
%type <cases> case_branch_list
%type <error_msg> error

/* Precedence declarations go here. */
/* lower rules have higher precedences */
/* cool-manual 3.2: All binary operations are left-associative, with the exception of assignment, which is right-associative, and the three comparison operations, which do not associate. */
%precedence LET_REDUCE
%right ASSIGN /* not <- (a <- foo) */
%left NOT  /* not (not foo) */
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID /* isvoid isvoid foo */
%left '~'
%left '@'
%left '.'

%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program : class_list    { /* make sure bison computes location information */
                          @$ = @1;
                          ast_root = program($1); }
        ;

class_list
        : class                 /* single class */
                { $$ = single_Classes($1);
                  parse_results = $$; }
        | class_list class     /* several classes */
                { $$ = append_Classes($1,single_Classes($2)); 
                  parse_results = $$; }
        /* If there is an error in a class definition but the class is terminated properly and the next class is syntactically correct, the parser should be able to restart at the next class definition. */
        | error ';' 
                { $$ = nil_Classes(); }
        ;

/* If no parent is specified, the class inherits from the Object class. */
class   : CLASS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,idtable.add_string("Object"),$4,
                              stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
        ;

/* Feature list may be empty, but no empty features in list. */
feature_list:
        /* the following 2 rules function like the Kleene Closure in CFGs, allowing us to match 0 or more features */
        
        /* empty */
         {  $$ = nil_Features(); }
        /* 1 or more features */
        | feature_list feature
                { $$ = append_Features($1, single_Features($2)); }
        /* error handling. error in a feature permissable as long it ends with a semi-colon */
        | error ';' 
                { $$ = nil_Features(); }
        ;

feature:
        /* method definition */
        OBJECTID '(' formal_list_w_empty ')' ':' TYPEID '{' expression '}' ';'
                { $$ = method($1, $3 , $6, $8);  }
        /* attribute definition */
        | OBJECTID ':' TYPEID optional_assign ';'
                { $$ = attr($1, $3, $4); }
        ;

optional_assign:
        /* shared by attribute assignment and let assignment */

        ASSIGN expression
                { $$ = $2; }
        | { node_lineno = 0;  $$ = no_expr(); }
        ;

formal_list_w_empty:
        /* 0 or more formals delimited by ',' */

        formal_list
                { $$ = $1; }
        | { $$ = nil_Formals(); }
        ;

formal_list:
        /* 1 or more formals delimited by ',' */
        /* can not put an empty rule here (func definition with no args) since the rule would match `formal_list ',' epsilon`. instead created formal_list_w_empty */

        formal
                { $$ = single_Formals($1); }
        /* several formals. must be delimited by comma. */
        | formal_list ',' formal
                { $$ = append_Formals($1, single_Formals($3)); }
        ;

formal:
        OBJECTID ':' TYPEID
                { $$ = formal($1, $3); }
        ;

expression_list_block:
        /* 1 or more expressions each ending with ';' */

        expression ';'
                { $$ = single_Expressions($1); }
        | expression_list_block expression ';'
                { $$ = append_Expressions($1, single_Expressions($2)); }
        | error ';'
                { $$ = nil_Expressions(); }
        ;

expression_list_dispatch_w_empty:
        /* list of ',' delimited expressions or no expressions */
        /* used for method dispatch */

        expression_list_dispatch
                { $$ = $1;}
        | { $$ = nil_Expressions(); }
        ;

expression_list_dispatch:
        /* 1 or more expressions delimited by ',' */
        /* used for method dispatch */
        /* can not put an empty rule here (call with no args) since the rule would match `expression_list_dispatch ',' epsilon`. instead created expression_list_dispatch_w_empty */
        
        expression 
                { $$ = single_Expressions($1); }
        | expression_list_dispatch ',' expression
                { $$ = append_Expressions($1, single_Expressions($3)); }
        ;

expression:
        /* assign */
        OBJECTID ASSIGN expression 
                { $$ = assign($1, $3); }

        /* method dispatch */
        | expression '.' OBJECTID '(' expression_list_dispatch_w_empty ')'
                { $$ = dispatch($1, $3, $5); }
        | expression '@' TYPEID '.' OBJECTID '(' expression_list_dispatch_w_empty ')'
                { $$ = static_dispatch($1, $3, $5, $7); }

        /* self method dispatch shorthand */
        | OBJECTID '(' expression_list_dispatch_w_empty ')' 
                { $$ = dispatch(object(idtable.add_string("self")), $1, $3); }

        /* block expression */
        | '{' expression_list_block '}'
                { $$ = block($2); } 

        | IF expression THEN expression ELSE expression FI
                { $$ = cond($2, $4, $6); }
        | WHILE expression LOOP expression POOL
                { $$ = loop($2, $4); }

        | NEW TYPEID
                { $$ = new_($2); }
        | ISVOID expression
                { $$ = isvoid($2); }

        /* arithmetic expressions */
        | expression '+' expression
                { $$ = plus($1, $3); }
        | expression '-' expression
                { $$ = sub($1, $3); }
        | expression '*' expression
                { $$ = mul($1, $3); }
        | expression '/' expression
                { $$ = divide($1, $3); }
        | '~' expression
                { $$ = neg($2); }

        /* conditional expressions */
        | expression '<' expression
                { $$ = lt($1, $3); }
        | expression LE expression
                { $$ = leq($1, $3); }
        | expression '=' expression
                { $$ = eq($1, $3); }
        | NOT expression
                { $$ = comp($2); }

        | '(' expression ')'
                { $$ = $2 ; }
        

        /* constants */
        | OBJECTID
                {  $$ = object($1); }
        | INT_CONST 
                {  $$ = int_const($1); }
        | STR_CONST
                {  $$ = string_const($1); }
        | BOOL_CONST
                {  $$ = bool_const($1); }

        | LET let
                { $$ = $2; }

        | CASE expression OF case_branch_list ESAC
                { $$ = typcase($2, $4); }
        ;

let:
        /* need the "IN expression" bit here to do AST node assignment with let() */
        /* precedence rule to solve shit/reduce conflict with the expression non-terminal. set low precedence so that let doesnt reduce until expression evaluated (shift has higher precedence) */
        /* comma delimited */

        /* this matches last if a list */
        OBJECTID ':' TYPEID optional_assign IN expression %prec LET_REDUCE
                { $$ = let($1, $3, $4, $6); }
        /* this would match first if it is a list */
        | OBJECTID ':' TYPEID optional_assign ',' let
                { $$ = let($1, $3, $4, $6); }
        | error ',' let
                { node_lineno = 0;  $$ = no_expr(); }
        ;

case_branch_list:
        /* one or more case branches each ending with ';' */
        case_branch
                { $$ = single_Cases($1); }
        | case_branch_list case_branch
                { $$ = append_Cases($1, single_Cases($2)); }
        ;

case_branch:
        OBJECTID ':' TYPEID DARROW expression ';'
                { $$ = branch($1, $3, $5); }
        ;

/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. Don't change this. */
void yyerror(const char *s)
{
  extern int curr_lineno;

  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}

