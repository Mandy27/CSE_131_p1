/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should 
 *      accept the language as described in specification, and as augmented 
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */
 
/* yylval 
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser. 
 *
 * pp2: You will need to add new fields to this union as you add different 
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
    VarDecl *vardecl;
    FnDecl *fndecl;
    Type *type;
    Expr *priexpr;
    Expr *expr;
    Expr *postexpr;  //cant declare PostExpr
    Expr *mulexpr;
    Expr *addexpr;
    Expr *relativeexpr;
    Expr *equalityexpr;
    Expr *logandexpr;
    Expr *logorexpr;
    Expr *assignexpr;
    Expr *unaryexpr; 
    List<VarDecl*> *param;
    Identifier *identify;
    Operator *assignoper;
    Operator *unaryoper;
    Stmt *stmt;
    Stmt *simplestmt;
    List<Stmt*> *stmtlist;
    StmtBlock* stmtblock;
    IfStmt* selectionstmt;
    SwitchStmt *switchstmt;
    Stmt* compoundstmt;
    Expr * exprstmt ;
    Case * caselabel;
    Default *defaultlabel;
    WhileStmt *iterationstmt;
    List<Stmt*>  *SwitchStmtList;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Float 
%token	 T_Vec2 T_Vec3 T_Vec4 T_Mat2 T_Mat3 T_Mat4
%token   T_In T_Out
%token   T_Uniform
%token   T_Layout
%token   T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims
%token   T_And T_Or 
%token   T_While T_For T_If T_Else T_Return T_Break T_Continue
%token   T_Inc T_Dec T_Switch T_Case T_Default
%token   T_FieldSelection T_IncOp T_DecOp T_LeOp T_GeOp T_EqOp T_NeOp
%token   T_AndOp T_OrOp T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%token   T_LeftParen T_RightParen 
%token   T_LeftBrace T_RightBrace
%token   T_Dot
%token   T_Dash T_Plus T_Star T_Slash
%token   T_LeftAngle T_RightAngle

%token   <identifier> T_Identifier
%token   <integerConstant> T_IntConstant
%token   <doubleConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant


/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>  DeclList 
%type <decl>      Decl
%type <vardecl>   VarDecl
%type <type>  Type
%type <priexpr>  PriExpr
%type <expr> Expr
%type <postexpr> PostExpr
%type <unaryexpr> UnaryExpr
%type <fndecl> FnDecl
%type <param>  Param
%type <vardecl>    Var
%type <identify> Identifier
%type <mulexpr> MulExpr
%type <addexpr> AddExpr
%type <relativeexpr> RelationalExpr
%type <equalityexpr> EqualityExpr
%type <logandexpr> LogAndExpr
%type <logorexpr> LogOrExpr
%type <assignexpr> AssignExpr
%type <assignoper> AssignOper
%type <assignoper> UnaryOper
%type <stmt> Stmt
%type <simplestmt> SimpleStmt
%type <stmtlist> StmtList
%type <stmtblock> StmtBlock
%type <switchstmt> SwitchStmt
%type <compoundstmt> CompoundStmt
%type <selectionstmt> SelectionStmt
%type <exprstmt> ExprStmt  
%type <caselabel> CaseLabel
%type <iterationstmt> IterationStmt
%type <SwitchStmtList> SwitchStmtList
%type <defaultlabel> DefaultLabel

%nonassoc "No_Else"
%nonassoc T_Else
%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
	 
 */
Program   :    DeclList            { 
                                      @1; 
                                      /* pp2: The @1 is needed to convince 
                                       * yacc to set up yylloc. You can remove 
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0) 
                                          program->Print(0);
                                    }
          ;

DeclList  :    DeclList Decl                              { ($$=$1)->Append($2); }
          |    Decl                                       { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      :    VarDecl ';'                                {$$ = $1;}
 	  |    FnDecl                                     {$$ = $1;}
          ;
          
VarDecl   :   Var           	                          { $$ = $1;}
          |   Var '=' PriExpr                             {}
	  ;

Var       :  Type Identifier        			  {$$= new VarDecl($2,$1);}
          ;

Identifier:  T_Identifier   	                          {$$=new Identifier(@1, $1);}
          ;

Type      :  T_Int    			                  { $$ = Type::intType;}      /*type_specifier_noarray*/
  	  |  T_Float			                  { $$ = Type::floatType;}
	  |  T_Bool  			                  { $$ = Type::boolType;} 
	  |  T_Vec2			                  { $$ = Type::vec2Type;}
	  |  T_Vec3			                  { $$ = Type::vec3Type;}
	  |  T_Vec4			                  { $$ = Type::vec3Type;}
	  |  T_Mat2			                  { $$ = Type::mat2Type;}
	  |  T_Mat3			                  { $$ = Type::mat3Type;}
	  |  T_Mat4			                  { $$ = Type::mat4Type;}
          ;

FnDecl	  :  Type Identifier '(' Param ')' CompoundStmt               { $$ = new FnDecl($2, $1, $4); $$->SetFunctionBody($6);} 
  	  |  T_Void Identifier '(' Param ')' CompoundStmt              { $$ = new FnDecl($2, Type::voidType, $4);$$->SetFunctionBody($6);} 
	  ;
	   
Param	  : Param ',' Var	                          {($$ = $1)->Append($3);}
 	  | Var          				  {($$ = new List<VarDecl*>)->Append($1);}
/*no param*/
	  |						  {$$ = new List<VarDecl*>;}                             
	  ;

PriExpr   : T_IntConstant				  {$$ = new IntConstant(@1,$1);}
	  | T_FloatConstant				  {$$ = new FloatConstant(@1,$1);}
	  | T_BoolConstant			          {$$ = new BoolConstant(@1,$1);}
          | Identifier					  {$$ = new FieldAccess(NULL, $1);}
	  | '(' Expr ')'				  {$$ = $2;}
	  ;
          
Expr 	  : AssignExpr                                    { $$ =$1;}
          ;

MulExpr   : UnaryExpr	/*multiplicative*/		  { $$ = $1;}
	  | MulExpr '*' UnaryExpr			  { $$ = new ArithmeticExpr($1, new Operator(@2, "*"), $3);}
	  | MulExpr '/' UnaryExpr                         { $$ = new ArithmeticExpr($1, new Operator(@2, "/"), $3);}
	  ;

AddExpr	  : MulExpr					  { $$ = $1;}
          | AddExpr '+' MulExpr 			  {$$ = new ArithmeticExpr($1, new Operator(@2, "+"), $3);}
	  | AddExpr '-' MulExpr				  {$$ = new ArithmeticExpr($1, new Operator(@2, "-"), $3);}
	  ;


RelationalExpr: AddExpr				          {$$ =$1;}
              | RelationalExpr '<' AddExpr	          {$$ = new RelationalExpr($1, new Operator(@2, "<"), $3);}
              | RelationalExpr '>' AddExpr		  {$$ = new RelationalExpr($1, new Operator(@2, ">"), $3);}
              | RelationalExpr "<=" AddExpr               {$$ = new RelationalExpr($1, new Operator(@2, "<="), $3);}
              | RelationalExpr ">=" AddExpr               {$$ = new RelationalExpr($1, new Operator(@2, ">="), $3);}
              ;

EqualityExpr: RelationalExpr				  { $$ =$1;}
	    | EqualityExpr "==" RelationalExpr            { $$= new EqualityExpr( $1, new Operator(@2, "=="), $3);}
	    | EqualityExpr "!=" RelationalExpr            { $$= new EqualityExpr( $1, new Operator(@2, "!="), $3);}
	    ;

LogAndExpr: EqualityExpr 				  {$$ = $1;}
	  | LogAndExpr "&&" EqualityExpr  		  {$$= new LogicalExpr( $1, new Operator(@2, "&&"), $3);}
	  ;

LogOrExpr : LogAndExpr					  { $$ = $1;}
	  | LogOrExpr "||"  LogAndExpr			  {$$= new LogicalExpr( $1, new Operator(@2, "||"), $3);}
	  ;

AssignExpr: LogOrExpr    				  { $$ =$1;}
	  | UnaryExpr AssignOper AssignExpr               { $$ = new AssignExpr($1, $2, $3); }
	  ;

AssignOper: '='                                           {$$= new Operator(@1, "=");}
          | "*="                                          {$$= new Operator(@1, "*=");}
          | "/="                                          {$$= new Operator(@1, "/=");}
          | "+="                                          {$$= new Operator(@1, "+=");}
          | "-="                                          {$$= new Operator(@1, "-=");}
          ;

PostExpr  : PriExpr					  {$$=$1;}
	  | PostExpr "++" 				  {$$ = new PostfixExpr($1, new Operator(@2, "++"));}
	  | PostExpr "--" 				  {$$ = new PostfixExpr($1, new Operator(@2, "--"));}
          | PostExpr '.' Identifier	                  {$$ = new FieldAccess($1, $3);} 
	  ;
          
UnaryOper : '+'                                           {$$= new Operator(@1, "+");}
          | '-'                                           {$$= new Operator(@1, "-");}
          ;
          
UnaryExpr : PostExpr					  {$$ = $1;}
	  | "++" UnaryExpr				  {$$ = new PostfixExpr($2, new Operator(@1, "++"));}
	  | "--" UnaryExpr				  {$$ = new PostfixExpr($2, new Operator(@1, "--"));}
	  | UnaryOper UnaryExpr				  {$$ = new PostfixExpr($2, $1);}
	  ;

SimpleStmt : ExprStmt                                     { $$ =$1;}
           | SwitchStmt                                   { $$=$1;}
           /*| Decl                                         { }
           | CaseLabel                                    { $$= $1;}
	   */
           | SelectionStmt                                {$$=$1;}
           | IterationStmt                                { $$ =$1;}
           ;
           
Stmt : SimpleStmt                                         {$$ =$1;}
     | CompoundStmt                                       {$$=$1;}
     ;

StmtList : Stmt                                           {($$ = new List<Stmt*>)->Append($1);}
         | StmtList Stmt                                  { ($$ = $1)->Append($2);}
         ;
         
SwitchStmtList: StmtList                                  {$$=$1;}
              /* NOTHING */
	      |						  {$$ = new List<Stmt*>;}
              ;

SwitchStmt: T_Switch '(' Expr ')' '{' CaseList DefaultLabel '}'  {}
          ;

CaseList : CaseLabel      {}
	 | CaseList CaseLabel {}
	 ;

CaseLabel: T_Case Expr ':' SwitchStmtList                    {}
         ;

DefaultLabel  : T_Default ':' SwitchStmtList                      {}
	 |                                                   {}
 	 ;

CompoundStmt : '{''}'                                     { $$ = new StmtBlock(new List<VarDecl*>, new List<Stmt*>);}
             | '{' StmtList '}'                           {$$ = new StmtBlock(new List<VarDecl*>, $2);}
             ;
             
StmtBlock : CompoundStmt                                   {}
          | SimpleStmt                                     {}
          ;
          
ExprStmt : ';'                                            {$$ = new EmptyExpr();}
         |  Expr ';'                                      {$$ =$1;}
         ;
         
         
SelectionStmt : T_If '(' Expr ')' StmtBlock   %prec "No_Else"                         {}
              | T_If '(' Expr ')' StmtBlock T_Else StmtBlock        {}
              ;
                  
IterationStmt: T_While '(' Condition ')' StmtBlock  {}
             | T_For '(' ForInitStmt ForRestStmt ')' StmtBlock  {}
             ;
             
ForInitStmt: ExprStmt                                     {}
           | Decl                             {}
           ;
           
ForRestStmt: ConditionOpt ';'                             {}
           | ConditionOpt ';' Expr                        {}
           ;

ConditionOpt: Condition                                   {}
            /* EMPTY */
            ;           

Condition : Expr                                          {}
          | Type Identifier T_Equal AssignExpr           {}
          ;
%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
