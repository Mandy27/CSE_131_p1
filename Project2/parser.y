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
    char field[4];
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
    Expr *relationexpr;
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
    Case * caselabel;
    List<Case*> *caselist;
    Stmt* compoundstmt;
    Expr * exprstmt ;
    Default *defaultlabel;
    Stmt *iterationstmt;
    List<Stmt*>  *SwitchStmtList;
    List<VarDecl*> *vardecllist;
    Expr *conditionopt;
    Expr *condition;
    List<Expr*> *paramList;

    Call *vector;
   /* struct init{
      VarDecl *l;
      Expr    *r;
    }init;*/
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
%token   <field> T_Fields

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
%type <relationexpr> RelationalExpr
%type <equalityexpr> EqualityExpr
%type <logandexpr> LogAndExpr
%type <logorexpr> LogOrExpr
%type <assignexpr> AssignExpr
%type <assignoper> AssignOper
%type <assignoper> UnaryOper
%type <stmt> Stmt
%type <simplestmt> SimpleStmt
%type <stmtlist> StmtList
%type <switchstmt> SwitchStmt
%type <compoundstmt> CompoundStmt
%type <selectionstmt> SelectionStmt
%type <exprstmt> ExprStmt  
%type <caselabel> CaseLabel
%type <caselist> CaseList
%type <iterationstmt> IterationStmt
%type <SwitchStmtList> SwitchStmtList
%type <defaultlabel> DefaultLabel
%type <vardecllist> VarDeclList
%type <conditionopt> ConditionOpt
%type <condition> Condition
%type <paramList> ParamList
%type <vector> Vector
//%type <init> Init

%right '=' T_AddAssign T_SubAssign T_MulAssign T_DivAssign
%left T_Or
%left T_And
%left T_Equal T_NotEqual
%left '<' '>' T_LessEqual T_GreaterEqual
%left T_Plus T_Dash
%left T_Star T_Slash
%right T_Inc T_Dec
%left T_Dot
%nonassoc T_LeftParen T_RightParen
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

Decl      :    VarDecl                                    {$$ = $1;}
 	  |    FnDecl                                     {$$ = $1;}
          ;
          
VarDecl   :   Var ';'          	                          { $$ = $1;}
          |   Type Identifier '=' PriExpr ';'             {/*printf("here"); 
	  						  VarDecl *n1 = new VarDecl($2,$1); Expr* = new AssignExpr($2, new Operator(@1, "="), $4);*/}
	  ;

Var       :  Type Identifier        			  {$$= new VarDecl($2,$1);}
          ;

VarDeclList : VarDecl                                     {($$ = new List<VarDecl*>)->Append($1); }
            | VarDeclList VarDecl                         {($$=$1)->Append($2); }
            ;

Identifier:  T_Identifier   	                          {$$=new Identifier(@1, $1);}
          ;

Type      :  T_Int    			                  { $$ = Type::intType;}      /*type_specifier_noarray*/
  	  |  T_Float			                  { $$ = Type::floatType;}
	  |  T_Bool  			                  { $$ = Type::boolType;} 
	  |  T_Vec2			                  { $$ = Type::vec2Type;}
	  |  T_Vec3			                  { $$ = Type::vec3Type;}
	  |  T_Vec4			                  { $$ = Type::vec4Type;}
	  |  T_Mat2			                  { $$ = Type::mat2Type;}
	  |  T_Mat3			                  { $$ = Type::mat3Type;}
	  |  T_Mat4			                  { $$ = Type::mat4Type;}
          ;

FnDecl	  :  Type Identifier T_LeftParen Param T_RightParen CompoundStmt   {$$ = new FnDecl($2, $1, $4); $$->SetFunctionBody($6);} 
  	  |  T_Void Identifier T_LeftParen Param T_RightParen CompoundStmt {$$ = new FnDecl($2, Type::voidType, $4);$$->SetFunctionBody($6);} 
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
	  | T_LeftParen Expr T_RightParen		  {$$ = $2;}
	  | Vector 					  {$$ =$1;}
	  ;
          
Expr 	  : AssignExpr                                    { $$ =$1;}
          ;

Vector	  : T_Vec2 T_LeftParen ParamList T_RightParen    {$$ = new Call(@1,NULL, new Identifier(@1, "vec2"), $3);}
	  | T_Vec3 T_LeftParen ParamList T_RightParen    {$$ = new Call(@1,NULL, new Identifier(@1, "vec3"), $3);}
	  | T_Vec4 T_LeftParen ParamList T_RightParen    {$$ = new Call(@1,NULL, new Identifier(@1, "vec4"), $3);}
          ;

ParamList : ParamList ',' Expr                            {($$=$1)->Append($3);}
          | Expr                                          {($$ = new List<Expr*>)->Append($1);}
	  ;

MulExpr   : UnaryExpr                    		  { $$ = $1;}
	  | MulExpr T_Star UnaryExpr			  { $$ = new ArithmeticExpr($1, new Operator(@2, "*"), $3);}
	  | MulExpr T_Slash UnaryExpr                         { $$ = new ArithmeticExpr($1, new Operator(@2, "/"), $3);}
	  ;

AddExpr	  : MulExpr					  { $$ = $1;}
          | AddExpr T_Plus MulExpr 			  {$$ = new ArithmeticExpr($1, new Operator(@2, "+"), $3);}
	  | AddExpr T_Dash MulExpr				  {$$ = new ArithmeticExpr($1, new Operator(@2, "-"), $3);}
	  ;


RelationalExpr: AddExpr				          {$$ =$1;}
              | RelationalExpr '<' AddExpr	          {$$ = new RelationalExpr($1, new Operator(@2, "<"), $3);}
              | RelationalExpr '>' AddExpr		  {$$ = new RelationalExpr($1, new Operator(@2, ">"), $3);}
              | RelationalExpr T_LessEqual AddExpr        {$$ = new RelationalExpr($1, new Operator(@2, "<="), $3);}
              | RelationalExpr T_GreaterEqual AddExpr     {$$ = new RelationalExpr($1, new Operator(@2, ">="), $3);}
              ;

EqualityExpr: RelationalExpr				  { $$ =$1;}
	    | EqualityExpr T_Equal RelationalExpr         { $$= new EqualityExpr( $1, new Operator(@2, "=="), $3);}
	    | EqualityExpr T_NotEqual RelationalExpr      { $$= new EqualityExpr( $1, new Operator(@2, "!="), $3);}
	    ;

LogAndExpr: EqualityExpr 				  {$$ = $1;}
	  | LogAndExpr T_And EqualityExpr  		  {$$= new LogicalExpr( $1, new Operator(@2, "&&"), $3);}
	  ;

LogOrExpr : LogAndExpr					  { $$ = $1;}
	  | LogOrExpr T_Or  LogAndExpr			  {$$= new LogicalExpr( $1, new Operator(@2, "||"), $3);}
	  ;

AssignExpr: LogOrExpr    				  { $$ =$1;}
	  | UnaryExpr AssignOper AssignExpr               { $$ = new AssignExpr($1, $2, $3); }
	  ;

AssignOper: '='                                           {$$= new Operator(@1, "=");}
          | T_MulAssign                                   {$$= new Operator(@1, "*=");}
          | T_DivAssign                                   {$$= new Operator(@1, "/=");}
          | T_AddAssign                                   {$$= new Operator(@1, "+=");}
          | T_SubAssign                                   {$$= new Operator(@1, "-=");}
          ;

PostExpr  : PriExpr					  {$$=$1;}
	  | PostExpr T_Inc 				  {$$ = new PostfixExpr($1, new Operator(@2, "++"));}
	  | PostExpr T_Dec 				  {$$ = new PostfixExpr($1, new Operator(@2, "--"));}
          | PostExpr T_Dot T_Fields	                  {$$ = new FieldAccess($1, new Identifier(@3,$3));} 
	  ;
          
UnaryOper : T_Plus                                        {$$= new Operator(@1, "+");}
          | T_Dash                                        {$$= new Operator(@1, "-");}
          ;
          
UnaryExpr : PostExpr					  {$$ = $1;}
	  | T_Inc UnaryExpr				  {$$ = new PostfixExpr($2, new Operator(@1, "++"));}
	  | T_Dec UnaryExpr				  {$$ = new PostfixExpr($2, new Operator(@1, "--"));}
	  | UnaryOper UnaryExpr				  {$$ = new PostfixExpr($2, $1);}
	  ;

SimpleStmt : ExprStmt                                     { $$ =$1;}
           | SwitchStmt                                   { $$=$1;}
           | SelectionStmt                                { $$=$1;}
           | IterationStmt                                { $$ =$1;}
           ;
           

StmtList : Stmt                                           {($$ = new List<Stmt*>)->Append($1);}
         | StmtList Stmt                                  { ($$ = $1)->Append($2);}
         ;
         
SwitchStmtList: StmtList                                  {$$=$1;}
              /* NOTHING */
	      |						  {$$ = new List<Stmt*>;}
              ;

SwitchStmt: T_Switch T_LeftParen Expr T_RightParen T_LeftBrace CaseList DefaultLabel T_RightBrace  {$$ =new SwitchStmt($3, $6,$7);}
          | T_Switch T_LeftParen Expr T_RightParen T_LeftBrace DefaultLabel T_RightBrace  {ReportError::Formatted(&@$,"No Case in Switch Statment Body !!!"); $$=new SwitchStmt($3, new List<Case*>, NULL);}
          | T_Switch T_LeftParen Expr T_RightParen T_LeftBrace error T_RightBrace  {ReportError::Formatted(&@$,"Empty Switch Statment Body !!!"); $$=new SwitchStmt($3, new List<Case*>, NULL);}
          ;

CaseList : CaseLabel      				   {($$ = new List<Case*>)->Append($1);}
	 | CaseList CaseLabel 				   {($$ = $1)->Append($2);}
	 ;

CaseLabel: T_Case Expr ':' SwitchStmtList         {$$ = new Case($2, $4);}
         ;

DefaultLabel  : T_Default ':' SwitchStmtList               {$$= new  Default($3);}
	 |/*TODO*/                                         {$$ = NULL;}
 	 ;

CompoundStmt : T_LeftBrace T_RightBrace                           {$$ = new StmtBlock(new List<VarDecl*>, new List<Stmt*>);}
             | T_LeftBrace StmtList T_RightBrace                  {$$ = new StmtBlock(new List<VarDecl*>, $2);}
             | T_LeftBrace VarDeclList StmtList T_RightBrace      {$$ = new StmtBlock($2, $3);}
             | T_LeftBrace VarDeclList T_RightBrace               {$$ = new StmtBlock($2, new List<Stmt*>);}
             ;
             
Stmt      : CompoundStmt                                   {$$=$1;}
          | SimpleStmt                                     {$$=$1;}
          ;
          
ExprStmt : ';'                                            {$$ = new EmptyExpr();}
         |  Expr ';'                                      {$$ =$1;}
         ;
         
         
SelectionStmt : T_If T_LeftParen Expr T_RightParen Stmt   %prec "No_Else"       {$$= new IfStmt($3, $5,NULL);}
              | T_If T_LeftParen Expr T_RightParen Stmt T_Else Stmt        {$$= new IfStmt($3, $5,$7);}
              ;
                  
IterationStmt: T_While T_LeftParen Condition T_RightParen Stmt                      {$$ = new WhileStmt($3, $5);}
             | T_For T_LeftParen ExprStmt ConditionOpt ';' T_RightParen Stmt        {$$ = new ForStmt($3, $4, new EmptyExpr(), $7);}
             /*| T_For T_LeftParen ';'  ConditionOpt ';' T_RightParen StmtBlock            {$$ = new ForStmt(new EmptyExpr(), $4, new EmptyExpr(), $7);}*/
             | T_For T_LeftParen ExprStmt ConditionOpt ';' Expr  T_RightParen Stmt  {$$ = new ForStmt($3, $4, $6, $8);}
             /*| T_For T_LeftParen ';'  ConditionOpt ';' Expr T_RightParen StmtBlock       {$$ = new ForStmt(new EmptyExpr(), $4, $6, $8);}*/
             ;
             
ConditionOpt: Condition                                   {$$=$1;}
            /* EMPTY */
	    |						  {$$= new EmptyExpr();}
            ;           

Condition : Expr                                          {$$=$1;}
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
