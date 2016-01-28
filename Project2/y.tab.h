
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     T_Void = 258,
     T_Bool = 259,
     T_Int = 260,
     T_Float = 261,
     T_Vec2 = 262,
     T_Vec3 = 263,
     T_Vec4 = 264,
     T_Mat2 = 265,
     T_Mat3 = 266,
     T_Mat4 = 267,
     T_In = 268,
     T_Out = 269,
     T_Uniform = 270,
     T_Layout = 271,
     T_LessEqual = 272,
     T_GreaterEqual = 273,
     T_Equal = 274,
     T_NotEqual = 275,
     T_Dims = 276,
     T_And = 277,
     T_Or = 278,
     T_While = 279,
     T_For = 280,
     T_If = 281,
     T_Else = 282,
     T_Return = 283,
     T_Break = 284,
     T_Continue = 285,
     T_Inc = 286,
     T_Dec = 287,
     T_Switch = 288,
     T_Case = 289,
     T_Default = 290,
     T_FieldSelection = 291,
     T_IncOp = 292,
     T_DecOp = 293,
     T_LeOp = 294,
     T_GeOp = 295,
     T_EqOp = 296,
     T_NeOp = 297,
     T_AndOp = 298,
     T_OrOp = 299,
     T_MulAssign = 300,
     T_DivAssign = 301,
     T_AddAssign = 302,
     T_SubAssign = 303,
     T_LeftParen = 304,
     T_RightParen = 305,
     T_LeftBrace = 306,
     T_RightBrace = 307,
     T_Dot = 308,
     T_Dash = 309,
     T_Plus = 310,
     T_Star = 311,
     T_Slash = 312,
     T_LeftAngle = 313,
     T_RightAngle = 314,
     T_Identifier = 315,
     T_IntConstant = 316,
     T_FloatConstant = 317,
     T_BoolConstant = 318
   };
#endif
/* Tokens.  */
#define T_Void 258
#define T_Bool 259
#define T_Int 260
#define T_Float 261
#define T_Vec2 262
#define T_Vec3 263
#define T_Vec4 264
#define T_Mat2 265
#define T_Mat3 266
#define T_Mat4 267
#define T_In 268
#define T_Out 269
#define T_Uniform 270
#define T_Layout 271
#define T_LessEqual 272
#define T_GreaterEqual 273
#define T_Equal 274
#define T_NotEqual 275
#define T_Dims 276
#define T_And 277
#define T_Or 278
#define T_While 279
#define T_For 280
#define T_If 281
#define T_Else 282
#define T_Return 283
#define T_Break 284
#define T_Continue 285
#define T_Inc 286
#define T_Dec 287
#define T_Switch 288
#define T_Case 289
#define T_Default 290
#define T_FieldSelection 291
#define T_IncOp 292
#define T_DecOp 293
#define T_LeOp 294
#define T_GeOp 295
#define T_EqOp 296
#define T_NeOp 297
#define T_AndOp 298
#define T_OrOp 299
#define T_MulAssign 300
#define T_DivAssign 301
#define T_AddAssign 302
#define T_SubAssign 303
#define T_LeftParen 304
#define T_RightParen 305
#define T_LeftBrace 306
#define T_RightBrace 307
#define T_Dot 308
#define T_Dash 309
#define T_Plus 310
#define T_Star 311
#define T_Slash 312
#define T_LeftAngle 313
#define T_RightAngle 314
#define T_Identifier 315
#define T_IntConstant 316
#define T_FloatConstant 317
#define T_BoolConstant 318




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 41 "parser.y"

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
    Expr *expr;
    Expr *ex;
    PostfixExpr *postexpr;  //cant declare PostExpr
    ArithmeticExpr *mulexpr;
    ArithmeticExpr *addexpr;
    RelationalExpr *relativeexpr;
    EqualityExpr  *equalityExpr;
    LogicalExpr  *logandexpr;
    LogicalExpr  *logorexpr;
    AssignExpr   *assignexpr;
    CompoundExpr *unaryExpr; 
    List<VarDecl*> *param;
    Identifier *identify;




/* Line 1676 of yacc.c  */
#line 207 "y.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYLTYPE yylloc;

