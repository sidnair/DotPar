
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
     TRUE = 258,
     FALSE = 259,
     NIL = 260,
     STRUCT = 261,
     NUMBER = 262,
     BOOLEAN = 263,
     CHAR = 264,
     ARRAY = 265,
     IF = 266,
     ELSE = 267,
     ELIF = 268,
     FOR = 269,
     IN = 270,
     APPEND = 271,
     AND = 272,
     OR = 273,
     NOT = 274,
     GT = 275,
     LT = 276,
     GEQ = 277,
     LEQ = 278,
     EQ = 279,
     NEQ = 280,
     ASSIGN = 281,
     PLUS = 282,
     MINUS = 283,
     MULT = 284,
     DIV = 285,
     MOD = 286
   };
#endif
/* Tokens.  */
#define TRUE 258
#define FALSE 259
#define NIL 260
#define STRUCT 261
#define NUMBER 262
#define BOOLEAN 263
#define CHAR 264
#define ARRAY 265
#define IF 266
#define ELSE 267
#define ELIF 268
#define FOR 269
#define IN 270
#define APPEND 271
#define AND 272
#define OR 273
#define NOT 274
#define GT 275
#define LT 276
#define GEQ 277
#define LEQ 278
#define EQ 279
#define NEQ 280
#define ASSIGN 281
#define PLUS 282
#define MINUS 283
#define MULT 284
#define DIV 285
#define MOD 286




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


