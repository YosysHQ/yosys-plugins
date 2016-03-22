/*
    vhd2vl v2.3
    VHDL to Verilog RTL translator
    Copyright (C) 2001 Vincenzo Liguori - Ocean Logic Pty Ltd - http://www.ocean-logic.com
    Modifications Copyright (C) 2006 Mark Gonzales - PMC Sierra Inc
    Modifications Copyright (C) 2008-2010 Larry Doolittle - LBNL
    Copyright (C) 2016 Sebastian Kuzminsky <seb@highlab.com>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

%option noinput
%option nounput
%option noyywrap
%option prefix="frontend_vhdl_yy"
%option debug
%option yylineno

%x SYNOPSYS_TRANSLATE_OFF

%{

#ifdef __clang__
// bison generates code using the 'register' storage class specifier
#pragma clang diagnostic ignored "-Wdeprecated-register"
#endif

#include <stdio.h>
#include <string.h>

#include "kernel/log.h"
#include "vhdl_frontend.h"
#include "frontends/ast/ast.h"

#include "def.h"
#include "vhdl_parser.tab.h"

USING_YOSYS_NAMESPACE
using namespace AST;
using namespace VHDL_FRONTEND;

YOSYS_NAMESPACE_BEGIN
namespace VHDL_FRONTEND {
        std::vector<std::string> fn_stack;
        std::vector<int> ln_stack;
}
YOSYS_NAMESPACE_END


extern int lineno;

extern int skipRem;

void getstring(unsigned skip);
void getbasedstring(unsigned skip);

#define YY_INPUT(buf,result,max_size) \
    result = readsome(*VHDL_FRONTEND::lexin, buf, max_size)

%}

%%

<INITIAL,SYNOPSYS_TRANSLATE_OFF>"`file_push "[^\n]* {
        fn_stack.push_back(current_filename);
        ln_stack.push_back(frontend_vhdl_yyget_lineno());
        current_filename = yytext+11;
        if (!current_filename.empty() && current_filename.front() == '"')
                current_filename = current_filename.substr(1);
        if (!current_filename.empty() && current_filename.back() == '"')
                current_filename = current_filename.substr(0, current_filename.size()-1);
        frontend_vhdl_yyset_lineno(0);
}

<INITIAL,SYNOPSYS_TRANSLATE_OFF>"`file_pop"[^\n]*\n {
        current_filename = fn_stack.back();
        fn_stack.pop_back();
        frontend_vhdl_yyset_lineno(ln_stack.back());
        ln_stack.pop_back();
}


[ \t] {;}

"--".*\n {
  lineno++;
  if (skipRem == 0) {
    /* sometimes comments should be dropped by lex -
     * e.g. in expressions - this makes the grammar much simpler
     */
    size_t l=strlen(yytext);
    frontend_vhdl_yylval.txt=(char*)malloc(l+1);
    strcpy(frontend_vhdl_yylval.txt, yytext);
    frontend_vhdl_yylval.txt[0]='/';
    frontend_vhdl_yylval.txt[1]='/';
    if(frontend_vhdl_yylval.txt[l-2]=='\r') {
       frontend_vhdl_yylval.txt[l-2]='\n';
       frontend_vhdl_yylval.txt[l-1]='\0';
    }
    return REM;
  }
}
"library ".*\n {lineno++;}
"use ".*\n {lineno++;}

"\x0d\n" |
\n { lineno++;}

"entity" { return ENTITY; }
"is" { return IS; }
"port" { return PORT; }
"generic" { return GENERIC; }
"map" { return MAP; }
"in" { return IN; }
"out" { return OUT; }
"inout" { return INOUT; }
"time" |
"natural" |
"positive" |
"integer" { return INTEGER; }
"boolean" |
"std_logic" |
"std_ulogic" { return BIT; }
"signed" |
"unsigned" |
"std_logic_vector" |
"std_ulogic_vector" { return BITVECT; }
"downto" { return DOWNTO; }
"to" { return TO; }
"type" {return TYPE; }
"end" { return END; }
"for" { return FOR; }
"loop" { return LOOP; }
"generate" { return GENERATE; }
"architecture" { return ARCHITECTURE; }
"component" { return COMPONENT; }
"of" { return OF; }
"array" {return ARRAY; }
"signal" { return SIGNAL; }
"begin" { return BEGN; }
"not" { return NOT; }
"when" { return WHEN; }
"exit" { return EXIT; }
"with" { return WITH; }
"select" { return SELECT; }
"others" { return OTHERS; }
"range" { return RANGE; }
"process" { return PROCESS; }
"variable" { return VARIABLE; }
"constant" { return CONSTANT; }
"null" { return NULLV; }
"open" { return OPEN; }
"if" { return IF; }
"then" { return THEN; }
"elsif" { return ELSIF; }
"else" { return ELSE; }
"case" { return CASE; }
"after" { return AFTER; }
"and" { return AND; }
"or" { return OR; }
"xor" { return XOR; }
"xnor" { return XNOR; }
"mod" { return MOD; }
"event" { return EVENT; }
"rising_edge" { return POSEDGE;}
"falling_edge" { return NEGEDGE;}
"resize" { return CONVFUNC_2;}
"to_unsigned" { return CONVFUNC_2;}
"conv_integer" { return CONVFUNC_1;}
"left" { return LEFT;}
"high" { return LEFT;}

\"[ \!#-~]*\" |
\'[01xz]\' { getstring(1); return STRING;}

#[0-9a-f]*# {
  getbasedstring(1); /* skip leading # */
  return BASED;
}

[a-zA-Z_$][a-zA-Z0-9_$.]* {
  frontend_vhdl_yylval.txt=(char*)malloc(strlen(yytext)+2);
  frontend_vhdl_yylval.txt[0] = '\\';
  frontend_vhdl_yylval.txt[1] = '\0';
  strcat(frontend_vhdl_yylval.txt, yytext);
  return NAME;
}

[0-9]+ {
  sscanf(yytext, "%d", &frontend_vhdl_yylval.n);
  return NATURAL;
}

[0-9]+\.[0-9]+ {
  getstring(0);
  return FLOAT;
}

. { return yytext[0]; }

%%

void getstring(unsigned skip){
/* Gets a string excluding " or ' */
  unsigned i;
  for(i=skip; yytext[i]!='"' && yytext[i]!='\'' && yytext[i]!=0; i++);
  yytext[i]=0;
  frontend_vhdl_yylval.txt=(char*)malloc(i+1);
  strcpy(frontend_vhdl_yylval.txt, yytext+skip);
}

void getbasedstring(unsigned skip){
/* Gets a string excluding # */
  unsigned i;
  for(i=skip; yytext[i]!='#' && yytext[i]!=0; i++);
  yytext[i]=0;
  frontend_vhdl_yylval.txt=(char*)malloc(i+1);
  strcpy(frontend_vhdl_yylval.txt, yytext+skip);
}

void yyerror(char *s){
  fprintf(stderr,"%s at \"%s\" in line %d.\n\n",s,yytext,lineno);
}
