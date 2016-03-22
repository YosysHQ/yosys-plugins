/*
 *  yosys -- Yosys Open SYnthesis Suite
 *
 *  Copyright (C) 2012  Clifford Wolf <clifford@clifford.at>
 *  Copyright (C) 2016  Sebastian Kuzminsky <seb@highlab.com>
 *
 *  Permission to use, copy, modify, and/or distribute this software for any
 *  purpose with or without fee is hereby granted, provided that the above
 *  copyright notice and this permission notice appear in all copies.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 *  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 *  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 *  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 *  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  ---
 *
 *  The VHDL frontend.
 *
 *  This frontend is using the AST frontend library (see frontends/ast/).
 *  Thus this frontend does not generate RTLIL code directly but creates an
 *  AST directly from the VHDL parse tree and then passes this AST to
 *  the AST frontend library.
 *
 */

#ifndef VHDL_FRONTEND_H
#define VHDL_FRONTEND_H

#include "kernel/yosys.h"
#include "frontends/ast/ast.h"
#include <stdio.h>
#include <stdint.h>
#include <list>

YOSYS_NAMESPACE_BEGIN

namespace VHDL_FRONTEND
{
	// this variable is set to a new AST_DESIGN node and then filled with the AST by the bison parser
	extern struct AST::AstNode *current_ast;

	// this function converts a VHDL constant to an AST_CONSTANT node
	AST::AstNode *const2ast(std::string code, char case_type = 0, bool warn_z = false);

	// state of `default_nettype
	extern bool default_nettype_wire;

	// lexer input stream
	extern std::istream *lexin;
}

// the pre-processor
std::string frontend_vhdl_preproc(std::istream &f, std::string filename, const std::map<std::string, std::string> pre_defines_map, const std::list<std::string> include_dirs);

YOSYS_NAMESPACE_END

// the usual bison/flex stuff
extern int frontend_vhdl_yydebug;
extern int frontend_vhdl_yy_flex_debug;
int frontend_vhdl_yylex(void);
void frontend_vhdl_yyerror(char const *fmt, ...);
void frontend_vhdl_yyrestart(FILE *f);
int frontend_vhdl_yyparse(void);
int frontend_vhdl_yylex_destroy(void);
int frontend_vhdl_yyget_lineno(void);
void frontend_vhdl_yyset_lineno (int);

#endif
