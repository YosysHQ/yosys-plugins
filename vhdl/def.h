/*
    vhd2vl v2.3
    VHDL to Verilog RTL translator
    Copyright (C) 2001 Vincenzo Liguori - Ocean Logic Pty Ltd - http://www.ocean-logic.com
    Modifications (C) 2006 Mark Gonzales - PMC Sierra Inc

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

#ifndef __def_h
#define __def_h

#define MAXINDENT 36
#define MAXEDGES  1000  /* maximum number of @(edge) processes supported in a source file */
typedef struct signal {
  char *name; /* Signal name */
  int reg; /* Verilog reg */
  struct signal *next;
} signal;

typedef struct nlist {
  char *name; /* Name */
  struct nlist *next;
} nlist;

typedef struct slist {
  unsigned int type;
  struct slist *slst;
  union {
    struct slist *sl; /* type 0 */
    char *txt;        /* type 1 */
    char **ptxt;      /* type 3!*/
    int val;          /* type 2,4 */
  } data;
} slist;

enum vrangeType {tSCALAR, tSUBSCRIPT, tVRANGE};
typedef struct vrange {
  /*  int hi, lo; */
  enum vrangeType vtype;
  struct slist *nhi, *nlo; /* MAG index is a simple expression */
  slist *size_expr;        /* expression that calculates size (width) of this vrange */
  int    sizeval;          /* precalculated size value */
  struct slist *xhi, *xlo; /* array index range; 0,0 for normal scalars */
} vrange;

typedef struct slval {
  slist *sl;
  int val; /* Signal size */
  vrange *range; /* Signal size */
} slval;

typedef enum {
	EXPDATA_TYPE_NONE = 0,
	EXPDATA_TYPE_AST,
	EXPDATA_TYPE_BITS,
	EXPDATA_TYPE_INT,
	EXPDATA_TYPE_FLOAT,

	EXPDATA_TYPE_TILDE = '~',
	EXPDATA_TYPE_ADD = '+',
	EXPDATA_TYPE_SUBTRACT = '-',
	EXPDATA_TYPE_MULT = '*',
	EXPDATA_TYPE_DIV = '/',
	EXPDATA_TYPE_MOD = '%',
	EXPDATA_TYPE_AND = '&',
	EXPDATA_TYPE_CARET = '^',
	EXPDATA_TYPE_OR = '|',

	EXPDATA_TYPE_C = 'c',
	EXPDATA_TYPE_M = 'm',
	EXPDATA_TYPE_N = 'n',
	EXPDATA_TYPE_OTHERS = 'o',
	EXPDATA_TYPE_P = 'p',
	EXPDATA_TYPE_TERMINAL = 't',
} expdata_type_t;

typedef struct expdata {
	expdata_type_t op;
	bool is_others;
  int value;  /* only set for simple_expr */
  slist *sl;
	std::vector<Yosys::RTLIL::State> bits;
	Yosys::AST::AstNode *node;
} expdata;

typedef struct sglist {
  char *name; /* Signal name */
  char *type; /* Reg or wire */
  const char *dir; /* input, output, inout */
  vrange *range; /* Signal size */
  struct sglist *next;
} sglist;

typedef struct blknamelist {
  char *name; /* Optional name */
  struct blknamelist *next;
} blknamelist;

#endif
