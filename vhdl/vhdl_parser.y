// vim: set noexpandtab tabstop=8 shiftwidth=8 :
/*
    vhd2vl v2.5
    VHDL to Verilog RTL translator
    Copyright (C) 2001 Vincenzo Liguori - Ocean Logic Pty Ltd - http://www.ocean-logic.com
    Modifications (C) 2006 Mark Gonzales - PMC Sierra Inc
    Modifications (C) 2010 Shankar Giri
    Modifications (C) 2002, 2005, 2008-2010, 2015 Larry Doolittle - LBNL

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

    ---

    The VHDL frontend.

    This frontend is using the AST frontend library (see frontends/ast/).
    Thus this frontend does not generate RTLIL code directly but creates an
    AST directly from the VHDL parse tree and then passes this AST to
    the AST frontend library.

    ---

    This is the actual bison parser for VHDL code. It is based on the
    "vhd2vl" parser (by Vincenzo Liguori, Mark Gonzales, Shankar Giri,
    and Larry Doolittle) and the Yosys Verilog Frontend (by Clifford
    Wolf).  The AST ist created directly from the bison reduce functions
    here. Note that this code uses a few global variables to hold the
    state of the AST generator and therefore this parser is not reentrant.

*/

%{

#include <assert.h>
#include <list>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "kernel/log.h"
#include "vhdl_frontend.h"
#include "def.h"

USING_YOSYS_NAMESPACE
using namespace AST;
using namespace VHDL_FRONTEND;

YOSYS_NAMESPACE_BEGIN
namespace VHDL_FRONTEND {
        // std::map<std::string, AstNode*> attr_list;
        std::map<std::string, AstNode*> default_attr_list;
        std::map<std::string, AstNode*> modules;
        // std::map<std::string, AstNode*> *albuf;

        struct AstNode *astbuf1, *astbuf2, *astbuf3;
        // struct AstNode *current_function_or_task;

	// This gets initialized by the caller (the vhdl frontend) to the
	// AST_DESIGN node that we're to parse the VHDL into.
        struct AstNode *current_ast;

        struct AstNode *current_ast_mod;
        // int current_function_or_task_port_id;
        // std::vector<char> case_type_stack;
        // bool do_not_require_port_stubs;
        bool default_nettype_wire;
        std::istream *lexin;

	typedef enum {
		DIR_IN = 0,
		DIR_OUT = 1,
		DIR_INOUT = 2,
		DIR_NONE
	} port_dir_t;

	const char *port_dir_str[] = { "IN", "OUT", "INOUT" };

#define NOT_IMPLEMENTED log_error("NOT IMPLEMENTED: %s:%d (%s:%d)\n", AST::current_filename.c_str(), frontend_vhdl_yyget_lineno(), __FILE__, __LINE__)

}
YOSYS_NAMESPACE_END


int vlog_ver=0;  /* default is -g1995 */

/* You will of course want to tinker with this if you use a debugging
 * malloc(), otherwise all the line numbers will point here.
 */
void *xmalloc(size_t size) {
	void *p = calloc(1, size);
	if (!p) {
		perror("calloc");
		exit(2);
	}
	return p;
}

int skipRem = 0;
int lineno=1;

sglist *io_list=NULL;
sglist *sig_list=NULL;
sglist *type_list=NULL;
blknamelist *blkname_list=NULL;

/* need a stack of clock-edges because all edges are processed before all processes are processed.
 * Edges are processed in source file order, processes are processed in reverse source file order.
 * The original scheme of just one clkedge variable makes all clocked processes have the edge sensitivity
 * of the last clocked process in the file.
 */
int clkedges[MAXEDGES];
int clkptr = 0;
int delay=1;
int dolist=1;
int np=1;
char wire[]="wire";
char reg[]="reg";
int dowith=0;
slist *slwith;

/* Indentation variables */
int indent=0;
slist *indents[MAXINDENT];

struct vrange *new_vrange(enum vrangeType t)
{
  struct vrange *v=(vrange*)xmalloc(sizeof(vrange));
  v->vtype=t;
  v->nlo = NULL;
  v->nhi = NULL;
  v->size_expr = NULL;
  v->sizeval = 0;
  v->xlo = NULL;
  v->xhi = NULL;
  return v;
}

void fslprint(FILE *fp,slist *sl){
  // if(sl){
    // assert(sl != sl->slst);
    // fslprint(fp,sl->slst);
    // switch(sl->type){
    // case 0 :
      // assert(sl != sl->data.sl);
      // fslprint(fp,sl->data.sl);
      // break;
    // case 1 : case 4 :
      // fprintf(fp,"%s",sl->data.txt);
      // break;
    // case 2 :
      // fprintf(fp,"%d",sl->data.val);
      // break;
    // case 3 :
      // fprintf(fp,"%s",*(sl->data.ptxt));
      // break;
    // }
  // }
}

void slprint(slist *sl){
  fslprint(stdout, sl);
}

slist *copysl(slist *sl){
  if(sl){
    slist *newsl;
    newsl = (slist*)xmalloc(sizeof(slist));
    *newsl = *sl;
    if (sl->slst != NULL) {
      assert(sl != sl->slst);
      newsl->slst = copysl(sl->slst);
    }
    switch(sl->type){
    case 0 :
      if (sl->data.sl != NULL) {
        assert(sl != sl->data.sl);
        newsl->data.sl = copysl(sl->data.sl);
      }
      break;
    case 1 : case 4 :
      newsl->data.txt = (char*)xmalloc(strlen(sl->data.txt) + 1);
      strcpy(newsl->data.txt, sl->data.txt);
      break;
    }
    return newsl;
  }
  return NULL;
}

slist *addtxt(slist *sl, const char *s){
  slist *p;

  if(s == NULL)
    return sl;
  p = (slist*)xmalloc(sizeof *p);
  p->type = 1;
  p->slst = sl;
  p->data.txt = (char*)xmalloc(strlen(s) + 1);
  strcpy(p->data.txt, s);

  return p;
}

slist *addothers(slist *sl, char *s){
  slist *p;

  if(s == NULL)
    return sl;
  p = (slist*)xmalloc(sizeof *p);
  p->type = 4;
  p->slst = sl;
  p->data.txt = (char*)xmalloc(strlen(s) + 1);
  strcpy(p->data.txt, s);

  return p;
}

slist *addptxt(slist *sl, char **s){
  slist *p;

  if(s == NULL)
    return sl;

  p = (slist*)xmalloc(sizeof *p);
  p->type = 3;
  p->slst = sl;
  p->data.ptxt = s;

  return p;
}

slist *addval(slist *sl, int val){
  slist *p;

  p = (slist*)xmalloc(sizeof(slist));
  p->type = 2;
  p->slst = sl;
  p->data.val = val;

  return p;
}

slist *addsl(slist *sl, slist *sl2){
  slist *p;
  if(sl2 == NULL) return sl;
  p = (slist*)xmalloc(sizeof(slist));
  p->type = 0;
  p->slst = sl;
  p->data.sl = sl2;
  return p;
}

slist *addvec(slist *sl, char *s){
  sl=addval(sl,strlen(s));
  sl=addtxt(sl,"'b ");
  sl=addtxt(sl,s);
  return sl;
}

slist *addvec_base(slist *sl, char *b, char *s){
  const char *base_str="'b ";
  int base_mult=1;
  if (strcasecmp(b,"X") == 0) {
     base_str="'h "; base_mult=4;
  } else if (strcasecmp(b,"O") == 0) {
     base_str="'o "; base_mult=3;
  } else {
     fprintf(stderr,"Warning on line %d: NAME STRING rule matched but "
       "NAME='%s' is not X or O.\n",lineno, b);
  }
  sl=addval(sl,strlen(s)*base_mult);
  sl=addtxt(sl,base_str);
  sl=addtxt(sl,s);
  return sl;
}

slist *addind(slist *sl){
  if(sl)
    sl=addsl(indents[indent],sl);
  return sl;
}

slist *addpar(slist *sl, vrange *v){
  if(v->nlo != NULL) {   /* indexes are simple expressions */
    sl=addtxt(sl," [");
    if(v->nhi != NULL){
      sl=addsl(sl,v->nhi);
      sl=addtxt(sl,":");
    }
    sl=addsl(sl,v->nlo);
    sl=addtxt(sl,"] ");
  } else {
    sl=addtxt(sl," ");
  }
  return sl;
}

slist *addpar_snug(slist *sl, vrange *v){
  if(v->nlo != NULL) {   /* indexes are simple expressions */
    sl=addtxt(sl,"[");
    if(v->nhi != NULL){
      sl=addsl(sl,v->nhi);
      sl=addtxt(sl,":");
    }
    sl=addsl(sl,v->nlo);
    sl=addtxt(sl,"]");
  }
  return sl;
}

/* This function handles array of vectors in signal lists */
slist *addpar_snug2(slist *sl, vrange *v, vrange *v1){
  if(v->nlo != NULL) {   /* indexes are simple expressions */
    sl=addtxt(sl,"[");
    if(v->nhi != NULL){
      sl=addsl(sl,v->nhi);
      sl=addtxt(sl,":");
    }
    sl=addsl(sl,v->nlo);
    sl=addtxt(sl,"]");
  }
  if(v1->nlo != NULL) {   /* indexes are simple expressions */
    sl=addtxt(sl,"[");
    if(v1->nhi != NULL){
      sl=addsl(sl,v1->nhi);
      sl=addtxt(sl,":");
    }
    sl=addsl(sl,v1->nlo);
    sl=addtxt(sl,"]");
  }
  return sl;
}

slist *addpost(slist *sl, vrange *v){
  if(v->xlo != NULL) {
    sl=addtxt(sl,"[");
    if(v->xhi != NULL){
      sl=addsl(sl,v->xhi);
      sl=addtxt(sl,":");
    }
    sl=addsl(sl,v->xlo);
    sl=addtxt(sl,"]");
  }
  return sl;
}

slist *addwrap(const char *l,slist *sl,const char *r){
slist *s;
  s=addtxt(NULL,l);
  s=addsl(s,sl);
  return addtxt(s,r);
}

expdata *addnest(struct expdata *inner)
{
  expdata *e;
  e=(expdata*)xmalloc(sizeof(expdata));
  if (inner->op == EXPDATA_TYPE_C) {
    e->sl=addwrap("{",inner->sl,"}");
  } else {
    e->sl=addwrap("(",inner->sl,")");
  }
  return e;
}

slist *addrem(slist *sl, slist *rem)
{
  if (rem) {
    sl=addtxt(sl, "  ");
    sl=addsl(sl, rem);
  } else {
    sl=addtxt(sl, "\n");
  }
  return sl;
}

sglist *lookup(sglist *sg,char *s){
  for(;;){
    if(sg == NULL || strcmp(sg->name,s)==0)
      return sg;
    sg=sg->next;
  }
}

char *sbottom(slist *sl){
  while(sl->slst != NULL)
    sl=sl->slst;
  return sl->data.txt;
}

const char *inout_string(int type)
{
  const char *name=NULL;
  switch(type) {
    case 0: name="input"  ; break;
    case 1: name="output" ; break;
    case 2: name="inout"  ; break;
    default: break;
  }
  return name;
}

int prec(int op){
  switch(op){
  case EXPDATA_TYPE_OTHERS: /* others */
    return 9;
    break;
  case EXPDATA_TYPE_TERMINAL:
  case EXPDATA_TYPE_N:
    return 8;
    break;
  case EXPDATA_TYPE_TILDE:
    return 7;
    break;
  case EXPDATA_TYPE_P:
  case EXPDATA_TYPE_M:
    return 6;
    break;
  case EXPDATA_TYPE_MULT:
  case EXPDATA_TYPE_DIV:
  case EXPDATA_TYPE_MOD:
    return 5;
    break;
  case EXPDATA_TYPE_ADD:
  case EXPDATA_TYPE_SUBTRACT:
    return 4;
    break;
  case EXPDATA_TYPE_AND:
    return 3;
    break;
  case EXPDATA_TYPE_CARET:
    return 2;
    break;
  case EXPDATA_TYPE_OR:
    return 1;
    break;
   default:
    return 0;
    break;
  }
}

expdata *addexpr(expdata *expr1,int op,const char* opstr,expdata *expr2){
slist *sl1,*sl2;
  if(expr1 == NULL)
    sl1=NULL;
  else if(expr1->op == EXPDATA_TYPE_C)
    sl1=addwrap("{",expr1->sl,"}");
  else if(prec(expr1->op) < prec(op))
    sl1=addwrap("(",expr1->sl,")");
  else
    sl1=expr1->sl;

  if(expr2->op == EXPDATA_TYPE_C)
    sl2=addwrap("{",expr2->sl,"}");
  else if(prec(expr2->op) < prec(op))
    sl2=addwrap("(",expr2->sl,")");
  else
    sl2=expr2->sl;

  if(expr1 == NULL)
    expr1=expr2;
  else
    free(expr2);

  expr1->op = (expdata_type_t)op;
  sl1=addtxt(sl1,opstr);
  sl1=addsl(sl1,sl2);
  expr1->sl=sl1;
  return expr1;
}

void slTxtReplace(slist *sl, const char *match, const char *replace){
  if(sl){
    slTxtReplace(sl->slst, match, replace);
    switch(sl->type) {
    case 0 :
      slTxtReplace(sl->data.sl, match, replace);
      break;
    case 1 :
      if (strcmp(sl->data.txt, match) == 0) {
        sl->data.txt = strdup(replace);
      }
      break;
    case 3 :
      if (strcmp(*(sl->data.ptxt), match) == 0) {
        *(sl->data.ptxt) = strdup(replace);
      }
      break;
    }
  }
}


/* XXX todo: runtime engage clkedge debug */
void push_clkedge(int val, const char *comment)
{
  if (0) fprintf(stderr,"clock event push: line=%d clkptr=%d, value=%d (%s)\n",lineno,clkptr,val,comment);
  clkedges[clkptr++]=val;
  assert(clkptr < MAXEDGES);
}

int pull_clkedge(slist *sensitivities)
{
  int clkedge;
  assert(clkptr>0);
  clkedge = clkedges[--clkptr];
  if (0) {
     fprintf(stderr,"clock event pull: value=%d, sensistivity list = ", clkedge);
     fslprint(stderr,sensitivities);
     fprintf(stderr,"\n");
  }
  return clkedge;
}

/* XXX maybe it's a bug that some uses don't munge clocks? */
slist *add_always(slist *sl, slist *sensitivities, slist *decls, int munge)
{
           int clkedge;
           sl=addsl(sl,indents[indent]);
           sl=addtxt(sl,"always @(");
           if (munge) {
             clkedge = pull_clkedge(sensitivities);
             if(clkedge) {
               sl=addtxt(sl,"posedge ");
               /* traverse $4->sl replacing " or " with " or posedge " if there is a clockedge */
               slTxtReplace(sensitivities," or ", " or posedge ");
             } else {
               sl=addtxt(sl,"negedge ");
               slTxtReplace(sensitivities," or ", " or negedge ");
             }
           }
           sl=addsl(sl,sensitivities);
           sl=addtxt(sl,") begin");
           if(decls){
             sl=addtxt(sl," : P");
             sl=addval(sl,np++);
             sl=addtxt(sl,"\n");
             sl=addsl(sl,decls);
           }
           sl=addtxt(sl,"\n");
           return sl;
}

void fixothers(slist *size_expr, slist *sl) {
  if(sl) {
    fixothers(size_expr, sl->slst);
    switch(sl->type) {
    case 0 :
      fixothers(size_expr,sl->data.sl);
      break;
    case 4 : {
      /* found an (OTHERS => 'x') clause - change to type 0, and insert the
       * size_expr for the corresponding signal */
      slist *p;
      slist *size_copy = (slist*)xmalloc(sizeof(slist));
      size_copy = copysl(size_expr);
      if (0) {
        fprintf(stderr,"fixothers type 4 size_expr ");
        fslprint(stderr,size_expr);
        fprintf(stderr,"\n");
      }
      p = addtxt(NULL, "1'b");
      p = addtxt(p, sl->data.txt);
      p = addwrap("{",p,"}");
      p = addsl(size_copy, p);
      p = addwrap("{",p,"}");
      sl->type=0;
      sl->slst=p;
      sl->data.sl=NULL;
      break;
    } /* case 4 */
    } /* switch */
  }
}

void findothers(slval *sgin,slist *sl){
  sglist *sg = NULL;
  int size = -1;
  int useExpr=0;
  if (0) {
    fprintf(stderr,"findothers lhs ");
    fslprint(stderr,sgin->sl);
    fprintf(stderr,", sgin->val %d\n", sgin->val);
  }
  if(sgin->val>0) {
    size=sgin->val;
  } else if (sgin->range != NULL) {
    if (sgin->range->vtype != tVRANGE) {
      size=1;
    } else if (sgin->range->sizeval > 0) {
      size=sgin->range->sizeval;
    } else if (sgin->range->size_expr != NULL) {
      useExpr = 1;
      fixothers(sgin->range->size_expr, sl);
    }
  } else {
    if((sg=lookup(io_list,sgin->sl->data.txt))==NULL) {
      sg=lookup(sig_list,sgin->sl->data.txt);
    }
    if(sg) {
      if(sg->range->vtype != tVRANGE) {
        size=1;
      } else {
        if (sg->range->sizeval > 0) {
          size = sg->range->sizeval;
        } else {
          assert (sg->range->size_expr != NULL);
          useExpr = 1;
          fixothers(sg->range->size_expr, sl);
        }
      }
    } else {
      /* lookup failed, there was no vrange or size value in sgin - so just punt, and assign size=1 */
      size=1;
    }  /* if(sg) */
  }
  if (!useExpr) {
    slist *p;
    assert(size>0);
    /* use size */
    p = addval(NULL,size);
    fixothers(p,sl);
  }
}

/* code to find bit number of the msb of n */
int find_msb(int n)
{
    int k=0;
    if(n&0xff00){
        k|=8;
        n&=0xff00;
    }
    if(n&0xf0f0){
        k|=4;
        n&=0xf0f0;
    }
    if(n&0xcccc){
        k|=2;
        n&=0xcccc;
    }
    if(n&0xaaaa){
        k|=1;
        n&=0xaaaa;
    }
    return k;
}

static char time_unit[2]= { '\0', '\0' }, new_unit[2]= { '\0', '\0' };
static void set_timescale(const char *s)
{
    if (0) fprintf(stderr,"set_timescale (%s)\n", s);
    new_unit[0] = time_unit[0];
         if (strcasecmp(s,"ms") == 0) { new_unit[0] = 'm'; }
    else if (strcasecmp(s,"us") == 0) { new_unit[0] = 'u'; }
    else if (strcasecmp(s,"ns") == 0) { new_unit[0] = 'n'; }
    else if (strcasecmp(s,"ps") == 0) { new_unit[0] = 'p'; }
    else {
        fprintf(stderr,"Warning on line %d: AFTER NATURAL NAME pattern"
               " matched, but NAME='%s' should be a time unit.\n",lineno,s);
    }
    if (new_unit[0] != time_unit[0]) {
        if (time_unit[0] != 0) {
            fprintf(stderr,"Warning on line %d: inconsistent time unit (%s) ignored\n", lineno, s);
        } else {
            time_unit[0] = new_unit[0];
        }
    }
}

slist *output_timescale(slist *sl)
{
    if (time_unit[0] != 0) {
        sl = addtxt(sl, "`timescale 1 ");
        sl = addtxt(sl, time_unit);
        sl = addtxt(sl, "s / 1 ");
        sl = addtxt(sl, time_unit);
        sl = addtxt(sl, "s\n");
    } else {
        sl = addtxt(sl, "// no timescale needed\n");
    }
    return sl;
}

slist *setup_port(sglist *s_list, int dir, vrange *type) {
  slist *sl;
  sglist *p;
  if (vlog_ver == 1) {
    sl=addtxt(NULL,NULL);
  }
  else {
    sl=addtxt(NULL,inout_string(dir));
    sl=addpar(sl,type);
  }
  p=s_list;
  for(;;){
    p->type=wire;
    if (vlog_ver == 1) p->dir=inout_string(dir);
    p->range=type;
    if (vlog_ver == 0) sl=addtxt(sl, p->name);
    if(p->next==NULL)
      break;
    p=p->next;
    if (vlog_ver == 0) sl=addtxt(sl,", ");
  }
  if (vlog_ver == 0) sl=addtxt(sl,";\n");
  p->next=io_list;
  io_list=s_list;
  return sl;
}

slist *emit_io_list(slist *sl)
{
              // sglist *p;
              // sl=addtxt(sl,"(\n");
              // p=io_list;
              // for(;;){
                // if (vlog_ver == 1) {
                  // sl=addtxt(sl,p->dir);
                  // sl=addtxt(sl," ");
                  // sl=addptxt(sl,&(p->type));
                  // sl=addpar(sl,p->range);
                // }
                // sl=addtxt(sl,p->name);
                // p=p->next;
                // if(p)
                  // sl=addtxt(sl,",\n");
                // else{
                  // sl=addtxt(sl,"\n");
                  // break;
                // }
              // }
              // sl=addtxt(sl,");\n\n");
              return sl;
}


AstNode *make_wire(std::string name, port_dir_t dir, struct vrange *type) {
	struct AstNode *wire;
	wire = new AstNode(AST_WIRE);
	wire->str = name;
	switch (dir) {
		case DIR_IN:
			wire->is_input = true;
			break;
		case DIR_OUT:
			wire->is_output = true;
			break;
		case DIR_INOUT:
			wire->is_input = true;
			wire->is_output = true;
			break;
		case DIR_NONE:
			break;
		default:
			delete wire;
			frontend_vhdl_yyerror("unhandled direction %d\n", (int)dir);
	}

	switch (type->vtype) {
		case tSCALAR:
			break;

		case tVRANGE: {
			log_assert(type->nhi->type == 2);
			log_assert(type->nlo->type == 2);
			AstNode *high = AstNode::mkconst_int(type->nhi->data.val, false, 32);
			AstNode *low = AstNode::mkconst_int(type->nlo->data.val, false, 32);
			AstNode *range = new AstNode(AST_RANGE, high, low);
			wire->children.push_back(range);
			break;
		}

		// case tSUBSCRIPT:
		default:
			delete wire;
			frontend_vhdl_yyerror("unhandled type\n");
			// not reached
	}

	return wire;
}


void add_wire(std::vector<AstNode*> *v, AstNode *wire) {
	if (v == NULL) {
		frontend_vhdl_yyerror("adding wire to a NULL vector\n");
		return;
	}
	if (wire == NULL) {
		frontend_vhdl_yyerror("adding a NULL wire\n");
		return;
	}
	v->push_back(wire);
}


void add_wire(std::vector<AstNode*> *v, std::string name, port_dir_t dir, struct vrange *type) {
	add_wire(v, make_wire(name, dir, type));
}


void add_portlist_wires(AstNode *module, std::vector<AstNode*> *portlist) {
	int port_id = 1;
	for (auto &wire: *portlist) {
		wire->port_id = port_id;
		port_id++;
		module->children.push_back(wire);
	}
}


AstNode *expr_to_ast(expdata *expr) {
	struct AstNode *node = NULL;

	if (expr->op == EXPDATA_TYPE_AST) {
		node = expr->node;

	} else if (expr->op == EXPDATA_TYPE_BITS) {
		// bit vector
		node = Yosys::AST::AstNode::mkconst_bits(expr->bits, false);
		if (node == NULL) {
			frontend_vhdl_yyerror("failed to make AST node from bit string\n");
		}

	} else {
		frontend_vhdl_yyerror("unhandled expression\n");
	}

	return node;
}


void print_signal_list(std::vector<std::string> *signal_list) {
	printf("signal list %p:\n", signal_list);
	for (auto &name: *signal_list) {
		printf("    port %s\n", name.c_str());
	}
}


void print_type(struct vrange *vrange) {
	printf("vrange %p:", vrange);
	switch (vrange->vtype) {
		case tSCALAR:
			printf(" tSCALAR");
			break;
		case tSUBSCRIPT:
			printf(" tSCUBSCRIPT");
			break;
		case tVRANGE:
			printf(" tVRANGE");
			break;
		default:
			printf(" (unknown type)");
			break;
	}
	printf("\n");
}

void string_to_bits(std::vector<RTLIL::State> &bits, std::string s) {
	if (s.length() != 1) {
		frontend_vhdl_yyerror("invalid string constant `%s'.", s.c_str());
		return;
	}

	bits.clear();

	if (s[0] == '0') {
		bits.push_back(RTLIL::S0);
	} else if (s[0] == '1') {
		bits.push_back(RTLIL::S1);
	} else {
		frontend_vhdl_yyerror("invalid string constant `%s'.", s.c_str());
		return;
	}
}

void expr_set_bits(expdata *e, std::string s) {
	printf("setting bits of expdata %p to %s\n", e, s.c_str());

	if (s.length() != 1) {
		frontend_vhdl_yyerror("invalid string constant `%s'.", s.c_str());
		return;
	}

	log_assert(e != NULL);

	e->op = EXPDATA_TYPE_BITS;
	e->bits.clear();

	if (s[0] == '0') {
		e->bits.push_back(RTLIL::S0);
	} else if (s[0] == '1') {
		e->bits.push_back(RTLIL::S1);
	} else {
		frontend_vhdl_yyerror("invalid string constant `%s'.", s.c_str());
		return;
	}
}

%}

%name-prefix "frontend_vhdl_yy"

%union {
	std::string *string;
	struct Yosys::AST::AstNode *ast;
	std::map<std::string, Yosys::AST::AstNode*> *al;
	std::vector<std::string> *vector_string;
	std::vector<Yosys::AST::AstNode*> *vector_ast;
	bool boolean;
  char * txt; /* String */
  int n;      /* Value */
  vrange *v;  /* Signal range */
  sglist *sg; /* Signal list */
  slist *sl;  /* String list */
  expdata *e; /* Expression structure */
  slval *ss;  /* Signal structure */
}

%token <txt> REM ENTITY IS PORT GENERIC IN OUT INOUT MAP
%token <txt> INTEGER BIT BITVECT DOWNTO TO TYPE END
%token <txt> ARCHITECTURE COMPONENT OF ARRAY
%token <txt> SIGNAL BEGN NOT WHEN WITH EXIT
%token <txt> SELECT OTHERS PROCESS VARIABLE CONSTANT
%token <txt> IF THEN ELSIF ELSE CASE
%token <txt> FOR LOOP GENERATE
%token <txt> AFTER AND OR XOR MOD
%token <txt> LASTVALUE EVENT POSEDGE NEGEDGE
%token <txt> STRING NAME RANGE NULLV OPEN
%token <txt> CONVFUNC_1 CONVFUNC_2 BASED FLOAT LEFT
%token <n> NATURAL

%type <ast> trad
%type <sl> rem  remlist
%type <ast> entity_name
%type <ast> entity
%type <sl> genlist
%type <ast> architecture
%type <vector_ast> portlist
%type <vector_ast> a_decl
%type <sl> p_decl oname
%type <vector_ast> a_body
%type <sl> map_list map_item mvalue
%type <e> sigvalue
%type <sl> generic_map_list generic_map_item
%type <sl> optname gen_optname
%type <ast> exprc
%type <ast> conf
%type <ast> p_body
%type <vector_ast> sign_list
%type <ast> edge
%type <sl> wlist wvalue cases
%type <vector_ast> elsepart  // one or more AST_COND, ending with an AST_DEFAULT one
%type <sl> with_item with_list
%type <vector_string> s_list
%type <n> dir delay
%type <v> type vec_range
%type <n> updown
%type <e> expr
%type <e> simple_expr
%type <ast> signal
%type <txt> opt_is opt_generic opt_entity opt_architecture opt_begin
%type <txt> generate endgenerate

%right '='
/* Logic operators: */
%left ORL
%left ANDL
/* Binary operators: */
%left OR
%left XOR
%left XNOR
%left AND
%left MOD
/* Comparison: */
%left '<'  '>'  BIGEQ  LESSEQ  NOTEQ  EQUAL
%left  '+'  '-'  '&'
%left  '*'  '/'
%right UMINUS  UPLUS  NOTL  NOT
%error-verbose

/* rule for "...ELSE IF edge THEN..." causes 1 shift/reduce conflict */
/* rule for opt_begin causes 1 shift/reduce conflict */
%expect 2

%debug

/* glr-parser is needed because processes can start with if statements, but
 * not have edges in them - more than one level of look-ahead is needed in that case
 * %glr-parser
 * unfortunately using glr-parser causes slists to become self-referential, causing core dumps!
 */
%%

input: trad {
	// current_ast is the callers AST_DESIGN node
	// $trad is the AST_MODULE we just parsed
	current_ast->children.push_back($trad);
};

/* Input file must contain entity declaration followed by architecture */
trad  : rem entity rem architecture rem {
		printf("trad: entity(%s) architecture(%s)\n", $entity->str.c_str(), $architecture->str.c_str());
		log_assert($entity->str.compare($architecture->str) == 0);
		$trad = $entity;
		for (auto &i: $architecture->children) {
			$trad->children.push_back(i);
		}

/* some people put entity declarations and architectures in separate files -
 * translate each piece - note that this will not make a legal Verilog file
 * - let them take care of that manually
 */
	} | rem entity rem  {
		printf("trad: entity\n");
		$trad = $entity;

	} | rem architecture rem {
		printf("trad: architecture\n");
		$trad = $architecture;
	};

/* Comments */
rem      : /* Empty */ {$$=NULL; }
         | remlist {$$=$1; }
         ;

remlist  : REM {$$=addtxt(indents[indent],$1);}
         | REM remlist {
           slist *sl;
           sl=addtxt(indents[indent],$1);
           $$=addsl(sl,$2);}
         ;

opt_is   : /* Empty */ {$$=NULL;} | IS ;

opt_entity   : /* Empty */ {$$=NULL;} | ENTITY ;

opt_architecture   : /* Empty */ {$$=NULL;} | ARCHITECTURE ;

opt_begin    : /* Empty */ {$$=NULL;} | BEGN;

generate       : GENERATE opt_begin;

endgenerate    : END GENERATE;

/* tell the lexer to discard or keep comments ('-- ') - this makes the grammar much easier */
norem : /*Empty*/ {skipRem = 1;}
yesrem : /*Empty*/ {skipRem = 0;}

entity_name: ENTITY NAME {
	printf("entity, name='%s'\n", $2);
	AstNode *module = new AstNode(AST_MODULE);
	module->str = $2;
	modules[$NAME] = module;
	$entity_name = module;
};

/* Entity */
entity:
/*      1           2  3   4    5   6   7        8   9   10  11  12 */
	entity_name IS rem PORT '(' rem portlist ')' ';' rem END opt_entity oname ';' {
		printf("entity1 %s\n", $entity_name->str.c_str());
		$entity = $entity_name;
		add_portlist_wires($entity, $portlist);

            // slist *sl;
            // sglist *p;
              // sl=addtxt(NULL,"\nmodule ");
              // sl=addtxt(sl,current_ast_mod->str.c_str()); /* NAME */
              // /* Add the signal list */
              // sl=emit_io_list(sl);
              // sl=addsl(sl,$6); /* rem */
              // sl=addsl(sl,$7); /* portlist */
              // sl=addtxt(sl,"\n");
              // p=io_list;
              // if (vlog_ver == 0) {
                // do{
                // sl=addptxt(sl,&(p->type));
                // /*sl=addtxt(sl,p->type);*/
                // sl=addpar(sl,p->range);
                // sl=addtxt(sl,p->name);
                // /* sl=addpost(sl,p->range); */
                // sl=addtxt(sl,";\n");
                // p=p->next;
                // } while(p!=NULL);
              // }
              // sl=addtxt(sl,"\n");
              // sl=addsl(sl,$10); /* rem2 */
              // $$=addtxt(sl,"\n");
		current_ast_mod = NULL;
            }
 /*         1           2  3       4       5   6   7        8   9   10  11   12      13  14  15       16  17  18  19  20         21    22 */
          | entity_name IS GENERIC yeslist '(' rem genlist  ')' ';' rem PORT yeslist '(' rem portlist ')' ';' rem END opt_entity oname ';' {
		printf("done with entity2 %s\n", $entity_name->str.c_str());
		$entity = $entity_name;
		add_portlist_wires($entity, $portlist);

            // slist *sl;
            // sglist *p;
              // if (0) fprintf(stderr,"matched ENTITY GENERIC\n");
              // sl=addtxt(NULL,"\nmodule ");
              // sl=addtxt(sl,current_ast_mod->str.c_str()); /* NAME */
              // sl=emit_io_list(sl);
              // sl=addsl(sl,$6);  /* rem */
              // sl=addsl(sl,$7);  /* genlist */
              // sl=addsl(sl,$10); /* rem */
              // sl=addsl(sl,$14); /* rem */
              // sl=addsl(sl,$15); /* portlist */
              // sl=addtxt(sl,"\n");
              // p=io_list;
              // if (vlog_ver == 0) {
                // do{
                // sl=addptxt(sl,&(p->type));
                // /*sl=addtxt(sl,p->type);*/
                // sl=addpar(sl,p->range);
                // sl=addtxt(sl,p->name);
                // sl=addtxt(sl,";\n");
                // p=p->next;
                // } while(p!=NULL);
              // }
              // sl=addtxt(sl,"\n");
              // sl=addsl(sl,$18); /* rem2 */
              // $$=addtxt(sl,"\n");
		current_ast_mod = NULL;
	};


          /* 1     2  3     4   5  6    7 */
genlist  : s_list ':' type ':' '=' expr rem {
		NOT_IMPLEMENTED;
          // if(dolist){
            // slist *sl;
            // sglist *p;
            // sl=addtxt(NULL,"parameter");
            // sl=addpar(sl,$3); /* type */
            // p=$1;
            // for(;;){
              // sl=addtxt(sl,p->name);
              // sl=addtxt(sl,"=");
              // sl=addsl(sl, $6->sl); /* expr */
              // sl=addtxt(sl,";\n");
              // p=p->next;
              // if(p==NULL) break;
            // }
            // $$=addsl(sl,$7); /* rem */
          // } else {
            // $$=NULL;
          // }

          /* 1     2  3     4   5  6     7  8    9 */
	} | s_list ':' type ':' '=' expr ';' rem genlist {
		NOT_IMPLEMENTED;
          // if(dolist){
            // slist *sl;
            // sglist *p;
            // sl=addtxt(NULL,"parameter");
            // sl=addpar(sl,$3); /* type */
            // p=$1;
            // for(;;){
              // sl=addtxt(sl,p->name);
              // sl=addtxt(sl,"=");
              // sl=addsl(sl, $6->sl); /* expr */
              // sl=addtxt(sl,";\n");
              // p=p->next;
              // if(p==NULL) break;
            // }
            // $$=addsl(sl,$8); /* rem */
            // $$=addsl(sl,$9); /* genlist */
          // } else {
            // $$=NULL;
          // }

          /* 1     2  3     4   5  6 */
	} | s_list ':' type ';' rem genlist {
		NOT_IMPLEMENTED;
          // if(dolist){
            // slist *sl;
            // sglist *p;
            // sl=addtxt(NULL,"parameter");
            // sl=addpar(sl,$3); /* type */
            // p=$1;
            // for(;;){
              // sl=addtxt(sl,p->name);
              // sl=addtxt(sl,";\n");
              // p=p->next;
              // if(p==NULL) break;
            // }
            // $$=addsl(sl,$5); /* rem */
            // $$=addsl(sl,$6); /* genlist */
          // } else {
            // $$=NULL;
          // }

          /* 1     2  3    4   */
	} | s_list ':' type rem  {
		NOT_IMPLEMENTED;
          // if(dolist){
            // slist *sl;
            // sglist *p;
            // sl=addtxt(NULL,"parameter");
            // sl=addpar(sl,$3); /* type */
            // p=$1;
            // for(;;){
              // sl=addtxt(sl,p->name);
              // sl=addtxt(sl,";\n");
              // p=p->next;
              // if(p==NULL) break;
            // }
            // $$=addsl(sl,$4); /* rem */
          // } else {
            // $$=NULL;
          // }
	};


          /* 1      2   3   4    5 */
portlist[portlist_new]  : s_list ':' dir type rem {
		$portlist_new = new std::vector<AstNode*>;
		for (auto &name: *$s_list) {
			$portlist_new->insert($portlist_new->begin(), make_wire(name, (port_dir_t)$dir, $type));
		}

            // slist *sl;

              // if(dolist){
                // io_list=NULL;
                // sl=setup_port($1,$3,$4);  /* modifies io_list global */
                // $$=addsl(sl,$5);
              // } else{
                // free($5);
                // free($4);
              // }

          /* 1      2   3   4    5   6   7     */
	} | s_list ':' dir type ';' rem portlist[portlist_orig] {
		$portlist_new = $portlist_orig;
		for (auto &name: *$s_list) {
			$portlist_new->insert($portlist_new->begin(), make_wire(name, (port_dir_t)$dir, $type));
		}
            // slist *sl;

              // if(dolist){
                // sl=setup_port($1,$3,$4);  /* modifies io_list global */
                // sl=addsl(sl,$6);
                // $$=addsl(sl,$7);
              // } else{
                // free($6);
                // free($4);
              // }
          /* 1      2   3   4    5   6   7    8 */
	} | s_list ':' dir type ':' '=' expr rem {
		$portlist_new = new std::vector<AstNode*>;
		NOT_IMPLEMENTED;
            // slist *sl;
              // fprintf(stderr,"Warning on line %d: "
                // "port default initialization ignored\n",lineno);
              // if(dolist){
                // io_list=NULL;
                // sl=setup_port($1,$3,$4);  /* modifies io_list global */
                // $$=addsl(sl,$8);
              // } else{
                // free($8);
                // free($4);
              // }

          /* 1      2   3   4    5   6   7    8   9   10     */
	} | s_list ':' dir type ':' '=' expr ';' rem portlist[portlist_orig] {
		$portlist_new = $portlist_orig;
		NOT_IMPLEMENTED;
            // slist *sl;
              // fprintf(stderr,"Warning on line %d: "
                // "port default initialization ignored\n",lineno);
              // if(dolist){
                // sl=setup_port($1,$3,$4);  /* modifies io_list global */
                // sl=addsl(sl,$9);
                // $$=addsl(sl,$10);
              // } else{
                // free($9);
                // free($4);
              // }
	};


dir         : IN    { $$ = DIR_IN; }
            | OUT   { $$ = DIR_OUT; }
            | INOUT { $$ = DIR_INOUT; }
            ;


type        : BIT {
		$$=new_vrange(tSCALAR);

	} | INTEGER RANGE expr TO expr {
		NOT_IMPLEMENTED;
                // fprintf(stderr,"Warning on line %d: integer range ignored\n",lineno);
                // $$=new_vrange(tSCALAR);
                // $$->nlo = addtxt(NULL,"0");
                // $$->nhi = addtxt(NULL,"31");

	} | INTEGER {
		NOT_IMPLEMENTED;
                // $$=new_vrange(tSCALAR);
                // $$->nlo = addtxt(NULL,"0");
                // $$->nhi = addtxt(NULL,"31");

	} | BITVECT '(' vec_range ')' {
		$$ = $3;

	} | NAME {
		NOT_IMPLEMENTED;
              // sglist *sg;
                // sg=lookup(type_list,$1);
                // if(sg)
                  // $$=sg->range;
                // else{
                  // fprintf(stderr,"Undefined type '%s' on line %d\n",$1,lineno);
                  // YYABORT;
                // }
	};


/* using expr instead of simple_expr here makes the grammar ambiguous (why?) */
vec_range : simple_expr updown simple_expr {
              $$=new_vrange(tVRANGE);
              $$->nhi=$1->sl;
              $$->nlo=$3->sl;
              $$->sizeval = -1; /* undefined size */
              /* calculate the width of this vrange */
              if ($1->op == EXPDATA_TYPE_N && $3->op == EXPDATA_TYPE_N) {
                if ($2==-1) { /* (nhi:natural downto nlo:natural) */
                  $$->sizeval = $1->value - $3->value + 1;
                } else {      /* (nhi:natural to     nlo:natural) */
                  $$->sizeval = $3->value - $1->value + 1;
                }
              } else {
                /* make an expression to calculate the width of this vrange:
                 * create an expression that calculates:
                 *   size expr = (simple_expr1) - (simple_expr2) + 1
                 */
                expdata *size_expr1  = (expdata*)xmalloc(sizeof(expdata));
                expdata *size_expr2  = (expdata*)xmalloc(sizeof(expdata));
                expdata *diff12  = (expdata*)xmalloc(sizeof(expdata));
                expdata *plusone = (expdata*)xmalloc(sizeof(expdata));
                expdata *finalexpr = (expdata*)xmalloc(sizeof(expdata));
                size_expr1->sl = addwrap("(",$1->sl,")");
                size_expr2->sl = addwrap("(",$3->sl,")");
                plusone->op=EXPDATA_TYPE_TERMINAL;
                plusone->sl=addtxt(NULL,"1");
                if ($2==-1) {
                  /* (simple_expr1 downto simple_expr1) */
                  diff12 = addexpr(size_expr1,'-',"-",size_expr2);
                } else {
                  /* (simple_expr1   to   simple_expr1) */
                  diff12 = addexpr(size_expr2,'-',"-",size_expr1);
                }
                finalexpr = addexpr(diff12,'+',"+",plusone);
                finalexpr->sl = addwrap("(",finalexpr->sl,")");
                $$->size_expr = finalexpr->sl;
              }

	} | simple_expr {
	      NOT_IMPLEMENTED;
              // $$=new_vrange(tSUBSCRIPT);
              // $$->nlo=$1->sl;
	} | NAME '\'' RANGE {
		NOT_IMPLEMENTED;
              // /* lookup NAME and copy its vrange */
              // sglist *sg = NULL;
              // if((sg=lookup(io_list,$1))==NULL) {
                // sg=lookup(sig_list,$1);
              // }
              // if(sg) {
                // $$ = sg->range;
              // } else {
                // fprintf(stderr,"Undefined range \"%s'range\" on line %d\n",$1,lineno);
                // YYABORT;
              // }
	};


updown : DOWNTO {$$=-1;}
       | TO {$$=1;}
       ;

/* Architecture */
architecture:
	ARCHITECTURE NAME OF NAME IS rem a_decl BEGN doindent a_body END opt_architecture oname ';' unindent {
		printf("architecture, name='%s'\n", $4);
		$architecture = new AstNode(AST_MODULE);
		$architecture->str = $4;
		modules[$4] = $architecture;

		for (auto &i: *$a_decl) {
			$architecture->children.push_back(i);
		}
		delete $a_decl;

		for (auto &i: *$a_body) {
			$architecture->children.push_back(i);
		}
		delete $a_body;
	};

/* Extends indentation by one level */
doindent : /* Empty */ {indent= indent < MAXINDENT ? indent + 1 : indent;}
         ;
/* Shorten indentation by one level */
unindent : /* Empty */ {indent= indent > 0 ? indent - 1 : indent;}

/* Declarative part of the architecture */
a_decl[a_decl_new]    : {
		$$ = NULL;

	} | a_decl[a_decl_orig] SIGNAL s_list ':' type ';' rem {
		NOT_IMPLEMENTED;
            // sglist *sg;
            // slist *sl;
// 
              // sl=$1;
              // sg=$3;
              // for(;;){
                // sg->type=wire;
                // sg->range=$5;
                // sl=addptxt(sl,&(sg->type));
                // sl=addpar(sl,$5);
                // sl=addtxt(sl,sg->name);
                // sl=addpost(sl,$5);
                // sl=addtxt(sl,";");
                // if(sg->next == NULL)
                  // break;
                // sl=addtxt(sl," ");
                // sg=sg->next;
              // }
              // sg->next=sig_list;
              // sig_list=$3;
              // $$=addrem(sl,$7);

	} | a_decl[a_decl_orig] SIGNAL s_list ':' type ':' '=' expr ';' rem {
		$a_decl_new = $a_decl_orig;
		if ($a_decl_new == NULL) {
			$a_decl_new = new std::vector<AstNode*>;
		}
		for (auto &name: *$s_list) {
			add_wire($a_decl_new, name, DIR_NONE, $type);

			struct AstNode *identifier = new AstNode(AST_IDENTIFIER);
			identifier->str = name;

			struct AstNode *value = expr_to_ast($expr);

			struct AstNode *assign = new AstNode(AST_ASSIGN_LE, identifier, value);
			struct AstNode *initial = new AstNode(AST_INITIAL);
			initial->children.push_back(assign);

			$a_decl_new->push_back(initial);
		}

            // sglist *sg;
            // slist *sl;
// 
              // sl=$1;
              // sg=$3;
              // for(;;){
                // sg->type=wire;
                // sg->range=$5;
                // sl=addptxt(sl,&(sg->type));
                // sl=addpar(sl,$5);
                // sl=addtxt(sl,sg->name);
                // sl=addpost(sl,$5);
                // sl=addtxt(sl," = ");
                // sl=addsl(sl,$8->sl);
                // sl=addtxt(sl,";");
                // if(sg->next == NULL)
                  // break;
                // sl=addtxt(sl," ");
                // sg=sg->next;
              // }
              // sg->next=sig_list;
              // sig_list=$3;
              // $$=addrem(sl,$10);

	} | a_decl CONSTANT NAME ':' type ':' '=' expr ';' rem {
		NOT_IMPLEMENTED;
            // slist * sl;
              // sl=addtxt($1,"parameter ");
              // sl=addtxt(sl,$3);
              // sl=addtxt(sl," = ");
              // sl=addsl(sl,$8->sl);
              // sl=addtxt(sl,";");
              // $$=addrem(sl,$10);

	} | a_decl TYPE NAME IS '(' s_list ')' ';' rem {
		NOT_IMPLEMENTED;
            // slist *sl, *sl2;
            // sglist *p;
            // int n,k;
              // n=0;
              // sl=NULL;
              // p=$6;
              // for(;;){
                // sl=addtxt(sl,"  ");
                // sl=addtxt(sl,p->name);
                // sl=addtxt(sl," = ");
                // sl=addval(sl,n++);
                // p=p->next;
                // if(p==NULL){
                  // sl=addtxt(sl,";\n");
                  // break;
                // } else
                  // sl=addtxt(sl,",\n");
              // }
              // n--;
              // k=find_msb(n);
              // sl2=addtxt(NULL,"parameter [");
              // sl2=addval(sl2,k);
              // sl2=addtxt(sl2,":0]\n");
              // sl=addsl(sl2,sl);
              // sl=addsl($1,sl);
              // $$=addrem(sl,$9);
              // p=(sglist*)xmalloc(sizeof(sglist));
              // p->name=$3;
              // if(k>0) {
                // p->range=new_vrange(tVRANGE);
                // p->range->sizeval = k+1;
                // p->range->nhi=addval(NULL,k);
                // p->range->nlo=addtxt(NULL,"0");
              // } else {
                // p->range=new_vrange(tSCALAR);
              // }
              // p->next=type_list;
              // type_list=p;

	} | a_decl TYPE NAME IS ARRAY '(' vec_range ')' OF type ';' rem {
		NOT_IMPLEMENTED;
            // slist *sl=NULL;
            // sglist *p;
              // $$=addrem(sl,$12);
              // p=(sglist*)xmalloc(sizeof(sglist));
              // p->name=$3;
              // p->range=$10;
              // p->range->xhi=$7->nhi;
              // p->range->xlo=$7->nlo;
              // p->next=type_list;
              // type_list=p;

/*           1     2          3   4      5r1   6   7       8  9r2      10   11  12 13r3 14        15  16   17      18 19r4 */
	} | a_decl COMPONENT NAME opt_is rem  opt_generic PORT nolist '(' rem portlist ')' ';' rem END COMPONENT oname ';' yeslist rem {
		NOT_IMPLEMENTED;
              // $$=addsl($1,$20); /* a_decl, rem4 */
              // free($3); /* NAME */
              // free($10); /* rem2 */
              // free($14);/* rem3 */
	};

opt_generic : /* Empty */ {
		NOT_IMPLEMENTED;
		// $$=NULL;
	} | GENERIC nolist '(' rem genlist ')' ';' rem {
		NOT_IMPLEMENTED;
             // if (0) fprintf(stderr,"matched opt_generic\n");
             // free($4);  /* rem */
             // free($8);  /* rem */
             // $$=NULL;
	};

nolist : /*Empty*/ {
	NOT_IMPLEMENTED;
	dolist = 0;
}

yeslist : /*Empty*/ {
	NOT_IMPLEMENTED;
	// dolist = 1;
}

/* XXX wishlist: record comments into slist, play them back later */
s_list[s_list_new] : NAME rem {
	$s_list_new = new std::vector<std::string>;
	$s_list_new->push_back($NAME);

         // sglist * sg;
           // if(dolist){
             // sg=(sglist*)xmalloc(sizeof(sglist));
             // sg->name=$1;
             // sg->next=NULL;
             // $$=sg;
           // } else{
             // free($1);
             // $$=NULL;
           // }
           // free($2);
 } | NAME ',' rem s_list[s_list_orig] {
	$s_list_new = $s_list_orig;
	$s_list_new->push_back($NAME);

         // sglist * sg;
           // if(dolist){
             // sg=(sglist*)xmalloc(sizeof(sglist));
             // sg->name=$1;
             // sg->next=$4;
             // $$=sg;
           // } else{
             // free($1);
             // $$=NULL;
           // }
           // free($3);
};

a_body[a_body_new] : rem {
		// $$=addind($1);

       /* 1   2      3   4   5   6     7        8      9 */
	} | rem signal '<' '=' rem norem sigvalue yesrem a_body[a_body_orig] {
		$a_body_new = $a_body_orig;
		if ($a_body_new == NULL) {
			$a_body_new = new std::vector<AstNode*>;
		}
		struct AstNode *assign = new AstNode(AST_ASSIGN, $signal, expr_to_ast($sigvalue));
		$a_body_new->insert($a_body_new->begin(), assign);
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"assign ");
           // sl=addsl(sl,$2->sl);
           // findothers($2,$7);
           // free($2);
           // sl=addtxt(sl," = ");
           // sl=addsl(sl,$7);
           // sl=addtxt(sl,";\n");
           // $$=addsl(sl,$9);

	} | rem BEGN signal '<' '=' rem norem sigvalue yesrem a_body[a_body_orig] END NAME ';' {
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"assign ");
           // sl=addsl(sl,$3->sl);
           // findothers($3,$8);
           // free($3);
           // sl=addtxt(sl," = ");
           // sl=addsl(sl,$8);
           // sl=addtxt(sl,";\n");
           // $$=addsl(sl,$10);

       /* 1   2     3    4    5   6       7       8   9     10     11 */
	} | rem WITH expr SELECT rem yeswith signal '<' '=' with_list a_body[a_body_orig] {
		NOT_IMPLEMENTED;
         // slist *sl;
         // sglist *sg;
         // char *s;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"always @(*) begin\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"  case(");
           // sl=addsl(sl,$3->sl);
           // free($3);
           // sl=addtxt(sl,")\n");
           // if($5)
             // sl=addsl(sl,$5);
           // s=sbottom($7->sl);
           // if((sg=lookup(io_list,s))==NULL)
             // sg=lookup(sig_list,s);
           // if(sg)
             // sg->type=reg;
           // findothers($7,$10);
           // free($7);
           // sl=addsl(sl,$10);
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"  endcase\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"end\n\n");
           // $$=addsl(sl,$11);

       /* 1   2   3     4  5   6    7    8   9         10     11  12  13  14       15 */
	} | rem NAME ':' NAME rem PORT MAP '(' doindent map_list rem ')' ';' unindent a_body[a_body_orig] {
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,$4); /* NAME2 */
           // sl=addtxt(sl," ");
           // sl=addtxt(sl,$2); /* NAME1 */
           // sl=addtxt(sl,"(\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addsl(sl,$10);  /* map_list */
           // sl=addtxt(sl,");\n\n");
           // $$=addsl(sl,$15); /* a_body */

       /* 1   2   3     4  5   6        7  8    9       10               11  12       13   14   15     16   17       18  19  20       21 */
	} | rem NAME ':' NAME rem GENERIC MAP '(' doindent generic_map_list ')' unindent PORT MAP '(' doindent map_list ')' ';' unindent a_body[a_body_orig] {
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,$4); /* NAME2 (component name) */
           // if ($5) {
             // sl=addsl(sl,$5);
             // sl=addsl(sl,indents[indent]);
           // }
           // sl=addtxt(sl," #(\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addsl(sl,$10); /* (generic) map_list */
           // sl=addtxt(sl,")\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,$2); /* NAME1 (instance name) */
           // sl=addtxt(sl,"(\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addsl(sl,$17); /* map_list */
           // sl=addtxt(sl,");\n\n");
           // $$=addsl(sl,$21); /* a_body */

         } | optname PROCESS '(' sign_list ')' p_decl opt_is BEGN doindent p_body END PROCESS oname ';' unindent a_body[a_body_orig] {
		log_assert($p_body != NULL);
		log_assert($p_body->type == AST_BLOCK);
		$a_body_new = $a_body_orig;
		if ($a_body_new == NULL) {
			$a_body_new = new std::vector<AstNode*>;
		}
		AstNode *always = new AstNode(AST_ALWAYS);
		for (auto &i: *$sign_list) {
			AstNode *edge = new AstNode(AST_EDGE);
			edge->children.push_back(i);
			always->children.push_back(edge);
		}
		always->children.push_back($p_body);
		$a_body_new->insert($a_body_new->begin(), always);

	} | optname PROCESS '(' sign_list ')' p_decl opt_is BEGN doindent rem IF edge THEN p_body END IF ';' END PROCESS oname ';' unindent a_body[a_body_orig] {
		log_assert($p_body != NULL);
		log_assert($p_body->type == AST_BLOCK);

		// FIXME: the sign_list is ignored, that's probably not right

		$a_body_new = $a_body_orig;
		if ($a_body_new == NULL) {
			$a_body_new = new std::vector<AstNode*>;
		}
		AstNode *always = new AstNode(AST_ALWAYS, $edge, $p_body);
		$a_body_new->insert($a_body_new->begin(), always);

           // slist *sl;
             // if (0) fprintf(stderr,"process style 2: if then end if\n");
             // sl=add_always($1,$4,$6,1);
             // if($10){
               // sl=addsl(sl,indents[indent]);
               // sl=addsl(sl,$10);
             // }
             // sl=addsl(sl,$14);
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"end\n\n");
             // $$=addsl(sl,$23);

       /* 1      2        3  4          5  6       7      8     9 */
	} | optname PROCESS '(' sign_list ')' p_decl opt_is BEGN doindent
         /* 10 11 12    13    14         15  16       17    18   19   20       21     22       23  24 25 */
           rem IF exprc THEN doindent p_body[p_body_if] unindent ELSIF edge THEN doindent p_body[p_body_elseif] unindent END IF ';'
         /* 26      27    28 29       30   31    */
           END PROCESS oname ';' unindent a_body[a_body_orig] {
		log_assert(($p_body_if != NULL) && ($p_body_if->type == AST_BLOCK));
		log_assert(($p_body_elseif != NULL) && ($p_body_elseif->type == AST_BLOCK));
		NOT_IMPLEMENTED;
           // slist *sl;
             // if (0) fprintf(stderr,"process style 3: if then elsif then end if\n");
             // sl=add_always($1,$4,$6,1);
             // if($10){
               // sl=addsl(sl,indents[indent]);
               // sl=addsl(sl,$10);
             // }
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"  if(");
             // sl=addsl(sl,$12);
             // sl=addtxt(sl,") begin\n");
             // sl=addsl(sl,$15);
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"  end else begin\n");
             // sl=addsl(sl,$21);
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"  end\n");
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"end\n\n");
             // $$=addsl(sl,$31);

       /* 1      2        3  4          5  6       7      8     9 */
	} | optname PROCESS '(' sign_list ')' p_decl opt_is BEGN doindent
         /* 10 11 12    13    14         15  16       17   18   19   20       21     22     23   24  25 26  27  28 29 */
           rem IF exprc THEN doindent p_body[p_body_if] unindent ELSE IF edge THEN doindent p_body[p_body_elseif] unindent END IF ';' END IF ';'
         /* 30      31    32 33       34   35    */
           END PROCESS oname ';' unindent a_body[a_body_orig] {
		log_assert(($p_body_if == NULL) || ($p_body_if->type == AST_BLOCK));
		log_assert(($p_body_elseif == NULL) || ($p_body_elseif->type == AST_BLOCK));
		NOT_IMPLEMENTED;
           // slist *sl;
             // if (0) fprintf(stderr,"process style 4: if then else if then end if\n");
             // sl=add_always($1,$4,$6,1);
             // if($10){
               // sl=addsl(sl,indents[indent]);
               // sl=addsl(sl,$10);
             // }
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"  if(");
             // sl=addsl(sl,$12); /* exprc */
             // sl=addtxt(sl,") begin\n");
             // sl=addsl(sl,$15); /* p_body:1 */
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"  end else begin\n");
             // sl=addsl(sl,$22); /* p_body:2 */
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"  end\n");
             // sl=addsl(sl,indents[indent]);
             // sl=addtxt(sl,"end\n\n");
             // $$=addsl(sl,$35); /* a_body */

       /* note vhdl does not allow an else in an if generate statement */
       /* 1       2   3          4       5       6        7     8        9   10   11  12 */
	} | gen_optname IF exprc generate  doindent a_body[a_body_orig1]  unindent endgenerate oname ';' a_body[a_body_orig2] {
		NOT_IMPLEMENTED;
         // slist *sl;
         // blknamelist *tname_list;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"generate ");
           // sl=addtxt(sl,"if (");
           // sl=addsl(sl,$3); /* exprc */
           // sl=addtxt(sl,") begin: ");
           // tname_list=blkname_list;
           // sl=addtxt(sl,tname_list->name);
           // blkname_list=blkname_list->next;
           // if (tname_list!=NULL) {
           // free(tname_list->name);
           // free(tname_list);
           // }
           // sl=addtxt(sl,"\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addsl(sl,$6);   /* a_body:1 */
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"end\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"endgenerate\n");
           // $$=addsl(sl,$11);    /* a_body:2 */

       /* 1       2       3    4 5    6   7  8        9      10      11       12    13     14    15  16 */
	} | gen_optname FOR  signal IN expr TO expr generate doindent a_body[a_body_orig1]  unindent endgenerate oname ';' a_body[a_body_orig2] {
		NOT_IMPLEMENTED;
         // slist *sl;
         // blknamelist *tname_list;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"genvar ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl,";\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"generate ");
           // sl=addtxt(sl,"for (");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl,"=");
           // sl=addsl(sl,$5->sl); /* expr:1 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," <= ");
           // sl=addsl(sl,$7->sl); /* expr:2 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," = ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," + 1) begin: ");
           // tname_list=blkname_list;
           // sl=addtxt(sl,tname_list->name);
           // blkname_list=blkname_list->next;
           // if (tname_list!=NULL) {
           // free(tname_list->name);
           // free(tname_list);
           // }
           // sl=addtxt(sl,"\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addsl(sl,$10);   /* a_body:1 */
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"end\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"endgenerate\n");
           // $$=addsl(sl,$15);    /* a_body:2 */

       /* 1           2       3   4   5    6     7      8        9      10      11       12    13     14    15  16 */
	} | gen_optname FOR  signal IN expr DOWNTO expr generate doindent a_body[a_body_orig1]  unindent endgenerate oname ';' a_body[a_body_orig2] {
		NOT_IMPLEMENTED;
         // slist *sl;
           // blknamelist* tname_list;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"genvar ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl,";\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"generate ");
           // sl=addtxt(sl,"for (");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl,"=");
           // sl=addsl(sl,$5->sl); /* expr:1 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," >= ");
           // sl=addsl(sl,$7->sl); /* expr:2 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," = ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," - 1) begin: ");
           // tname_list=blkname_list;
           // sl=addtxt(sl,tname_list->name);
           // blkname_list=blkname_list->next;
           // if (tname_list!=NULL) {
           // free(tname_list->name);
           // free(tname_list);
           // }
           // sl=addtxt(sl,"\n");
           // sl=addsl(sl,$10);   /* a_body:1 */
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"end\n");
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"endgenerate\n");
           // $$=addsl(sl,$15);    /* a_body:2 */
	};

oname : {
		$$=NULL;
	} | NAME {
		// FIXME: must match the matching name
		free($1);
		$$=NULL;
	};

optname : rem {
		$$ = NULL;

	} | rem NAME ':' {
		NOT_IMPLEMENTED;
		// $$=$1;
		// free($2);
	}

gen_optname : rem {
		NOT_IMPLEMENTED;
		// $$=$1;

	} | rem NAME ':' {
	   NOT_IMPLEMENTED;
           // blknamelist *tname_list;
           // tname_list = (blknamelist*)xmalloc (sizeof(blknamelist));
           // tname_list->name = (char*)xmalloc(strlen($2));
           // strcpy(tname_list->name, $2);
           // tname_list->next = blkname_list;
           // blkname_list=tname_list;
           // $$=$1;
           // free($2);
	};

edge : '(' edge ')' {
		NOT_IMPLEMENTED;
		// $$ = $2;
		// $$=addwrap("(",$2,")");

	} | NAME '\'' EVENT AND exprc {
		NOT_IMPLEMENTED;
		// push_clkedge($5->data.sl->data.txt[0]-'0', "name'event and exprc");

	} | exprc AND NAME '\'' EVENT {
		NOT_IMPLEMENTED;
		// push_clkedge($1->data.sl->data.txt[0]-'0', "exprc and name'event");

	} | POSEDGE '(' NAME ')' {
		AstNode *id = new AstNode(AST_IDENTIFIER);
		id->str = $NAME;

		AstNode *edge = new AstNode(AST_POSEDGE);
		edge->children.push_back(id);

		$$ = edge;

		// push_clkedge(1, "explicit");

	} | NEGEDGE '(' NAME ')' {
		NOT_IMPLEMENTED;
		// AstNode *always = new AstNode(AST_ALWAYS);
		// push_clkedge(0, "explicit");
	};

yeswith : {
	NOT_IMPLEMENTED;
	// dowith=1;
}

with_list : with_item ';' {
		NOT_IMPLEMENTED;
		// $$=$1;
	} | with_item ',' rem with_list {
		NOT_IMPLEMENTED;
	    // slist *sl;
	      // if($3){
		// sl=addsl($1,$3);
		// $$=addsl(sl,$4);
	      // } else
		// $$=addsl($1,$4);

	} | expr delay WHEN OTHERS ';' {
		NOT_IMPLEMENTED;
            // slist *sl;
              // sl=addtxt(indents[indent],"    default : ");
              // sl=addsl(sl,slwith);
              // sl=addtxt(sl," <= ");
              // if(delay && $2){
                // sl=addtxt(sl,"# ");
                // sl=addval(sl,$2);
                // sl=addtxt(sl," ");
              // }
              // if($1->op == 'c')
                // sl=addsl(sl,addwrap("{",$1->sl,"}"));
              // else
                // sl=addsl(sl,$1->sl);
              // free($1);
              // delay=1;
              // $$=addtxt(sl,";\n");
	}

with_item : expr delay WHEN wlist {
	NOT_IMPLEMENTED;
            // slist *sl;
              // sl=addtxt(indents[indent],"    ");
              // sl=addsl(sl,$4);
              // sl=addtxt(sl," : ");
              // sl=addsl(sl,slwith);
              // sl=addtxt(sl," <= ");
              // if(delay && $2){
                // sl=addtxt(sl,"# ");
                // sl=addval(sl,$2);
                // sl=addtxt(sl," ");
              // }
              // if($1->op == 'c')
                // sl=addsl(sl,addwrap("{",$1->sl,"}"));
              // else
                // sl=addsl(sl,$1->sl);
              // free($1);
              // delay=1;
              // $$=addtxt(sl,";\n");
	}

p_decl : rem {
		$$ = NULL;

	} | rem VARIABLE s_list ':' type ';' p_decl {
		NOT_IMPLEMENTED;
         // slist *sl;
         // sglist *sg, *p;
           // sl=addtxt($1,"    reg");
           // sl=addpar(sl,$5);
           // free($5);
           // sg=$3;
           // for(;;){
             // sl=addtxt(sl,sg->name);
             // p=sg;
             // sg=sg->next;
             // free(p);
             // if(sg)
               // sl=addtxt(sl,", ");
             // else
               // break;
           // }
           // sl=addtxt(sl,";\n");
           // $$=addsl(sl,$7);

	} | rem VARIABLE s_list ':' type ':' '=' expr ';' p_decl {
		NOT_IMPLEMENTED;
         // slist *sl;
         // sglist *sg, *p;
           // sl=addtxt($1,"    reg");
           // sl=addpar(sl,$5);
           // free($5);
           // sg=$3;
           // for(;;){
             // sl=addtxt(sl,sg->name);
             // p=sg;
             // sg=sg->next;
             // free(p);
             // if(sg)
               // sl=addtxt(sl,", ");
             // else
               // break;
           // }
           // sl=addtxt(sl," = ");
           // sl=addsl(sl,$8->sl);
           // sl=addtxt(sl,";\n");
           // $$=addsl(sl,$10);
	};


p_body[p_body_result] : rem {
		$$ = NULL;
	/* 1   2     3    4  5     6     7    8     9 */
	} | rem signal ':' '=' norem expr ';' yesrem  p_body[p_body_orig] {
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addsl(sl,$2->sl);
           // findothers($2,$6->sl);
           // sl=addtxt(sl," = ");
           // if($6->op == 'c')
             // sl=addsl(sl,addwrap("{",$6->sl,"}"));
           // else
             // sl=addsl(sl,$6->sl);
           // sl=addtxt(sl,";\n");
           // $$=addsl(sl,$9);
       /* 1   2     3      4   5     6         7     8   */
	} | rem signal norem '<' '=' sigvalue yesrem  p_body[p_body_orig] {
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		log_assert($sigvalue->op == EXPDATA_TYPE_AST);
		AstNode *n = new AstNode(AST_ASSIGN_LE, $signal, $sigvalue->node);
		if ($p_body_orig == NULL) {
			$p_body_orig = new AstNode(AST_BLOCK);
		}
		$p_body_orig->children.insert($p_body_orig->children.begin(), n);
		free($sigvalue);
		$p_body_result = $p_body_orig;


/*           1   2    3     4 5        6:1      7        8      9   10  11    12:2  */
	} | rem IF exprc THEN doindent p_body[p_body_if] unindent elsepart END IF ';' p_body[p_body_orig] {
		log_assert($p_body_if != NULL);
		log_assert($p_body_if->type == AST_BLOCK);
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		log_assert($exprc != NULL);

		$p_body_result = $p_body_orig;
		if ($p_body_result == NULL) {
			$p_body_result = new AstNode(AST_BLOCK);
		}

		AstNode *reduce;
		if ($exprc->type == AST_REDUCE_BOOL) {
			reduce = $exprc;
		} else {
			reduce = new AstNode(AST_REDUCE_BOOL);
			reduce->children.push_back($exprc);
		}

		AstNode *case_node = new AstNode(AST_CASE);
		case_node->children.push_back(reduce);

		AstNode *true_const;
		true_const = AstNode::mkconst_int(1, false);
		AstNode *if_cond = new AstNode(AST_COND, true_const, $p_body_if);

		case_node->children.push_back(if_cond);

		if ($elsepart != NULL) {
			for (auto &i: *$elsepart) {
				log_assert(i->type == AST_COND);
				case_node->children.push_back(i);
			}
		}

		$p_body_result->children.insert($p_body_result->children.begin(), case_node);

         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"if(");
           // sl=addsl(sl,$3);
           // sl=addtxt(sl,") begin\n");
           // sl=addsl(sl,$6);
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"end\n");
           // sl=addsl(sl,$8);
           // $$=addsl(sl,$12);
/*        1   2    3      4 5:1  6  7:2   8    9       10:1   11       12  13   14  15:2 */
	} | rem FOR  signal IN expr TO expr LOOP doindent p_body[p_body_internal] unindent END LOOP ';' p_body[p_body_orig] {
		log_assert(($p_body_internal == NULL) || ($p_body_internal->type == AST_BLOCK));
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"for (");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl,"=");
           // sl=addsl(sl,$5->sl); /* expr:1 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," <= ");
           // sl=addsl(sl,$7->sl); /* expr:2 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," = ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," + 1) begin\n");
           // sl=addsl(sl,$10);    /* p_body:1 */
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"end\n");
           // $$=addsl(sl,$15);    /* p_body:2 */
/*        1   2    3      4 5:1  6      7:2   8    9       10:1   11       12  13   14  15:2 */
	} | rem FOR  signal IN expr DOWNTO expr LOOP doindent p_body[p_body_internal] unindent END LOOP ';' p_body[p_body_orig] {
		log_assert(($p_body_internal == NULL) || ($p_body_internal->type == AST_BLOCK));
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"for (");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl,"=");
           // sl=addsl(sl,$5->sl); /* expr:1 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," >= ");
           // sl=addsl(sl,$7->sl); /* expr:2 */
           // sl=addtxt(sl,"; ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," = ");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl," - 1) begin\n");
           // sl=addsl(sl,$10);    /* p_body:1 */
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"end\n");
           // $$=addsl(sl,$15);    /* p_body:2 */
/*        1   2    3      4 5       6  7   8    9      10 */
	} | rem CASE signal IS rem cases END CASE ';' p_body[p_body_orig] {
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"case(");
           // sl=addsl(sl,$3->sl); /* signal */
           // sl=addtxt(sl,")\n");
           // if($5){
             // sl=addsl(sl,indents[indent]);
             // sl=addsl(sl,$5);
           // }
           // sl=addsl(sl,$6);
           // sl=addsl(sl,indents[indent]);
           // sl=addtxt(sl,"endcase\n");
           // $$=addsl(sl,$10);
	} | rem EXIT ';' p_body[p_body_orig] {
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addsl($1,indents[indent]);
           // sl=addtxt(sl,"disable;  //VHD2VL: add block name here\n");
           // $$=addsl(sl,$4);
	} | rem NULLV ';' p_body[p_body_orig] {
		log_assert(($p_body_orig == NULL) || ($p_body_orig->type == AST_BLOCK));
		NOT_IMPLEMENTED;
         // slist *sl;
           // if($1){
             // sl=addsl($1,indents[indent]);
             // $$=addsl(sl,$4);
           // }else
             // $$=$4;
	};


elsepart[elsepart_new] : {
		$$=NULL;

	} | ELSIF exprc THEN doindent p_body unindent elsepart[elsepart_orig] {
		log_assert(($p_body == NULL) || ($p_body->type == AST_BLOCK));
		$elsepart_new = $elsepart_orig;
		NOT_IMPLEMENTED;
		   // slist *sl;
		     // sl=addtxt(indents[indent],"else if(");
		     // sl=addsl(sl,$2);
		     // sl=addtxt(sl,") begin\n");
		     // sl=addsl(sl,$5);
		     // sl=addsl(sl,indents[indent]);
		     // sl=addtxt(sl,"end\n");
		     // $$=addsl(sl,$7);

	} | ELSE doindent p_body unindent {
		log_assert($p_body != NULL);
		log_assert($p_body->type == AST_BLOCK);
		AstNode *default_const = new AstNode(AST_DEFAULT);
		AstNode *else_cond = new AstNode(AST_COND, default_const, $p_body);
		$elsepart_new = new std::vector<AstNode*>;
		$elsepart_new->push_back(else_cond);
	};


cases : WHEN wlist '=' '>' doindent p_body unindent cases {
		log_assert(($p_body == NULL) || ($p_body->type == AST_BLOCK));
		NOT_IMPLEMENTED;
        // slist *sl;
          // sl=addsl(indents[indent],$2);
          // sl=addtxt(sl," : begin\n");
          // sl=addsl(sl,$6);
          // sl=addsl(sl,indents[indent]);
          // sl=addtxt(sl,"end\n");
          // $$=addsl(sl,$8);

	} | WHEN OTHERS '=' '>' doindent p_body unindent {
		log_assert(($p_body == NULL) || ($p_body->type == AST_BLOCK));
		NOT_IMPLEMENTED;
        // slist *sl;
          // sl=addtxt(indents[indent],"default : begin\n");
          // sl=addsl(sl,$6);
          // sl=addsl(sl,indents[indent]);
          // $$=addtxt(sl,"end\n");

	} | /* Empty */ {
		$$=NULL;
	};  /* List without WHEN OTHERS */


wlist: wvalue {
		NOT_IMPLEMENTED;
		// $$=$1;

	} | wlist '|' wvalue {
		NOT_IMPLEMENTED;
		// slist *sl;
		// sl=addtxt($1,",");
		// $$=addsl(sl,$3);
	};


wvalue : STRING {
		NOT_IMPLEMENTED;
		// $$=addvec(NULL,$1);

	} | NAME STRING {
		NOT_IMPLEMENTED;
		// $$=addvec_base(NULL,$1,$2);

	} | NAME {
		NOT_IMPLEMENTED;
		// $$=addtxt(NULL,$1);
	};


sign_list : signal {
		$$ = new std::vector<Yosys::AST::AstNode*>;
		$$->push_back($signal);

	} | signal ',' sign_list {
		$3->insert($3->begin(), $signal);
		$$ = $3;
	};


sigvalue[sigvalue_new]: expr delay ';' {
		$sigvalue_new = $expr;
		// FIXME: deal with the delay

           // slist *sl;
             // if(delay && $2){
               // sl=addtxt(NULL,"# ");
               // sl=addval(sl,$2);
               // sl=addtxt(sl," ");
             // } else
               // sl=NULL;
             // if($1->op == 'c')
               // sl=addsl(sl,addwrap("{",$1->sl,"}"));
             // else
               // sl=addsl(sl,$1->sl);
             // free($1);
             // delay=1;
             // $$=sl;

	} | expr delay WHEN exprc ';' {
		NOT_IMPLEMENTED;
             // fprintf(stderr,"Warning on line %d: Can't translate 'expr delay WHEN exprc;' expressions\n",lineno);
             // $$=NULL;

	} | expr delay WHEN exprc ELSE nodelay sigvalue[sigvalue_orig] {
		log_assert($exprc != NULL);

		log_assert($expr != NULL);
		log_assert($expr->op == EXPDATA_TYPE_AST);

		log_assert($sigvalue_orig != NULL);
		log_assert($sigvalue_orig->op == EXPDATA_TYPE_AST);

		AstNode *ternary = new AstNode(AST_TERNARY);
		ternary->children.push_back($exprc);
		ternary->children.push_back($expr->node);
		ternary->children.push_back($sigvalue_orig->node);
		free($expr);

		$sigvalue_new = $sigvalue_orig;
		$sigvalue_new->node = ternary;

           // slist *sl;
             // sl=addtxt($4," ? ");
             // if($1->op == 'c')
               // sl=addsl(sl,addwrap("{",$1->sl,"}"));
             // else
               // sl=addsl(sl,$1->sl);
             // free($1);
             // sl=addtxt(sl," : ");
             // $$=addsl(sl,$7);
	};

nodelay  : /* empty */ {
		delay=0;
	};

delay    : /* empty */ {
		$$=0;
	} | AFTER NATURAL NAME {
		NOT_IMPLEMENTED;
             // set_timescale($3);
             // $$=$2;
	};

map_list : rem map_item {
		NOT_IMPLEMENTED;
           // slist *sl;
           // sl=addsl($1,indents[indent]);
           // $$=addsl(sl,$2);

	} | rem map_item ',' map_list {
		NOT_IMPLEMENTED;
           // slist *sl;
             // sl=addsl($1,indents[indent]);
             // sl=addsl(sl,$2);
             // sl=addtxt(sl,",\n");
             // $$=addsl(sl,$4);
	};

map_item : mvalue {
		NOT_IMPLEMENTED;
		// $$=$1;

	} | NAME '=' '>' mvalue {
		NOT_IMPLEMENTED;
           // slist *sl;
             // sl=addtxt(NULL,".");
             // sl=addtxt(sl,$1);
             // sl=addtxt(sl,"(");
             // sl=addsl(sl,$4);
             // $$=addtxt(sl,")");
	};

mvalue : STRING {
		NOT_IMPLEMENTED;
		// $$=addvec(NULL,$1);

	} | signal {
		NOT_IMPLEMENTED;
		// $$=addsl(NULL,$1->sl);

	} | NATURAL {
		NOT_IMPLEMENTED;
		// $$=addval(NULL,$1);

	} | NAME STRING {
		NOT_IMPLEMENTED;
		// $$=addvec_base(NULL,$1,$2);

	} | OPEN {
		NOT_IMPLEMENTED;
		// $$=addtxt(NULL,"/* open */");

	} | '(' OTHERS '=' '>' STRING ')' {
		NOT_IMPLEMENTED;
             // $$=addtxt(NULL,"{broken{");
             // $$=addtxt($$,$5);
             // $$=addtxt($$,"}}");
             // fprintf(stderr,"Warning on line %d: broken width on port with OTHERS\n",lineno);
	};


generic_map_list : rem generic_map_item {
		NOT_IMPLEMENTED;
           // slist *sl;
           // sl=addsl($1,indents[indent]);
           // $$=addsl(sl,$2);

	} | rem generic_map_item ',' generic_map_list {
		NOT_IMPLEMENTED;
           // slist *sl;
             // sl=addsl($1,indents[indent]);
             // sl=addsl(sl,$2);
             // sl=addtxt(sl,",\n");
             // $$=addsl(sl,$4);
	} | rem expr {  /* only allow a single un-named map item */
		NOT_IMPLEMENTED;
             // $$=addsl(NULL,$2->sl);
	};

generic_map_item : NAME '=' '>' expr {
		NOT_IMPLEMENTED;
	printf("generic_map_item\n");
           // slist *sl;
             // sl=addtxt(NULL,".");
             // sl=addtxt(sl,$1);
             // sl=addtxt(sl,"(");
             // sl=addsl(sl,$4->sl);
             // $$=addtxt(sl,")");
	};

signal : NAME {
	AstNode *wire = new AstNode(AST_IDENTIFIER);
	wire->str = $NAME;
	$$ = wire;
         // slist *sl;
         // slval *ss;
           // ss=(slval*)xmalloc(sizeof(slval));
           // sl=addtxt(NULL,$1);
           // if(dowith){
             // slwith=sl;
             // dowith=0;
           // }
           // ss->sl=sl;
           // ss->val=-1;
           // ss->range=NULL;
           // $$=ss;

	 } | NAME '(' vec_range ')' {
		NOT_IMPLEMENTED;
         // slval *ss;
         // slist *sl;
           // ss=(slval*)xmalloc(sizeof(slval));
           // sl=addtxt(NULL,$1);
           // sl=addpar_snug(sl,$3);
           // if(dowith){
             // slwith=sl;
             // dowith=0;
           // }
           // ss->sl=sl;
           // ss->range=$3;
           // if($3->vtype==tVRANGE) {
             // if (0) {
               // fprintf(stderr,"ss->val set to 1 for ");
               // fslprint(stderr,ss->sl);
               // fprintf(stderr,", why?\n");
             // }
             // ss->val = -1; /* width is in the vrange */
           // } else {
             // ss->val = 1;
           // }
           // $$=ss;

	} | NAME '(' vec_range ')' '(' vec_range ')' {
		NOT_IMPLEMENTED;
         // slval *ss;
         // slist *sl;
           // ss=(slval*)xmalloc(sizeof(slval));
           // sl=addtxt(NULL,$1);
           // sl=addpar_snug2(sl,$3, $6);
           // if(dowith){
             // slwith=sl;
             // dowith=0;
           // }
           // ss->sl=sl;
           // ss->range=$3;
           // if($3->vtype==tVRANGE) {
             // ss->val = -1; /* width is in the vrange */
           // } else {
             // ss->val = 1;
           // }
           // $$=ss;
	};

/* Expressions */
expr : signal {
		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = $signal;
		$$ = e;

	} | STRING {
		std::vector<RTLIL::State> bits;
		string_to_bits(bits, $STRING);

		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = Yosys::AST::AstNode::mkconst_bits(bits, false);

		$$ = e;

	} | FLOAT {
		NOT_IMPLEMENTED;
         // expdata *e=(expdata*)xmalloc(sizeof(expdata));
           // e->op='t'; /* Terminal symbol */
           // e->sl=addtxt(NULL,$1);
           // $$=e;

	} | NATURAL {
		expdata *e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = Yosys::AST::AstNode::mkconst_int($NATURAL, false);
		$$ = e;

	} | NATURAL BASED {  /* e.g. 16#55aa# */
		NOT_IMPLEMENTED;
         /* XXX unify this code with addvec_base */
         // expdata *e=(expdata*)xmalloc(sizeof(expdata));
         // char *natval = (char*)xmalloc(strlen($2)+34);
           // e->op='t'; /* Terminal symbol */
           // switch($1) {
           // case  2:
             // sprintf(natval, "'B%s",$2);
             // break;
           // case  8:
             // sprintf(natval, "'O%s",$2);
             // break;
           // case 10:
             // sprintf(natval, "'D%s",$2);
             // break;
           // case 16:
             // sprintf(natval, "'H%s",$2);
             // break;
           // default:
             // sprintf(natval,"%d#%s#",$1,$2);
             // fprintf(stderr,"Warning on line %d: Can't translate based number %s (only bases of 2, 8, 10, and 16 are translatable)\n",lineno,natval);
           // }
           // e->sl=addtxt(NULL,natval);
           // $$=e;

	} | NAME STRING {
		NOT_IMPLEMENTED;
         // expdata *e=(expdata*)xmalloc(sizeof(expdata));
           // e->op='t'; /* Terminal symbol */
           // e->sl=addvec_base(NULL,$1,$2);
           // $$=e;

	} | '(' OTHERS '=' '>' STRING ')' {
		// FIXME: OTHERS in a vector still needs to be dealt with

		std::vector<RTLIL::State> bits;
		string_to_bits(bits, $STRING);

		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = Yosys::AST::AstNode::mkconst_bits(bits, false);

		$$ = e;

	} | expr '&' expr { /* Vector chaining */
		NOT_IMPLEMENTED;
         // slist *sl;
           // sl=addtxt($1->sl,",");
           // sl=addsl(sl,$3->sl);
           // free($3);
           // $1->op='c';
           // $1->sl=sl;
           // $$=$1;

	} | '-' expr %prec UMINUS {
		NOT_IMPLEMENTED;
		// $$=addexpr(NULL,'m'," -",$2);

	} | '+' expr %prec UPLUS {
		NOT_IMPLEMENTED;
		// $$=addexpr(NULL,'p'," +",$2);

	} | expr '+' expr {
		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = new AstNode(AST_ADD, $1->node, $3->node);
		$$ = e;

	} | expr '-' expr {
		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = new AstNode(AST_SUB, $1->node, $3->node);
		$$ = e;

	} | expr '*' expr {
		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = new AstNode(AST_MUL, $1->node, $3->node);
		$$ = e;

	} | expr '/' expr {
		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = new AstNode(AST_DIV, $1->node, $3->node);
		$$ = e;

	} | expr[expr1] MOD expr[expr2] {
		log_assert($expr1 != NULL);
		log_assert($expr1->op == EXPDATA_TYPE_AST);
		log_assert($expr2 != NULL);
		log_assert($expr2->op == EXPDATA_TYPE_AST);

		expdata *e;
		e = (expdata*)xmalloc(sizeof(expdata));
		e->op = EXPDATA_TYPE_AST;
		e->node = new AstNode(AST_MOD, $expr1->node, $expr2->node);
		$$ = e;
		free($expr1);
		free($expr2);

	} | NOT expr[expr1] {
		log_assert($expr1 != NULL);
		log_assert($expr1->op == EXPDATA_TYPE_AST);
		AstNode *not_node = new AstNode(AST_BIT_NOT);
		not_node->children.push_back($expr1->node);
		$expr1->node = not_node;
		$$ = $expr1;

	} | expr AND expr {
		NOT_IMPLEMENTED;
		// $$=addexpr($1,'&'," & ",$3);

	} | expr OR expr {
		NOT_IMPLEMENTED;
		// $$=addexpr($1,'|'," | ",$3);

	} | expr XOR expr {
		NOT_IMPLEMENTED;
		// $$=addexpr($1,'^'," ^ ",$3);

	} | expr XNOR expr {
		NOT_IMPLEMENTED;
		// $$=addexpr(NULL,'~'," ~",addexpr($1,'^'," ^ ",$3));

	} | BITVECT '(' expr ')' {
		NOT_IMPLEMENTED;
       /* single argument type conversion function e.g. std_ulogic_vector(x) */
       // expdata *e;
       // e=(expdata*)xmalloc(sizeof(expdata));
       // if ($3->op == 'c') {
         // e->sl=addwrap("{",$3->sl,"}");
       // } else {
         // e->sl=addwrap("(",$3->sl,")");
       // }
       // $$=e;

	} | CONVFUNC_2 '(' expr ',' NATURAL ')' {
		NOT_IMPLEMENTED;
       /* two argument type conversion e.g. to_unsigned(x, 3) */
       // $$ = addnest($3);

	} | CONVFUNC_2 '(' expr ',' NAME ')' {
		NOT_IMPLEMENTED;
       // $$ = addnest($3);

	} | '(' expr[expr1] ')' {
		log_assert($expr1 != NULL);
		log_assert($expr1->op == EXPDATA_TYPE_AST);
		$$ = $expr1;
	};

/* Conditional expressions */
exprc : conf {
		log_assert($conf != NULL);
		$exprc = $conf;

	} | '(' exprc ')' {
		log_assert($2 != NULL);
		$$ = $2;

	} | exprc AND exprc %prec ANDL {
		NOT_IMPLEMENTED;
		// slist *sl;
		// sl=addtxt($1," && ");
		// $$=addsl(sl,$3);
	} | exprc OR exprc %prec ORL {
		NOT_IMPLEMENTED;
		// slist *sl;
		// sl=addtxt($1," || ");
		// $$=addsl(sl,$3);
	} | NOT exprc %prec NOTL {
		NOT_IMPLEMENTED;
		// slist *sl;
		// sl=addtxt(NULL,"!");
		// $$=addsl(sl,$2);
	};

/* Comparisons */
conf : expr[expr1] '=' expr[expr2] %prec EQUAL {
		log_assert(($expr1 != NULL) && ($expr1->op = EXPDATA_TYPE_AST));
		log_assert(($expr2 != NULL) && ($expr2->op = EXPDATA_TYPE_AST));
		$conf = new AstNode(AST_EQ, $expr1->node, $expr2->node);
		free($expr1);
		free($expr2);

       // slist *sl;
         // if($1->op == 'c')
           // sl=addwrap("{",$1->sl,"} == ");
         // else if($1->op != 't')
           // sl=addwrap("(",$1->sl,") == ");
         // else
           // sl=addtxt($1->sl," == ");
         // if($3->op == 'c')
           // $$=addsl(sl,addwrap("{",$3->sl,"}"));
         // else if($3->op != 't')
           // $$=addsl(sl,addwrap("(",$3->sl,")"));
         // else
           // $$=addsl(sl,$3->sl);
         // free($1);
         // free($3);
	} | expr[expr1] '>' expr[expr2] {
		log_assert(($expr1 != NULL) && ($expr1->op = EXPDATA_TYPE_AST));
		log_assert(($expr2 != NULL) && ($expr2->op = EXPDATA_TYPE_AST));
		$conf = new AstNode(AST_GT, $expr1->node, $expr2->node);
		free($expr1);
		free($expr2);
       // slist *sl;
         // if($1->op == 'c')
           // sl=addwrap("{",$1->sl,"} > ");
         // else if($1->op != 't')
           // sl=addwrap("(",$1->sl,") > ");
         // else
           // sl=addtxt($1->sl," > ");
         // if($3->op == 'c')
           // $$=addsl(sl,addwrap("{",$3->sl,"}"));
         // else if($3->op != 't')
           // $$=addsl(sl,addwrap("(",$3->sl,")"));
         // else
           // $$=addsl(sl,$3->sl);
         // free($1);
         // free($3);
	} | expr[expr1] '>' '=' expr[expr2] %prec BIGEQ {
		log_assert(($expr1 != NULL) && ($expr1->op = EXPDATA_TYPE_AST));
		log_assert(($expr2 != NULL) && ($expr2->op = EXPDATA_TYPE_AST));
		$conf = new AstNode(AST_GE, $expr1->node, $expr2->node);
		free($expr1);
		free($expr2);
       // slist *sl;
         // if($1->op == 'c')
           // sl=addwrap("{",$1->sl,"} >= ");
         // else if($1->op != 't')
           // sl=addwrap("(",$1->sl,") >= ");
         // else
           // sl=addtxt($1->sl," >= ");
         // if($4->op == 'c')
           // $$=addsl(sl,addwrap("{",$4->sl,"}"));
         // else if($4->op != 't')
           // $$=addsl(sl,addwrap("(",$4->sl,")"));
         // else
           // $$=addsl(sl,$4->sl);
         // free($1);
         // free($4);
	} | expr[expr1] '<' expr[expr2] {
		log_assert(($expr1 != NULL) && ($expr1->op = EXPDATA_TYPE_AST));
		log_assert(($expr2 != NULL) && ($expr2->op = EXPDATA_TYPE_AST));
		$conf = new AstNode(AST_LT, $expr1->node, $expr2->node);
		free($expr1);
		free($expr2);
       // slist *sl;
         // if($1->op == 'c')
           // sl=addwrap("{",$1->sl,"} < ");
         // else if($1->op != 't')
           // sl=addwrap("(",$1->sl,") < ");
         // else
           // sl=addtxt($1->sl," < ");
         // if($3->op == 'c')
           // $$=addsl(sl,addwrap("{",$3->sl,"}"));
         // else if($3->op != 't')
           // $$=addsl(sl,addwrap("(",$3->sl,")"));
         // else
           // $$=addsl(sl,$3->sl);
         // free($1);
         // free($3);
	} | expr[expr1] '<' '=' expr[expr2] %prec LESSEQ {
		log_assert(($expr1 != NULL) && ($expr1->op = EXPDATA_TYPE_AST));
		log_assert(($expr2 != NULL) && ($expr2->op = EXPDATA_TYPE_AST));
		$conf = new AstNode(AST_LE, $expr1->node, $expr2->node);
		free($expr1);
		free($expr2);
       // slist *sl;
         // if($1->op == 'c')
           // sl=addwrap("{",$1->sl,"} <= ");
         // else if($1->op != 't')
           // sl=addwrap("(",$1->sl,") <= ");
         // else
           // sl=addtxt($1->sl," <= ");
         // if($4->op == 'c')
           // $$=addsl(sl,addwrap("{",$4->sl,"}"));
         // else if($4->op != 't')
           // $$=addsl(sl,addwrap("(",$4->sl,")"));
         // else
           // $$=addsl(sl,$4->sl);
         // free($1);
         // free($4);
	} | expr[expr1] '/' '=' expr[expr2] %prec NOTEQ {
		log_assert(($expr1 != NULL) && ($expr1->op = EXPDATA_TYPE_AST));
		log_assert(($expr2 != NULL) && ($expr2->op = EXPDATA_TYPE_AST));
		$conf = new AstNode(AST_NE, $expr1->node, $expr2->node);
		free($expr1);
		free($expr2);
       // slist *sl;
         // if($1->op == 'c')
           // sl=addwrap("{",$1->sl,"} != ");
         // else if($1->op != 't')
           // sl=addwrap("(",$1->sl,") != ");
         // else
           // sl=addtxt($1->sl," != ");
         // if($4->op == 'c')
           // $$=addsl(sl,addwrap("{",$4->sl,"}"));
         // else if($4->op != 't')
           // $$=addsl(sl,addwrap("(",$4->sl,")"));
         // else
           // $$=addsl(sl,$4->sl);
         // free($1);
         // free($4);
	};

simple_expr : signal {
		NOT_IMPLEMENTED;
         // expdata *e;
         // e=(expdata*)xmalloc(sizeof(expdata));
         // e->op='t'; /* Terminal symbol */
         // e->sl=$1->sl;
         // free($1);
         // $$=e;

	} | STRING {
		expdata *e;
		e=(expdata*)xmalloc(sizeof(expdata));
		expr_set_bits(e, $STRING);
		$$=e;

	} | NATURAL {
		expdata *e;
		e=(expdata*)xmalloc(sizeof(expdata));
		e->op=EXPDATA_TYPE_N;
		e->value=$1;
		e->sl=addval(NULL,$1);
		$$=e;

	} | NAME '\'' LEFT {
		NOT_IMPLEMENTED;
	      /* lookup NAME and get its left */
              // sglist *sg = NULL;
              // if((sg=lookup(io_list,$1))==NULL) {
                // sg=lookup(sig_list,$1);
              // }
              // if(sg) {
                // expdata *e;
                // e=(expdata*)xmalloc(sizeof(expdata));
                // e->sl=addwrap("(",sg->range->nhi,")");  /* XXX left vs. high? */
                // $$=e;
              // } else {
                // fprintf(stderr,"Undefined left \"%s'left\" on line %d\n",$1,lineno);
                // YYABORT;
              // }

	} | simple_expr '+' simple_expr {
		NOT_IMPLEMENTED;
       // $$=addexpr($1,'+'," + ",$3);

	} | simple_expr '-' simple_expr {
		NOT_IMPLEMENTED;
       // $$=addexpr($1,'-'," - ",$3);

	} | simple_expr '*' simple_expr {
		NOT_IMPLEMENTED;
       // $$=addexpr($1,'*'," * ",$3);

	} | simple_expr '/' simple_expr {
		NOT_IMPLEMENTED;
       // $$=addexpr($1,'/'," / ",$3);

	} | CONVFUNC_1 '(' simple_expr ')' {
		NOT_IMPLEMENTED;
       /* one argument type conversion e.g. conv_integer(x) */
       // expdata *e;
       // e=(expdata*)xmalloc(sizeof(expdata));
       // e->sl=addwrap("(",$3->sl,")");
       // $$=e;

	} | '(' simple_expr ')' {
		NOT_IMPLEMENTED;
       // expdata *e;
       // e=(expdata*)xmalloc(sizeof(expdata));
       // e->sl=addwrap("(",$2->sl,")");
       // $$=e;
	};

%%

const char *outfile;    /* Output file */
const char *sourcefile; /* Input file */

