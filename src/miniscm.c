/*
 *      ---------- Mini-Scheme Interpreter Version 0.85 ----------
 *
 *                coded by Atsushi Moriwaki (11/5/1989)
 *
 *            E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 *
 *               THIS SOFTWARE IS IN THE PUBLIC DOMAIN
 *               ------------------------------------
 * This software is completely free to copy, modify and/or re-distribute.
 * But I would appreciate it if you left my name on the code as the author.
 *
 */
/*--
 *
 *  This version has been modified by R.C. Secrist.
 *
 *  Mini-Scheme is now maintained by Akira KIDA.
 *
 *  This is a revised and modified version by Akira KIDA.
 *	current version is 0.85k4 (15 May 1994)
 *
 *  This version has been modified by Tatsuya WATANABE.
 *	current version is 0.85w2 (2016)
 *--
 */

/*
 * Define or undefine following symbols as you need.
 */
/* #define USE_SCHEME_STACK */	/* define this if you want original-Stack */
#define USE_COPYING_GC	/* undef this if you do not want to use Copying GC */


/*--
 *  If your machine can't support "forward single quotation character"
 *  i.e., '`',  you may have trouble to use backquote.
 *  So use '^' in place of '`'.
 */
# define BACKQUOTE '`'

/*
 *  Basic memory allocation units
 */

#define CELL_SEGSIZE 500000	/* # of cells in one segment */


#define banner "Hello, This is Mini-Scheme Interpreter Version 0.85w2.\n"


#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>

#include <string.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#define prompt "> "
#define InitFile "init.scm"
#ifdef _WIN32
#define snprintf _snprintf
#define stricmp _stricmp
#endif

/* cell structure */
struct cell {
	unsigned short _flag;
	unsigned char  _extflag;
	unsigned char  _isfixnum;
	union {
		struct {
			char   *_svalue;
			size_t  _length;
		} _string;
		struct {
			long    _ivalue;
			long    _rvalue;
		} _number;
		struct {
			FILE   *_file;
			char   *_curr;
		} _port;
		struct {
			struct cell *_car;
			struct cell *_cdr;
		} _cons;
#ifdef USE_COPYING_GC
		struct cell *_forwarded;
#endif
	} _object;
};

typedef struct cell *pointer;

#define T_STRING         1	/* 0000000000000001 */
#define T_NUMBER         2	/* 0000000000000010 */
#define T_SYMBOL         4	/* 0000000000000100 */
#define T_SYNTAX         8	/* 0000000000001000 */
#define T_PROC          16	/* 0000000000010000 */
#define T_PAIR          32	/* 0000000000100000 */
#define T_CLOSURE       64	/* 0000000001000000 */
#define T_CONTINUATION 128	/* 0000000010000000 */
#define T_CHARACTER    256	/* 0000000100000000 */
#define T_PORT         512	/* 0000001000000000 */
#define T_VECTOR      1024	/* 0000010000000000 */
#define T_ENVIRONMENT 8192	/* 0010000000000000 */
#define T_ATOM       16384	/* 0100000000000000 */	/* only for gc */
#define CLRATOM      49151	/* 1011111111111111 */	/* only for gc */
#define MARK         32768	/* 1000000000000000 */
#define UNMARK       32767	/* 0111111111111111 */
#ifdef USE_COPYING_GC
# define T_FORWARDED 32768	/* 1000000000000000 */	/* only for gc */
#endif

#define T_VALUES         1	/* 0000000000000001 */	/* for call-with-values */

#define T_PROMISE        1	/* 00000001 */
#define T_RESULTREADY    2	/* 00000010 */
#define T_MACRO          4	/* 00000100 */
#define T_DEFMACRO       8	/* 00001000 */	/* for define-macro */

/* macros for cell operations */
#define type(p)         ((p)->_flag)
#define exttype(p)      ((p)->_extflag)

#define is_string(p)    (type(p)&T_STRING)
#define strvalue(p)     ((p)->_object._string._svalue)
#define strlength(p)    ((p)->_object._string._length)

#define is_number(p)    (type(p)&T_NUMBER)
#define ivalue(p)       ((p)->_object._number._ivalue)
#define rvalue(p)       (*(double *)&(p)->_object._number._ivalue)
#define nvalue(p)       ((p)->_isfixnum ? ivalue(p) : rvalue(p))
#define is_integer(p)   (is_number(p) && ((p)->_isfixnum || floor(rvalue(p) + 0.5) == rvalue(p)))
#define set_num_integer(p)   ((p)->_isfixnum = 1)
#define set_num_real(p)      ((p)->_isfixnum = 0)

#define is_pair(p)      (type(p)&T_PAIR)
#define car(p)          ((p)->_object._cons._car)
#define cdr(p)          ((p)->_object._cons._cdr)

#define is_symbol(p)    (type(p)&T_SYMBOL)
#define symname(p)      strvalue(car(p))
#define hasprop(p)      (type(p)&T_SYMBOL)
#define symprop(p)      cdr(p)

#define is_syntax(p)    (type(p)&T_SYNTAX)
#define is_proc(p)      (type(p)&T_PROC)
#define syntaxname(p)   strvalue(car(p))
#define syntaxnum(p)    (*(short *)&car(p)->_extflag)
#define procnum(p)      ivalue(p)

#define is_closure(p)   (type(p)&T_CLOSURE)
#define is_macro(p)     (exttype(p)&T_MACRO)
#define closure_code(p) car(p)
#define closure_env(p)  cdr(p)

#define is_continuation(p) (type(p)&T_CONTINUATION)
#define cont_dump(p)    cdr(p)

#define is_character(p) (type(p)&T_CHARACTER)

enum {
	port_input = 1,
	port_output = 2,
	port_file = 4,
	port_string = 8,
};
#define is_port(p)      (type(p) & T_PORT)
#define is_inport(p)    (is_port(p) && ((p)->_isfixnum & port_input))
#define is_outport(p)   (is_port(p) && ((p)->_isfixnum & port_output))
#define is_fileport(p)  (is_port(p) && ((p)->_isfixnum & port_file))
#define is_strport(p)   (is_port(p) && ((p)->_isfixnum & port_string))
#define port_file(p)    ((p)->_object._port._file)
#define port_curr(p)    ((p)->_object._port._curr)

#define is_vector(p)    (type(p)&T_VECTOR)

#define is_environment(p) (type(p) & T_ENVIRONMENT)
#define setenvironment(p) type(p) |= T_ENVIRONMENT

#define is_promise(p)   (exttype(p) & T_PROMISE)
#define setpromise(p)   exttype(p) |= T_PROMISE
#define is_resultready(p) (exttype(p) & T_RESULTREADY)
#define setresultready(p) exttype(p) |= T_RESULTREADY

#define is_atom(p)      (type(p)&T_ATOM)
#define setatom(p)      type(p) |= T_ATOM
#define clratom(p)      type(p) &= CLRATOM

#define is_mark(p)      (type(p)&MARK)
#define setmark(p)      type(p) |= MARK
#define clrmark(p)      type(p) &= UNMARK

#define caar(p)         car(car(p))
#define cadr(p)         car(cdr(p))
#define cdar(p)         cdr(car(p))
#define cddr(p)         cdr(cdr(p))
#define cadar(p)        car(cdr(car(p)))
#define caddr(p)        car(cdr(cdr(p)))
#define cadaar(p)       car(cdr(car(car(p))))
#define cadddr(p)       car(cdr(cdr(cdr(p))))
#define cddddr(p)       cdr(cdr(cdr(cdr(p))))

/* array for segments */
struct cell cell_seg[CELL_SEGSIZE * 2];

/* We use 4 registers. */
pointer args;			/* register for arguments of function */
pointer envir;			/* stack register for current environment */
pointer code;			/* register for current code */
pointer dump;			/* stack register for next evaluation */

struct cell _NIL;
pointer NIL = &_NIL;	/* special cell representing empty cell */
struct cell _T;
pointer T = &_T;		/* special cell representing #t */
struct cell _F;
pointer F = &_F;		/* special cell representing #f */
struct cell _EOF_OBJ;
pointer EOF_OBJ = &_EOF_OBJ;	/* special cell representing end-of-file */
pointer oblist = &_NIL;	/* pointer to symbol table */
pointer global_env;		/* pointer to global environment */
struct cell _ZERO;		/* special cell representing integer 0 */
struct cell _ONE;		/* special cell representing integer 1 */

pointer inport = &_NIL;		/* pointer to current-input-port */
pointer outport = &_NIL;	/* pointer to current-output-port */

pointer winders = &_NIL;	/* pointer to winders list */

#ifdef USE_COPYING_GC
pointer gcell_list = &_NIL;	/* pointer to cell table */
#define gcell_next(p) car((p) + 1)
#endif

/* global pointers to special symbols */
pointer LAMBDA;			/* pointer to syntax lambda */
pointer QUOTE;			/* pointer to syntax quote */

pointer QQUOTE;			/* pointer to symbol quasiquote */
pointer UNQUOTE;		/* pointer to symbol unquote */
pointer UNQUOTESP;		/* pointer to symbol unquote-splicing */

pointer free_cell = &_NIL;	/* pointer to top of free cells */
long    fcells = 0;		/* # of free cells */

FILE   *srcfp;			/* source file */

jmp_buf error_jmp;

char    gc_verbose;		/* if gc_verbose is not zero, print gc status */

void gc(register pointer *a, register pointer *b);
void FatalError(char *s);

#ifndef USE_SCHEME_STACK
#define dump_prev(p)  car(p)
#define dump_next(p)  cdr(p)
#define dump_op(p)    car((p) + 1)
#define dump_args(p)  cdr((p) + 1)
#define dump_envir(p) car((p) + 2)
#define dump_code(p)  cdr((p) + 2)

pointer dump_base; /* pointer to base of allocated dump stack */
#endif

#ifdef USE_COPYING_GC
pointer *sink[2];
pointer **psink = sink;
#define push_sink(x) (*psink++ = (x))
#define pop_sink() (--psink)
#else
#define push_sink(x)
#define pop_sink()
#endif

/* allocate new cell segment */
#ifdef USE_COPYING_GC
pointer from_space;
pointer to_space;

void alloc_cellseg()
{
	fcells = CELL_SEGSIZE;
	free_cell = from_space = cell_seg;
	to_space = cell_seg + CELL_SEGSIZE;
}
#else
void alloc_cellseg()
{
	register pointer p;
	register long i;

	p = free_cell = cell_seg;
	fcells += CELL_SEGSIZE;
	for (i = 0; i < CELL_SEGSIZE - 1; i++, p++) {
		type(p) = 0;
		car(p) = NIL;
		cdr(p) = p + 1;
	}
	type(p) = 0;
	car(p) = NIL;
	cdr(p) = NIL;
}
#endif

/* get new cell.  parameter a, b is marked by gc. */
pointer get_cell(register pointer *a, register pointer *b)
{
#ifndef USE_COPYING_GC
	register pointer x;
#endif

	if (fcells == 0) {
		gc(a, b);
		if (fcells == 0) {
			FatalError("run out of cells --- unable to recover cells");
		}
	}

#ifdef USE_COPYING_GC
	--fcells;
	return free_cell++;
#else
	x = free_cell;
	free_cell = cdr(x);
	--fcells;
	return x;
#endif
}

#ifdef USE_COPYING_GC
pointer find_consecutive_cells(int n)
{
	if (fcells >= n) {
		pointer p = free_cell;

		free_cell += n;
		fcells -= n;
		return p;
	}
	return NIL;
}
#else
pointer find_consecutive_cells(int n)
{
	pointer *pp = &free_cell;

	while (*pp != NIL) {
		pointer p = *pp;
		int cnt;
		for (cnt = 1; cnt < n; cnt++) {
			if (cdr(p) != p + 1) {
				break;
			}
			p = cdr(p);
		}
		if (cnt >= n) {
			pointer x = *pp;
			*pp = cdr(*pp + n - 1);
			fcells -= n;
			return x;
		}
		pp = &cdr(*pp + cnt - 1);
	}
	return NIL;
}
#endif

pointer get_consecutive_cells(int n, pointer *a)
{
	pointer x;

	x = find_consecutive_cells(n);
	if (x == NIL) {
		gc(a, &NIL);
		x = find_consecutive_cells(n);
		if (x == NIL) {
			FatalError("run out of cells  --- unable to recover consecutive cells");
		}
	}
	return x;
}

/* get new cons cell */
pointer cons(pointer a, pointer b)
{
	register pointer x = get_cell(&a, &b);

	type(x) = T_PAIR;
	car(x) = a;
	cdr(x) = b;
	return x;
}

pointer mk_character(register int c)
{
	register pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_CHARACTER | T_ATOM);
	ivalue(x) = c;
	set_num_integer(x);
	return x;
}

pointer mk_integer(register long num)
{
	register pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	ivalue(x) = num;
	set_num_integer(x);
	return x;
}

pointer mk_real(register double num)
{
	register pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	rvalue(x) = num;
	set_num_real(x);
	return x;
}

/* get number atom */
pointer mk_number(struct cell *v)
{
	return v->_isfixnum ? mk_integer(ivalue(v)) : mk_real(rvalue(v));
}

/* get new string */
pointer mk_string(const char *str)
{
	size_t len = strlen(str);
	pointer x = get_consecutive_cells(2 + len / sizeof (struct cell), &NIL);

#ifdef USE_COPYING_GC
	strvalue(x) = (char *)(x + 1);
#else
	x += 1 + len / sizeof(struct cell);
	strvalue(x) = (char *)(x - (1 + len / sizeof(struct cell)));
#endif
	type(x) = (T_STRING | T_ATOM);
	strlength(x) = len;
	snprintf(strvalue(x), len + 1, "%s", str);
	return x;
}

pointer mk_empty_string(size_t len, char fill)
{
	pointer x = get_consecutive_cells(2 + len / sizeof(struct cell), &NIL);

#ifdef USE_COPYING_GC
	strvalue(x) = (char *)(x + 1);
#else
	x += 1 + len / sizeof(struct cell);
	strvalue(x) = (char *)(x - (1 + len / sizeof(struct cell)));
#endif
	type(x) = (T_STRING | T_ATOM);
	strlength(x) = len;
	memset(strvalue(x), fill, len);
	strvalue(x)[len] = 0;
	return x;
}

/* get new symbol */
pointer mk_symbol(char *name)
{
	register pointer x, y = NIL;

	/* fisrt check oblist */
	for (x = oblist; x != NIL; y = x, x = cdr(x)) {
		if (!strcmp(name, symname(car(x)))) {
			if (y != NIL) {
				cdr(y) = cdr(x);
				cdr(x) = oblist;
				oblist = x;
			}
			return car(x);
		}
	}

	x = cons(mk_string(name), NIL);
	type(x) = T_SYMBOL;
	oblist = cons(x, oblist);
	return car(oblist);
}

/* get new uninterned-symbol */
pointer mk_uninterned_symbol(char *name)
{
	register pointer x;

	x = cons(mk_string(name), NIL);
	type(x) = T_SYMBOL;
	return x;
}

pointer gensym()
{
	char name[40];
	static unsigned long gensym_cnt;

	snprintf(name, 40, "gensym-%lu", gensym_cnt++);
	return mk_uninterned_symbol(name);
}

/* make symbol or number atom from string */
pointer mk_atom(char *q)
{
	char c, *p;
	int has_dec_point = 0;
	int has_fp_exp = 0;

	p = q;
	c = *p++;
	if ((c == '+') || (c == '-')) {
		c = *p++;
		if (c == '.') {
			has_dec_point = 1;
			c = *p++;
		}
		if (!isdigit(c)) {
			return mk_symbol(q);
		}
	} else if (c == '.') {
		has_dec_point = 1;
		c = *p++;
		if (!isdigit(c)) {
			return mk_symbol(q);
		}
	} else if (!isdigit(c)) {
		return mk_symbol(q);
	}

	for ( ; (c = *p) != 0; ++p) {
		if (!isdigit(c)) {
			if (c == '.') {
				if (!has_dec_point) {
					has_dec_point = 1;
					continue;
				}
			} else if ((c == 'e') || (c == 'E')) {
				if (!has_fp_exp) {
					has_fp_exp = 1;
					has_dec_point = 1;
					p++;
					if ((*p == '-') || (*p == '+') || isdigit(*p)) {
					continue;
					}
				}
			}
			return mk_symbol(q);
		}
	}
	if (has_dec_point) {
		return mk_real(atof(q));
	}
	return mk_integer(atol(q));
}

/* make constant */
pointer mk_const(char *name)
{
	long    x;
	char    tmp[256];

	if (!strcmp(name, "t"))
		return T;
	else if (!strcmp(name, "f"))
		return F;
	else if (*name == 'o') {/* #o (octal) */
		sprintf(tmp, "0%s", &name[1]);
		sscanf(tmp, "%lo", &x);
		return mk_integer(x);
	} else if (*name == 'd') {	/* #d (decimal) */
		sscanf(&name[1], "%ld", &x);
		return mk_integer(x);
	} else if (*name == 'x') {	/* #x (hex) */
		sprintf(tmp, "0x%s", &name[1]);
		sscanf(tmp, "%lx", &x);
		return mk_integer(x);
	} else if (*name == 'b') {	/* #b (binary) */
		x = 0;
		for (name++; *name == '0' || *name == '1'; name++) {
			x <<= 1;
			x += *name - '0';
		}
		return mk_integer(x);
	} else if (*name == '\\') { /* #\w (character) */
		if (stricmp(name + 1, "space") == 0) {
			return mk_character(' ');
		} else if (stricmp(name + 1, "newline") == 0) {
			return mk_character('\n');
		} else if (stricmp(name + 1, "return") == 0) {
			return mk_character('\r');
		} else if (stricmp(name + 1, "tab") == 0) {
			return mk_character('\t');
		} else if (name[1] == 'x' && name[2] != 0) {
			int c = 0;
			if (sscanf(name + 2, "%x", (unsigned int *)&c) == 1 && c < UCHAR_MAX) {
				return mk_character(c);
			} else {
				return NIL;
			}
		} else if (name[2] == 0) {
			return mk_character(name[1]);
		} else {
			return NIL;
		}
	} else
		return NIL;
}

pointer mk_port(FILE *fp, int prop)
{
	pointer x = get_consecutive_cells(2, &NIL);

	type(x + 1) = type(x) = (T_PORT | T_ATOM);
	(x + 1)->_isfixnum = x->_isfixnum = prop | port_file;
	port_file(x) = fp;
#ifdef USE_COPYING_GC
	gcell_next(x) = gcell_list;
	gcell_list = x;
#endif
	return x;
}

pointer mk_port_string(pointer p, int prop)
{
	pointer x = get_cell(&p, &NIL);

	type(x) = (T_PORT | T_ATOM);
	x->_isfixnum = prop | port_string;
	port_file(x) = (FILE *)p;
	port_curr(x) = strvalue(p);
	return x;
}

void fill_vector(pointer v, pointer a)
{
	int i;
	int n = 1 + ivalue(v) / 2 + ivalue(v) % 2;

	for (i = 1; i < n; i++) {
		type(v + i) = T_PAIR;
		cdr(v + i) = car(v + i) = a;
	}
}

pointer mk_vector(int len)
{
	int n = 1 + len / 2 + len % 2;
	pointer x = get_consecutive_cells(n, &NIL);

	type(x) = (T_VECTOR | T_ATOM);
	ivalue(x) = len;
	set_num_integer(x);
	fill_vector(x, NIL);
	return x;
}

pointer vector_elem(pointer v, int i)
{
	pointer x = v + 1 + i / 2;

	if (i % 2 == 0) {
		return car(x);
	} else {
		return cdr(x);
	}
}

pointer set_vector_elem(pointer v, int i, pointer a)
{
	pointer x = v + 1 + i / 2;

	if (i % 2 == 0) {
		return car(x) = a;
	} else {
		return cdr(x) = a;
	}
}

#ifndef USE_SCHEME_STACK
/* get dump stack */
pointer mk_dumpstack(pointer next)
{
	pointer x = get_consecutive_cells(3, &next);

	type(x) = T_PAIR;
	type(x + 1) = T_NUMBER;
	type(x + 2) = T_NUMBER;
	car(x) = NIL;
	cdr(x) = next;
	return x;
}
#endif

/* ========== garbage collector ========== */
#ifdef USE_COPYING_GC
pointer next;

pointer forward(pointer x)
{
	if (x < from_space || from_space + CELL_SEGSIZE <= x) {
		return x;
	}

	if (type(x) == T_FORWARDED) {
		return x->_object._forwarded;
	}

	*next = *x;
	type(x) = T_FORWARDED;
	x->_object._forwarded = next;
	if (is_string(next)) {
		int i;
		int n = 1 + strlength(next) / sizeof(struct cell);
		strvalue(next) = (char *)(next + 1);
		for (i = 0; i < n; i++) {
			*++next = *++x;
		}
		return next++ - n;
	} else if (is_fileport(next)) {
		*++next = *++x;
		type(x) = T_FORWARDED;
		x->_object._forwarded = next;
		return next++ - 1;
	} else if (is_vector(next)) {
		int i;
		int n = ivalue(next) / 2 + ivalue(next) % 2;
		for (i = 0; i < n; i++) {
			*++next = *++x;
			type(x) = T_FORWARDED;
			x->_object._forwarded = next;
		}
		return next++ - n;
	}
	return next++;
}

void gc(register pointer *a, register pointer *b)
{
	register pointer scan, **pp;
	register pointer p, q;
	char temp[32];

	if (gc_verbose)
		printf("gc...");

	scan = next = to_space;

	/* forward system globals */
	oblist = forward(oblist);
	global_env = forward(global_env);
	inport = forward(inport);
	outport = forward(outport);
	winders = forward(winders);

	/* forward special symbols */
	LAMBDA = forward(LAMBDA);
	QUOTE = forward(QUOTE);
	QQUOTE = forward(QQUOTE);
	UNQUOTE = forward(UNQUOTE);
	UNQUOTESP = forward(UNQUOTESP);

	/* forward current registers */
	args = forward(args);
	envir = forward(envir);
	code = forward(code);
#ifndef USE_SCHEME_STACK
	for (p = dump_base; p != dump; p = dump_prev(p)) {
		register pointer q = forward(p);
		forward(p + 1);
		forward(p + 2);
		dump_args(q) = forward(dump_args(q));
		dump_envir(q) = forward(dump_envir(q));
		dump_code(q) = forward(dump_code(q));
	}
	for ( ; p != NIL; p = dump_prev(p)) {
		forward(p);
		forward(p + 1);
		forward(p + 2);
	}
	dump_base = forward(dump_base);
#endif
	dump = forward(dump);

	for (pp = sink; pp != psink; pp++) {
		**pp = forward(**pp);
	}

	/* forward variables a, b */
	*a = forward(*a);
	*b = forward(*b);

	while (scan < next) {
		switch (type(scan) & 0x07ff) {
		case T_NUMBER:
		case T_PROC:
		case T_CHARACTER:
		case T_VECTOR:
			break;
		case T_STRING:
			scan += 1 + strlength(scan) / sizeof(struct cell);
			break;
		case T_PORT:
			if (is_strport(scan)) {
				size_t curr_len = port_curr(scan) - strvalue(car(scan));
				car(scan) = forward(car(scan));
				port_curr(scan) = strvalue(car(scan)) + curr_len;
			}
			break;
		case T_SYMBOL:
		case T_SYNTAX | T_SYMBOL:
		case T_PAIR:
		case T_CLOSURE:
		case T_CONTINUATION:
			car(scan) = forward(car(scan));
			cdr(scan) = forward(cdr(scan));
			break;
		default:
			sprintf(temp, "GC: Unknown type %d", type(scan));
			FatalError(temp);
			break;
		}
		++scan;
	}

	for (p = gcell_list, gcell_list = NIL; p != NIL; ) {
		if (type(p) == T_FORWARDED) {
			q = p->_object._forwarded;
			p = gcell_next(q);
			gcell_next(q) = gcell_list;
			gcell_list = q;
		} else {
			if (port_file(p) != NULL) {
				if (is_fileport(p)) {
					fclose(port_file(p));
				}
			}
			p = gcell_next(p);
		}
	}

	fcells = CELL_SEGSIZE - (scan - to_space);
	free_cell = scan;

	if (from_space == cell_seg) {
		from_space = cell_seg + CELL_SEGSIZE;
		to_space = cell_seg;
	} else {
		from_space = cell_seg;
		to_space = cell_seg + CELL_SEGSIZE;
	}

	if (gc_verbose)
		printf(" done %ld cells are recovered.\n", fcells);
}

#else /* USE_COPYING_GC */

/*--
 *  We use algorithm E (Kunuth, The Art of Computer Programming Vol.1,
 *  sec.3.5) for marking.
 */
void mark(register pointer p)
{
	register pointer t = 0, q;

E2:	setmark(p);
	if (is_port(p)) {
		if (is_fileport(p)) {
			setmark(p + 1);
		} else {
			mark(car(p));
		}
	} else if (is_vector(p)) {
		int i;
		int n = 1 + ivalue(p) / 2 + ivalue(p) % 2;
		for (i = 1; i < n; i++) {
			mark(p + i);
		}
	}
	if (is_atom(p))
		goto E6;
	q = car(p);
	if (q && !is_mark(q)) {
		setatom(p);
		car(p) = t;
		t = p;
		p = q;
		goto E2;
	}
E5:	q = cdr(p);
	if (q && !is_mark(q)) {
		cdr(p) = t;
		t = p;
		p = q;
		goto E2;
	}
E6:	if (!t)
		return;
	q = t;
	if (is_atom(q)) {
		clratom(q);
		t = car(q);
		car(q) = p;
		p = q;
		goto E5;
	} else {
		t = cdr(q);
		cdr(q) = p;
		p = q;
		goto E6;
	}
}


/* garbage collection. parameter a, b is marked. */
void gc(register pointer *a, register pointer *b)
{
	register pointer p;

	if (gc_verbose)
		printf("gc...");

	/* mark system globals */
	mark(oblist);
	mark(global_env);
	mark(inport);
	mark(outport);
	mark(winders);

	/* mark current registers */
	mark(args);
	mark(envir);
	mark(code);
#ifndef USE_SCHEME_STACK
	for (p = dump_base; p != dump; p = dump_prev(p)) {
		setmark(p);
		setmark(p + 1);
		setmark(p + 2);
		mark(dump_args(p));
		mark(dump_envir(p));
		mark(dump_code(p));
	}
	for ( ; p != NIL; p = dump_prev(p)) {
		setmark(p);
		setmark(p + 1);
		setmark(p + 2);
	}
#else
	mark(dump);
#endif

	/* mark variables a, b */
	mark(*a);
	mark(*b);

	/* garbage collect */
	clrmark(NIL);
	fcells = 0;
	free_cell = NIL;
	p = cell_seg + CELL_SEGSIZE;
	while (--p >= cell_seg) {
		if (is_mark(p)) {
			clrmark(p);
			if (is_string(p)) {
				p = (pointer)strvalue(p);
			}
		} else {
			if (is_string(p)) {
				pointer q = (pointer)strvalue(p);
				do {
					type(p) = 0;
					cdr(p) = free_cell;
					car(p) = NIL;
					free_cell = p;
					++fcells;
				} while (--p > q);
			} else if (is_fileport(p)) {
				type(p) = 0;
				cdr(p) = free_cell;
				car(p) = NIL;
				free_cell = p;
				++fcells;
				if (port_file(--p) != NULL) {
					fclose(port_file(p));
				}
			}
			type(p) = 0;
			cdr(p) = free_cell;
			car(p) = NIL;
			free_cell = p;
			++fcells;
		}
	}

	if (gc_verbose)
		printf(" done %ld cells are recovered.\n", fcells);
}
#endif /* USE_COPYING_GC */

/* ========== Rootines for Ports ========== */

pointer port_from_filename(const char *filename, int prop)
{
	FILE *fp = NULL;

	if (prop == port_input) {
		fp = fopen(filename, "r");
	} else if (prop == port_output) {
		fp = fopen(filename, "w");
	} else if (prop == (port_input | port_output)) {
		fp = fopen(filename, "a+");
	}
	if (fp == NULL) {
		return NIL;
	}
	return mk_port(fp, prop);
}

#define BLOCK_SIZE 256

pointer port_from_scratch()
{
	return mk_port_string(mk_empty_string(BLOCK_SIZE, '\0'), port_output);
}

pointer port_from_string(const char *str, int prop)
{
	return mk_port_string(mk_string(str), prop);
}

pointer realloc_port_string(pointer p)
{
	size_t curr_len = port_curr(p) - strvalue(car(p));
	size_t new_size = strlength(car(p)) + BLOCK_SIZE;
	pointer x = get_consecutive_cells(2 + new_size / sizeof(struct cell), &p);

#ifdef USE_COPYING_GC
	strvalue(x) = (char *)(x + 1);
#else
	x += 1 + new_size / sizeof(struct cell);
	strvalue(x) = (char *)(x - (1 + new_size / sizeof(struct cell)));
#endif
	type(x) = (T_STRING | T_ATOM);
	strlength(x) = new_size;
	memcpy(strvalue(x), strvalue(car(p)), strlength(car(p)));
	memset(strvalue(x) + strlength(car(p)), 0, BLOCK_SIZE);
	car(p) = x;
	port_curr(p) = strvalue(x) + curr_len;
	return p;
}

void port_close(pointer p)
{
	if (port_file(p) != NULL) {
		if (is_fileport(p)) {
			fclose(port_file(p));
		}
		port_file(p) = NULL;
	}
}

/* ========== Rootines for Reading ========== */

#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP   10
#define TOK_VEC     11

char    strbuff[256];

/* get new character from input file */
int inchar()
{
	int c;

	if (is_fileport(inport)) {
		if (feof(port_file(inport))) {
			fclose(port_file(inport));
			port_file(inport) = NULL;
			return EOF;
		}

		c = fgetc(port_file(inport));
		if (c == EOF) {
			if (port_file(inport) == stdin) {
				fprintf(stderr, "Good-bye\n");
				port_file(inport) = NULL;
			}
		}
	} else {
		if (port_curr(inport) == strvalue(car(inport)) + strlength(car(inport))) {
			port_file(inport) = NULL;
			c = EOF;
		} else {
			c = *(port_curr(inport)++);
		}
	}
	return c;
}

/* back to standard input */
void flushinput()
{
	if (is_fileport(inport) && port_file(inport) != stdin) {
		fclose(port_file(inport));
		port_file(inport) = stdin;
	} else if (is_strport(inport)) {
		port_file(inport) = stdin;
		inport->_isfixnum = port_input | port_file;
	}
}

/* check c is delimiter */
int isdelim(char *s, char c)
{
	if (c == EOF) return 0;
	while (*s)
		if (*s++ == c)
			return 0;
	return 1;
}

/* back character to input buffer */
void backchar(int c)
{
	if (c != EOF) {
		if (is_fileport(inport)) {
			ungetc(c, port_file(inport));
		} else if (port_curr(inport) != strvalue(car(inport))) {
			--port_curr(inport);
		}
	}
}

void putstr(const char *s)
{
	if (is_fileport(outport)) {
		fputs(s, port_file(outport));
	} else {
		char *endp = strvalue(car(outport)) + strlength(car(outport));
		while (*s) {
			if (port_curr(outport) < endp) {
				*port_curr(outport)++ = *s++;
				if (port_curr(outport) == endp) {
					outport = realloc_port_string(outport);
					endp = strvalue(car(outport)) + strlength(car(outport));
				}
			}
		}
	}
}

void putcharacter(const int c)
{
	if (is_fileport(outport)) {
		fputc(c, port_file(outport));
	} else {
		char *endp = strvalue(car(outport)) + strlength(car(outport));
		if (port_curr(outport) < endp) {
			*port_curr(outport)++ = c;
			if (port_curr(outport) == endp) {
				outport = realloc_port_string(outport);
			}
		}
	}
}

/* read chacters to delimiter */
char *readstr(char *delim)
{
	char   *p = strbuff;

	while (p - strbuff < sizeof(strbuff) && isdelim(delim, (*p++ = inchar())))
		;
	if (p == strbuff + 2 && p[-2] == '\\') {
		*p = 0;
	} else {
		backchar(*--p);
		*p = '\0';
	}
	return strbuff;
}

/* read string expression "xxx...xxx" */
pointer readstrexp()
{
	char *p = strbuff;
	int c, c1 = 0;
	enum { st_ok, st_bsl, st_x1, st_x2, st_oct1, st_oct2 } state = st_ok;

	for (;;) {
		c = inchar();
		if (c == EOF || p - strbuff > sizeof(strbuff) - 1) {
			return F;
		}
		if (state == st_ok) {
			switch (c) {
			case '\\':
				state = st_bsl;
				break;
			case '"':
				*p = 0;
				return mk_string(strbuff);
			default:
				*p++ = c;
				break;
			}
		} else if (state == st_bsl) {
			switch (c) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				state = st_oct1;
				c1 = c - '0';
				break;
			case 'x':
			case 'X':
				state = st_x1;
				c1 = 0;
				break;
			case 'n':
				*p++ = '\n';
				state = st_ok;
				break;
			case 't':
				*p++ = '\t';
				state = st_ok;
				break;
			case 'r':
				*p++ = '\r';
				state = st_ok;
				break;
			case '"':
				*p++ = '"';
				state = st_ok;
				break;
			default:
				*p++ = c;
				state = st_ok;
				break;
			}
		} else if (state == st_x1 || state == st_x2) {
			c = toupper(c);
			if (c >= '0' && c <= 'F') {
				if (c <= '9') {
					c1 = (c1 << 4) + c - '0';
				} else {
					c1 = (c1 << 4) + c - 'A' + 10;
				}
				if (state == st_x1) {
					state = st_x2;
				} else {
					*p++ = c1;
					state = st_ok;
				}
			} else {
				return F;
			}
		} else {
			if (c < '0' || c > '7') {
				*p++ = c1;
				backchar(c);
				state = st_ok;
			} else {
				if (state == st_oct2 && c1 >= 32) {
					return F;
				}
				c1 = (c1 << 3) + (c - '0');
				if (state == st_oct1) {
					state = st_oct2;
				} else {
					*p++ = c1;
					state = st_ok;
				}
			}
		}
	}
}

/* skip white characters */
int skipspace()
{
	int c;

	while (isspace(c = inchar()))
		;
	backchar(c);
	return c;
}

/* get token */
int token()
{
	int c = skipspace();
	if (c == EOF) {
		return TOK_EOF;
	}
	switch (c = inchar()) {
	case '(':
		return TOK_LPAREN;
	case ')':
		return TOK_RPAREN;
	case '.':
		return TOK_DOT;
	case '\'':
		return TOK_QUOTE;
	case ';':
		while ((c = inchar()) != '\n' && c != EOF)
			;
		if (c == EOF) {
			return TOK_EOF;
		}
		return token();
	case '"':
		return TOK_DQUOTE;
	case BACKQUOTE:
		return TOK_BQUOTE;
	case ',':
		if ((c = inchar()) == '@')
			return TOK_ATMARK;
		else {
			backchar(c);
			return TOK_COMMA;
		}
	case '#':
		if ((c = inchar()) == '(') {
			return TOK_VEC;
		} else {
			backchar(c);
			return TOK_SHARP;
		}
	default:
		backchar(c);
		return TOK_ATOM;
	}
}

/* ========== Rootines for Printing ========== */
#define	ok_abbrev(x)	(is_pair(x) && cdr(x) == NIL)

void printslashstring(unsigned char *s)
{
	int d;

	putcharacter('"');
	for ( ; *s; s++) {
		if (*s == 0xff || *s == '"' || *s < ' ' || *s == '\\') {
			putcharacter('\\');
			switch (*s) {
			case '"':
				putcharacter('"');
				break;
			case '\n':
				putcharacter('n');
				break;
			case '\t':
				putcharacter('t');
				break;
			case '\r':
				putcharacter('r');
				break;
			case '\\':
				putcharacter('\\');
				break;
			default:
				putcharacter('x');
				d = *s / 16;
				putcharacter(d < 10 ? d + '0' : d - 10 + 'A');
				d = *s % 16;
				putcharacter(d < 10 ? d + '0' : d - 10 + 'A');
				break;
			}
		} else {
			putcharacter(*s);
		}
	}
	putcharacter('"');
}

char *atom2str(pointer l, int f)
{
	char *p;
	if (l == NIL)
		p = "()";
	else if (l == T)
		p = "#t";
	else if (l == F)
		p = "#f";
	else if (l == EOF_OBJ)
		p = "#<EOF>";
	else if (is_number(l)) {
		p = strbuff;
		if (f <= 1 || f == 10) {
			if (l->_isfixnum) {
				sprintf(p, "%ld", ivalue(l));
			} else {
				sprintf(p, "%.10g", rvalue(l));
				f = strcspn(p, ".e");
				if (p[f] == 0) {
					p[f] = '.';
					p[f + 1] = '0';
					p[f + 2] = 0;
				}
			}
		} else if (f == 16) {
			if (ivalue(l) >= 0)
				sprintf(p, "%lx", ivalue(l));
			else
				sprintf(p, "-%lx", -ivalue(l));
		} else if (f == 8) {
			if (ivalue(l) >= 0)
				sprintf(p, "%lo", ivalue(l));
			else
				sprintf(p, "-%lo", -ivalue(l));
		} else if (f == 2) {
			unsigned long b = (ivalue(l) < 0) ? -ivalue(l) : ivalue(l);
			p = &p[sizeof(strbuff) - 1];
			*p = 0;
			do { *--p = (b & 1) ? '1' : '0'; b >>= 1; } while (b != 0);
			if (ivalue(l) < 0) *--p = '-';
		} else {
			p = NULL;
		}
	} else if (is_string(l)) {
		if (!f) {
			p = strvalue(l);
		} else {
			printslashstring(strvalue(l));
			p = NULL;
		}
	} else if (is_character(l)) {
		int c = ivalue(l);
		p = strbuff;
		if (!f) {
			p[0] = c;
			p[1] = 0;
		} else {
			switch (c) {
			case ' ':
				sprintf(p, "#\\space");
				break;
			case '\n':
				sprintf(p, "#\\newline");
				break;
			case '\r':
				sprintf(p, "#\\return");
				break;
			case '\t':
				sprintf(p, "#\\tab");
				break;
			default:
				if (c < 32) {
					sprintf(p, "#\\x%x", c);
				} else {
					sprintf(p, "#\\%c", c);
				}
				break;
			}
		}
	} else if (is_symbol(l))
		p = symname(l);
	else if (is_proc(l)) {
		p = strbuff;
		sprintf(p, "#<PROCEDURE %ld>", procnum(l));
	} else if (is_port(l)) {
		if (port_file(l) != NULL) {
			p = "#<PORT>";
		} else {
			p = "#<PORT (CLOSED)>";
		}
	} else if (is_closure(l)) {
		if (is_promise(l)) {
			if (is_resultready(l)) {
				p = "#<PROMISE (FORCED)>";
			} else {
				p = "#<PROMISE>";
			}
		} else if (is_macro(l)) {
			p = "#<MACRO>";
		} else {
			p = "#<CLOSURE>";
		}
	} else if (is_continuation(l)) {
		p = "#<CONTINUATION>";
	} else {
		p = "#<ERROR>";
	}
	return p;
}

/* print atoms */
int printatom(pointer l, int f)
{
	char *p = atom2str(l, f);

	if (p == NULL) {
		return 0;
	}
	if (f < 0) {
		return strlen(p);
	}
	putstr(p);
	return 0;
}


/* ========== Rootines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
pointer mk_closure(pointer c, pointer e)
{
	register pointer x = get_cell(&c, &e);

	type(x) = T_CLOSURE;
	exttype(x) = 0;
	car(x) = c;
	cdr(x) = e;
	return x;
}

/* make continuation. */
pointer mk_continuation(pointer d)
{
	register pointer x = get_cell(&NIL, &d);

	type(x) = T_CONTINUATION;
	car(x) = NIL;
	cont_dump(x) = d;
	return x;
}

/* reverse list -- make new cells */
pointer reverse(pointer a) /* a must be checked by gc */
{
	register pointer p = NIL;

	push_sink(&a);
	for ( ; is_pair(a); a = cdr(a))
		p = cons(car(a), p);
	pop_sink();
	return p;
}

/* reverse list --- no make new cells */
pointer non_alloc_rev(pointer term, pointer list)
{
	register pointer p = list, result = term, q;

	while (p != NIL) {
		q = cdr(p);
		cdr(p) = result;
		result = p;
		p = q;
	}
	return result;
}

/* append list -- make new cells */
pointer append(pointer a, pointer b)
{
	register pointer q;

	if (a != NIL) {
		push_sink(&b);
		a = reverse(a);
		pop_sink();
		while (a != NIL) {
			q = cdr(a);
			cdr(a) = b;
			b = a;
			a = q;
		}
	}
	return b;
}

/* list length */
int list_length(pointer a)
{
	int i = 0;
	pointer slow, fast;

	slow = fast = a;
	while (1) {
		if (fast == NIL)
			return i;
		if (!is_pair(fast))
			return -2 - i;
		fast = cdr(fast);
		++i;
		if (fast == NIL)
			return i;
		if (!is_pair(fast))
			return -2 - i;
		++i;
		fast = cdr(fast);
		slow = cdr(slow);
		if (fast == slow) {
			return -1;
		}
	}
}

/* shared tail */
pointer shared_tail(pointer a, pointer b)
{
	int alen = list_length(a);
	int blen = list_length(b);

	if (alen > blen) {
		while (alen > blen) {
			a = cdr(a);
			--alen;
		}
	} else {
		while (alen < blen) {
			b = cdr(b);
			--blen;
		}
	}

	while (a != b) {
		a = cdr(a);
		b = cdr(b);
	}
	return a;
}

/* equivalence of atoms */
int eqv(register pointer a, register pointer b)
{
	if (is_string(a)) {
		if (is_string(b))
			return (strvalue(a) == strvalue(b));
		else
			return 0;
	} else if (is_number(a)) {
		if (is_number(b))
			return (ivalue(a) == ivalue(b));
		else
			return 0;
	} else if (is_character(a)) {
		if (is_character(b))
			return (ivalue(a) == ivalue(b));
		else
			return 0;
	} else
		return (a == b);
}

/* equivalence of pairs, vectors and strings recursively */
int equal(register pointer a, register pointer b)
{
	if (is_pair(a)) {
		if (is_pair(b))
			return equal(car(a), car(b)) && equal(cdr(a), cdr(b));
		else
			return 0;
	} else if (is_vector(a)) {
		if (is_vector(b))
			if (ivalue(a) == ivalue(b)) {
				int i;
				for (i = 0; i < ivalue(a); i++) {
					if (!equal(vector_elem(a, i), vector_elem(b, i)))
						return 0;
				}
				return 1;
			} else {
				return 0;
			}
		else
			return 0;
	} else if (is_string(a)) {
		if (is_string(b))
			return strcmp(strvalue(a), strvalue(b)) == 0;
		else
			return 0;
	} else {
		return eqv(a, b);
	}
}

/* make cons list for quasiquote */
pointer mcons(pointer f, pointer l, pointer r)
{
	pointer x;

	if (is_pair(r) && car(r) == QUOTE && cadr(r) == cdr(f) &&
		is_pair(l) && car(l) == QUOTE && cadr(l) == cdr(f)) {
		x = cons(f, NIL);
		return cons(QUOTE, x);
	} else {
		args = l;
		x = cons(r, NIL);
		x = cons(args, x);
		return cons(mk_symbol("cons"), x);
	}
}

/* make append list for quasiquote */
pointer mappend(pointer f, pointer l, pointer r)
{
	pointer x;

	if (car(f) == NIL ||
		is_pair(r) && car(l) == QUOTE && cadr(r) == NIL) {
		return l;
	} else {
		args = l;
		x = cons(r, NIL);
		x = cons(args, x);
		return cons(mk_symbol("append"), x);
	}
}

/* greatest common divisor */
int gcd(int a, int b)
{
	int c;
	while (a != 0) {
		c = a;
		a = b % a;
		b = c;
	}
	return abs(b);
}

/* least common multiple */
int lcm(int a, int b)
{
	if (a == 0 || b == 0) {
		return 0;
	}
	return abs(a) / gcd(a, b) * abs(b);
}

/* true or false value macro */
#define istrue(p)       ((p) != F)
#define isfalse(p)      ((p) == F)

/* Error macro */
#define	BEGIN	do {
#define	END	} while (0)

#define Error_0(s) BEGIN                       \
	args = cons(mk_string((s)), NIL);          \
	operator = (short)OP_ERR0;                 \
	goto LOOP; END

#define Error_1(s, a) BEGIN                    \
	args = cons((a), NIL);                     \
	args = cons(mk_string((s)), args);         \
	operator = (short)OP_ERR0;                 \
	goto LOOP; END

/* control macros for Eval_Cycle */
#define s_goto(a) BEGIN                        \
	operator = (short)(a);                     \
	goto a; END

#ifndef USE_SCHEME_STACK

#define s_save(a, b, c) BEGIN                  \
	if (dump_prev(dump) == NIL) {              \
		dump_prev(dump) = mk_dumpstack(dump);  \
	}                                          \
	dump_op(dump) = (pointer)(a);              \
	dump_args(dump) = (b);                     \
	dump_envir(dump) = envir;                  \
	dump_code(dump) = (c);                     \
	dump = dump_prev(dump); END

#define s_return(a) BEGIN                      \
	value = (a);                               \
	dump = dump_next(dump);                    \
	operator = (short)dump_op(dump);           \
	args = dump_args(dump);                    \
	envir = dump_envir(dump);                  \
	code = dump_code(dump);                    \
	goto LOOP; END

#define s_next_op() ((short)dump_op(dump_next(dump)))

pointer s_clone(pointer d) {
	pointer p;

	if (d == NIL) return dump_base;

	p = s_clone(cddddr(d));
	dump_op(p) = (pointer)ivalue(car(d));
	dump_args(p) = cadr(d);
	dump_envir(p) = caddr(d);
	dump_code(p) = cadddr(d);
	return dump_prev(p);
}

pointer s_clone_save() {
	pointer p = NIL, d;

	push_sink(&d);
	for (d = dump_base; d != dump; d = dump_prev(d)) {
		p = cons(dump_code(d), p);
		p = cons(dump_envir(d), p);
		args = cons(dump_args(d), p);
		p = mk_integer((long)dump_op(d));
		p = cons(p, args);
	}
	pop_sink();
	return p;
}

#else

#define s_save(a, b, c)  (                     \
	dump = cons((c), dump),                    \
	dump = cons(envir, dump),                  \
	dump = cons((b), dump),                    \
	x = mk_integer((long)(a)),                 \
	dump = cons(x, dump))

#define s_return(a) BEGIN                      \
	value = (a);                               \
	operator = (short)ivalue(car(dump));       \
	args = cadr(dump);                         \
	envir = caddr(dump);                       \
	code = cadddr(dump);                       \
	dump = cddddr(dump);                       \
	goto LOOP; END

#define s_next_op() ((short)ivalue(car(dump)))

#endif /* USE_SCHEME_STACK */

#define s_retbool(tf)	s_return((tf) ? T : F)

/* ========== Evaluation Cycle ========== */

/* operator code */
enum {
	OP_T0LVL = 0,
	OP_T1LVL,
	OP_READ_INTERNAL,
	OP_VALUEPRINT,
	OP_LOAD,
	OP_EVAL,
	OP_E0ARGS,
	OP_E1ARGS,
	OP_APPLY,
	OP_APPLYCONT,
	OP_DOMACRO,
	OP_GENSYM,

	OP_LAMBDA,
	OP_MKCLOSURE,
	OP_QUOTE,
	OP_QQUOTE0,
	OP_QQUOTE1,
	OP_QQUOTE2,
	OP_QQUOTE3,
	OP_QQUOTE4,
	OP_QQUOTE5,
	OP_QQUOTE6,
	OP_QQUOTE7,
	OP_QQUOTE8,
	OP_QQUOTE9,
	OP_DEF0,
	OP_DEF1,
	OP_DEFP,
	OP_BEGIN,
	OP_IF0,
	OP_IF1,
	OP_SET0,
	OP_SET1,
	OP_LET0,
	OP_LET1,
	OP_LET2,
	OP_LET0AST,
	OP_LET1AST,
	OP_LET2AST,
	OP_LET0REC,
	OP_LET1REC,
	OP_LET2REC,
	OP_DO0,
	OP_DO1,
	OP_DO2,
	OP_DO3,
	OP_DO4,
	OP_DO5,
	OP_COND0,
	OP_COND1,
	OP_DELAY,
	OP_AND0,
	OP_AND1,
	OP_OR0,
	OP_OR1,
	OP_C0STREAM,
	OP_C1STREAM,
	OP_0MACRO,
	OP_DEFMACRO0,
	OP_1MACRO,
	OP_DEFMACRO1,
	OP_CASE0,
	OP_CASE1,
	OP_CASE2,
	OP_WHEN0,
	OP_WHEN1,
	OP_UNLESS0,
	OP_UNLESS1,

	OP_PEVAL,
	OP_PAPPLY,
	OP_MAP0,
	OP_MAP1,
	OP_FOREACH0,
	OP_FOREACH1,
	OP_CONTINUATION,
	OP_VALUES,
	OP_WITHVALUES0,
	OP_WITHVALUES1,
	OP_DYNAMICWIND0,
	OP_DYNAMICWIND1,
	OP_DYNAMICWIND2,
	OP_DYNAMICWIND3,
	OP_DOWINDS0,
	OP_DOWINDS1,
	OP_DOWINDS2,
	OP_DOWINDS3,
	OP_DOWINDS4,
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_ABS,
	OP_QUO,
	OP_REM,
	OP_MOD,
	OP_GCD,
	OP_LCM,
	OP_FLOOR,
	OP_CEILING,
	OP_TRUNCATE,
	OP_ROUND,
	OP_EXP,
	OP_LOG,
	OP_SIN,
	OP_COS,
	OP_TAN,
	OP_ASIN,
	OP_ACOS,
	OP_ATAN,
	OP_SQRT,
	OP_EXPT,
	OP_EX2INEX,
	OP_INEX2EX,
	OP_NUM2STR,
	OP_STR2NUM,
	OP_CAR,
	OP_CDR,
	OP_CONS,
	OP_SETCAR,
	OP_SETCDR,
	OP_CAAR,
	OP_CADR,
	OP_CDAR,
	OP_CDDR,
	OP_CAAAR,
	OP_CAADR,
	OP_CADAR,
	OP_CADDR,
	OP_CDAAR,
	OP_CDADR,
	OP_CDDAR,
	OP_CDDDR,
	OP_CAAAAR,
	OP_CAAADR,
	OP_CAADAR,
	OP_CAADDR,
	OP_CADAAR,
	OP_CADADR,
	OP_CADDAR,
	OP_CADDDR,
	OP_CDAAAR,
	OP_CDAADR,
	OP_CDADAR,
	OP_CDADDR,
	OP_CDDAAR,
	OP_CDDADR,
	OP_CDDDAR,
	OP_CDDDDR,
	OP_LIST,
	OP_LISTTAIL,
	OP_LISTREF,
	OP_LASTPAIR,
	OP_CHAR2INT,
	OP_INT2CHAR,
	OP_CHARUPCASE,
	OP_CHARDNCASE,
	OP_MKSTRING,
	OP_STRING,
	OP_STRLEN,
	OP_STRREF,
	OP_STRSET,
	OP_STREQU,
	OP_STRLSS,
	OP_STRGTR,
	OP_STRLEQ,
	OP_STRGEQ,
	OP_STRCIEQU,
	OP_STRCILSS,
	OP_STRCIGTR,
	OP_STRCILEQ,
	OP_STRCIGEQ,
	OP_SUBSTR,
	OP_STRAPPEND,
	OP_STR2LIST,
	OP_LIST2STR,
	OP_STRCOPY,
	OP_STRFILL,
	OP_VECTOR,
	OP_MKVECTOR,
	OP_VECLEN,
	OP_VECREF,
	OP_VECSET,
	OP_VEC2LIST,
	OP_LIST2VEC,
	OP_VECFILL,
	OP_NOT,
	OP_BOOL,
	OP_NULL,
	OP_EOFOBJP,
	OP_ZEROP,
	OP_POSP,
	OP_NEGP,
	OP_ODD,
	OP_EVEN,
	OP_NEQ,
	OP_LESS,
	OP_GRE,
	OP_LEQ,
	OP_GEQ,
	OP_MAX,
	OP_MIN,
	OP_SYMBOL,
	OP_SYM2STR,
	OP_STR2SYM,
	OP_NUMBER,
	OP_STRINGP,
	OP_INTEGER,
	OP_REAL,
	OP_EXACT,
	OP_INEXACT,
	OP_CHAR,
	OP_CHAREQU,
	OP_CHARLSS,
	OP_CHARGTR,
	OP_CHARLEQ,
	OP_CHARGEQ,
	OP_CHARCIEQU,
	OP_CHARCILSS,
	OP_CHARCIGTR,
	OP_CHARCILEQ,
	OP_CHARCIGEQ,
	OP_CHARAP,
	OP_CHARNP,
	OP_CHARWP,
	OP_CHARUP,
	OP_CHARLP,
	OP_PROC,
	OP_PAIR,
	OP_LISTP,
	OP_PORTP,
	OP_INPORTP,
	OP_OUTPORTP,
	OP_VECTORP,
	OP_ENVP,
	OP_EQ,
	OP_EQV,
	OP_EQUAL,
	OP_FORCE,
	OP_FORCED,
	OP_WRITE_CHAR,
	OP_WRITE,
	OP_DISPLAY,
	OP_NEWLINE,
	OP_ERR0,
	OP_ERR1,
	OP_REVERSE,
	OP_APPEND,
	OP_PUT,
	OP_GET,
	OP_QUIT,
	OP_GC,
	OP_GCVERB,
	OP_CALL_INFILE0,
	OP_CALL_INFILE1,
	OP_CALL_OUTFILE0,
	OP_CALL_OUTFILE1,
	OP_CURR_INPORT,
	OP_CURR_OUTPORT,
	OP_WITH_INFILE0,
	OP_WITH_INFILE1,
	OP_WITH_OUTFILE0,
	OP_WITH_OUTFILE1,
	OP_OPEN_INFILE,
	OP_OPEN_OUTFILE,
	OP_OPEN_INOUTFILE,
	OP_OPEN_INSTRING,
	OP_OPEN_OUTSTRING,
	OP_OPEN_INOUTSTRING,
	OP_GET_OUTSTRING,
	OP_CLOSE_INPORT,
	OP_CLOSE_OUTPORT,
	OP_CLOSE_PORT,
	OP_INT_ENV,
	OP_CURR_ENV,

	OP_READ,
	OP_READ_CHAR,
	OP_PEEK_CHAR,
	OP_CHAR_READY,
	OP_SET_INPORT,
	OP_SET_OUTPORT,
	OP_RDSEXPR,
	OP_RDLIST,
	OP_RDDOT,
	OP_RDQUOTE,
	OP_RDQQUOTE,
	OP_RDQQUOTEVEC,
	OP_RDUNQUOTE,
	OP_RDUQTSP,
	OP_RDVEC,

	OP_P0LIST,
	OP_P1LIST,
	OP_PVECFROM,

	OP_LIST_LENGTH,
	OP_MEMQ,
	OP_MEMV,
	OP_MEMBER,
	OP_ASSQ,
	OP_ASSV,
	OP_ASSOC,
	OP_GET_CLOSURE,
	OP_CLOSUREP,
	OP_MACROP,
	OP_MACRO_EXPAND0,
	OP_MACRO_EXPAND1,
	OP_MACRO_EXPAND2,
	OP_ATOMP,
};

#define TST_NONE 0
#define TST_ANY "\001"
#define TST_STRING "\002"
#define TST_SYMBOL "\003"
#define TST_PORT "\004"
#define TST_INPORT "\005"
#define TST_OUTPORT "\006"
#define TST_ENVIRONMENT "\007"
#define TST_PAIR "\010"
#define TST_LIST "\011"
#define TST_CHAR "\012"
#define TST_VECTOR "\013"
#define TST_NUMBER "\014"
#define TST_INTEGER "\015"
#define TST_NATURAL "\016"

char msg[256];

int validargs(char *name, int min_arity, int max_arity, char *arg_tests)
{
	pointer x;
	int n = 0, i = 0;

	for (x = args; is_pair(x); x = cdr(x)) {
		++n;
	}

	if (n < min_arity) {
		snprintf(msg, sizeof(msg), "%s: needs%s %d argument(s)",
			name, min_arity == max_arity ? "" : " at least", min_arity);
		return 0;
	} else if (n > max_arity) {
		snprintf(msg, sizeof(msg), "%s: needs%s %d argument(s)",
			name, min_arity == max_arity ? "" : " at most", max_arity);
		return 0;
	} else if (arg_tests) {
		for (x = args; i++ < n; x = cdr(x)) {
			switch (arg_tests[0]) {
			case '\001': /* TST_ANY */
				break;
			case '\002': /* TST_STRING */
				if (!is_string(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: string", name, i);
					return 0;
				}
				break;
			case '\003': /* TST_SYMBOL */
				if (!is_symbol(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: symbol", name, i);
					return 0;
				}
				break;
			case '\004': /* TST_PORT */
				if (!is_port(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: port", name, i);
					return 0;
				}
				break;
			case '\005': /* TST_INPORT */
				if (!is_inport(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: input port", name, i);
					return 0;
				}
				break;
			case '\006': /* TST_OUTPORT */
				if (!is_outport(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: output port", name, i);
					return 0;
				}
				break;
			case '\007': /* TST_ENVIRONMENT */
				if (!is_environment(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: environment", name, i);
					return 0;
				}
				break;
			case '\010': /* TST_PAIR */
				if (!is_pair(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: pair", name, i);
					return 0;
				}
				break;
			case '\011': /* TST_LIST */
				if (!is_pair(car(x)) && car(x) != NIL) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: pair or '()", name, i);
					return 0;
				}
				break;
			case '\012': // TST_CHAR */
				if (!is_character(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: character", name, i);
					return 0;
				}
				break;
			case '\013': /* TST_VECTOR */
				if (!is_vector(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: vector", name, i);
					return 0;
				}
				break;
			case '\014': /* TST_NUMBER */
				if (!is_number(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: number", name, i);
					return 0;
				}
				break;
			case '\015': /* TST_INTEGER */
				if (!is_integer(car(x))) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: integer", name, i);
					return 0;
				}
				break;
			case '\016': /* TST_NATURAL */
				if (!is_integer(car(x)) || ivalue(car(x)) < 0) {
					snprintf(msg, sizeof(msg), "%s: argument %d must be: non-negative integer", name, i);
					return 0;
				}
				break;
			default:
				break;
			}

			if (arg_tests[1] != 0) { /* last test is replicated as necessary */
				arg_tests++;
			}
		}
	}

	return 1;
}

/* kernel of this intepreter */
int Eval_Cycle(short operator)
{
	FILE *tmpfp;
	int tok;
	int print_flag;
	pointer value;
	pointer x, y;
	struct cell v;
	long w;

LOOP:
	switch (operator) {
	case OP_EVAL:		/* main part of evalution */
OP_EVAL:
		if (is_symbol(code)) {	/* symbol */
			for (x = envir; x != NIL; x = cdr(x)) {
				register pointer z = NIL;
				for (y = car(x); y != NIL; z = y, y = cdr(y)) {
					if (caar(y) == code) {
						if (z != NIL) {
							cdr(z) = cdr(y);
							cdr(y) = car(x);
							car(x) = y;
						}
						s_return(cdar(y));
					}
				}
			}
			Error_1("Unbounded variable", code);
		} else if (is_pair(code)) {
			if (is_syntax(x = car(code))) {	/* SYNTAX */
				code = cdr(code);
				operator = (short)syntaxnum(x);
				goto LOOP;
			} else {/* first, eval top element and eval arguments */
				s_save(OP_E0ARGS, NIL, code);
				code = car(code);
				s_goto(OP_EVAL);
			}
		} else {
			s_return(code);
		}

	case OP_E0ARGS:	/* eval arguments */
		if (is_closure(value) && is_macro(value)) {	/* macro expansion */
			if (exttype(value) & T_DEFMACRO) {
				args = cdr(code);
				code = value;
			} else {
				args = code;
				code = value;
				args = cons(args, NIL);
			}
			s_save(OP_DOMACRO, NIL, NIL);
			s_goto(OP_APPLY);
		} else {
			code = cdr(code);
			s_goto(OP_E1ARGS);
		}

	case OP_E1ARGS:	/* eval arguments */
OP_E1ARGS:
		args = cons(value, args);
		if (is_pair(code)) {	/* continue */
			s_save(OP_E1ARGS, args, cdr(code));
			code = car(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			args = reverse(args);
			code = car(args);
			args = cdr(args);
			s_goto(OP_APPLY);
		}

	case OP_APPLY:		/* apply 'code' to 'args' */
OP_APPLY:
		if (is_proc(code)) {
			operator = (short)procnum(code);	/* PROCEDURE */
			goto LOOP;
		} else if (is_closure(code)) {	/* CLOSURE */
			/* make environment */
			envir = cons(NIL, closure_env(code));
			setenvironment(envir);
			push_sink(&x);
			for (x = car(closure_code(code));
			     is_pair(x); x = cdr(x), args = cdr(args)) {
				if (args == NIL) {
					pop_sink();
					Error_0("Few arguments");
				} else {
					y = cons(car(x), car(args));
					car(envir) = cons(y, car(envir));
				}
			}
			pop_sink();
			if (x == NIL) {
				/*--
				 * if (args != NIL) {
				 * 	Error_0("Many arguments");
				 * }
				 */
			} else if (is_symbol(x)) {
				x = cons(x, args);
				car(envir) = cons(x, car(envir));
			} else {
				Error_0("Syntax error in closure");
			}
			code = cdr(closure_code(code));
			args = NIL;
			s_goto(OP_BEGIN);
		} else if (is_continuation(code)) {	/* CONTINUATION */
			code = cont_dump(code);
			if (winders != car(code)) {
				s_save(OP_APPLYCONT, args, code);
				args = winders;
				code = car(code);
				s_goto(OP_DOWINDS0);
			}
			s_goto(OP_APPLYCONT);
		} else {
			Error_0("Illegal function");
		}

	case OP_APPLYCONT:
OP_APPLYCONT:
#ifndef USE_SCHEME_STACK
		dump = s_clone(cdr(code));
#else
		dump = cdr(code);
#endif
		if (s_next_op() == OP_WITHVALUES1) {
			args = cons(args, NIL);
			type(args) |= T_VALUES;
			s_return(args);
		} else {
			s_return(args != NIL ? car(args) : NIL);
		}
	default:
		break;
	}

	switch (operator) {
	case OP_LOAD:		/* load */
		if (!validargs("load", 1, 1, TST_STRING)) Error_0(msg);
		if (port_file(inport) == stdin) {
			fprintf(port_file(outport), "loading %s\n", strvalue(car(args)));
		}
		srcfp = port_file(inport);
		if ((port_file(inport) = fopen(strvalue(car(args)), "r")) == NULL) {
			port_file(inport) = srcfp;
			Error_1("Unable to open", car(args));
		}
		s_goto(OP_T0LVL);

	case OP_T0LVL:	/* top level */
OP_T0LVL:
		if (port_file(inport) == NULL) {
			if (srcfp == NULL) {
				break;
			}
			port_file(inport) = srcfp;
			srcfp = NULL;
		}
		if (port_file(inport) == stdin) {
			putstr("\n");
		}
#ifndef USE_SCHEME_STACK
		dump = dump_base;
#else
		dump = NIL;
#endif
		envir = global_env;
		s_save(OP_VALUEPRINT, NIL, NIL);
		s_save(OP_T1LVL, NIL, NIL);
		if (port_file(inport) == stdin) {
			printf(prompt);
		}
		s_goto(OP_READ_INTERNAL);

	case OP_T1LVL:	/* top level */
		code = value;
		s_goto(OP_EVAL);

	case OP_READ_INTERNAL:		/* read internal */
OP_READ_INTERNAL:
		tok = token();
		if (tok == TOK_EOF) {
			s_return(EOF_OBJ);
		}
		s_goto(OP_RDSEXPR);

	case OP_VALUEPRINT:	/* print evalution result */
		print_flag = 1;
		args = value;
		if (port_file(inport) == stdin) {
			s_save(OP_T0LVL, NIL, NIL);
			s_goto(OP_P0LIST);
		} else {
			s_goto(OP_T0LVL);
		}

	case OP_DOMACRO:	/* do macro */
		code = value;
		s_goto(OP_EVAL);

	case OP_GENSYM:
		if (!validargs("gensym", 0, 0, TST_NONE)) Error_0(msg);
		s_return(gensym());

	case OP_LAMBDA:	/* lambda */
		s_return(mk_closure(code, envir));

	case OP_MKCLOSURE:	/* make-closure */
		if (!validargs("make-closure", 1, 2, TST_PAIR TST_ENVIRONMENT)) Error_0(msg);
		x = car(args);
		if (car(x) == LAMBDA) {
			x = cdr(x);
		}
		if (cdr(args) == NIL) {
			y = envir;
		}
		else {
			y = cadr(args);
		}
		s_return(mk_closure(x, y));

	case OP_QUOTE:		/* quote */
		s_return(car(code));

	case OP_QQUOTE0:	/* quasiquote */
		args = mk_integer(0);
		code = car(code);
		s_save(OP_QQUOTE9, NIL, NIL);
		/* fall through */

	case OP_QQUOTE1:	/* quasiquote -- expand */
OP_QQUOTE1:
		if (is_vector(code)) {
			s_save(OP_QQUOTE2, NIL, NIL);
			x = NIL;
			for (w = ivalue(code) - 1; w >= 0; w--) {
				x = cons(vector_elem(code, w), x);
			}
			code = x;
			s_goto(OP_QQUOTE1);
		} else if (!is_pair(code)) {
			x = cons(code, NIL);
			s_return(cons(QUOTE, x));
		} else if (QQUOTE == car(code)) {
			s_save(OP_QQUOTE3, NIL, NIL);
			args = mk_integer(ivalue(args) + 1);
			code = cdr(code);
			s_goto(OP_QQUOTE1);
		} else if (ivalue(args) > 0) {
			if (UNQUOTE == car(code)) {
				s_save(OP_QQUOTE4, NIL, NIL);
				args = mk_integer(ivalue(args) - 1);
				code = cdr(code);
				s_goto(OP_QQUOTE1);
			} else if (UNQUOTESP == car(code)) {
				s_save(OP_QQUOTE5, NIL, NIL);
				args = mk_integer(ivalue(args) - 1);
				code = cdr(code);
				s_goto(OP_QQUOTE1);
			} else {
				s_save(OP_QQUOTE6, args, code);
				code = car(code);
				s_goto(OP_QQUOTE1);
			}
		} else {
			if (UNQUOTE == car(code)) {
				s_return(cadr(code));
			} else if (UNQUOTESP == car(code)) {
				Error_1("Unquote-splicing wasn't in a list:", code);
			} else if (is_pair(car(code)) && UNQUOTESP == caar(code)) {
				s_save(OP_QQUOTE8, NIL, code);
				code = cdr(code);
				s_goto(OP_QQUOTE1);
			} else {
				s_save(OP_QQUOTE6, args, code);
				code = car(code);
				s_goto(OP_QQUOTE1);
			}
		}

	case OP_QQUOTE2:	/* quasiquote -- 'vector */
		x = cons(value, NIL);
		x = cons(mk_symbol("vector"), x);
		s_return(cons(mk_symbol("apply"), x));

	case OP_QQUOTE3:	/* quasiquote -- 'quasiquote */
		args = value;
		x = cons(QQUOTE, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, args));

	case OP_QQUOTE4:	/* quasiquote -- 'unquote */
		args = value;
		x = cons(UNQUOTE, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, args));

	case OP_QQUOTE5:	/* quasiquote -- 'unquote-splicing */
		args = value;
		x = cons(UNQUOTESP, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, args));

	case OP_QQUOTE6:	/* quasiquote -- 'cons */
		push_sink(&value);
		s_save(OP_QQUOTE7, value, code);
		pop_sink();
		code = cdr(code);
		s_goto(OP_QQUOTE1);

	case OP_QQUOTE7:	/* quasiquote -- 'cons */
		s_return(mcons(code, args, value));

	case OP_QQUOTE8:	/* quasiquote -- 'append */
		s_return(mappend(code, cadar(code), value));

	case OP_QQUOTE9:	/* quasiquote -- return */
		code = value;
		s_goto(OP_EVAL);

	case OP_DEF0:	/* define */
		if (is_pair(car(code))) {
			y = cons(cdar(code), cdr(code));
			y = cons(LAMBDA, y);
			args = caar(code);
			code = y;
		} else {
			args = car(code);
			code = cadr(code);
		}
		if (!is_symbol(args)) {
			Error_0("Variable is not symbol");
		}
		s_save(OP_DEF1, NIL, args);
		args = NIL;
		s_goto(OP_EVAL);

	case OP_DEF1:	/* define */
		for (x = car(envir); x != NIL; x = cdr(x))
			if (caar(x) == code)
				break;
		if (x != NIL)
			cdar(x) = value;
		else {
			x = cons(code, value);
			car(envir) = cons(x, car(envir));
		}
		s_return(code);

	case OP_DEFP:	/* defined? */
		if (!validargs("defined?", 1, 2, TST_SYMBOL TST_ENVIRONMENT)) Error_0(msg);
		if (cdr(args) != NIL) {
			x = cadr(args);
		} else {
			x = envir;
		}
		for (x = car(x); x != NIL; x = cdr(x))
			if (caar(x) == car(args))
				break;
		s_retbool(x != NIL);

	case OP_SET0:		/* set! */
		s_save(OP_SET1, NIL, car(code));
		code = cadr(code);
		s_goto(OP_EVAL);

	case OP_SET1:		/* set! */
		for (x = envir; x != NIL; x = cdr(x)) {
			register pointer z = NIL;
			for (y = car(x); y != NIL; z = y, y = cdr(y)) {
				if (caar(y) == code) {
					if (z != NIL) {
						cdr(z) = cdr(y);
						cdr(y) = car(x);
						car(x) = y;
					}
					cdar(y) = value;
					s_return(value);
				}
			}
		}
		Error_1("Unbounded variable", code);

	case OP_BEGIN:		/* begin */
OP_BEGIN:
		if (!is_pair(code)) {
			s_return(code);
		}
		if (cdr(code) != NIL) {
			s_save(OP_BEGIN, NIL, cdr(code));
		}
		code = car(code);
		s_goto(OP_EVAL);

	case OP_IF0:		/* if */
		s_save(OP_IF1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_IF1:		/* if */
		if (istrue(value))
			code = car(code);
		else
			code = cadr(code);	/* (if #f 1) ==> () because
						 * car(NIL) = NIL */
		s_goto(OP_EVAL);

	case OP_LET0:		/* let */
		args = NIL;
		value = code;
		code = is_symbol(car(code)) ? cadr(code) : car(code);
		s_goto(OP_LET1);

	case OP_LET1:		/* let (caluculate parameters) */
OP_LET1:
		args = cons(value, args);
		if (is_pair(code)) {	/* continue */
			s_save(OP_LET1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in let :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			args = reverse(args);
			code = car(args);
			args = cdr(args);
			s_goto(OP_LET2);
		}

	case OP_LET2:		/* let */
OP_LET2:
		envir = cons(NIL, envir);
		setenvironment(envir);
		push_sink(&x);
		for (x = is_symbol(car(code)) ? cadr(code) : car(code);
			args != NIL; x = cdr(x), args = cdr(args)) {
			y = cons(caar(x), car(args));
			car(envir) = cons(y, car(envir));
		}
		if (is_symbol(car(code))) {	/* named let */
			for (x = cadr(code), args = NIL; x != NIL; x = cdr(x))
				args = cons(caar(x), args);
			pop_sink();
			y = reverse(args);
			y = cons(y, cddr(code));
			y = mk_closure(y, envir);
			y = cons(car(code), y);
			car(envir) = cons(y, car(envir));
			code = cddr(code);
			args = NIL;
		} else {
			pop_sink();
			code = cdr(code);
			args = NIL;
		}
		s_goto(OP_BEGIN);

	case OP_LET0AST:	/* let* */
		if (car(code) == NIL) {
			envir = cons(NIL, envir);
			setenvironment(envir);
			code = cdr(code);
			s_goto(OP_BEGIN);
		}
		s_save(OP_LET1AST, cdr(code), car(code));
		code = cadaar(code);
		s_goto(OP_EVAL);

	case OP_LET1AST:	/* let* (make new frame) */
		envir = cons(value, envir);	/* save value for gc */
		setenvironment(envir);
#ifdef USE_COPYING_GC
		value = car(envir);
#endif
		car(envir) = NIL;
		s_goto(OP_LET2AST);

	case OP_LET2AST:	/* let* (caluculate parameters) */
OP_LET2AST:
		x = cons(caar(code), value);
		car(envir) = cons(x, car(envir));
		code = cdr(code);
		if (is_pair(code)) {	/* continue */
			s_save(OP_LET2AST, args, code);
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in let* :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			code = args;
			args = NIL;
			s_goto(OP_BEGIN);
		}

	case OP_LET0REC:	/* letrec */
		envir = cons(NIL, envir);
		setenvironment(envir);
		args = NIL;
		value = code;
		code = car(code);
		s_goto(OP_LET1REC);

	case OP_LET1REC:	/* letrec (caluculate parameters) */
OP_LET1REC:
		args = cons(value, args);
		if (is_pair(code)) {	/* continue */
			s_save(OP_LET1REC, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in letrec :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			args = reverse(args);
			code = car(args);
			args = cdr(args);
			s_goto(OP_LET2REC);
		}

	case OP_LET2REC:	/* letrec */
OP_LET2REC:
		push_sink(&x);
		for (x = car(code); args != NIL; x = cdr(x), args = cdr(args)) {
			y = cons(caar(x), car(args));
			car(envir) = cons(y, car(envir));
		}
		pop_sink();
		code = cdr(code);
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_DO0:		/* do */
		envir = cons(NIL, envir);
		setenvironment(envir);
		args = NIL;
		value = code;
		code = car(code);
		/* fall through */

	case OP_DO1:		/* do -- init */
		args = cons(value, args);
		if (is_pair(code)) {
			s_save(OP_DO1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in do :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		}
		args = reverse(args);
		code = car(args);
		args = cdr(args);
		/* fall through */

	case OP_DO2:		/* do -- test */
OP_DO2:
		push_sink(&x);
		for (x = car(code); args != NIL; x = cdr(x), args = cdr(args)) {
			y = cons(caar(x), car(args));
			car(envir) = cons(y, car(envir));
		}
		pop_sink();
		s_save(OP_DO3, NIL, code);
		code = car(cadr(code));
		s_goto(OP_EVAL);

	case OP_DO3:		/* do -- command */
		if (value == F) {
			s_save(OP_DO4, NIL, code);
			code = cddr(code);
		} else {		/* expression */
			code = cdr(cadr(code));
		}
		s_goto(OP_BEGIN);

	case OP_DO4:		/* do -- step */
		value = code;
		code = car(code);
		/* fall through */

	case OP_DO5:		/* do -- step */
		args = cons(value, args);
		if (is_pair(code)) {
			s_save(OP_DO5, args, cdr(code));
			code = car(code);
			if (is_pair(cddr(code))) {
				code = caddr(code);
			} else {
				code = car(code);
			}
			args = NIL;
			s_goto(OP_EVAL);
		}
		envir = cons(NIL, envir);
		setenvironment(envir);
		args = reverse(args);
		code = car(args);
		args = cdr(args);
		s_goto(OP_DO2);

	case OP_COND0:		/* cond */
		if (!is_pair(code)) {
			Error_0("Syntax error in cond");
		}
		s_save(OP_COND1, NIL, code);
		code = caar(code);
		s_goto(OP_EVAL);

	case OP_COND1:		/* cond */
		if (istrue(value)) {
			if ((code = cdar(code)) == NIL) {
				s_return(value);
			}
			s_goto(OP_BEGIN);
		} else {
			if ((code = cdr(code)) == NIL) {
				s_return(NIL);
			} else {
				s_save(OP_COND1, NIL, code);
				code = caar(code);
				s_goto(OP_EVAL);
			}
		}

	case OP_DELAY:		/* delay */
		x = cons(NIL, code);
		x = mk_closure(x, envir);
		setpromise(x);
		s_return(x);

	case OP_AND0:		/* and */
		if (code == NIL) {
			s_return(T);
		}
		s_save(OP_AND1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_AND1:		/* and */
		if (isfalse(value)) {
			s_return(value);
		} else if (code == NIL) {
			s_return(value);
		} else {
			s_save(OP_AND1, NIL, cdr(code));
			code = car(code);
			s_goto(OP_EVAL);
		}

	case OP_OR0:		/* or */
		if (code == NIL) {
			s_return(F);
		}
		s_save(OP_OR1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_OR1:		/* or */
		if (istrue(value)) {
			s_return(value);
		} else if (code == NIL) {
			s_return(value);
		} else {
			s_save(OP_OR1, NIL, cdr(code));
			code = car(code);
			s_goto(OP_EVAL);
		}

	case OP_C0STREAM:	/* cons-stream */
		s_save(OP_C1STREAM, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_C1STREAM:	/* cons-stream */
		args = value;	/* save value to register args for gc */
		x = cons(NIL, code);
		x = mk_closure(x, envir);
		setpromise(x);
		s_return(cons(args, x));

	case OP_0MACRO:		/* macro */
	case OP_DEFMACRO0:	/* define-macro */
		if (is_pair(car(code))) {
			if (!is_symbol(caar(code))) {
				Error_0("Variable is not symbol");
			}
			s_save((operator == OP_0MACRO) ? OP_1MACRO : OP_DEFMACRO1, NIL, caar(code));
			y = cons(cdar(code), cdr(code));
			code = cons(LAMBDA, y);
		} else {
			if (!is_symbol(car(code))) {
				Error_0("Variable is not symbol");
			}
			s_save((operator == OP_0MACRO) ? OP_1MACRO : OP_DEFMACRO1, NIL, car(code));
			code = cadr(code);
		}
		s_goto(OP_EVAL);

	case OP_1MACRO:		/* macro */
	case OP_DEFMACRO1:	/* define-macro */
		exttype(value) |= (operator == OP_1MACRO) ? T_MACRO : (T_MACRO | T_DEFMACRO);
		for (x = car(envir); x != NIL; x = cdr(x))
			if (caar(x) == code)
				break;
		if (x != NIL)
			cdar(x) = value;
		else {
			x = cons(code, value);
			car(envir) = cons(x, car(envir));
		}
		s_return(code);

	case OP_CASE0:		/* case */
		s_save(OP_CASE1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_CASE1:		/* case */
		for (x = code; x != NIL; x = cdr(x)) {
			if (!is_pair(y = caar(x)))
				break;
			for ( ; y != NIL; y = cdr(y))
				if (eqv(car(y), value))
					break;
			if (y != NIL)
				break;
		}
		if (x != NIL) {
			if (is_pair(caar(x))) {
				code = cdar(x);
				s_goto(OP_BEGIN);
			} else {/* else */
				code = car(x);
				s_save(OP_CASE2, NIL, cdr(code));
				code = car(code);
				s_goto(OP_EVAL);
			}
		} else {
			s_return(NIL);
		}

	case OP_CASE2:		/* case */
		if (istrue(value)) {
			s_goto(OP_BEGIN);
		} else {
			s_return(NIL);
		}

	case OP_WHEN0:		/* when */
		s_save(OP_WHEN1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_WHEN1:		/* when */
		if (istrue(value)) {
			s_goto(OP_BEGIN);
		} else {
			s_return(NIL);
		}

	case OP_UNLESS0:	/* unless */
		s_save(OP_UNLESS1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_UNLESS1:	/* unless */
		if (isfalse(value)) {
			s_goto(OP_BEGIN);
		} else {
			s_return(NIL);
		}

	case OP_PAPPLY:	/* apply */
		if (!validargs("apply", 1, 65535, TST_NONE)) Error_0(msg);
		code = car(args);
		args = cadr(args);
		s_goto(OP_APPLY);

	case OP_PEVAL:	/* eval */
		if (!validargs("eval", 1, 1, TST_ANY)) Error_0(msg);
		code = car(args);
		args = NIL;
		s_goto(OP_EVAL);

	case OP_MAP0:	/* map */
		if (!validargs("map", 2, 65535, TST_ANY TST_LIST)) Error_0(msg);
		code = car(args);
		value = 0;

	case OP_MAP1:	/* map */
		if (value == 0) {
			car(args) = NIL;
		} else {
			car(args) = cons(value, car(args));
		}
		y = NIL;
		push_sink(&x);
		for (x = cdr(args); x != NIL; x = cdr(x)) {
			if (caar(x) == NIL) {
				pop_sink();
				s_return(non_alloc_rev(NIL, car(args)));
			}
			y = cons(caar(x), y);
			car(x) = cdar(x);
		}
		pop_sink();
		push_sink(&y);
		s_save(OP_MAP1, args, code);
		pop_sink();
		args = non_alloc_rev(NIL, y);
		s_goto(OP_APPLY);

	case OP_FOREACH0:	/* for-each */
		if (!validargs("for-each", 2, 65535, TST_ANY TST_LIST)) Error_0(msg);
		code = car(args);
		args = cdr(args);

	case OP_FOREACH1:	/* for-each */
		y = NIL;
		push_sink(&x);
		for (x = args; x != NIL; x = cdr(x)) {
			if (caar(x) == NIL) {
				pop_sink();
				s_return(T);
			}
			y = cons(caar(x), y);
			car(x) = cdar(x);
		}
		pop_sink();
		push_sink(&y);
		s_save(OP_FOREACH1, args, code);
		pop_sink();
		args = non_alloc_rev(NIL, y);
		s_goto(OP_APPLY);

	case OP_CONTINUATION:	/* call-with-current-continuation */
		if (!validargs("call-with-current-continuation", 1, 1, TST_NONE)) Error_0(msg);
		code = car(args);
#ifndef USE_SCHEME_STACK
		args = cons(mk_continuation(cons(winders, s_clone_save())), NIL);
#else
		args = cons(mk_continuation(cons(winders, dump)), NIL);
#endif
		s_goto(OP_APPLY);

	case OP_VALUES:			/* values */
		if (!validargs("values", 0, 65535, TST_NONE)) Error_0(msg);
		if (s_next_op() == OP_WITHVALUES1) {
			args = cons(args, NIL);
			type(args) |= T_VALUES;
			s_return(args);
		} else {
			s_return(args != NIL ? car(args) : NIL);
		}

	case OP_WITHVALUES0:	/* call-with-values */
		if (!validargs("call-with-values", 2, 2, TST_NONE)) Error_0(msg);
		s_save(OP_WITHVALUES1, args, code);
		code = car(args);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_WITHVALUES1:	/* call-with-values */
		code = cadr(args);
		if (is_pair(value) && (type(value) & T_VALUES)) {
			type(value) &= ~T_VALUES;
			args = car(value);
		} else {
			args = cons(value, NIL);
		}
		s_goto(OP_APPLY);

	case OP_DYNAMICWIND0:	/* dynamic-wind -- before */
		if (!validargs("dynamic-wind", 3, 3, TST_NONE)) Error_0(msg);
		s_save(OP_DYNAMICWIND1, args, code);
		code = car(args);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_DYNAMICWIND1:	/* dynamic-wind -- body */
		winders = cons(cons(car(args), caddr(args)), winders);
		s_save(OP_DYNAMICWIND2, args, code);
		code = cadr(args);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_DYNAMICWIND2:	/* dynamic-wind -- after */
		winders = cdr(winders);
		s_save(OP_DYNAMICWIND3, cons(value, args), code);
		code = caddr(args);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_DYNAMICWIND3:	/* dynamic-wind -- return */
		s_return(car(args));

	case OP_DOWINDS0:		/* winding -- after, before */
OP_DOWINDS0:
		args = shared_tail(args, code);
		if (winders != args && winders != NIL) {
			s_save(OP_DOWINDS2, args, code);
			code = args;
			args = winders;
			s_goto(OP_DOWINDS1);
		}
		if (args != code && code != NIL) {
			s_goto(OP_DOWINDS2);
		}
		s_return(T);

	case OP_DOWINDS1:		/* winding -- after */
OP_DOWINDS1:
		winders = args;
		if (args != code && args != NIL) {
			s_save(OP_DOWINDS1, cdr(args), code);
			code = cdar(args);
			args = NIL;
			s_goto(OP_APPLY);
		}
		s_return(T);

	case OP_DOWINDS2:		/* winding -- before */
OP_DOWINDS2:
		if (args != code && code != NIL) {
			s_save(OP_DOWINDS3, args, code);
			code = cdr(code);
			s_goto(OP_DOWINDS2);
		}
		winders = code;
		s_return(T);

	case OP_DOWINDS3:		/* winding -- before */
		s_save(OP_DOWINDS4, args, code);
		code = caar(code);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_DOWINDS4:		/* winding -- before */
		winders = code;
		s_return(T);

	case OP_ADD:		/* + */
		if (!validargs("+", 0, 65535, TST_NUMBER)) Error_0(msg);
		for (x = args, v = _ZERO; x != NIL; x = cdr(x)) {
			if (v._isfixnum) {
				if (car(x)->_isfixnum) {
					ivalue(&v) += ivalue(car(x));
				} else {
					rvalue(&v) = ivalue(&v) + rvalue(car(x));
					set_num_real(&v);
				}
			} else {
				rvalue(&v) += nvalue(car(x));
			}
		}
		s_return(mk_number(&v));

	case OP_SUB:		/* - */
		if (!validargs("-", 1, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			v = _ZERO;
			x = args;
		} else {
			v = *car(args);
			x = cdr(args);
		}
		for ( ; x != NIL; x = cdr(x)) {
			if (v._isfixnum) {
				if (car(x)->_isfixnum) {
					ivalue(&v) -= ivalue(car(x));
				} else {
					rvalue(&v) = ivalue(&v) - rvalue(car(x));
					set_num_real(&v);
				}
			} else {
				rvalue(&v) -= nvalue(car(x));
			}
		}
		s_return(mk_number(&v));

	case OP_MUL:		/* * */
		if (!validargs("*", 0, 65535, TST_NUMBER)) Error_0(msg);
		for (x = args, v = _ONE; x != NIL; x = cdr(x)) {
			if (v._isfixnum) {
				if (car(x)->_isfixnum) {
					ivalue(&v) *= ivalue(car(x));
				} else {
					rvalue(&v) = ivalue(&v) * rvalue(car(x));
					set_num_real(&v);
				}
			} else {
				rvalue(&v) *= nvalue(car(x));
			}
		}
		s_return(mk_number(&v));

	case OP_DIV:		/* / */
		if (!validargs("/", 1, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			v = _ONE;
			x = args;
		} else {
			v = *car(args);
			x = cdr(args);
		}
		for ( ; x != NIL; x = cdr(x)) {
			double d = nvalue(car(x));
			if (-DBL_MIN < d && d < DBL_MIN) {
				Error_0("Divided by zero");
			}
			if (v._isfixnum) {
				if (car(x)->_isfixnum && ivalue(&v) % ivalue(car(x)) == 0) {
					ivalue(&v) /= ivalue(car(x));
				} else {
					rvalue(&v) = ivalue(&v) / d;
					set_num_real(&v);
				}
			} else {
				rvalue(&v) /= d;
			}
		}
		s_return(mk_number(&v));

	case OP_ABS:		/* abs */
		if (!validargs("abs", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (x->_isfixnum) {
			s_return(mk_integer(abs(ivalue(x))));
		} else {
			s_return(mk_real(fabs(rvalue(x))));
		}

	case OP_QUO:		/* quotient */
		if (!validargs("quotient", 2, 2, TST_INTEGER)) Error_0(msg);
		v = *car(args);
		x = cadr(args);
		w = x->_isfixnum ? ivalue(x) : (long)rvalue(x);
		if (w == 0) {
			Error_0("Divided by zero");
		}
		if (v._isfixnum) {
			if (x->_isfixnum) {
				ivalue(&v) /= w;
			} else {
				rvalue(&v) = ivalue(&v) / w;
				set_num_real(&v);
			}
		} else {
			rvalue(&v) = (long)rvalue(&v) / w;
		}
		s_return(mk_number(&v));

	case OP_REM:		/* remainder */
		if (!validargs("remainder", 2, 2, TST_INTEGER)) Error_0(msg);
		v = *car(args);
		x = cadr(args);
		w = x->_isfixnum ? ivalue(x) : (long)rvalue(x);
		if (w == 0) {
			Error_0("Divided by zero");
		}
		if (v._isfixnum) {
			if (x->_isfixnum) {
				ivalue(&v) %= w;
			} else {
				rvalue(&v) = ivalue(&v) % w;
				set_num_real(&v);
			}
		} else {
			rvalue(&v) = (long)rvalue(&v) % w;
		}
		s_return(mk_number(&v));

	case OP_MOD:		/* modulo */
		if (!validargs("modulo", 2, 2, TST_INTEGER)) Error_0(msg);
		v = *car(args);
		x = cadr(args);
		w = x->_isfixnum ? ivalue(x) : (long)rvalue(x);
		if (w == 0) {
			Error_0("Divided by zero");
		}
		if (v._isfixnum) {
			if (x->_isfixnum) {
				ivalue(&v) %= w;
				if (ivalue(&v) * w < 0) {
					ivalue(&v) += w;
				}
			} else {
				rvalue(&v) = ivalue(&v) % w;
				set_num_real(&v);
				if (rvalue(&v) * w < 0) {
					rvalue(&v) += w;
				}
			}
		} else {
			rvalue(&v) = (long)rvalue(&v) % w;
			if (rvalue(&v) * w < 0) {
				rvalue(&v) += w;
			}
		}
		s_return(mk_number(&v));

	case OP_GCD:		/* gcd */
		if (!validargs("gcd", 0, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			v = _ZERO;
			x = args;
		} else {
			v = *car(args);
			x = cdr(args);
		}
		for (; x != NIL; x = cdr(x)) {
			if (v._isfixnum) {
				if (car(x)->_isfixnum) {
					ivalue(&v) = gcd(ivalue(&v), ivalue(car(x)));
				} else {
					rvalue(&v) = gcd(ivalue(&v), (long)rvalue(car(x)));
					set_num_real(&v);
				}
			} else {
				rvalue(&v) = gcd((long)rvalue(&v), (long)nvalue(car(x)));
			}
		}
		s_return(mk_number(&v));

	case OP_LCM:		/* lcm */
		if (!validargs("lcm", 0, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			v = _ONE;
			x = args;
		} else {
			v = *car(args);
			x = cdr(args);
		}
		for (; x != NIL; x = cdr(x)) {
			if (v._isfixnum) {
				if (car(x)->_isfixnum) {
					ivalue(&v) = lcm(ivalue(&v), ivalue(car(x)));
				} else {
					rvalue(&v) = lcm(ivalue(&v), (long)rvalue(car(x)));
					set_num_real(&v);
				}
			} else {
				rvalue(&v) = lcm((long)rvalue(&v), (long)nvalue(car(x)));
			}
		}
		s_return(mk_number(&v));

	case OP_FLOOR:		/* floor */
		if (!validargs("floor", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(floor(nvalue(car(args)))));

	case OP_CEILING:	/* ceiling */
		if (!validargs("ceiling", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(ceil(nvalue(car(args)))));

	case OP_TRUNCATE:	/* truncate */
		if (!validargs("truncate", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (nvalue(x) > 0) {
			s_return(mk_real(floor(nvalue(x))));
		} else {
			s_return(mk_real(ceil(nvalue(x))));
		}

	case OP_ROUND:		/* round */
		if (!validargs("round", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (x->_isfixnum) {
			s_return(x);
		} else {
			double fl = floor(rvalue(x));
			double ce = ceil(rvalue(x));
			double dfl = rvalue(x) - fl;
			double dce = ce - rvalue(x);
			if (dfl > dce) {
				s_return(mk_real(ce));
			} else if (dfl < dce) {
				s_return(mk_real(fl));
			} else {
				/* Round to even if midway */
				if (fmod(fl, 2.0) == 0.0) {
					s_return(mk_real(fl));
				} else {
					s_return(mk_real(ce));
				}
			}
		}

	case OP_EXP:		/* exp */
		if (!validargs("exp", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(exp(nvalue(car(args)))));

	case OP_LOG:		/* log */
		if (!validargs("log", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(log(nvalue(car(args)))));

	case OP_SIN:		/* sin */
		if (!validargs("sin", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(sin(nvalue(car(args)))));

	case OP_COS:		/* cos */
		if (!validargs("cos", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(cos(nvalue(car(args)))));

	case OP_TAN:		/* tan */
		if (!validargs("tan", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(tan(nvalue(car(args)))));

	case OP_ASIN:		/* asin */
		if (!validargs("asin", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(asin(nvalue(car(args)))));

	case OP_ACOS:		/* acos */
		if (!validargs("acos", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(acos(nvalue(car(args)))));

	case OP_ATAN:		/* atan */
		if (!validargs("atan", 1, 2, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			s_return(mk_real(atan(nvalue(car(args)))));
		} else {
			s_return(mk_real(atan2(nvalue(car(args)), nvalue(cadr(args)))));
		}

	case OP_SQRT:		/* sqrt */
		if (!validargs("sqrt", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(sqrt(nvalue(car(args)))));

	case OP_EXPT:		/* expt */
		if (!validargs("expt", 2, 2, TST_NUMBER)) Error_0(msg);
		x = car(args);
		y = cadr(args);
		if (nvalue(x) == 0 && nvalue(y) < 0) {
			s_return(&_ZERO);
		}
		rvalue(&v) = pow(nvalue(x), nvalue(y));
		if (x->_isfixnum && y->_isfixnum && rvalue(&v) == (long)rvalue(&v)) {
			s_return(mk_integer((long)rvalue(&v)));
		} else {
			s_return(mk_real(rvalue(&v)));
		}

	case OP_EX2INEX:	/* exact->inexact */
		if (!validargs("exact->inexact", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(nvalue(car(args))));

	case OP_INEX2EX:	/* inexact->exact */
		if (!validargs("inexact->exact", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (x->_isfixnum) {
			s_return(x);
		} else if (rvalue(x) == (long)rvalue(x)) {
			s_return(mk_integer((long)rvalue(x)));
		} else {
			Error_1("inexact->exact: cannot express :", x);
		}

	case OP_NUM2STR:	/* number->string */
		if (!validargs("number->string", 1, 2, TST_NUMBER TST_NATURAL)) Error_0(msg);
		if (cdr(args) != NIL) {
			w = ivalue(cadr(args));
			if (w != 16 && w != 10 && w != 8 && w != 2) {
				Error_1("number->string: bad base:", cadr(args));
			}
		} else {
			w = 10;
		}
		s_return(mk_string(atom2str(car(args), w)));

	case OP_STR2NUM:	/* string->number */
		if (!validargs("string->number", 1, 2, TST_STRING TST_NATURAL)) Error_0(msg);
		if (cdr(args) != NIL) {
			w = ivalue(cadr(args));
			if (w != 16 && w != 10 && w != 8 && w != 2) {
				Error_1("string->number: bad base:", cadr(args));
			}
		} else {
			w = 10;
		}
		if (*strvalue(car(args)) == '#') {
			s_return(mk_const(strvalue(car(args)) + 1));
		} else if (w == 10) {
			s_return(mk_atom(strvalue(car(args))));
		} else {
			char *ep;
			long iv = strtol(strvalue(car(args)), &ep, w);
			if (*ep) {
				s_return(F);
			}
			s_return(mk_integer(iv));
		}

	case OP_CAR:		/* car */
		if (!validargs("car", 1, 1, TST_PAIR)) Error_0(msg);
		s_return(caar(args));

	case OP_CDR:		/* cdr */
		if (!validargs("cdr", 1, 1, TST_PAIR)) Error_0(msg);
		s_return(cdar(args));

	case OP_CONS:		/* cons */
		if (!validargs("cons", 2, 2, TST_NONE)) Error_0(msg);
		cdr(args) = cadr(args);
		s_return(args);

	case OP_SETCAR:	/* set-car! */
		if (!validargs("set-car!", 2, 2, TST_PAIR TST_ANY)) Error_0(msg);
		caar(args) = cadr(args);
		s_return(car(args));

	case OP_SETCDR:	/* set-cdr! */
		if (!validargs("set-cdr!", 2, 2, TST_PAIR TST_ANY)) Error_0(msg);
		cdar(args) = cadr(args);
		s_return(car(args));

	case OP_CAAR: /* caar */
		if (!validargs("caar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caar: must be pair :", x);
		s_return(car(x));
	case OP_CADR: /* cadr */
		if (!validargs("cadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cadr: must be pair :", x);
		s_return(car(x));
	case OP_CDAR: /* cdar */
		if (!validargs("cdar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdar: must be pair :", x);
		s_return(cdr(x));
	case OP_CDDR: /* cddr */
		if (!validargs("cddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cddr: must be pair :", x);
		s_return(cdr(x));

	case OP_CAAAR: /* caaar */
		if (!validargs("caaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaar: must be pair :", x);
		s_return(car(x));
	case OP_CAADR: /* caadr */
		if (!validargs("caadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caadr: must be pair :", x);
		s_return(car(x));
	case OP_CADAR: /* cadar */
		if (!validargs("cadar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cadar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadar: must be pair :", x);
		s_return(car(x));
	case OP_CADDR: /* caddr */
		if (!validargs("caddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caddr: must be pair :", x);
		s_return(car(x));
	case OP_CDAAR: /* cdaar */
		if (!validargs("cdaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaar: must be pair :", x);
		s_return(cdr(x));
	case OP_CDADR: /* cdadr */
		if (!validargs("cdadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdadr: must be pair :", x);
		s_return(cdr(x));
	case OP_CDDAR: /* cddar */
		if (!validargs("cddar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddar: must be pair :", x);
		s_return(cdr(x));
	case OP_CDDDR: /* cdddr */
		if (!validargs("cdddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdddr: must be pair :", x);
		s_return(cdr(x));

	case OP_CAAAAR: /* caaaar */
		if (!validargs("caaaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaaar: must be pair :", x);
		s_return(car(x));
	case OP_CAAADR: /* caaadr */
		if (!validargs("caaadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaadr: must be pair :", x);
		s_return(car(x));
	case OP_CAADAR: /* caadar */
		if (!validargs("caadar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caadar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caadar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caadar: must be pair :", x);
		s_return(car(x));
	case OP_CAADDR: /* caaddr */
		if (!validargs("caaddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caaddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caaddr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaddr: must be pair :", x);
		s_return(car(x));
	case OP_CADAAR: /* cadaar */
		if (!validargs("cadaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cadaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cadaar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadaar: must be pair :", x);
		s_return(car(x));
	case OP_CADADR: /* cadadr */
		if (!validargs("cadadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cadadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cadadr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadadr: must be pair :", x);
		s_return(car(x));
	case OP_CADDAR: /* caddar */
		if (!validargs("caddar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caddar: must be pair :", x);
		s_return(car(x));
	case OP_CADDDR: /* cadddr" */
		if (!validargs("cadddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cadddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadddr: must be pair :", x);
		s_return(car(x));
	case OP_CDAAAR: /* cdaaar */
		if (!validargs("cdaaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaaar: must be pair :", x);
		s_return(cdr(x));
	case OP_CDAADR: /* cdaadr */
		if (!validargs("cdaadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaadr: must be pair :", x);
		s_return(cdr(x));
	case OP_CDADAR: /* cdadar" */
		if (!validargs("cdadar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdadar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdadar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdadar: must be pair :", x);
		s_return(cdr(x));
	case OP_CDADDR: /* cdaddr */
		if (!validargs("cdaddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdaddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdaddr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaddr: must be pair :", x);
		s_return(cdr(x));
	case OP_CDDAAR: /* cddaar */
		if (!validargs("cddaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cddaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cddaar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddaar: must be pair :", x);
		s_return(cdr(x));
	case OP_CDDADR: /* cddadr */
		if (!validargs("cddadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cddadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cddadr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddadr: must be pair :", x);
		s_return(cdr(x));
	case OP_CDDDAR: /* cdddar */
		if (!validargs("cdddar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdddar: must be pair :", x);
		s_return(cdr(x));
	case OP_CDDDDR: /* cddddr */
		if (!validargs("cddddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cddddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddddr: must be pair :", x);
		s_return(cdr(x));

	case OP_LIST:		/* list */
		if (!validargs("list", 0, 65535, TST_NONE)) Error_0(msg);
		s_return(args);

	case OP_LISTTAIL:	/* list-tail */
		if (!validargs("list-tail", 2, 2, TST_LIST TST_NATURAL)) Error_0(msg);
		x = car(args);
		w = list_length(x);
		if (w < 0) {
			Error_1("list-tail: not a proper list:", x);
		}
		if (w < ivalue(cadr(args))) {
			Error_1("list-tail: list length <", cadr(args));
		}
		for (w -= ivalue(cadr(args)); w > 0; --w) {
			x = cdr(x);
		}
		s_return(x);

	case OP_LISTREF:	/* list-ref */
		if (!validargs("list-ref", 2, 2, TST_LIST TST_NATURAL)) Error_0(msg);
		x = car(args);
		w = list_length(x);
		if (w < 0) {
			Error_1("list-ref: not a proper list:", x);
		}
		if (w < ivalue(cadr(args))) {
			Error_1("list-ref: list length <", cadr(args));
		}
		for (w -= ivalue(cadr(args)); w > 0; --w) {
			x = cdr(x);
		}
		s_return(car(x));

	case OP_LASTPAIR:	/* "last-pair" */
		if (!validargs("last-pair", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(args);
		while (is_pair(cdr(x))) {
			x = cdr(x);
		}
		s_return(x);

	case OP_CHAR2INT:	/* char->integer */
		if (!validargs("char->integer", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_integer(ivalue(car(args))));

	case OP_INT2CHAR:	/* integer->char */
		if (!validargs("integer->char", 1, 1, TST_NATURAL)) Error_0(msg);
		s_return(mk_character((unsigned char)ivalue(car(args))));

	case OP_CHARUPCASE:	/* char-upcase */
		if (!validargs("char-upcase", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_character(toupper((unsigned char)ivalue(car(args)))));

	case OP_CHARDNCASE:	/* char-downcase */
		if (!validargs("char-downcase", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_character(tolower((unsigned char)ivalue(car(args)))));

	case OP_MKSTRING:	/* make-string */
		if (!validargs("make-string", 1, 2, TST_NATURAL TST_CHAR)) Error_0(msg);
		if (cdr(args) != NIL) {
			s_return(mk_empty_string(ivalue(car(args)), (char)ivalue(cadr(args))));
		} else {
			s_return(mk_empty_string(ivalue(car(args)), ' '));
		}

	case OP_STRING:		/* string */
		if (!validargs("string", 0, 65535, TST_CHAR)) Error_0(msg);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			++w;
		}
		y = mk_empty_string(w, ' ');
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			strvalue(y)[w++] = (char)ivalue(car(x));
		}
		s_return(y);

	case OP_STRLEN:		/* string-length */
		if (!validargs("string-length", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_integer(strlength(car(args))));

	case OP_STRREF:		/* string-ref */
		if (!validargs("string-ref", 2, 2, TST_STRING TST_NATURAL)) Error_0(msg);
		w = ivalue(cadr(args));
		if (w >= (int)strlength(car(args))) {
			Error_1("string-ref: out of bounds:", cadr(args));
		}
		s_return(mk_character(((unsigned char *)strvalue(car(args)))[w]));

	case OP_STRSET:		/* string-set! */
		if (!validargs("string-set!", 3, 3, TST_STRING TST_NATURAL TST_CHAR)) Error_0(msg);
		w = ivalue(cadr(args));
		if (w >= (int)strlength(car(args))) {
			Error_1("string-set!: out of bounds:", cadr(args));
		}
		strvalue(car(args))[w] = (char)ivalue(caddr(args));
		s_return(car(args));

	case OP_STREQU:		/* string=? */
		if (!validargs("string=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) == 0);
	case OP_STRLSS:		/* string<? */
		if (!validargs("string<?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) < 0);
	case OP_STRGTR:		/* string>? */
		if (!validargs("string>?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) > 0);
	case OP_STRLEQ:		/* string<=? */
		if (!validargs("string<=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) <= 0);
	case OP_STRGEQ:		/* string>=? */
		if (!validargs("string>=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) >= 0);

	case OP_STRCIEQU:	/* string-ci=? */
		if (!validargs("string-ci=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(stricmp(strvalue(car(args)), strvalue(cadr(args))) == 0);
	case OP_STRCILSS:		/* string-ci<? */
		if (!validargs("string-ci<?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(stricmp(strvalue(car(args)), strvalue(cadr(args))) < 0);
	case OP_STRCIGTR:		/* string-ci>? */
		if (!validargs("string-ci>?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(stricmp(strvalue(car(args)), strvalue(cadr(args))) > 0);
	case OP_STRCILEQ:		/* string-ci<=? */
		if (!validargs("string-ci<=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(stricmp(strvalue(car(args)), strvalue(cadr(args))) <= 0);
	case OP_STRCIGEQ:		/* string-ci>=? */
		if (!validargs("string-ci>=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(stricmp(strvalue(car(args)), strvalue(cadr(args))) >= 0);

	case OP_SUBSTR:		/* substring */
		if (!validargs("substring", 2, 3, TST_STRING TST_NATURAL)) Error_0(msg);
		if (ivalue(cadr(args)) > (long)strlength(car(args))) {
			Error_1("substring: start out of bounds:", cadr(args));
		}
		if (cddr(args) != NIL) {
			if (ivalue(caddr(args)) > (long)strlength(car(args)) || ivalue(caddr(args)) < ivalue(cadr(args))) {
				Error_1("substring: end out of bounds:", caddr(args));
			}
			w = ivalue(caddr(args)) - ivalue(cadr(args));
		} else {
			w = strlength(car(args)) - ivalue(cadr(args));
		}
		x = mk_empty_string(w, ' ');
		memcpy(strvalue(x), strvalue(car(args)) + ivalue(cadr(args)), w);
		strvalue(x)[w] = '\0';
		s_return(x);

	case OP_STRAPPEND:	/* string-append */
		if (!validargs("string-append", 0, 65535, TST_STRING)) Error_0(msg);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			w += strlength(car(x));
		}
		y = mk_empty_string(w, ' ');
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			memcpy(strvalue(y) + w, strvalue(car(x)), strlength(car(x)));
			w += strlength(car(x));
		}
		s_return(y);

	case OP_STR2LIST:	/* string->list */
		if (!validargs("string->list", 1, 1, TST_STRING)) Error_0(msg);
		y = NIL;
		for (x = car(args), w = strlength(x) - 1; w >= 0; w--) {
			y = cons(mk_character(strvalue(x)[w]), y);
		}
		s_return(y);

	case OP_LIST2STR:	/* list->string */
		if (!validargs("list->string", 1, 1, TST_LIST)) Error_0(msg);
		x = car(args);
		w = list_length(x);
		if (w < 0) {
			Error_1("list->string: not a proper list:", x);
		}
		y = mk_empty_string(w, ' ');
		for (w = 0; x != NIL; x = cdr(x)) {
			if (!is_character(car(x))) {
				Error_1("list->string: not a charactor:", car(x));
			}
			strvalue(y)[w++] = (char)ivalue(car(x));
		}
		s_return(y);

	case OP_STRCOPY:	/* string-copy */
		if (!validargs("string-copy", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_string(strvalue(car(args))));

	case OP_STRFILL:	/* string-fill! */
		if (!validargs("string-fill!", 2, 2, TST_STRING TST_CHAR)) Error_0(msg);
		x = car(args);
		memset(strvalue(x), ivalue(cadr(args)), strlength(x));
		s_return(x);

	case OP_VECTOR:		/* vector */
OP_VECTOR:
		if (!validargs("vector", 0, 65535, TST_NONE)) Error_0(msg);
		w = list_length(args);
		if (w < 0) {
			Error_1("vector: not a proper list:", args);
		}
		y = mk_vector(w);
		for (x = args, w = 0; is_pair(x); x = cdr(x)) {
			set_vector_elem(y, w++, car(x));
		}
		s_return(y);

	case OP_MKVECTOR:	/* make-vector */
		if (!validargs("make-vector", 1, 2, TST_NATURAL TST_ANY)) Error_0(msg);
		w = ivalue(car(args));
		x = mk_vector(w);
		if (cdr(args) != NIL) {
			fill_vector(x, cadr(args));
		}
		s_return(x);

	case OP_VECLEN:		/* vector-length */
		if (!validargs("vector-length", 1, 1, TST_VECTOR)) Error_0(msg);
		s_return(mk_integer(ivalue(car(args))));

	case OP_VECREF:		/* vector-ref */
		if (!validargs("vector-ref", 2, 2, TST_VECTOR TST_NATURAL)) Error_0(msg);
		w = ivalue(cadr(args));
		if (w >= ivalue(car(args))) {
		   Error_1("vector-ref: out of bounds:", cadr(args));
		}
		s_return(vector_elem(car(args), w));

	case OP_VECSET:		/* vector-set! */
		if (!validargs("vector-set!", 3, 3, TST_VECTOR TST_NATURAL TST_ANY)) Error_0(msg);
		w = ivalue(cadr(args));
		if (w >= ivalue(car(args))) {
		   Error_1("vector-set!: out of bounds:", cadr(args));
		}
		set_vector_elem(car(args), w, caddr(args));
		s_return(car(args));

	case OP_VEC2LIST:		/* vector->list */
		if (!validargs("vector->list", 1, 1, TST_VECTOR)) Error_0(msg);
		y = NIL;
		for (w = ivalue(car(args)) - 1; w >= 0; w--) {
			y = cons(vector_elem(car(args), w), y);
		}
		s_return(y);

	case OP_LIST2VEC:		/* list->vector */
		if (!validargs("list->vector", 1, 1, TST_LIST)) Error_0(msg);
		args = car(args);
		w = list_length(args);
		if (w < 0) {
			Error_1("list->vector: not a proper list:", args);
		}
		y = mk_vector(w);
		for (w = 0; args != NIL; args = cdr(args)) {
			set_vector_elem(y, w++, car(args));
		}
		s_return(y);

	case OP_VECFILL:	/* vector-fill! */
		if (!validargs("vector-fill!", 2, 2, TST_VECTOR TST_ANY)) Error_0(msg);
		for (x = car(args), w = ivalue(x) - 1; w >= 0; w--) {
			set_vector_elem(car(args), w, cadr(args));
		}
		s_return(car(args));

	case OP_NOT:		/* not */
		if (!validargs("not", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(isfalse(car(args)));
	case OP_BOOL:		/* boolean? */
		if (!validargs("boolean?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(car(args) == F || car(args) == T);
	case OP_NULL:		/* null? */
		if (!validargs("null?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(car(args) == NIL);
	case OP_EOFOBJP:	/* eof-object? */
		if (!validargs("eof-object?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(car(args) == EOF_OBJ);
	case OP_ZEROP:		/* zero? */
		if (!validargs("zero?", 1, 1, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) == 0);
	case OP_POSP:		/* positive? */
		if (!validargs("positive?", 1, 1, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) > 0);
	case OP_NEGP:		/* negative? */
		if (!validargs("negative?", 1, 1, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) < 0);
	case OP_ODD:		/* odd? */
		if (!validargs("odd?", 1, 1, TST_INTEGER)) Error_0(msg);
		s_retbool((car(args)->_isfixnum ? ivalue(car(args)) : (long)rvalue(car(args))) % 2 == 1);
	case OP_EVEN:		/* even? */
		if (!validargs("even?", 1, 1, TST_INTEGER)) Error_0(msg);
		s_retbool((car(args)->_isfixnum ? ivalue(car(args)) : (long)rvalue(car(args))) % 2 == 0);
	case OP_NEQ:		/* = */
		if (!validargs("=", 2, 2, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) == nvalue(cadr(args)));
	case OP_LESS:		/* < */
		if (!validargs("<", 2, 2, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) < nvalue(cadr(args)));
	case OP_GRE:		/* > */
		if (!validargs(">", 2, 2, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) > nvalue(cadr(args)));
	case OP_LEQ:		/* <= */
		if (!validargs("<=", 2, 2, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) <= nvalue(cadr(args)));
	case OP_GEQ:		/* >= */
		if (!validargs(">=", 2, 2, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) >= nvalue(cadr(args)));

	case OP_MAX:		/* max */
		if (!validargs("max", 1, 65535, TST_NUMBER)) Error_0(msg);
		v = *car(args);
		for (x = cdr(args); x != NIL; x = cdr(x)) {
			if (v._isfixnum) {
				if (car(x)->_isfixnum) {
					if (ivalue(&v) < ivalue(car(x))) {
						ivalue(&v) = ivalue(car(x));
					}
				} else {
					if (ivalue(&v) < rvalue(car(x))) {
						rvalue(&v) = rvalue(car(x));
					} else {
						rvalue(&v) = ivalue(&v);
					}
					set_num_real(&v);
				}
			} else {
				if (rvalue(&v) < nvalue(car(x))) {
					rvalue(&v) = nvalue(car(x));
				}
			}
		}
		s_return(mk_number(&v));

	case OP_MIN:		/* min */
		if (!validargs("min", 1, 65535, TST_NUMBER)) Error_0(msg);
		v = *car(args);
		for (x = cdr(args); x != NIL; x = cdr(x)) {
			if (v._isfixnum) {
				if (car(x)->_isfixnum) {
					if (ivalue(&v) > ivalue(car(x))) {
						ivalue(&v) = ivalue(car(x));
					}
				} else {
					if (ivalue(&v) > rvalue(car(x))) {
						rvalue(&v) = rvalue(car(x));
					} else {
						rvalue(&v) = ivalue(&v);
					}
					set_num_real(&v);
				}
			} else {
				if (rvalue(&v) > nvalue(car(x))) {
					rvalue(&v) = nvalue(car(x));
				}
			}
		}
		s_return(mk_number(&v));

	case OP_SYMBOL:		/* symbol? */
		if (!validargs("symbol?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_symbol(car(args)));
	case OP_SYM2STR:	/* symbol->string */
		if (!validargs("symbol->string", 1, 1, TST_SYMBOL)) Error_0(msg);
		s_return(mk_string(symname(car(args))));
	case OP_STR2SYM:	/* string->symbol */
		if (!validargs("string->symbol", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_symbol(strvalue(car(args))));
	case OP_NUMBER:		/* number? */
		if (!validargs("number?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_number(car(args)));
	case OP_STRINGP:	/* string? */
		if (!validargs("string?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_string(car(args)));
	case OP_INTEGER:	/* integer? */
		if (!validargs("integer?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_integer(car(args)));
	case OP_REAL:		/* real? */
		if (!validargs("real?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_number(car(args)));
	case OP_EXACT:		/* exact? */
		if (!validargs("exact?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_number(car(args)) && car(args)->_isfixnum);
	case OP_INEXACT:	/* inexact? */
		if (!validargs("inexact?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(!is_number(car(args)) || !car(args)->_isfixnum);
	case OP_CHAR:		/* char? */
		if (!validargs("char?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_character(car(args)));
	case OP_CHAREQU:	/* char=? */
		if (!validargs("char=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) == ivalue(cadr(args)));
	case OP_CHARLSS:	/* char<? */
		if (!validargs("char<?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) < ivalue(cadr(args)));
	case OP_CHARGTR:	/* char>? */
		if (!validargs("char>?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) > ivalue(cadr(args)));
	case OP_CHARLEQ:	/* char<=? */
		if (!validargs("char<=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) <= ivalue(cadr(args)));
	case OP_CHARGEQ:	/* char>=? */
		if (!validargs("char>=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) >= ivalue(cadr(args)));
	case OP_CHARCIEQU:	/* char-ci=? */
		if (!validargs("char-ci=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(tolower((unsigned char)ivalue(car(args))) == tolower((unsigned char)ivalue(cadr(args))));
	case OP_CHARCILSS:	/* char-ci<? */
		if (!validargs("char-ci<?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(tolower((unsigned char)ivalue(car(args))) < tolower((unsigned char)ivalue(cadr(args))));
	case OP_CHARCIGTR:	/* char-ci>? */
		if (!validargs("char-ci>?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(tolower((unsigned char)ivalue(car(args))) > tolower((unsigned char)ivalue(cadr(args))));
	case OP_CHARCILEQ:	/* char-ci<=? */
		if (!validargs("char-ci<=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(tolower((unsigned char)ivalue(car(args))) <= tolower((unsigned char)ivalue(cadr(args))));
	case OP_CHARCIGEQ:	/* char-ci>=? */
		if (!validargs("char-ci>=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(tolower((unsigned char)ivalue(car(args))) >= tolower((unsigned char)ivalue(cadr(args))));
	case OP_CHARAP:		/* char-alphabetic? */
		if (!validargs("char-alphabetic?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(isascii(ivalue(car(args))) && isalpha(ivalue(car(args))));
	case OP_CHARNP:		/* char-numeric? */
		if (!validargs("char-numeric?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(isascii(ivalue(car(args))) && isdigit(ivalue(car(args))));
	case OP_CHARWP:		/* char-whitespace? */
		if (!validargs("char-whitespace?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(isascii(ivalue(car(args))) && isspace(ivalue(car(args))));
	case OP_CHARUP:		/* char-upper-case? */
		if (!validargs("char-upper-case?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(isascii(ivalue(car(args))) && isupper(ivalue(car(args))));
	case OP_CHARLP:		/* char-lower-case? */
		if (!validargs("char-lower-case?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(isascii(ivalue(car(args))) && islower(ivalue(car(args))));
	case OP_PROC:		/* procedure? */
		if (!validargs("procedure?", 1, 1, TST_ANY)) Error_0(msg);
		/*--
		 * continuation should be procedure by the example
		 * (call-with-current-continuation procedure?) ==> #t
		 * in R^3 report sec. 6.9
		 */
		s_retbool(is_proc(car(args)) || is_closure(car(args))
			  || is_continuation(car(args)));
	case OP_PAIR:		/* pair? */
		if (!validargs("pair?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_pair(car(args)));
	case OP_LISTP:		/* list? */
		if (!validargs("list?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(list_length(car(args)) >= 0);
	case OP_PORTP:		/* port? */
		if (!validargs("port?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_port(car(args)));
	case OP_INPORTP:	/* input-port? */
		if (!validargs("input-port?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_inport(car(args)));
	case OP_OUTPORTP:	/* output-port? */
		if (!validargs("output-port?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_outport(car(args)));
	case OP_VECTORP:	/* vector? */
		if (!validargs("vector?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_vector(car(args)));
	case OP_ENVP:		/* environment? */
		if (!validargs("environment?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_environment(car(args)));
	case OP_EQ:		/* eq? */
		if (!validargs("eq?", 2, 2, TST_ANY)) Error_0(msg);
		s_retbool(car(args) == cadr(args));
	case OP_EQV:		/* eqv? */
		if (!validargs("eqv?", 2, 2, TST_ANY)) Error_0(msg);
		s_retbool(eqv(car(args), cadr(args)));
	case OP_EQUAL:		/* equal? */
		if (!validargs("equal?", 2, 2, TST_ANY)) Error_0(msg);
		s_retbool(equal(car(args), cadr(args)));

	case OP_FORCE:		/* force */
		if (!validargs("force", 1, 1, TST_ANY)) Error_0(msg);
		code = car(args);
		if (is_promise(code)) {
			if (is_resultready(code)) {
				s_return(caar(code));
			}
			s_save(OP_FORCED, NIL, code);
			args = NIL;
			s_goto(OP_APPLY);
		} else {
			s_return(code);
		}

	case OP_FORCED:		/* force */
		setresultready(code);
		car(code) = cons(value, NIL);
		cdr(code) = NIL;
		s_return(caar(code));

	case OP_WRITE_CHAR:	/* write-char */
	case OP_WRITE:		/* write */
	case OP_DISPLAY:	/* display */
		switch (operator) {
		case OP_WRITE_CHAR:
			if (!validargs("write-char", 1, 2, TST_CHAR TST_OUTPORT)) Error_0(msg);
			break;
		case OP_WRITE:
			if (!validargs("write", 1, 2, TST_ANY TST_OUTPORT)) Error_0(msg);
			break;
		case OP_DISPLAY:
			if (!validargs("display", 1, 2, TST_ANY TST_OUTPORT)) Error_0(msg);
			break;
		default:
			break;
		}
		if (is_pair(cdr(args))) {
			if (cadr(args) != outport) {
				outport = cons(outport, NIL);
				s_save(OP_SET_OUTPORT, outport, NIL);
				outport = cadr(args);
			}
		}
		args = car(args);
		print_flag = (operator == OP_WRITE) ? 1 : 0;
		s_goto(OP_P0LIST);

	case OP_NEWLINE:	/* newline */
		if (!validargs("newline", 0, 1, TST_OUTPORT)) Error_0(msg);
		if (is_pair(args)) {
			if (car(args) != outport) {
				outport = cons(outport, NIL);
				s_save(OP_SET_OUTPORT, outport, NIL);
				outport = car(args);
			}
		}
		putstr("\n");
		s_return(T);

	case OP_ERR0:	/* error */
		if (!validargs("error", 1, 65535, TST_NONE)) Error_0(msg);
		tmpfp = port_file(outport);
		port_file(outport) = stderr;
		fprintf(stderr, "Error: ");
		fprintf(stderr, "%s", strvalue(car(args)));
		args = cdr(args);
		s_goto(OP_ERR1);

	case OP_ERR1:	/* error */
OP_ERR1:
		putstr(" ");
		if (args != NIL) {
			s_save(OP_ERR1, cdr(args), NIL);
			args = car(args);
			print_flag = 1;
			s_goto(OP_P0LIST);
		} else {
			putstr("\n");
			flushinput();
			port_file(outport) = tmpfp;
			s_goto(OP_T0LVL);
		}

	case OP_REVERSE:	/* reverse */
		if (!validargs("reverse", 1, 1, TST_LIST)) Error_0(msg);
		s_return(reverse(car(args)));

	case OP_APPEND:	/* append */
		if (!validargs("append", 0, 65535, TST_NONE)) Error_0(msg);
		s_return(append(car(args), cadr(args)));

	case OP_PUT:		/* put */
		if (!validargs("put", 3, 3, TST_NONE)) Error_0(msg);
		if (!hasprop(car(args)) || !hasprop(cadr(args))) {
			Error_0("Illegal use of put");
		}
		for (x = symprop(car(args)), y = cadr(args); x != NIL; x = cdr(x))
			if (caar(x) == y)
				break;
		if (x != NIL)
			cdar(x) = caddr(args);
		else {
			x = cons(y, caddr(args));
			symprop(car(args)) = cons(x, symprop(car(args)));
		}
		s_return(T);

	case OP_GET:		/* get */
		if (!validargs("get", 2, 2, TST_NONE)) Error_0(msg);
		if (!hasprop(car(args)) || !hasprop(cadr(args))) {
			Error_0("Illegal use of get");
		}
		for (x = symprop(car(args)), y = cadr(args); x != NIL; x = cdr(x))
			if (caar(x) == y)
				break;
		if (x != NIL) {
			s_return(cdar(x));
		} else {
			s_return(NIL);
		}

	case OP_QUIT:		/* quit */
		if (!validargs("quit", 0, 1, TST_INTEGER)) Error_0(msg);
		if (is_pair(args)) {
			return ivalue(car(args));
		}
		return 0;

	case OP_GC:		/* gc */
		if (!validargs("gc", 0, 0, TST_NONE)) Error_0(msg);
		gc(&NIL, &NIL);
		s_return(T);

	case OP_GCVERB:		/* gc-verbose */
		if (!validargs("gc-verbose", 0, 1, TST_NONE)) Error_0(msg);
		w = gc_verbose;
		gc_verbose = (car(args) != F);
		s_retbool(w);

	case OP_CALL_INFILE0:	/* call-with-input-file */
		if (!validargs("call-with-input-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), port_input);
		if (x == NIL) {
			s_return(F);
		}
		code = cadr(args);
		args = cons(x, NIL);
		s_save(OP_CALL_INFILE1, args, NIL);
		s_goto(OP_APPLY);

	case OP_CALL_INFILE1:	/* call-with-input-file */
		port_close(car(args));
		s_return(value);

	case OP_CALL_OUTFILE0:	/* call-with-output-file */
		if (!validargs("call-with-output-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), port_output);
		if (x == NIL) {
			s_return(F);
		}
		code = cadr(args);
		args = cons(x, NIL);
		s_save(OP_CALL_OUTFILE1, args, NIL);
		s_goto(OP_APPLY);

	case OP_CALL_OUTFILE1:	/* call-with-output-file */
		port_close(car(args));
		s_return(value);

	case OP_CURR_INPORT:	/* current-input-port */
		if (!validargs("current-input-port", 0, 0, TST_NONE)) Error_0(msg);
		s_return(inport);

	case OP_CURR_OUTPORT:	/* current-output-port */
		if (!validargs("current-output-port", 0, 0, TST_NONE)) Error_0(msg);
		s_return(outport);

	case OP_WITH_INFILE0:	/* with-input-from-file */
		if (!validargs("with-input-from-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), port_input);
		if (x == NIL) {
			s_return(F);
		}
		code = cadr(args);
		args = cons(x, inport);
		inport = car(args);
		s_save(OP_WITH_INFILE1, args, NIL);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_WITH_INFILE1:	/* with-input-from-file */
		port_close(car(args));
		inport = cdr(args);
		s_return(value);

	case OP_WITH_OUTFILE0:	/* with-output-to-file */
		if (!validargs("with-output-to-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), port_output);
		if (x == NIL) {
			s_return(F);
		}
		code = cadr(args);
		args = cons(x, outport);
		outport = car(args);
		s_save(OP_WITH_OUTFILE1, args, NIL);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_WITH_OUTFILE1:	/* with-output-to-file */
		port_close(car(args));
		outport = cdr(args);
		s_return(value);

	case OP_OPEN_INFILE:	/* open-input-file */
		if (!validargs("open-input-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), port_input);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_OUTFILE:	/* open-output-file */
		if (!validargs("open-output-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), port_output);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_INOUTFILE:	/* open-input-output-file */
		if (!validargs("open-input-output-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), port_input | port_output);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_INSTRING:	/* open-input-string */
		if (!validargs("open-input-string", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_string(strvalue(car(args)), port_input);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_OUTSTRING:	/* open-output-string */
		if (!validargs("open-output-string", 0, 0, TST_NONE)) Error_0(msg);
		x = port_from_scratch();
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_INOUTSTRING:	/* open-input-output-string */
		if (!validargs("open-input-output-string", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_string(strvalue(car(args)), port_input | port_output);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_GET_OUTSTRING:	/* get-output-string */
		if (!validargs("get-output-string", 1, 1, TST_OUTPORT)) Error_0(msg);
		x = car(args);
		if (is_strport(x) && port_file(x) != NULL) {
			s_return(mk_string(strvalue(car(x))));
		}
		s_return(F);

	case OP_CLOSE_INPORT: /* close-input-port */
		if (!validargs("close-input-port", 1, 1, TST_INPORT)) Error_0(msg);
		port_close(car(args));
		s_return(T);

	case OP_CLOSE_OUTPORT: /* close-output-port */
		if (!validargs("close-output-port", 1, 1, TST_OUTPORT)) Error_0(msg);
		port_close(car(args));
		s_return(T);

	case OP_CLOSE_PORT: /* close-port */
		if (!validargs("close-port", 1, 1, TST_PORT)) Error_0(msg);
		port_close(car(args));
		s_return(T);

	case OP_INT_ENV:	/* interaction-environment */
		if (!validargs("interaction-environment", 0, 0, TST_NONE)) Error_0(msg);
		s_return(global_env);

	case OP_CURR_ENV:	/* current-environment */
		if (!validargs("current-environment", 0, 0, TST_NONE)) Error_0(msg);
		s_return(envir);

		/* ========== reading part ========== */
	case OP_READ:			/* read */
		if (!validargs("read", 0, 1, TST_INPORT)) Error_0(msg);
		if (is_pair(args)) {
			if (port_file(car(args)) == NULL) {
				Error_0("Input port was closed");
			}
			if (car(args) != inport) {
				inport = cons(inport, NIL);
				s_save(OP_SET_INPORT, inport, NIL);
				inport = car(args);
			}
		}
		s_goto(OP_READ_INTERNAL);

	case OP_READ_CHAR:		/* read-char */
	case OP_PEEK_CHAR:		/* peek-char */
		switch (operator) {
		case OP_READ_CHAR:
			if (!validargs("read-char", 0, 1, TST_INPORT)) Error_0(msg);
			break;
		case OP_PEEK_CHAR:
			if (!validargs("peek-char", 0, 1, TST_INPORT)) Error_0(msg);
			break;
		default:
			break;
		}
		if (is_pair(args)) {
			if (port_file(car(args)) == NULL) {
				Error_0("Input port was closed");
			}
			if (car(args) != inport) {
				inport = cons(inport, NIL);
				s_save(OP_SET_INPORT, inport, NIL);
				inport = car(args);
			}
		}
		w = inchar();
		if (w == EOF) {
			s_return(EOF_OBJ);
		}
		if (operator == OP_PEEK_CHAR) {
			backchar(w);
		}
		s_return(mk_character(w));

	case OP_CHAR_READY:		/* char-ready? */
		if (!validargs("char-ready?", 0, 1, TST_INPORT)) Error_0(msg);
		if (is_pair(args)) {
			x = car(args);
		} else {
			x = inport;
		}
		s_retbool(is_fileport(x) || is_strport(x));

	case OP_SET_INPORT:		/* set-input-port */
		if (!validargs("set-input-port", 1, 1, TST_INPORT)) Error_0(msg);
		inport = car(args);
		s_return(value);

	case OP_SET_OUTPORT:	/* set-output-port */
		if (!validargs("set-output-port", 1, 1, TST_OUTPORT)) Error_0(msg);
		outport = car(args);
		s_return(value);

	case OP_RDSEXPR:
OP_RDSEXPR:
		switch (tok) {
		case TOK_EOF:
			s_return(EOF_OBJ);
		case TOK_VEC:
			s_save(OP_RDVEC, NIL, NIL);
			/* fall through */
		case TOK_LPAREN:
			tok = token();
			if (tok == TOK_RPAREN) {
				s_return(NIL);
			} else if (tok == TOK_DOT) {
				Error_0("syntax error -- illegal dot expression");
			} else {
				s_save(OP_RDLIST, NIL, NIL);
				s_goto(OP_RDSEXPR);
			}
		case TOK_QUOTE:
			s_save(OP_RDQUOTE, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_BQUOTE:
			tok = token();
			if (tok == TOK_VEC) {
				s_save(OP_RDQQUOTEVEC, NIL, NIL);
				tok = TOK_LPAREN;
			} else {
				s_save(OP_RDQQUOTE, NIL, NIL);
			}
			s_goto(OP_RDSEXPR);
		case TOK_COMMA:
			s_save(OP_RDUNQUOTE, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_ATMARK:
			s_save(OP_RDUQTSP, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_ATOM:
			s_return(mk_atom(readstr("();\t\n ")));
		case TOK_DQUOTE:
			s_return(readstrexp());
		case TOK_SHARP:
			if ((x = mk_const(readstr("();\t\n "))) == NIL) {
				Error_0("Undefined sharp expression");
			} else {
				s_return(x);
			}
		default:
			Error_0("syntax error -- illegal token");
		}
		break;

	case OP_RDLIST:
		args = cons(value, args);
		tok = token();
		if (tok == TOK_EOF) {
			s_return(EOF_OBJ);
		} else if (tok == TOK_RPAREN) {
			s_return(non_alloc_rev(NIL, args));
		} else if (tok == TOK_DOT) {
			s_save(OP_RDDOT, args, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		} else {
			s_save(OP_RDLIST, args, NIL);
			s_goto(OP_RDSEXPR);
		}

	case OP_RDDOT:
		if (token() != TOK_RPAREN) {
			Error_0("syntax error -- illegal dot expression");
		} else {
			s_return(non_alloc_rev(value, args));
		}

	case OP_RDQUOTE:
		x = cons(value, NIL);
		s_return(cons(QUOTE, x));

	case OP_RDQQUOTE:
		x = cons(value, NIL);
		s_return(cons(QQUOTE, x));

	case OP_RDQQUOTEVEC:
		x = cons(value, NIL);
		x = cons(QQUOTE, x);
		x = cons(x, NIL);
		x = cons(mk_symbol("vector"), x);
		s_return(cons(mk_symbol("apply"), x));

	case OP_RDUNQUOTE:
		x = cons(value, NIL);
		s_return(cons(UNQUOTE, x));

	case OP_RDUQTSP:
		x = cons(value, NIL);
		s_return(cons(UNQUOTESP, x));

	case OP_RDVEC:
		args = value;
		s_goto(OP_VECTOR);

	/* ========== printing part ========== */
	case OP_P0LIST:
OP_P0LIST:
		if (is_vector(args)) {
			putstr("#(");
			args = cons(args, mk_integer(0));
			s_goto(OP_PVECFROM);
		} else if (!is_pair(args)) {
			printatom(args, print_flag);
			s_return(T);
		} else if (car(args) == QUOTE && ok_abbrev(cdr(args))) {
			putstr("'");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else if (car(args) == QQUOTE && ok_abbrev(cdr(args))) {
			putstr("`");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else if (car(args) == UNQUOTE && ok_abbrev(cdr(args))) {
			putstr(",");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else if (car(args) == UNQUOTESP && ok_abbrev(cdr(args))) {
			putstr(",@");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else {
			putstr("(");
			s_save(OP_P1LIST, cdr(args), NIL);
			args = car(args);
			s_goto(OP_P0LIST);
		}

	case OP_P1LIST:
		if (is_pair(args)) {
			s_save(OP_P1LIST, cdr(args), NIL);
			putstr(" ");
			args = car(args);
			s_goto(OP_P0LIST);
		} else if (is_vector(args)) {
			s_save(OP_P1LIST, NIL, NIL);
			putstr(" . ");
			s_goto(OP_P0LIST);
		} else {
			if (args != NIL) {
				putstr(" . ");
				printatom(args, print_flag);
			}
			putstr(")");
			s_return(T);
		}

	case OP_PVECFROM:
OP_PVECFROM:
		w = ivalue(cdr(args));
		if (w == ivalue(car(args))) {
			putstr(")");
			s_return(T);
		} else {
			ivalue(cdr(args)) = w + 1;
			s_save(OP_PVECFROM, args, NIL);
			args = vector_elem(car(args), w);
			if (w > 0) putstr(" ");
			s_goto(OP_P0LIST);
		}

	case OP_LIST_LENGTH:	/* length */	/* a.k */
		if (!validargs("length", 1, 1, TST_LIST)) Error_0(msg);
		w = list_length(car(args));
		if (w < 0) {
			Error_1("length: not a list:", car(args));
		}
		s_return(mk_integer(w));

	case OP_MEMQ:		/* memq */
		if (!validargs("memq", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); y != NIL; y = cdr(y)) {
			if (x == car(y)) s_return(y);
		}
		s_return(F);

	case OP_MEMV:		/* memv */
		if (!validargs("memv", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); y != NIL; y = cdr(y)) {
			if (eqv(x, car(y))) s_return(y);
		}
		s_return(F);

	case OP_MEMBER:		/* member*/
		if (!validargs("member", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); y != NIL; y = cdr(y)) {
			if (equal(x, car(y))) s_return(y);
		}
		s_return(F);

	case OP_ASSQ:		/* assq */
		if (!validargs("assq", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
				Error_0("Unable to handle non pair element");
			}
			if (x == caar(y)) s_return(car(y));
		}
		s_return(F);

	case OP_ASSV:		/* assv*/
		if (!validargs("assv", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
				Error_0("Unable to handle non pair element");
			}
			if (eqv(x, caar(y))) s_return(car(y));
		}
		s_return(F);

	case OP_ASSOC:		/* assoc */
		if (!validargs("assoc", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
				Error_0("Unable to handle non pair element");
			}
			if (equal(x, caar(y))) s_return(car(y));
		}
		s_return(F);

	case OP_GET_CLOSURE:	/* get-closure-code */	/* a.k */
		if (!validargs("get-closure-code", 1, 1, TST_NONE)) Error_0(msg);
		args = car(args);
		if (args == NIL) {
			s_return(F);
		} else if (is_closure(args)) {
			s_return(cons(LAMBDA, closure_code(value)));
		} else {
			s_return(F);
		}

	case OP_CLOSUREP:		/* closure? */
		if (!validargs("closure?", 1, 1, TST_NONE)) Error_0(msg);
		/*
		 * Note, macro object is also a closure.
		 * Therefore, (closure? <#MACRO>) ==> #t
		 */
		if (car(args) == NIL) {
			s_return(F);
		}
		s_retbool(is_closure(car(args)));

	case OP_MACROP:		/* macro? */
		if (!validargs("macro?", 1, 1, TST_NONE)) Error_0(msg);
		if (car(args) == NIL) {
			s_return(F);
		}
		s_retbool(is_closure(car(args)) && is_macro(car(args)));

	case OP_MACRO_EXPAND0:	/* macro-expand */
		if (!validargs("macro-expand", 1, 1, TST_LIST)) Error_0(msg);
		s_save(OP_MACRO_EXPAND1, args, NIL);
		code = caar(args);
		args = NIL;
		s_goto(OP_EVAL);

	case OP_MACRO_EXPAND1:	/* macro-expand */
		if (is_closure(value) && is_macro(value)) {
			code = value;
			s_save(OP_MACRO_EXPAND2, args, code);
			code = cons(LAMBDA, closure_code(code));
			args = NIL;
			s_goto(OP_EVAL);
		}
		s_return(car(args));

	case OP_MACRO_EXPAND2:	/* macro-expand */
		if (exttype(code) & T_DEFMACRO) {
			args = cdar(args);
		}
		code = value;
		s_goto(OP_APPLY);

	case OP_ATOMP:		/* atom? */
		if (!validargs("atom?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(is_atom(car(args)));

	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);
	}

	return 0;
}

/* ========== Initialization of internal keywords ========== */

void mk_syntax(unsigned short op, char *name)
{
	pointer x;

	x = cons(mk_string(name), NIL);
	type(x) = (T_SYNTAX | T_SYMBOL);
	syntaxnum(x) = op;
	oblist = cons(x, oblist);
}

void mk_proc(unsigned short op, char *name)
{
	pointer x, y;

	x = mk_symbol(name);
	y = get_cell(&x, &NIL);
	type(y) = (T_PROC | T_ATOM);
	ivalue(y) = (long) op;
	set_num_integer(y);
	x = cons(x, y);
	car(global_env) = cons(x, car(global_env));
}


void init_vars_global()
{
	pointer x;

	oblist = NIL;
	winders = NIL;
#ifdef USE_COPYING_GC
	gcell_list = NIL;
	psink = sink;
#endif
	srcfp = NULL;
	/* init output file */
	outport = mk_port(stdout, port_output);
	/* init NIL */
	type(NIL) = (T_ATOM | MARK);
	car(NIL) = cdr(NIL) = NIL;
	/* init T */
	type(T) = (T_ATOM | MARK);
	car(T) = cdr(T) = T;
	/* init F */
	type(F) = (T_ATOM | MARK);
	car(F) = cdr(F) = F;
	/* init EOF_OBJ */
	type(EOF_OBJ) = (T_ATOM | MARK);
	car(EOF_OBJ) = cdr(EOF_OBJ) = EOF_OBJ;
	/* init global_env */
	global_env = cons(NIL, NIL);
	setenvironment(global_env);
	/* init else */
	x = cons(mk_symbol("else"), T);
	car(global_env) = cons(x, car(global_env));
	type(&_ZERO) = T_NUMBER;
	set_num_integer(&_ZERO);
	ivalue(&_ZERO) = 0;
	type(&_ONE) = T_NUMBER;
	set_num_integer(&_ONE);
	ivalue(&_ONE) = 1;
}


void init_syntax()
{
	/* init syntax */
	mk_syntax(OP_LAMBDA, "lambda");
	mk_syntax(OP_QUOTE, "quote");
	mk_syntax(OP_QQUOTE0, "quasiquote");
	mk_syntax(OP_DEF0, "define");
	mk_syntax(OP_IF0, "if");
	mk_syntax(OP_BEGIN, "begin");
	mk_syntax(OP_SET0, "set!");
	mk_syntax(OP_LET0, "let");
	mk_syntax(OP_LET0AST, "let*");
	mk_syntax(OP_LET0REC, "letrec");
	mk_syntax(OP_DO0, "do");
	mk_syntax(OP_COND0, "cond");
	mk_syntax(OP_DELAY, "delay");
	mk_syntax(OP_AND0, "and");
	mk_syntax(OP_OR0, "or");
	mk_syntax(OP_C0STREAM, "cons-stream");
	mk_syntax(OP_0MACRO, "macro");
	mk_syntax(OP_DEFMACRO0, "define-macro");
	mk_syntax(OP_CASE0, "case");
	mk_syntax(OP_WHEN0, "when");
	mk_syntax(OP_UNLESS0, "unless");
}


void init_procs()
{
	/* init procedure */
	mk_proc(OP_PEVAL, "eval");
	mk_proc(OP_PAPPLY, "apply");
	mk_proc(OP_MAP0, "map");
	mk_proc(OP_FOREACH0, "for-each");
	mk_proc(OP_CONTINUATION, "call-with-current-continuation");
	mk_proc(OP_VALUES, "values");
	mk_proc(OP_WITHVALUES0, "call-with-values");
	mk_proc(OP_DYNAMICWIND0, "dynamic-wind");
	mk_proc(OP_FORCE, "force");
	mk_proc(OP_CAR, "car");
	mk_proc(OP_CDR, "cdr");
	mk_proc(OP_CONS, "cons");
	mk_proc(OP_SETCAR, "set-car!");
	mk_proc(OP_SETCDR, "set-cdr!");
	mk_proc(OP_CAAR, "caar");
	mk_proc(OP_CADR, "cadr");
	mk_proc(OP_CDAR, "cdar");
	mk_proc(OP_CDDR, "cddr");
	mk_proc(OP_CAAAR, "caaar");
	mk_proc(OP_CAADR, "caadr");
	mk_proc(OP_CADAR, "cadar");
	mk_proc(OP_CADDR, "caddr");
	mk_proc(OP_CDAAR, "cdaar");
	mk_proc(OP_CDADR, "cdadr");
	mk_proc(OP_CDDAR, "cddar");
	mk_proc(OP_CDDDR, "cdddr");
	mk_proc(OP_CAAAAR, "caaaar");
	mk_proc(OP_CAAADR, "caaadr");
	mk_proc(OP_CAADAR, "caadar");
	mk_proc(OP_CAADDR, "caaddr");
	mk_proc(OP_CADAAR, "cadaar");
	mk_proc(OP_CADADR, "cadadr");
	mk_proc(OP_CADDAR, "caddar");
	mk_proc(OP_CADDDR, "cadddr");
	mk_proc(OP_CDAAAR, "cdaaar");
	mk_proc(OP_CDAADR, "cdaadr");
	mk_proc(OP_CDADAR, "cdadar");
	mk_proc(OP_CDADDR, "cdaddr");
	mk_proc(OP_CDDAAR, "cddaar");
	mk_proc(OP_CDDADR, "cddadr");
	mk_proc(OP_CDDDAR, "cdddar");
	mk_proc(OP_CDDDDR, "cddddr");
	mk_proc(OP_LIST, "list");
	mk_proc(OP_LISTTAIL, "list-tail");
	mk_proc(OP_LISTREF, "list-ref");
	mk_proc(OP_LASTPAIR, "last-pair");
	mk_proc(OP_ADD, "+");
	mk_proc(OP_SUB, "-");
	mk_proc(OP_MUL, "*");
	mk_proc(OP_DIV, "/");
	mk_proc(OP_ABS, "abs");
	mk_proc(OP_QUO, "quotient");
	mk_proc(OP_REM, "remainder");
	mk_proc(OP_MOD, "modulo");
	mk_proc(OP_GCD, "gcd");
	mk_proc(OP_LCM, "lcm");
	mk_proc(OP_FLOOR, "floor");
	mk_proc(OP_CEILING, "ceiling");
	mk_proc(OP_TRUNCATE, "truncate");
	mk_proc(OP_ROUND, "round");
	mk_proc(OP_EXP, "exp");
	mk_proc(OP_LOG, "log");
	mk_proc(OP_SIN, "sin");
	mk_proc(OP_COS, "cos");
	mk_proc(OP_TAN, "tan");
	mk_proc(OP_ASIN, "asin");
	mk_proc(OP_ACOS, "acos");
	mk_proc(OP_ATAN, "atan");
	mk_proc(OP_SQRT, "sqrt");
	mk_proc(OP_EXPT, "expt");
	mk_proc(OP_EX2INEX, "exact->inexact");
	mk_proc(OP_INEX2EX, "inexact->exact");
	mk_proc(OP_NUM2STR, "number->string");
	mk_proc(OP_STR2NUM, "string->number");
	mk_proc(OP_CHAR2INT, "char->integer");
	mk_proc(OP_INT2CHAR, "integer->char");
	mk_proc(OP_CHARUPCASE, "char-upcase");
	mk_proc(OP_CHARDNCASE, "char-downcase");
	mk_proc(OP_MKSTRING, "make-string");
	mk_proc(OP_STRING, "string");
	mk_proc(OP_STRLEN, "string-length");
	mk_proc(OP_STRREF, "string-ref");
	mk_proc(OP_STRSET, "string-set!");
	mk_proc(OP_STREQU, "string=?");
	mk_proc(OP_STRLSS, "string<?");
	mk_proc(OP_STRGTR, "string>?");
	mk_proc(OP_STRLEQ, "string<=?");
	mk_proc(OP_STRGEQ, "string>=?");
	mk_proc(OP_STRCIEQU, "string-ci=?");
	mk_proc(OP_STRCILSS, "string-ci<?");
	mk_proc(OP_STRCIGTR, "string-ci>?");
	mk_proc(OP_STRCILEQ, "string-ci<=?");
	mk_proc(OP_STRCIGEQ, "string-ci>=?");
	mk_proc(OP_SUBSTR, "substring");
	mk_proc(OP_STRAPPEND, "string-append");
	mk_proc(OP_STR2LIST, "string->list");
	mk_proc(OP_LIST2STR, "list->string");
	mk_proc(OP_STRCOPY, "string-copy");
	mk_proc(OP_STRFILL, "string-fill!");
	mk_proc(OP_VECTOR, "vector");
	mk_proc(OP_MKVECTOR, "make-vector");
	mk_proc(OP_VECLEN, "vector-length");
	mk_proc(OP_VECREF, "vector-ref");
	mk_proc(OP_VECSET, "vector-set!");
	mk_proc(OP_VEC2LIST, "vector->list");
	mk_proc(OP_LIST2VEC, "list->vector");
	mk_proc(OP_VECFILL, "vector-fill!");
	mk_proc(OP_NOT, "not");
	mk_proc(OP_BOOL, "boolean?");
	mk_proc(OP_SYMBOL, "symbol?");
	mk_proc(OP_SYM2STR, "symbol->string");
	mk_proc(OP_STR2SYM, "string->symbol");
	mk_proc(OP_NUMBER, "number?");
	mk_proc(OP_STRINGP, "string?");
	mk_proc(OP_INTEGER, "integer?");
	mk_proc(OP_REAL, "real?");
	mk_proc(OP_EXACT, "exact?");
	mk_proc(OP_INEXACT, "inexact?");
	mk_proc(OP_CHAR, "char?");
	mk_proc(OP_CHAREQU, "char=?");
	mk_proc(OP_CHARLSS, "char<?");
	mk_proc(OP_CHARGTR, "char>?");
	mk_proc(OP_CHARLEQ, "char<=?");
	mk_proc(OP_CHARGEQ, "char>=?");
	mk_proc(OP_CHARCIEQU, "char-ci=?");
	mk_proc(OP_CHARCILSS, "char-ci<?");
	mk_proc(OP_CHARCIGTR, "char-ci>?");
	mk_proc(OP_CHARCILEQ, "char-ci<=?");
	mk_proc(OP_CHARCIGEQ, "char-ci>=?");
	mk_proc(OP_CHARAP, "char-alphabetic?");
	mk_proc(OP_CHARNP, "char-numeric?");
	mk_proc(OP_CHARWP, "char-whitespace?");
	mk_proc(OP_CHARUP, "char-upper-case?");
	mk_proc(OP_CHARLP, "char-lower-case?");
	mk_proc(OP_PROC, "procedure?");
	mk_proc(OP_PAIR, "pair?");
	mk_proc(OP_LISTP, "list?");
	mk_proc(OP_PORTP, "port?");
	mk_proc(OP_INPORTP, "input-port?");
	mk_proc(OP_OUTPORTP, "output-port?");
	mk_proc(OP_VECTORP, "vector?");
	mk_proc(OP_ENVP, "environment?");
	mk_proc(OP_EQ, "eq?");
	mk_proc(OP_EQV, "eqv?");
	mk_proc(OP_EQUAL, "equal?");
	mk_proc(OP_NULL, "null?");
	mk_proc(OP_EOFOBJP, "eof-object?");
	mk_proc(OP_ZEROP, "zero?");
	mk_proc(OP_POSP, "positive?");
	mk_proc(OP_NEGP, "negative?");
	mk_proc(OP_ODD, "odd?");
	mk_proc(OP_EVEN, "even?");
	mk_proc(OP_NEQ, "=");
	mk_proc(OP_LESS, "<");
	mk_proc(OP_GRE, ">");
	mk_proc(OP_LEQ, "<=");
	mk_proc(OP_GEQ, ">=");
	mk_proc(OP_MAX, "max");
	mk_proc(OP_MIN, "min");
	mk_proc(OP_READ, "read");
	mk_proc(OP_CHAR_READY, "char-ready?");
	mk_proc(OP_WRITE_CHAR, "write-char");
	mk_proc(OP_WRITE, "write");
	mk_proc(OP_DISPLAY, "display");
	mk_proc(OP_NEWLINE, "newline");
	mk_proc(OP_LOAD, "load");
	mk_proc(OP_ERR0, "error");
	mk_proc(OP_REVERSE, "reverse");
	mk_proc(OP_APPEND, "append");
	mk_proc(OP_PUT, "put");
	mk_proc(OP_GET, "get");
	mk_proc(OP_GC, "gc");
	mk_proc(OP_GCVERB, "gc-verbose");
	mk_proc(OP_CALL_INFILE0, "call-with-input-file");
	mk_proc(OP_CALL_OUTFILE0, "call-with-output-file");
	mk_proc(OP_CURR_INPORT, "current-input-port");
	mk_proc(OP_CURR_OUTPORT, "current-output-port");
	mk_proc(OP_WITH_INFILE0, "with-input-from-file");
	mk_proc(OP_WITH_OUTFILE0, "with-output-to-file");
	mk_proc(OP_OPEN_INFILE, "open-input-file");
	mk_proc(OP_OPEN_OUTFILE, "open-output-file");
	mk_proc(OP_OPEN_INOUTFILE, "open-input-output-file");
	mk_proc(OP_OPEN_INSTRING, "open-input-string");
	mk_proc(OP_OPEN_OUTSTRING, "open-output-string");
	mk_proc(OP_OPEN_INOUTSTRING, "open-input-output-string");
	mk_proc(OP_GET_OUTSTRING, "get-output-string");
	mk_proc(OP_CLOSE_INPORT, "close-input-port");
	mk_proc(OP_CLOSE_OUTPORT, "close-output-port");
	mk_proc(OP_CLOSE_PORT, "close-port");
	mk_proc(OP_INT_ENV, "interaction-environment");
	mk_proc(OP_CURR_ENV, "current-environment");
	mk_proc(OP_READ_CHAR, "read-char");
	mk_proc(OP_PEEK_CHAR, "peek-char");
	mk_proc(OP_SET_INPORT, "set-input-port");
	mk_proc(OP_SET_OUTPORT, "set-output-port");
	mk_proc(OP_LIST_LENGTH, "length");	/* a.k */
	mk_proc(OP_MEMQ, "memq");
	mk_proc(OP_MEMV, "memv");
	mk_proc(OP_MEMBER, "member");
	mk_proc(OP_ASSQ, "assq");	/* a.k */
	mk_proc(OP_ASSV, "assv");
	mk_proc(OP_ASSOC, "assoc");
	mk_proc(OP_DEFP, "defined?");
	mk_proc(OP_MKCLOSURE, "make-closure");
	mk_proc(OP_GET_CLOSURE, "get-closure-code");	/* a.k */
	mk_proc(OP_CLOSUREP, "closure?");	/* a.k */
	mk_proc(OP_MACROP, "macro?");	/* a.k */
	mk_proc(OP_MACRO_EXPAND0, "macro-expand");
	mk_proc(OP_ATOMP, "atom?");
	mk_proc(OP_GENSYM, "gensym");
	mk_proc(OP_QUIT, "quit");
}


/* initialize several globals */
void init_globals()
{
	init_vars_global();
	init_syntax();
	init_procs();
	/* intialization of global pointers to special symbols */
	LAMBDA = mk_symbol("lambda");
	QUOTE = mk_symbol("quote");
	QQUOTE = mk_symbol("quasiquote");
	UNQUOTE = mk_symbol("unquote");
	UNQUOTESP = mk_symbol("unquote-splicing");
#ifndef USE_SCHEME_STACK
	dump_base = mk_dumpstack(NIL);
	dump = dump_base;
#else
	dump = NIL;
#endif
	envir = global_env;
	code = NIL;
}

/* initialization of Mini-Scheme */
void scheme_init()
{
	alloc_cellseg();
	gc_verbose = 0;
	init_globals();
}

void scheme_deinit()
{
	oblist = NIL;
	inport = NIL;
	outport = NIL;
	global_env = NIL;
	winders = NIL;
#ifdef USE_COPYING_GC
	gcell_list = NIL;
	psink = sink;
#endif
	args = NIL;
	envir = NIL;
	code = NIL;
#ifndef USE_SCHEME_STACK
	dump_base = NIL;
#endif
	dump = NIL;

	gc_verbose = 0;
	gc(&NIL, &NIL);
}

int scheme_load_file(FILE *fin)
{
	short op;

	if (setjmp(error_jmp) == 0) {
		inport = mk_port(fin, port_input);
		op = OP_T0LVL;
	} else {
		op = OP_QUIT;
	}
	return Eval_Cycle(op);
}

/* ========== Error ==========  */

void FatalError(char *s)
{
	fprintf(stderr, "Fatal error: %s\n", s);
	flushinput();
	args = NIL;
	longjmp(error_jmp, OP_QUIT);
}

/* ========== Main ========== */

int main(int argc, char *argv[])
{
	int ret;
	FILE *fin;

	scheme_init();

	/* Load "init.scm" */
	if ((fin = fopen(InitFile, "r")) != NULL) {
		ret = scheme_load_file(fin);
	} else {
		fprintf(stderr, "Unable to open %s\n", InitFile);
	}

	if (argc > 1) {
		fin = fopen(argv[1], "r");
		if (fin == NULL) {
			fprintf(stderr, "Unable to open %s\n", argv[1]);
			return 1;
		}
	} else {
		fin = stdin;
		printf(banner);
	}
	ret = scheme_load_file(fin);

	scheme_deinit();

	return ret;
}
