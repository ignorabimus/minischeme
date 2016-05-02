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
/* #define VERBOSE */	/* define this if you want verbose GC */
/* #define USE_SCHEME_STACK */	/* define this if you want original-Stack */
#define USE_SETJMP	/* undef this if you do not want to use setjmp() */
#define USE_QQUOTE	/* undef this if you do not need quasiquote */
#define USE_MACRO	/* undef this if you do not need macro */
#define USE_COPYING_GC	/* undef this if you do not want to use Copying GC */


#ifdef USE_QQUOTE
/*--
 *  If your machine can't support "forward single quotation character"
 *  i.e., '`',  you may have trouble to use backquote.
 *  So use '^' in place of '`'.
 */
# define BACKQUOTE '`'
#endif

/*
 *  Basic memory allocation units
 */

#define CELL_SEGSIZE 500000	/* # of cells in one segment */
#define STR_SEGSIZE    2500	/* bytes of one string segment */
#define STR_NSEGMENT    100	/* # of segments for strings */


#define banner "Hello, This is Mini-Scheme Interpreter Version 0.85w2.\n"


#include <stdio.h>
#include <ctype.h>
#ifdef USE_SETJMP
#include <setjmp.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <float.h>
#define prompt "> "
#define InitFile "init.scm"
#ifdef _WIN32
#define snprintf _snprintf
#define stricmp _stricmp
#endif

/* cell structure */
struct cell {
	unsigned short _flag;
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
			struct cell *_next;
		} _port;
		struct {
			char   *_curr;
			size_t  _size;
		} _port_string;
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
#ifdef USE_MACRO
# define T_MACRO      1024	/* 0000010000000000 */
#endif
#define T_PROMISE     2048	/* 0000100000000000 */
#define T_RESULTREADY 4096	/* 0001000000000000 */
#define T_ATOM       16384	/* 0100000000000000 */	/* only for gc */
#define CLRATOM      49151	/* 1011111111111111 */	/* only for gc */
#define MARK         32768	/* 1000000000000000 */
#define UNMARK       32767	/* 0111111111111111 */
#ifdef USE_COPYING_GC
# define T_FORWARDED 32768	/* 1000000000000000 */	/* only for gc */
#endif

/* macros for cell operations */
#define type(p)         ((p)->_flag)

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
#define syntaxnum(p)    strlength(car(p))
#define procnum(p)      ivalue(p)

#define is_closure(p)   (type(p)&T_CLOSURE)
#ifdef USE_MACRO
# define is_macro(p)    (type(p)&T_MACRO)
#endif
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
#define port_next(p)    ((p)->_object._port._next)
#define port_curr(p)    ((p + 1)->_object._port_string._curr)
#define port_size(p)    ((p + 1)->_object._port_string._size)

#define is_promise(p)   (type(p)&T_PROMISE)
#define setpromise(p)   type(p) |= T_PROMISE
#define is_resultready(p) (type(p) & T_RESULTREADY)
#define setresultready(p) type(p) |= T_RESULTREADY

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

/* arrays for segments */
pointer cell_seg;
char   *str_seg[STR_NSEGMENT];
int     str_seglast = -1;

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
pointer port_list = &_NIL;	/* pointer to port table */

/* global pointers to special symbols */
pointer LAMBDA;			/* pointer to syntax lambda */
pointer QUOTE;			/* pointer to syntax quote */

#ifdef USE_QQUOTE
pointer QQUOTE;			/* pointer to symbol quasiquote */
pointer UNQUOTE;		/* pointer to symbol unquote */
pointer UNQUOTESP;		/* pointer to symbol unquote-splicing */
#endif

pointer free_cell = &_NIL;	/* pointer to top of free cells */
long    fcells = 0;		/* # of free cells */

FILE   *srcfp;			/* source file */

#ifdef USE_SETJMP
jmp_buf error_jmp;

#endif
char    gc_verbose;		/* if gc_verbose is not zero, print gc status */

void gc(register pointer *a, register pointer *b);
void FatalError(char *s);
void Error(char *s);

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

int alloc_cellseg()
{
	cell_seg = (pointer)malloc(CELL_SEGSIZE * sizeof(struct cell) * 2);
	if (cell_seg == (pointer)0)
		return 0;
	fcells = CELL_SEGSIZE;
	free_cell = from_space = cell_seg;
	to_space = cell_seg + CELL_SEGSIZE;

	return 1;
}
#else
int alloc_cellseg()
{
	register pointer p;
	register long i;

	p = (pointer)malloc(CELL_SEGSIZE * sizeof(struct cell));
	if (p == (pointer)0)
		return 0;
	free_cell = cell_seg = p;
	fcells += CELL_SEGSIZE;
	for (i = 0; i < CELL_SEGSIZE - 1; i++, p++) {
		type(p) = 0;
		car(p) = NIL;
		cdr(p) = p + 1;
	}
	type(p) = 0;
	car(p) = NIL;
	cdr(p) = NIL;

	return 1;
}
#endif

/* allocate new string segment */
int alloc_strseg(int n)
{
	register char *p;
	register long i;
	register int k;

	for (k = 0; k < n; k++) {
		if (str_seglast >= STR_NSEGMENT)
			return k;
		p = (char *) malloc(STR_SEGSIZE * sizeof(char));
		if (p == (char *) 0)
			return k;
		str_seg[++str_seglast] = p;
		for (i = 0; i < STR_SEGSIZE; i++)
			*p++ = (char) (-1);
	}
	return n;
}

/* get new cell.  parameter a, b is marked by gc. */
pointer get_cell(register pointer *a, register pointer *b)
{
#ifndef USE_COPYING_GC
	register pointer x;
#endif

	if (fcells == 0) {
		gc(a, b);
		if (fcells == 0) {
#ifdef USE_SETJMP
			args = envir = code = dump = NIL;
			gc(&NIL, &NIL);
			if (fcells != 0)
				Error("run out of cells --- rerurn to top level");
			else
				FatalError("run out of cells --- unable to recover cells");
#else
			FatalError("run out of cells  --- unable to recover cells");
#endif
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

#ifndef USE_SCHEME_STACK
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
#endif /* USE_SCHEME_STACK */

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

/* allocate name to string area */
char *store_string(size_t len, const char *str, char fill)
{
	int i;
	char *q;
	size_t remain;

	for (i = 0; i <= str_seglast; i++) {
		for (q = str_seg[i]; *q != (char)(-1); ) {
			while (*q++)
				;	/* get next string */
		}
	}
	remain = STR_SEGSIZE - (q - str_seg[str_seglast]);
	if (remain < len + 2) {
		if (!alloc_strseg(1))
			FatalError("run out of string area");
		q = str_seg[str_seglast];
	}
	if (str != 0) {
		snprintf(q, len + 1, "%s", str);
	} else {
		memset(q, fill, len);
		q[len] = 0;
	}
	return q;
}

/* get new string */
pointer mk_string(char *str)
{
	pointer x = get_cell(&NIL, &NIL);
	size_t len = strlen(str);

	type(x) = (T_STRING | T_ATOM);
	strvalue(x) = store_string(len, str, 0);
	strlength(x) = len;
	return x;
}

pointer mk_empty_string(size_t len, char fill)
{
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_STRING | T_ATOM);
	strvalue(x) = store_string(len, 0, fill);
	strlength(x) = len;
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
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_PORT | T_ATOM);
	x->_isfixnum = prop | port_file;
	port_file(x) = fp;
	port_next(x) = port_list;
	port_list = x;
	return x;
}

pointer mk_port_string(char *str, size_t size, int prop)
{
	pointer x = get_consecutive_cells(2, &NIL);

	type(x + 1) = type(x) = (T_PORT | T_ATOM);
	x->_isfixnum = prop | port_string;
	port_file(x) = (FILE *)str;
	port_next(x) = port_list;
	port_list = x;
	port_curr(x) = str;
	port_size(x) = size;
	return x;
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
	if (is_strport(next)) {
		*++next = *++x;
		type(x) = T_FORWARDED;
		x->_object._forwarded = next;
		return next++ - 1;
	}
	return next++;
}

void gc(register pointer *a, register pointer *b)
{
	register pointer scan, **pp;
	register pointer p, q;

	if (gc_verbose)
		printf("gc...");

	scan = next = to_space;

	/* forward system globals */
	oblist = forward(oblist);
	global_env = forward(global_env);
	inport = forward(inport);
	outport = forward(outport);

	/* forward special symbols */
	LAMBDA = forward(LAMBDA);
	QUOTE = forward(QUOTE);
#ifdef USE_QQUOTE
	QQUOTE = forward(QQUOTE);
	UNQUOTE = forward(UNQUOTE);
	UNQUOTESP = forward(UNQUOTESP);
#endif

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
		switch (type(scan) & 0x03ff) {
		case T_STRING:
		case T_NUMBER:
		case T_PROC:
		case T_CHARACTER:
		case T_PORT:
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
			fprintf(stderr, "Error: Unknown type %d\n", type(scan));
			exit(1);
		}
		++scan;
	}

	for (p = port_list, port_list = NIL; p != NIL; ) {
		if (type(p) == T_FORWARDED) {
			q = p->_object._forwarded;
			p = port_next(q);
			port_next(q) = port_list;
			port_list = q;
		} else {
			if (port_file(p) != NULL) {
				if (is_fileport(p)) {
					fclose(port_file(p));
				} else {
					free(port_file(p));
				}
			}
			p = port_next(p);
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
	if (is_strport(p)) {
		setmark(p + 1);
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
		if (is_mark(p))
			clrmark(p);
		else {
			if (is_port(p)) {
				if (is_fileport(p) && port_file(p) != NULL) {
					fclose(port_file(p));
				} else if (is_strport(p) && port_file(p) != NULL) {
					free(port_file(p));
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
	char *p;

	p = (char *)malloc(BLOCK_SIZE);
	if (p == NULL) {
		return NIL;
	}
	memset(p, 0, BLOCK_SIZE);
	return mk_port_string(p, BLOCK_SIZE, port_output);
}

pointer port_from_string(const char *str, size_t len, int prop)
{
	char *p;

	p = (char *)malloc(len + 1);
	if (p == NULL) {
		return NIL;
	}
	memcpy(p, str, len);
	p[len] = '\0';
	return mk_port_string(p, len + 1, prop);
}

int realloc_port_string(pointer p)
{
	size_t new_size = port_size(p) + BLOCK_SIZE;
	char *str = (char *)malloc(new_size);
	if (str == NULL) {
		return 0;
	}
	memcpy(str, port_file(p), port_size(p));
	memset(str + port_size(p), 0, BLOCK_SIZE);
	free(port_file(p));
	port_file(p) = (FILE *)str;
	port_curr(p) = str + port_size(p);
	port_size(p) = new_size;
	return 1;
}

void port_close(pointer p, int flag)
{
	p->_isfixnum &= ~flag;
	if ((p->_isfixnum & (port_input | port_output)) == 0) {
		if (port_file(p) != NULL) {
			if (is_fileport(p)) {
				fclose(port_file(p));
			} else {
				free(port_file(p));
			}
			port_file(p) = NULL;
		}
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
#ifdef USE_QQUOTE
# define TOK_BQUOTE  7
# define TOK_COMMA   8
# define TOK_ATMARK  9
#endif
#define TOK_SHARP   10

char    strbuff[256];

/* get new character from input file */
int inchar()
{
	int c;

	if (is_fileport(inport)) {
		if (feof(port_file(inport))) {
			fclose(port_file(inport));
			port_file(inport) = srcfp;
			if (port_file(inport) == stdin) {
				printf(prompt);
			}
		}

		c = fgetc(port_file(inport));
		if (c == EOF) {
			if (port_file(inport) == stdin) {
				fprintf(stderr, "Good-bye\n");
				exit(0);
			}
			if (port_file(inport) == srcfp) {
				exit(0);
			}
		}
	} else {
		if (port_curr(inport) == (char *)port_file(inport) + port_size(inport)) {
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
		free(port_file(inport));
		port_file(inport) = stdin;
	}
}

/* check c is delimiter */
int isdelim(char *s, char c)
{
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
		} else if (port_curr(inport) != (char *)port_file(inport)) {
			--port_curr(inport);
		}
	}
}

void putstr(const char *s)
{
	if (is_fileport(outport)) {
		fputs(s, port_file(outport));
	} else {
		char *endp = (char *)port_file(outport) + port_size(outport);
		while (*s) {
			if (port_curr(outport) < endp) {
				*port_curr(outport)++ = *s++;
				if (port_curr(outport) == endp) {
					if (realloc_port_string(outport)) {
						endp = (char *)port_file(outport) + port_size(outport);
					}
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
		char *endp = (char *)port_file(outport) + port_size(outport);
		if (port_curr(outport) < endp) {
			*port_curr(outport)++ = c;
			if (port_curr(outport) == endp) {
				realloc_port_string(outport);
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
#ifdef USE_QQUOTE
	case BACKQUOTE:
		return TOK_BQUOTE;
	case ',':
		if ((c = inchar()) == '@')
			return TOK_ATMARK;
		else {
			backchar(c);
			return TOK_COMMA;
		}
#endif
	case '#':
		return TOK_SHARP;
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

/* print atoms */
int printatom(pointer l, int f)
{
	char *p = "";

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
		if (l->_isfixnum) {
			sprintf(p, "%ld", ivalue(l));
		} else {
			sprintf(p, "%.10g", rvalue(l));
			f = strcspn(p, ".e");
			if (p[f] == 0) {
				p[f] = '.';
				p[f+1] = '0';
				p[f+2] = 0;
			}
		}
	} else if (is_string(l)) {
		if (!f) {
			p = strvalue(l);
		} else {
			printslashstring(strvalue(l));
			return 0;
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
		p = "#<PORT>";
#ifdef USE_MACRO
	} else if (is_macro(l)) {
		p = "#<MACRO>";
#endif
	} else if (is_closure(l)) {
		if (is_promise(l)) {
			if (is_resultready(l)) {
				p = "#<PROMISE (FORCED)>";
			} else {
				p = "#<PROMISE>";
			}
		} else {
			p = "#<CLOSURE>";
		}
	} else if (is_continuation(l)) {
		p = "#<CONTINUATION>";
	} else {
		p = "#<ERROR>";
	}
	if (f < 0)
		return strlen(p);
	putstr(p);
	return 0;
}


/* ========== Rootines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
pointer mk_closure(pointer c, pointer e)
{
	register pointer x = get_cell(&c, &e);

	type(x) = T_CLOSURE;
	car(x) = c;
	cdr(x) = e;
	return x;
}

/* make continuation. */
pointer mk_continuation(pointer d)
{
	register pointer x = get_cell(&NIL, &d);

	type(x) = T_CONTINUATION;
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
pointer append(register pointer a, register pointer b)
{
	register pointer p = b, q;

	if (a != NIL) {
		a = reverse(a);
		while (a != NIL) {
			q = cdr(a);
			cdr(a) = p;
			p = a;
			a = q;
		}
	}
	return p;
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

/* true or false value macro */
#define istrue(p)       ((p) != NIL && (p) != F)
#define isfalse(p)      ((p) == NIL || (p) == F)

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

#endif /* USE_SCHEME_STACK */

#define s_retbool(tf)	s_return((tf) ? T : F)

/* ========== Evaluation Cycle ========== */

/* operator code */
enum {
	OP_LOAD = 0,
	OP_T0LVL,
	OP_T1LVL,
	OP_READ_INTERNAL,
	OP_VALUEPRINT,
	OP_EVAL,
	OP_E0ARGS,
	OP_E1ARGS,
	OP_APPLY,
	OP_DOMACRO,
	OP_GENSYM,

	OP_LAMBDA,
	OP_QUOTE,
	OP_DEF0,
	OP_DEF1,
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
	OP_1MACRO,
	OP_CASE0,
	OP_CASE1,
	OP_CASE2,

	OP_PEVAL,
	OP_PAPPLY,
	OP_CONTINUATION,
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_QUO,
	OP_REM,
	OP_MOD,
	OP_CAR,
	OP_CDR,
	OP_CONS,
	OP_SETCAR,
	OP_SETCDR,
	OP_CHAR2INT,
	OP_INT2CHAR,
	OP_CHARUPCASE,
	OP_CHARDNCASE,
	OP_MKSTRING,
	OP_NOT,
	OP_BOOL,
	OP_NULL,
	OP_EOFOBJP,
	OP_ZEROP,
	OP_POSP,
	OP_NEGP,
	OP_NEQ,
	OP_LESS,
	OP_GRE,
	OP_LEQ,
	OP_GEQ,
	OP_SYMBOL,
	OP_NUMBER,
	OP_STRING,
	OP_INTEGER,
	OP_REAL,
	OP_CHAR,
	OP_PROC,
	OP_PAIR,
	OP_PORTP,
	OP_INPORTP,
	OP_OUTPORTP,
	OP_EQ,
	OP_EQV,
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
	OP_CURR_INPORT,
	OP_CURR_OUTPORT,
	OP_OPEN_INFILE,
	OP_OPEN_OUTFILE,
	OP_OPEN_INOUTFILE,
	OP_OPEN_INSTRING,
	OP_OPEN_OUTSTRING,
	OP_OPEN_INOUTSTRING,
	OP_GET_OUTSTRING,
	OP_CLOSE_INPORT,
	OP_CLOSE_OUTPORT,

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
	OP_RDUNQUOTE,
	OP_RDUQTSP,

	OP_P0LIST,
	OP_P1LIST,

	OP_LIST_LENGTH,
	OP_ASSQ,
	OP_PRINT_WIDTH,
	OP_P0_WIDTH,
	OP_P1_WIDTH,
	OP_GET_CLOSURE,
	OP_CLOSUREP,
	OP_MACROP,
};

/* kernel of this intepreter */
pointer Eval_Cycle(short operator)
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
#ifdef USE_MACRO
				s_save(OP_E0ARGS, NIL, code);
#else
				s_save(OP_E1ARGS, NIL, cdr(code));
#endif
				code = car(code);
				s_goto(OP_EVAL);
			}
		} else {
			s_return(code);
		}

#ifdef USE_MACRO
	case OP_E0ARGS:	/* eval arguments */
		if (is_macro(value)) {	/* macro expansion */
			push_sink(&value);
			s_save(OP_DOMACRO, NIL, NIL);
			args = cons(code, NIL);
			pop_sink();
			code = value;
			s_goto(OP_APPLY);
		} else {
			code = cdr(code);
			s_goto(OP_E1ARGS);
		}
#endif

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
#ifndef USE_SCHEME_STACK
			dump = s_clone(cont_dump(code));
#else
			dump = cont_dump(code);
#endif
			s_return(args != NIL ? car(args) : NIL);
		} else {
			Error_0("Illegal function");
		}
	default:
		break;
	}

	switch (operator) {
	case OP_LOAD:		/* load */
		if (!is_string(car(args))) {
			Error_0("load -- argument is not string");
		}
		if ((port_file(inport) = fopen(strvalue(car(args)), "r")) == NULL) {
			port_file(inport) = srcfp;
			Error_1("Unable to open", car(args));
		}
		if (port_file(inport) == stdin) {
			fprintf(port_file(outport), "loading %s", strvalue(car(args)));
		}
		s_goto(OP_T0LVL);

	case OP_T0LVL:	/* top level */
OP_T0LVL:
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

#ifdef USE_MACRO
	case OP_DOMACRO:	/* do macro */
		code = value;
		s_goto(OP_EVAL);
#endif

	case OP_GENSYM:
		s_return(gensym());

	case OP_LAMBDA:	/* lambda */
		s_return(mk_closure(code, envir));

	case OP_QUOTE:		/* quote */
		s_return(car(code));

	case OP_DEF0:	/* define */
		if (is_pair(car(code))) {
			y = cons(cdar(code), cdr(code));
			y = cons(LAMBDA, y);
			x = caar(code);
			code = y;
		} else {
			x = car(code);
			code = cadr(code);
		}
		if (!is_symbol(x)) {
			Error_0("Variable is not symbol");
		}
		s_save(OP_DEF1, NIL, x);
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
			code = cdr(code);
			s_goto(OP_BEGIN);
		}
		s_save(OP_LET1AST, cdr(code), car(code));
		code = cadaar(code);
		s_goto(OP_EVAL);

	case OP_LET1AST:	/* let* (make new frame) */
		envir = cons(value, envir);	/* save value for gc */
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
		args = NIL;
		value = code;
		code = car(code);
		s_goto(OP_LET1REC);

	case OP_LET1REC:	/* letrec (caluculate parameters) */
OP_LET1REC:
		args = cons(value, args);
		if (is_pair(code)) {	/* continue */
			s_save(OP_LET1REC, args, cdr(code));
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

#ifdef USE_MACRO
	case OP_0MACRO:	/* macro */
		if (is_pair(x = car(code))) {
			if (!is_symbol(car(x))) {
				Error_0("Variable is not symbol");
			}
			s_save(OP_1MACRO, NIL, car(x));
			y = cons(cdar(code), cdr(code));
			code = cons(LAMBDA, y);
		} else {
			if (!is_symbol(x)) {
				Error_0("Variable is not symbol");
			}
			s_save(OP_1MACRO, NIL, x);
			code = cadr(code);
		}
		s_goto(OP_EVAL);

	case OP_1MACRO:	/* macro */
		type(value) |= T_MACRO;
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
#endif

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
				s_save(OP_CASE2, NIL, cdar(x));
				code = caar(x);
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
	case OP_PAPPLY:	/* apply */
		code = car(args);
		args = cadr(args);
		s_goto(OP_APPLY);

	case OP_PEVAL:	/* eval */
		code = car(args);
		args = NIL;
		s_goto(OP_EVAL);

	case OP_CONTINUATION:	/* call-with-current-continuation */
		code = car(args);
#ifndef USE_SCHEME_STACK
		args = cons(mk_continuation(s_clone_save()), NIL);
#else
		args = cons(mk_continuation(dump), NIL);
#endif
		s_goto(OP_APPLY);

	case OP_ADD:		/* + */
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

	case OP_QUO:		/* quotient */
		if (args == NIL || cdr(args) == NIL) {
			Error_0("Needs 2 arguments");
		}
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
		if (args == NIL || cdr(args) == NIL) {
			Error_0("Needs 2 arguments");
		}
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
		if (args == NIL || cdr(args) == NIL) {
			Error_0("Needs 2 arguments");
		}
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

	case OP_CAR:		/* car */
		if (is_pair(car(args))) {
			s_return(caar(args));
		} else {
			Error_0("Unable to car for non-cons cell");
		}

	case OP_CDR:		/* cdr */
		if (is_pair(car(args))) {
			s_return(cdar(args));
		} else {
			Error_0("Unable to cdr for non-cons cell");
		}

	case OP_CONS:		/* cons */
		cdr(args) = cadr(args);
		s_return(args);

	case OP_SETCAR:	/* set-car! */
		if (is_pair(car(args))) {
			caar(args) = cadr(args);
			s_return(car(args));
		} else {
			Error_0("Unable to set-car! for non-cons cell");
		}

	case OP_SETCDR:	/* set-cdr! */
		if (is_pair(car(args))) {
			cdar(args) = cadr(args);
			s_return(car(args));
		} else {
			Error_0("Unable to set-cdr! for non-cons cell");
		}

	case OP_CHAR2INT:	/* char->integer */
		s_return(mk_integer(ivalue(car(args))));

	case OP_INT2CHAR:	/* integer->char */
		s_return(mk_character((unsigned char)ivalue(car(args))));

	case OP_CHARUPCASE:	/* char-upcase */
		s_return(mk_character(toupper((unsigned char)ivalue(car(args)))));

	case OP_CHARDNCASE:	/* char-downcase */
		s_return(mk_character(tolower((unsigned char)ivalue(car(args)))));

	case OP_MKSTRING:	/* make-string */
		if (cdr(args) != NIL) {
			s_return(mk_empty_string(ivalue(car(args)), (char)ivalue(cadr(args))));
		} else {
			s_return(mk_empty_string(ivalue(car(args)), ' '));
		}

	case OP_NOT:		/* not */
		s_retbool(isfalse(car(args)));
	case OP_BOOL:		/* boolean? */
		s_retbool(car(args) == F || car(args) == T);
	case OP_NULL:		/* null? */
		s_retbool(car(args) == NIL);
	case OP_EOFOBJP:	/* eof-object? */
		s_retbool(car(args) == EOF_OBJ);
	case OP_ZEROP:		/* zero? */
		s_retbool(nvalue(car(args)) == 0);
	case OP_POSP:		/* positive? */
		s_retbool(nvalue(car(args)) > 0);
	case OP_NEGP:		/* negative? */
		s_retbool(nvalue(car(args)) < 0);
	case OP_NEQ:		/* = */
		s_retbool(nvalue(car(args)) == nvalue(cadr(args)));
	case OP_LESS:		/* < */
		s_retbool(nvalue(car(args)) < nvalue(cadr(args)));
	case OP_GRE:		/* > */
		s_retbool(nvalue(car(args)) > nvalue(cadr(args)));
	case OP_LEQ:		/* <= */
		s_retbool(nvalue(car(args)) <= nvalue(cadr(args)));
	case OP_GEQ:		/* >= */
		s_retbool(nvalue(car(args)) >= nvalue(cadr(args)));
	case OP_SYMBOL:		/* symbol? */
		s_retbool(is_symbol(car(args)));
	case OP_NUMBER:		/* number? */
		s_retbool(is_number(car(args)));
	case OP_STRING:		/* string? */
		s_retbool(is_string(car(args)));
	case OP_INTEGER:	/* integer? */
		s_retbool(is_integer(car(args)));
	case OP_REAL:		/* real? */
		s_retbool(is_number(car(args)));
	case OP_CHAR:		/* char? */
		s_retbool(is_character(car(args)));
	case OP_PROC:		/* procedure? */
		/*--
		 * continuation should be procedure by the example
		 * (call-with-current-continuation procedure?) ==> #t
		 * in R^3 report sec. 6.9
		 */
		s_retbool(is_proc(car(args)) || is_closure(car(args))
			  || is_continuation(car(args)));
	case OP_PAIR:		/* pair? */
		s_retbool(is_pair(car(args)));
	case OP_PORTP:		/* port? */
		s_retbool(is_port(car(args)));
	case OP_INPORTP:	/* input-port? */
		s_retbool(is_inport(car(args)));
	case OP_OUTPORTP:	/* output-port? */
		s_retbool(is_outport(car(args)));
	case OP_EQ:		/* eq? */
		s_retbool(car(args) == cadr(args));
	case OP_EQV:		/* eqv? */
		s_retbool(eqv(car(args), cadr(args)));

	case OP_FORCE:		/* force */
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
		s_return(value);

	case OP_WRITE_CHAR:	/* write-char */
		if (!is_character(car(args))) {
			Error_0("write-char -- first argument must be character");
		}
	case OP_WRITE:		/* write */
	case OP_DISPLAY:	/* display */
		if (is_pair(cdr(args))) {
			if (cadr(args) != outport) {
				x = cons(outport, NIL);
				s_save(OP_SET_OUTPORT, x, NIL);
				outport = cadr(args);
			}
		}
		args = car(args);
		print_flag = (operator == OP_WRITE) ? 1 : 0;
		s_goto(OP_P0LIST);

	case OP_NEWLINE:	/* newline */
		if (is_pair(args)) {
			if (car(args) != outport) {
				x = cons(outport, NIL);
				s_save(OP_SET_OUTPORT, x, NIL);
				outport = car(args);
			}
		}
		putstr("\n");
		s_return(T);

	case OP_ERR0:	/* error */
		if (!is_string(car(args))) {
			Error_0("error -- first argument must be string");
		}
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
		s_return(reverse(car(args)));

	case OP_APPEND:	/* append */
		s_return(append(car(args), cadr(args)));

	case OP_PUT:		/* put */
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
		break;

	case OP_GC:		/* gc */
		gc(&NIL, &NIL);
		s_return(T);

	case OP_GCVERB:		/* gc-verbose */
	{
		int was = gc_verbose;
		gc_verbose = (car(args) != F);
		s_retbool(was);
	}

	case OP_CURR_INPORT:	/* current-input-port */
		s_return(inport);

	case OP_CURR_OUTPORT:	/* current-output-port */
		s_return(outport);

	case OP_OPEN_INFILE:	/* open-input-file */
		if (!is_string(car(args))) {
			Error_0("open-input-file -- first argument must be string");
		}
		x = port_from_filename(strvalue(car(args)), port_input);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_OUTFILE:	/* open-output-file */
		if (!is_string(car(args))) {
			Error_0("open-output-file -- first argument must be string");
		}
		x = port_from_filename(strvalue(car(args)), port_output);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_INOUTFILE:	/* open-input-output-file */
		if (!is_string(car(args))) {
			Error_0("open-input-output-file -- first argument must be string");
		}
		x = port_from_filename(strvalue(car(args)), port_input | port_output);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_INSTRING:	/* open-input-string */
		if (!is_string(car(args))) {
			Error_0("open-input-string -- first argument must be string");
		}
		x = port_from_string(strvalue(car(args)), strlen(strvalue(car(args))), port_input);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_OUTSTRING:	/* open-output-string */
		x = port_from_scratch();
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_OPEN_INOUTSTRING:	/* open-input-output-string */
		if (!is_string(car(args))) {
			Error_0("open-input-string -- first argument must be string");
		}
		x = port_from_string(strvalue(car(args)), strlen(strvalue(car(args))), port_input | port_output);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case OP_GET_OUTSTRING:	/* get-output-string */
		x = car(args);
		if (is_strport(x) && port_file(x) != NULL) {
			s_return(mk_string((char *)port_file(x)));
		}
		s_return(F);

	case OP_CLOSE_INPORT: /* close-input-port */
		port_close(car(args), port_input);
		s_return(T);

	case OP_CLOSE_OUTPORT: /* close-output-port */
		port_close(car(args), port_output);
		s_return(T);

		/* ========== reading part ========== */
	case OP_READ:			/* read */
		if (is_pair(args)) {
			if (!is_inport(car(args))) {
				Error_1("read: not an input port:", car(args));
			}
			if (car(args) != inport) {
				x = inport;
				x = cons(x, NIL);
				s_save(OP_SET_INPORT, x, NIL);
				inport = car(args);
			}
		}
		s_goto(OP_READ_INTERNAL);

	case OP_READ_CHAR:		/* read-char */
	case OP_PEEK_CHAR:		/* peek-char */
		if (is_pair(args)) {
			if (car(args) != inport) {
				x = inport;
				x = cons(x, NIL);
				s_save(OP_SET_INPORT, x, NIL);
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

	case OP_SET_INPORT:		/* set-input-port */
		inport = car(args);
		s_return(value);

	case OP_SET_OUTPORT:	/* set-output-port */
		outport = car(args);
		s_return(value);

	case OP_RDSEXPR:
OP_RDSEXPR:
		switch (tok) {
		case TOK_EOF:
			s_return(EOF_OBJ);
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
#ifdef USE_QQUOTE
		case TOK_BQUOTE:
			s_save(OP_RDQQUOTE, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_COMMA:
			s_save(OP_RDUNQUOTE, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_ATMARK:
			s_save(OP_RDUQTSP, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
#endif
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

#ifdef USE_QQUOTE
	case OP_RDQQUOTE:
		x = cons(value, NIL);
		s_return(cons(QQUOTE, x));

	case OP_RDUNQUOTE:
		x = cons(value, NIL);
		s_return(cons(UNQUOTE, x));

	case OP_RDUQTSP:
		x = cons(value, NIL);
		s_return(cons(UNQUOTESP, x));
#endif

	/* ========== printing part ========== */
	case OP_P0LIST:
OP_P0LIST:
		if (!is_pair(args)) {
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
		} else {
			if (args != NIL) {
				putstr(" . ");
				printatom(args, print_flag);
			}
			putstr(")");
			s_return(T);
		}

	case OP_LIST_LENGTH:	/* length */	/* a.k */
		for (x = car(args), w = 0; is_pair(x); x = cdr(x))
			++w;
		s_return(mk_integer(w));

	case OP_ASSQ:		/* assq */	/* a.k */
		x = car(args);
		for (y = cadr(args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
				Error_0("Unable to handle non pair element");
			}
			if (x == caar(y))
				break;
		}
		if (is_pair(y)) {
			s_return(car(y));
		} else {
			s_return(F);
		}

	case OP_PRINT_WIDTH:	/* print-width */	/* a.k */
		w = 0;
		args = car(args);
		print_flag = -1;
		s_goto(OP_P0_WIDTH);

	case OP_P0_WIDTH:
OP_P0_WIDTH:
		if (!is_pair(args)) {
			w += printatom(args, print_flag);
			s_return(mk_integer(w));
		} else if (car(args) == QUOTE
			   && ok_abbrev(cdr(args))) {
			++w;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else if (car(args) == QQUOTE
			   && ok_abbrev(cdr(args))) {
			++w;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else if (car(args) == UNQUOTE
			   && ok_abbrev(cdr(args))) {
			++w;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else if (car(args) == UNQUOTESP
			   && ok_abbrev(cdr(args))) {
			w += 2;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else {
			++w;
			s_save(OP_P1_WIDTH, cdr(args), NIL);
			args = car(args);
			s_goto(OP_P0_WIDTH);
		}

	case OP_P1_WIDTH:
		if (is_pair(args)) {
			s_save(OP_P1_WIDTH, cdr(args), NIL);
			++w;
			args = car(args);
			s_goto(OP_P0_WIDTH);
		} else {
			if (args != NIL)
				w += 3 + printatom(args, print_flag);
			++w;
			s_return(mk_integer(w));
		}

	case OP_GET_CLOSURE:	/* get-closure-code */	/* a.k */
		args = car(args);
		if (args == NIL) {
			s_return(F);
		} else if (is_closure(args)) {
			s_return(cons(LAMBDA, closure_code(value)));
#ifdef USE_MACRO
		} else if (is_macro(args)) {
			s_return(cons(LAMBDA, closure_code(value)));
#endif
		} else {
			s_return(F);
		}

	case OP_CLOSUREP:		/* closure? */
		/*
		 * Note, macro object is also a closure.
		 * Therefore, (closure? <#MACRO>) ==> #t
		 */
		if (car(args) == NIL) {
			s_return(F);
		}
		s_retbool(is_closure(car(args)));
#ifdef USE_MACRO
	case OP_MACROP:		/* macro? */
		if (car(args) == NIL) {
			s_return(F);
		}
		s_retbool(is_macro(car(args)));
#endif

	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);
	}

	return NIL;
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

	/* init input/output file */
	inport = mk_port(srcfp, port_input);
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
	mk_syntax(OP_DEF0, "define");
	mk_syntax(OP_IF0, "if");
	mk_syntax(OP_BEGIN, "begin");
	mk_syntax(OP_SET0, "set!");
	mk_syntax(OP_LET0, "let");
	mk_syntax(OP_LET0AST, "let*");
	mk_syntax(OP_LET0REC, "letrec");
	mk_syntax(OP_COND0, "cond");
	mk_syntax(OP_DELAY, "delay");
	mk_syntax(OP_AND0, "and");
	mk_syntax(OP_OR0, "or");
	mk_syntax(OP_C0STREAM, "cons-stream");
#ifdef USE_MACRO
	mk_syntax(OP_0MACRO, "macro");
#endif
	mk_syntax(OP_CASE0, "case");
}


void init_procs()
{
	/* init procedure */
	mk_proc(OP_PEVAL, "eval");
	mk_proc(OP_PAPPLY, "apply");
	mk_proc(OP_CONTINUATION, "call-with-current-continuation");
	mk_proc(OP_FORCE, "force");
	mk_proc(OP_CAR, "car");
	mk_proc(OP_CDR, "cdr");
	mk_proc(OP_CONS, "cons");
	mk_proc(OP_SETCAR, "set-car!");
	mk_proc(OP_SETCDR, "set-cdr!");
	mk_proc(OP_ADD, "+");
	mk_proc(OP_SUB, "-");
	mk_proc(OP_MUL, "*");
	mk_proc(OP_DIV, "/");
	mk_proc(OP_QUO, "quotient");
	mk_proc(OP_REM, "remainder");
	mk_proc(OP_MOD, "modulo");
	mk_proc(OP_CHAR2INT, "char->integer");
	mk_proc(OP_INT2CHAR, "integer->char");
	mk_proc(OP_CHARUPCASE, "char-upcase");
	mk_proc(OP_CHARDNCASE, "char-downcase");
	mk_proc(OP_MKSTRING, "make-string");
	mk_proc(OP_NOT, "not");
	mk_proc(OP_BOOL, "boolean?");
	mk_proc(OP_SYMBOL, "symbol?");
	mk_proc(OP_NUMBER, "number?");
	mk_proc(OP_STRING, "string?");
	mk_proc(OP_INTEGER, "integer?");
	mk_proc(OP_REAL, "real?");
	mk_proc(OP_CHAR, "char?");
	mk_proc(OP_PROC, "procedure?");
	mk_proc(OP_PAIR, "pair?");
	mk_proc(OP_PORTP, "port?");
	mk_proc(OP_INPORTP, "input-port?");
	mk_proc(OP_OUTPORTP, "output-port?");
	mk_proc(OP_EQV, "eqv?");
	mk_proc(OP_EQ, "eq?");
	mk_proc(OP_NULL, "null?");
	mk_proc(OP_EOFOBJP, "eof-object?");
	mk_proc(OP_ZEROP, "zero?");
	mk_proc(OP_POSP, "positive?");
	mk_proc(OP_NEGP, "negative?");
	mk_proc(OP_NEQ, "=");
	mk_proc(OP_LESS, "<");
	mk_proc(OP_GRE, ">");
	mk_proc(OP_LEQ, "<=");
	mk_proc(OP_GEQ, ">=");
	mk_proc(OP_READ, "read");
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
	mk_proc(OP_CURR_INPORT, "current-input-port");
	mk_proc(OP_CURR_OUTPORT, "current-output-port");
	mk_proc(OP_OPEN_INFILE, "open-input-file");
	mk_proc(OP_OPEN_OUTFILE, "open-output-file");
	mk_proc(OP_OPEN_INOUTFILE, "open-input-output-file");
	mk_proc(OP_OPEN_INSTRING, "open-input-string");
	mk_proc(OP_OPEN_OUTSTRING, "open-output-string");
	mk_proc(OP_OPEN_INOUTSTRING, "open-input-output-string");
	mk_proc(OP_GET_OUTSTRING, "get-output-string");
	mk_proc(OP_CLOSE_INPORT, "close-input-port");
	mk_proc(OP_CLOSE_OUTPORT, "close-output-port");
	mk_proc(OP_READ_CHAR, "read-char");
	mk_proc(OP_PEEK_CHAR, "peek-char");
	mk_proc(OP_SET_INPORT, "set-input-port");
	mk_proc(OP_SET_OUTPORT, "set-output-port");
	mk_proc(OP_LIST_LENGTH, "length");	/* a.k */
	mk_proc(OP_ASSQ, "assq");	/* a.k */
	mk_proc(OP_PRINT_WIDTH, "print-width");	/* a.k */
	mk_proc(OP_GET_CLOSURE, "get-closure-code");	/* a.k */
	mk_proc(OP_CLOSUREP, "closure?");	/* a.k */
#ifdef USE_MACRO
	mk_proc(OP_MACROP, "macro?");	/* a.k */
#endif
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
#ifdef USE_QQUOTE
	QQUOTE = mk_symbol("quasiquote");
	UNQUOTE = mk_symbol("unquote");
	UNQUOTESP = mk_symbol("unquote-splicing");
#endif
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
void init_scheme()
{
	if (alloc_cellseg() == 0)
		FatalError("Unable to allocate initial cell segments");
	if (!alloc_strseg(1))
		FatalError("Unable to allocate initial string segments");
#ifdef VERBOSE
	gc_verbose = 1;
#else
	gc_verbose = 0;
#endif
	init_globals();
}

/* ========== Error ==========  */

void FatalError(char *s)
{
	fprintf(stderr, "Fatal error: %s\n", s);
	exit(1);
}

#ifdef USE_SETJMP
void Error(char *s)
{
	fprintf(stderr, "Error: %s\n", s);
	flushinput();
	longjmp(error_jmp, OP_T0LVL);
}
#endif

/* ========== Main ========== */

int main(int argc, char *argv[])
{
	short op = (short)OP_LOAD;

	if (argc > 1) {
		if ((srcfp = fopen(argv[1], "r")) == NULL) {
			fprintf(stderr, "Unable to open %s\n", argv[1]);
			return 1;
		}
	} else {
		srcfp = stdin;
		printf(banner);
	}

	init_scheme();
	args = cons(mk_string(InitFile), NIL);
#ifdef USE_SETJMP
	op = (short)setjmp(error_jmp);
#endif
	Eval_Cycle(op);

	return 0;
}
