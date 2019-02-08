/*
 * This software is released under the MIT License, see the LICENSE file.
 *
 * This version has been modified by Tatsuya WATANABE.
 *	current version is 0.85w11 (2019)
 *
 * Below are the original credits.
 */
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
 *--
 */

#include "miniscm.h"

/*--
 *  If your machine can't support "forward single quotation character"
 *  i.e., '`',  you may have trouble to use backquote.
 *  So use '^' in place of '`'.
 */
#define BACKQUOTE '`'

#if STANDALONE
#define banner "Hello, This is Mini-Scheme Interpreter Version 0.85w11.\n"
#define InitFile "init.scm"
#endif

#include <ctype.h>

#include <string.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#define prompt "> "
#ifdef _WIN32
#define snprintf _snprintf
#define stricmp _stricmp
#endif

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
struct cell _UNDEF;
pointer UNDEF = &_UNDEF;	/* special cell representing undefined */
pointer oblist = &_NIL;	/* pointer to symbol table */
pointer global_env;		/* pointer to global environment */
struct cell _ZERO;		/* special cell representing integer 0 */
struct cell _ONE;		/* special cell representing integer 1 */

pointer inport = &_NIL;		/* pointer to current-input-port */
pointer outport = &_NIL;	/* pointer to current-output-port */

pointer winders = &_NIL;	/* pointer to winders list */

pointer strbuff = &_NIL;	/* pointer to string buffer */

#ifdef USE_COPYING_GC
pointer gcell_list = &_NIL;	/* pointer to cell table */
#define gcell_next(p) car((p) + 1)
#endif

pointer value;
pointer mark_x;
pointer mark_y;

pointer c_nest;			/* pointer to nested C calls list */
pointer c_sink;			/* pointer to sink arguments list */

/* global pointers to special symbols */
pointer LAMBDA;			/* pointer to syntax lambda */
pointer QUOTE;			/* pointer to syntax quote */

pointer QQUOTE;			/* pointer to symbol quasiquote */
pointer UNQUOTE;		/* pointer to symbol unquote */
pointer UNQUOTESP;		/* pointer to symbol unquote-splicing */

pointer ELLIPSIS;		/* pointer to symbol ... */

pointer free_cell = &_NIL;	/* pointer to top of free cells */
size_t  fcells = 0;			/* # of free cells */

pointer load_stack[MAXFIL];	/* stack of loading files */
int     load_files;			/* # of loading files */

jmp_buf error_jmp;

char    gc_verbose;		/* if gc_verbose is not zero, print gc status */
int     interactive_repl = 0;

void gc(pointer *a, pointer *b);
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

/* ========== Routines for UTF-8 characters ========== */

unsigned char getc_save[4];	/* getc save buffer */
int getc_save_count = 0;	/* getc save count */

int internal_fgetc(FILE *fin)
{
	if (fin == stdin && getc_save_count > 0) {
		return getc_save[--getc_save_count];
	}
	return fgetc(fin);
}

void internal_ungetc(int c, FILE *fin)
{
	if (fin == stdin) {
		if (getc_save_count < sizeof(getc_save)) {
			getc_save[getc_save_count++] = (unsigned char)c;
		} else {
			ungetc(c, fin);
		}
	} else {
		if (ftell(fin) > 0) {
			fseek(fin, -1, SEEK_CUR);
		} else {
			ungetc(c, fin);
		}
	}
}

/* internal description of "extended" UTF-32
 *
 *  0x00000000 - 0x0000007F (UTF-8  0x00 - 7F)
 *  0x00000080 - 0x000007FF (UTF-8  0xC2 80 - DF BF)
 *  0x00000800 - 0x0000FFFF (UTF-8  0xE0 A0 80 - EF BF BF)
 *  0x00010000 - 0x0010FFFF (UTF-8  0xF0 90 80 80 - F4 8F BF BF)
 *  0xFFFFFF00 - 0xFFFFFF7F (!UTF-8 0x80 - FF)
 *  0xFFFFFFFF              (EOF)
 */
size_t utf32_to_utf8(const int utf32, char *const utf8)
{
	if (utf32 < 0x00) {
		if (utf8 != NULL) {
			utf8[0] = (char)(-utf32);
		}
		return 1;
	}
	if (utf32 < 0x80) {
		if (utf8 != NULL) {
			utf8[0] = (char)utf32;
		}
		return 1;
	}
	if (utf32 < 0x800) {
		if (utf8 != NULL) {
			utf8[0] = 0xC0 | (char)(utf32 >> 6);
			utf8[1] = 0x80 | (utf32 & 0x3F);
		}
		return 2;
	}
	if (utf32 < 0x10000) {
		if (utf8 != NULL) {
			utf8[0] = 0xE0 | (char)(utf32 >> 12);
			utf8[1] = 0x80 | (utf32 >> 6 & 0x3F);
			utf8[2] = 0x80 | (utf32 & 0x3F);
		}
		return 3;
	}
	if (utf32 < 0x110000) {
		if (utf8 != NULL) {
			utf8[0] = 0xF0 | (char)(utf32 >> 18);
			utf8[1] = 0x80 | (utf32 >> 12 & 0x3F);
			utf8[2] = 0x80 | (utf32 >> 6 & 0x3F);
			utf8[3] = 0x80 | (utf32 & 0x3F);
		}
		return 4;
	}
	return 0;
}

int utf32_toupper(int c)
{
	return isascii(c) ? toupper(c) : c;	/* only ASCII */
}

int utf32_tolower(int c)
{
	return isascii(c) ? tolower(c) : c;	/* only ASCII */
}

int utf32_isalpha(int c)
{
	return isascii(c) && isalpha(c);	/* only ASCII */
}

int utf32_isdigit(int c)
{
	return isascii(c) && isdigit(c);	/* only ASCII */
}

int utf32_isspace(int c)
{
	return isascii(c) && isspace(c);	/* only ASCII */
}

int utf32_isupper(int c)
{
	return isascii(c) && isupper(c);	/* only ASCII */
}

int utf32_islower(int c)
{
	return isascii(c) && islower(c);	/* only ASCII */
}

int utf8_fgetc(FILE *fin)
{
	int p[4];

	p[0] = internal_fgetc(fin);
	if (p[0] < 0x80) {
		return p[0];
	} else if (p[0] < 0xC2) {
		return -p[0];
	} else if (p[0] < 0xE0)  {
		p[1] = internal_fgetc(fin);
		if (p[1] < 0x80 || 0xBF < p[1]) {
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		return ((p[0] & 0x1F) << 6) | (p[1] & 0x3F);
	} else if (p[0] < 0xF0) {
		p[1] = internal_fgetc(fin);
		if (p[1] < (p[0] == 0xE0 ? 0xA0 : 0x80) || (p[0] == 0xED ? 0x9F : 0xBF) < p[1]) {
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		p[2] = internal_fgetc(fin);
		if (p[2] < 0x80 || 0xBF < p[2]) {
			internal_ungetc(p[2], fin);
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		return ((p[0] & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F);
	} else if (p[0] < 0xF5) {
		p[1] = internal_fgetc(fin);
		if (p[1] < (p[0] == 0xF0 ? 0x90 : 0x80) || (p[0] == 0xF4 ? 0x8F : 0xBF) < p[1]) {
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		p[2] = internal_fgetc(fin);
		if (p[2] < 0x80 || 0xBF < p[2]) {
			internal_ungetc(p[2], fin);
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		p[3] = internal_fgetc(fin);
		if (p[3] < 0x80 || 0xBF < p[3]) {
			internal_ungetc(p[3], fin);
			internal_ungetc(p[2], fin);
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		return ((p[0] & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F);
	} else {
		return -p[0];
	}
}

size_t utf8_get_next(const char *utf8, int *utf32)
{
	const unsigned char *p = (unsigned char *)utf8;

	if (p[0] < 0x80) {
		if (utf32 != NULL) {
			*utf32 = p[0];
		}
		return 1;
	} else if (p[0] < 0xC2) {
		if (utf32 != NULL) {
			*utf32 = -p[0];
		}
		return 1;
	} else if (p[0] < 0xE0)  {
		if (p[1] < 0x80 || 0xBF < p[1]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (utf32 != NULL) {
			*utf32 = ((p[0] & 0x1F) << 6) | (p[1] & 0x3F);
		}
		return 2;
	} else if (p[0] < 0xF0) {
		if (p[1] < (p[0] == 0xE0 ? 0xA0 : 0x80) || (p[0] == 0xED ? 0x9F : 0xBF) < p[1]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (p[2] < 0x80 || 0xBF < p[2]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (utf32 != NULL) {
			*utf32 = ((p[0] & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F);
		}
		return 3;
	} else if (p[0] < 0xF5) {
		if (p[1] < (p[0] == 0xF0 ? 0x90 : 0x80) || (p[0] == 0xF4 ? 0x8F : 0xBF) < p[1]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (p[2] < 0x80 || 0xBF < p[2]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (p[3] < 0x80 || 0xBF < p[3]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (utf32 != NULL) {
			*utf32 = ((p[0] & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F);
		}
		return 4;
	} else {
		if (utf32 != NULL) {
			*utf32 = -p[0];
		}
		return 1;
	}
}

size_t utf8_strlen(const char *s)
{
	size_t count = 0;

	while (*s) {
		s += utf8_get_next(s, NULL);
		count++;
	}
	return count;
}

int utf8_strref(const char *s, size_t pos)
{
	int c;

	while (*s) {
		s += utf8_get_next(s, &c);
		if (pos-- == 0) return c;
	}
	return -1;
}

int utf8_strpos(const char *s, size_t pos)
{
	const char *t = s;

	while (*s || pos == 0) {
		if (pos-- == 0) return (int)(s - t);
		s += utf8_get_next(s, NULL);
	}
	return -1;
}

int utf8_stricmp(const char *s1, const char *s2)
{
	const char *p1 = s1, *p2 = s2;
	int c1, c2;

	do {
		p1 += utf8_get_next(p1, &c1);
		p2 += utf8_get_next(p2, &c2);
		c1 = utf32_tolower(c1);
		c2 = utf32_tolower(c2);
	} while (c1 != 0 && c2 != 0 && c1 == c2);

	return c1 - c2;
}

/* ========== Routines for Cells ========== */

/* allocate new cell segment */
#ifdef USE_COPYING_GC
pointer from_space;
pointer to_space;

void alloc_cellseg(void)
{
	fcells = CELL_SEGSIZE;
	free_cell = from_space = cell_seg;
	to_space = cell_seg + CELL_SEGSIZE;
}
#else
void alloc_cellseg(void)
{
	pointer p;
	int i;

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
pointer get_cell(pointer *a, pointer *b)
{
#ifndef USE_COPYING_GC
	pointer x;
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
pointer find_consecutive_cells(size_t n)
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
pointer find_consecutive_cells(size_t n)
{
	pointer *pp = &free_cell;

	while (*pp != NIL) {
		pointer p = *pp;
		size_t cnt;
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

pointer get_consecutive_cells(size_t n, pointer *a, pointer *b)
{
	pointer x;

	x = find_consecutive_cells(n);
	if (x == NIL) {
		gc(a, b);
		x = find_consecutive_cells(n);
		if (x == NIL) {
			FatalError("run out of cells  --- unable to recover consecutive cells");
		}
	}
	return x;
}

void push_recent_alloc(pointer recent)
{
	pointer holder = get_cell(&recent, &NIL);

	type(holder) = T_PAIR;
	car(holder) = recent;
	cdr(holder) = c_sink;
	c_sink = holder;
}

/* get new cons cell */
pointer cons(pointer a, pointer b)
{
	pointer x = get_cell(&a, &b);

	type(x) = T_PAIR;
	exttype(x) = 0;
	car(x) = a;
	cdr(x) = b;
	return x;
}

pointer mk_character(int c)
{
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_CHARACTER | T_ATOM);
	exttype(x) = 0;
	ivalue(x) = c;
	set_num_integer(x);
	return x;
}

pointer mk_integer(int32_t num)
{
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	exttype(x) = 0;
	ivalue(x) = num;
	bignum(x) = NIL;
	set_num_integer(x);
	return x;
}

pointer mk_bignum(int32_t col, pointer bn)
{
	pointer x = get_cell(&bn, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	exttype(x) = 0;
	ivalue(x) = col;
	bignum(x) = bn;
	set_num_integer(x);
	return x;
}

pointer mk_real(double num)
{
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	exttype(x) = 0;
	rvalue(x) = num;
	set_num_real(x);
	return x;
}

/* get number atom */
pointer mk_number(pointer v)
{
	return v->_isfixnum ? (bignum(v) == NIL ? mk_integer(ivalue(v)) : mk_bignum(ivalue(v), bignum(v))) : mk_real(rvalue(v));
}

/* get new memblock */
pointer mk_memblock(size_t len, pointer *a, pointer *b)
{
	pointer x = get_consecutive_cells(2 + len / sizeof(struct cell), a, b);

#ifdef USE_COPYING_GC
	strvalue(x) = (char *)(x + 1);
#else
	x += 1 + len / sizeof(struct cell);
	strvalue(x) = (char *)(x - (1 + len / sizeof(struct cell)));
#endif
	type(x) = (T_MEMBLOCK | T_ATOM);
	strlength(x) = len;
	return x;
}

pointer mk_integer_from_str(const char *s, size_t len, int b)
{
	int32_t i, j, col, sign;
	pointer m, x;
	uint32_t *temp;

	for (i = 0; isspace(s[i]); i++);
	if (s[i] == '-') {
		sign = -1;
		i++;
	} else if (s[i] == '+') {
		sign = 1;
		i++;
	} else {
		sign = 1;
	}
	col = ((int32_t)((len - i) * log(b) / log(2) + 1) + 31) / 32;
	m = mk_memblock(col * sizeof(uint32_t), &NIL, &NIL);
	temp = (uint32_t *)strvalue(m);
	memset(temp, 0, col * sizeof(uint32_t));
	for (col = 1; s[i]; i++) {
		uint64_t t;
		if (b <= 10) {
			if (s[i] < '0' || '0' + b - 1 < s[i]) {
				return F;
			}
			t = (uint64_t)(s[i] - '0') << 32;
		} else {
			int c = toupper(s[i]);
			if ('0' <= c && c <= '9') {
				c -= '0';
			} else if ('A' <= c && c <= 'A' + b - 11){
				c += 10 - 'A';
			} else {
				return F;
			}
			t = (uint64_t)c << 32;
		}
		for (j = 0; j < col; j++) {
			t = (uint64_t)temp[j] * b + (t >> 32);
			temp[j] = (uint32_t)(t & UINT32_MAX);
		}
		if (t >> 32) temp[col++] = (uint32_t)(t >> 32);
	}
	x = get_cell(&m, &NIL);
	type(x) = (T_NUMBER | T_ATOM);
	exttype(x) = 0;
	if (col == 1 && ((sign == 1 && temp[0] <= INT32_MAX) || (sign == -1 && temp[0] <= INT32_MAX + 1UL))) {
		ivalue(x) = sign * temp[0];
		bignum(x) = NIL;
	} else {
		ivalue(x) = sign * col;
		bignum(x) = m;
	}
	set_num_integer(x);
	return x;
}

/* get new string */
pointer get_string_cell(size_t len, pointer *a)
{
	pointer x = mk_memblock(len, a, &NIL);
	pointer y = get_cell(&x, a);

	type(y) = (T_STRING | T_ATOM);
	exttype(y) = 0;
	strvalue(y) = (char *)car(x);
	strlength(y) = len;
	return y;
}

pointer mk_string(const char *str)
{
	size_t len = strlen(str);
	pointer x = get_string_cell(len, &NIL);
	snprintf(strvalue(x), len + 1, "%s", str);
	return x;
}

pointer mk_counted_string(const char *str, size_t len)
{
	pointer x = get_string_cell(len, &NIL);
	snprintf(strvalue(x), len + 1, "%s", str);
	return x;
}

pointer mk_empty_string(size_t len, int fill)
{
	char utf8[4];
	size_t i, n = utf32_to_utf8(fill, utf8);
	pointer x = get_string_cell(n * len, &NIL);
	for (i = 0; i < len; i++) {
		memcpy(strvalue(x) + n * i, utf8, n);
	}
	strvalue(x)[n * len] = 0;
	return x;
}

/* get new symbol */
pointer mk_symbol(const char *name)
{
	pointer x, y = NIL;

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

	x = mk_string(name);
	type(x) |= T_SYMBOL;
	oblist = cons(x, oblist);
	return car(oblist);
}

/* get new uninterned-symbol */
pointer mk_uninterned_symbol(const char *name)
{
	pointer x;

	x = mk_string(name);
	type(x) |= T_SYMBOL;
	return x;
}

pointer gensym(void)
{
	char name[40];
	static unsigned long gensym_cnt;

	snprintf(name, 40, "gensym-%lu", gensym_cnt++);
	return mk_uninterned_symbol(name);
}

/* make symbol or number atom from string */
pointer mk_atom(const char *q)
{
	const char *p;
	char c;
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
		if (!isdigit((unsigned char)c)) {
			return mk_symbol(q);
		}
	} else if (c == '.') {
		has_dec_point = 1;
		c = *p++;
		if (!isdigit((unsigned char)c)) {
			return mk_symbol(q);
		}
	} else if (!isdigit((unsigned char)c)) {
		return mk_symbol(q);
	}

	for ( ; (c = *p) != 0; ++p) {
		if (!isdigit((unsigned char)c)) {
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
					if ((*p == '-') || (*p == '+') || isdigit((unsigned char)*p)) {
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
	return mk_integer_from_str(q, p - q, 10);
}

/* make constant */
pointer mk_const(const char *name)
{
	if (!strcmp(name, "t")) {
		return T;
	} else if (!strcmp(name, "f")) {
		return F;
	} else if (*name == 'b') {	/* #b (binary) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 2);
	} else if (*name == 'o') {	/* #o (octal) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 8);
	} else if (*name == 'd') {	/* #d (decimal) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 10);
	} else if (*name == 'x') {	/* #x (hex) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 16);
	} else if (*name == '\\') { /* #\w (character) */
		int c = 0;
		if (utf8_stricmp(name + 1, "space") == 0) {
			return mk_character(' ');
		} else if (utf8_stricmp(name + 1, "newline") == 0) {
			return mk_character('\n');
		} else if (utf8_stricmp(name + 1, "return") == 0) {
			return mk_character('\r');
		} else if (utf8_stricmp(name + 1, "tab") == 0) {
			return mk_character('\t');
		} else if (name[1] == 'x' && name[2] != 0) {
			if (sscanf(name + 2, "%x", (unsigned int *)&c) == 1 && c < 0x110000) {
				return mk_character(c);
			} else {
				return NIL;
			}
		} else if (name[utf8_get_next(name + 1, &c) + 1] == '\0') {
			return mk_character(c);
		} else {
			return NIL;
		}
	} else {
		return NIL;
	}
}

pointer mk_port(FILE *fp, int prop)
{
	pointer x = get_consecutive_cells(2, &NIL, &NIL);

	type(x + 1) = type(x) = (T_PORT | T_ATOM);
	exttype(x) = 0;
	(x + 1)->_isfixnum = x->_isfixnum = (unsigned char)(prop | port_file);
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
	exttype(x) = 0;
	x->_isfixnum = (unsigned char)(prop | port_string);
	port_file(x) = (FILE *)p;
	port_curr(x) = strvalue(p);
	return x;
}

void fill_vector(pointer v, pointer a)
{
	int i;
	int n = 1 + (int)ivalue(v) / 2 + (int)ivalue(v) % 2;

	for (i = 1; i < n; i++) {
		type(v + i) = T_PAIR;
		cdr(v + i) = car(v + i) = a;
	}
}

pointer mk_vector(int len)
{
	int n = 1 + len / 2 + len % 2;
	pointer x = get_consecutive_cells(n, &NIL, &NIL);

	type(x) = (T_VECTOR | T_ATOM);
	exttype(x) = 0;
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

pointer mk_foreign_func(foreign_func ff, pointer *pp)
{
	pointer x = get_cell(pp, &NIL);

	type(x) = (T_FOREIGN | T_ATOM);
	exttype(x) = 0;
	foreignfnc(x) = ff;
	return x;
}

#ifndef USE_SCHEME_STACK
/* get dump stack */
pointer mk_dumpstack(pointer next)
{
	pointer x = get_consecutive_cells(3, &next, &NIL);

	type(x) = T_PAIR;
	type(x + 1) = T_NUMBER | T_ATOM;
	type(x + 2) = T_NUMBER | T_ATOM;
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
	if (is_memblock(next)) {
		size_t i;
		size_t n = 1 + strlength(next) / sizeof(struct cell);
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
		int n = (int)ivalue(next) / 2 + (int)ivalue(next) % 2;
		for (i = 0; i < n; i++) {
			*++next = *++x;
			type(x) = T_FORWARDED;
			x->_object._forwarded = next;
		}
		return next++ - n;
	}
	return next++;
}

#ifndef USE_SCHEME_STACK
void forward_dump(pointer base, pointer curr)
{
	pointer p, q;

	for (p = base; p != curr; p = dump_prev(p)) {
		q = forward(p);
		forward(p + 1);
		forward(p + 2);
		dump_args(q) = forward(dump_args(q));
		dump_envir(q) = forward(dump_envir(q));
		dump_code(q) = forward(dump_code(q));
	}
	for (; p != NIL; p = dump_prev(p)) {
		forward(p);
		forward(p + 1);
		forward(p + 2);
	}
}
#endif

void gc(pointer *a, pointer *b)
{
	pointer scan;
	pointer p, q;
	char temp[32];
	int i;

	if (gc_verbose)
		printf("gc...");

	scan = next = to_space;

	/* forward system globals */
	oblist = forward(oblist);
	global_env = forward(global_env);
	inport = forward(inport);
	outport = forward(outport);
	winders = forward(winders);
	strbuff = forward(strbuff);

	/* forward special symbols */
	LAMBDA = forward(LAMBDA);
	QUOTE = forward(QUOTE);
	QQUOTE = forward(QQUOTE);
	UNQUOTE = forward(UNQUOTE);
	UNQUOTESP = forward(UNQUOTESP);
	ELLIPSIS = forward(ELLIPSIS);

	/* forward current registers */
	args = forward(args);
	envir = forward(envir);
	code = forward(code);
#ifndef USE_SCHEME_STACK
	forward_dump(dump_base, dump);
	dump_base = forward(dump_base);

	for (p = c_nest; p != NIL; p = cdr(p)) {
		q = cdr(cdar(c_nest));
		forward_dump(cdr(q), car(q));
	}
#endif
	dump = forward(dump);

	value = forward(value);
	mark_x = forward(mark_x);
	mark_y = forward(mark_y);
	c_nest = forward(c_nest);
	c_sink = forward(c_sink);
	for (i = 0; i < load_files; i++) {
		load_stack[i] = forward(load_stack[i]);
	}

	/* forward variables a, b */
	*a = forward(*a);
	*b = forward(*b);

	while (scan < next) {
		switch (type(scan) & 0x1fff) {
		case T_NUMBER:
			if (scan->_isfixnum) {
				bignum(scan) = forward(bignum(scan));
			}
			break;
		case T_PROC:
		case T_CHARACTER:
		case T_VECTOR:
		case T_FOREIGN:
			break;
		case T_STRING:
		case T_STRING | T_SYMBOL:
		case T_STRING | T_SYMBOL | T_SYNTAX:
			p = forward(car(scan) - 1);
			strvalue(scan) = strvalue(p);
			break;
		case T_MEMBLOCK:
			scan += 1 + strlength(scan) / sizeof(struct cell);
			break;
		case T_PORT:
			if (is_strport(scan) && port_file(scan) != NULL) {
				size_t curr_len = port_curr(scan) - strvalue(car(scan));
				car(scan) = forward(car(scan));
				p = forward(caar(scan) - 1);
				port_curr(scan) = strvalue(p) + curr_len;
			}
			break;
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
		printf(" done %zu cells are recovered.\n", fcells);
}

#else /* USE_COPYING_GC */

/*--
 *  We use algorithm E (Knuth, The Art of Computer Programming Vol.1,
 *  sec.3.5) for marking.
 */
void mark(pointer p)
{
	pointer t = 0, q;

E2:	setmark(p);
	if (is_number(p) && p->_isfixnum) {
		setmark(bignum(p));
	} else if (is_string(p)) {
		size_t n = 1 + strlength(p) / sizeof(struct cell);
		setmark((pointer)strvalue(p) + n);
	} else if (is_port(p)) {
		if (is_fileport(p)) {
			setmark(p + 1);
		} else {
			mark(car(p));
		}
	} else if (is_vector(p)) {
		int i;
		int n = 1 + (int)ivalue(p) / 2 + (int)ivalue(p) % 2;
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

#ifndef USE_SCHEME_STACK
void mark_dump(pointer base, pointer curr)
{
	pointer p;

	for (p = base; p != curr; p = dump_prev(p)) {
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
}
#endif

/* garbage collection. parameter a, b is marked. */
void gc(pointer *a, pointer *b)
{
	pointer p;
	int i;

	if (gc_verbose)
		printf("gc...");

	/* mark system globals */
	mark(oblist);
	mark(global_env);
	mark(inport);
	mark(outport);
	mark(winders);
	mark(strbuff);

	/* mark current registers */
	mark(args);
	mark(envir);
	mark(code);
#ifndef USE_SCHEME_STACK
	mark_dump(dump_base, dump);

	for (p = c_nest; p != NIL; p = cdr(p)) {
		pointer q = cdr(cdar(c_nest));
		mark_dump(cdr(q), car(q));
	}
#else
	mark(dump);
#endif

	mark(value);
	mark(mark_x);
	mark(mark_y);
	mark(c_nest);
	mark(c_sink);
	for (i = 0; i < load_files; i++) {
		mark(load_stack[i]);
	}

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
			if (is_memblock(p)) {
				p = (pointer)strvalue(p);
			}
		} else {
			if (is_memblock(p)) {
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
		printf(" done %zu cells are recovered.\n", fcells);
}
#endif /* USE_COPYING_GC */

/* ========== Routines for Ports ========== */

pointer port_from_filename(const char *filename, int prop)
{
	FILE *fp = NULL;

	if (prop == port_input) {
		fp = fopen(filename, "rb");
	} else if (prop == port_output) {
		fp = fopen(filename, "wb");
	} else if (prop == (port_input | port_output)) {
		fp = fopen(filename, "a+b");
	}
	if (fp == NULL) {
		return NIL;
	}
	return mk_port(fp, prop);
}

#define BLOCK_SIZE 256

pointer port_from_scratch(void)
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
	pointer x = get_string_cell(new_size, &p);

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

/* ========== Routines for Reading ========== */

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

#define TOK_PAREN_SQUARE 16
#define TOK_PAREN_CURLY  32

/* get new character from input file */
int inchar(void)
{
	int c;

	if (port_file(inport) == NULL || is_eofport(inport)) {
		return EOF;
	}
	if (is_fileport(inport)) {
		if (feof(port_file(inport))) {
			inport->_isfixnum |= port_eof;
			return EOF;
		}

		c = utf8_fgetc(port_file(inport));
		if (c == EOF) {
			inport->_isfixnum |= port_eof;
			if (port_file(inport) == stdin) {
				fprintf(stderr, "Good-bye\n");
				port_file(inport) = NULL;
			}
		}
	} else {
		if (port_curr(inport) == strvalue(car(inport)) + strlength(car(inport))) {
			inport->_isfixnum |= port_eof;
			return EOF;
		} else {
			port_curr(inport) += utf8_get_next(port_curr(inport), &c);
		}
	}
	return c;
}

/* back to standard input */
void flushinput(void)
{
	while (load_files > 1) {
		inport = load_stack[--load_files];
		if (is_fileport(inport) && port_file(inport) != stdin && port_file(inport) != NULL) {
			fclose(port_file(inport));
		}
	}

	inport = load_stack[0];
}

/* check c is delimiter */
int isdelim(char *s, int c)
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
			char utf8[4];
			size_t n = utf32_to_utf8(c, utf8);
			while (n > 0) {
				internal_ungetc(utf8[--n], port_file(inport));
			}
		} else if (port_curr(inport) != strvalue(car(inport))) {
			port_curr(inport) -= utf32_to_utf8(c, NULL);
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
			*port_curr(outport)++ = (unsigned char)c;
			if (port_curr(outport) == endp) {
				outport = realloc_port_string(outport);
			}
		}
	}
}

/* read chacters to delimiter */
char *readstr(char *delim)
{
	char *p = strvalue(strbuff);
	int c;
	do {
		size_t len;
		c = inchar();
		if (c == EOF) {
			*p = '\0';
			return strvalue(strbuff);
		} else if ((len = p - strvalue(strbuff)) + 4 > strlength(strbuff)) {
			pointer x = mk_memblock(strlength(strbuff) + 256, &NIL, &NIL);
			memcpy(strvalue(x), strvalue(strbuff), strlength(strbuff));
			strbuff = x;
			p = strvalue(strbuff) + len;
		}
		p += utf32_to_utf8(c, p);
	} while (isdelim(delim, c));
	if (p != strvalue(strbuff) + 2 || p[-2] != '\\') {
		backchar(*--p);
	}
	*p = '\0';
	return strvalue(strbuff);
}

/* read string expression "xxx...xxx" */
pointer readstrexp(void)
{
	char *p = strvalue(strbuff);
	int c, c1 = 0;
	enum { st_ok, st_bsl, st_x1, st_x2, st_oct1, st_oct2 } state = st_ok;

	for (;;) {
		size_t len;
		c = inchar();
		if (c == EOF) {
			return F;
		} else if ((len = p - strvalue(strbuff)) + 4 > strlength(strbuff)) {
			pointer x = mk_memblock(strlength(strbuff) + 256, &NIL, &NIL);
			memcpy(strvalue(x), strvalue(strbuff), strlength(strbuff));
			strbuff = x;
			p = strvalue(strbuff) + len;
		}
		if (state == st_ok) {
			switch (c) {
			case '\\':
				state = st_bsl;
				break;
			case '"':
				*p = 0;
				return mk_counted_string(strvalue(strbuff), p - strvalue(strbuff));
			default:
				p += utf32_to_utf8(c, p);
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
				p += utf32_to_utf8(c, p);
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
					*p++ = (char)c1;
					state = st_ok;
				}
			} else {
				return F;
			}
		} else {
			if (c < '0' || c > '7') {
				*p++ = (char)c1;
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
					*p++ = (char)c1;
					state = st_ok;
				}
			}
		}
	}
}

/* skip white characters */
int skipspace(void)
{
	int c;

	while (utf32_isspace(c = inchar()))
		;
	backchar(c);
	return c;
}

/* get token */
int token(void)
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
	case '[':
		return TOK_LPAREN | TOK_PAREN_SQUARE;
	case ']':
		return TOK_RPAREN | TOK_PAREN_SQUARE;
	case '{':
		return TOK_LPAREN | TOK_PAREN_CURLY;
	case '}':
		return TOK_RPAREN | TOK_PAREN_CURLY;
	case '.':
		if ((c = inchar()) == '.') {
			backchar(c);
			backchar(c);
			return TOK_ATOM;
		}
		backchar(c);
		return TOK_DOT;
	case '\'':
		return TOK_QUOTE;
	case ';':
		while ((c = inchar()) != '\n' && c != '\r' && c != EOF)
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
		} else if (c == '[') {
			return TOK_VEC | TOK_PAREN_SQUARE;
		} else if (c == '{') {
			return TOK_VEC | TOK_PAREN_CURLY;
		} else {
			backchar(c);
			return TOK_SHARP;
		}
	case EOF:
		return TOK_EOF;
	default:
		backchar(c);
		return TOK_ATOM;
	}
}

/* ========== Routines for Printing ========== */
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
		p = strvalue(strbuff);
		if (f <= 1 || f == 10) {
			if (l->_isfixnum) {
				if (bignum(l) == NIL) {
					sprintf(p, "%d", ivalue(l));
				} else {
					int32_t i, col = abs(ivalue(l));
					size_t len = (size_t)((col * 32 * log(2) + 3) / log(10)) + 1;
					pointer q = mk_memblock(col * sizeof(uint32_t), &l, &NIL);
					uint32_t *temp = (uint32_t *)strvalue(q);
					memcpy(temp, strvalue(bignum(l)), col * sizeof(uint32_t));
					if (len >= strlength(strbuff)) {
						strbuff = mk_memblock((len + 255) / 256 * 256, &l, &q);
						p = strvalue(strbuff);
					}
					p = &p[strlength(strbuff) - 1];
					*p = 0;
					while (col > 0) {
						uint64_t t = 0;
						for (i = col - 1; i >= 0; i--) {
							t = t << 32 | temp[i];
							temp[i] = (uint32_t)(t / 1000000000);
							t = t % 1000000000;
						}
						while (col > 0) {
							if (temp[col - 1] > 0) {
								break;
							}
							col--;
						}
						for (i = 0; i < 9 && (col > 0 || t > 0); i++, t /= 10) {
							*--p = (char)(t % 10) + '0';
						}
					}
					if (ivalue(l) < 0) *--p = '-';
				}
			} else {
				sprintf(p, "%.10g", rvalue(l));
				f = (int)strcspn(p, ".e");
				if (p[f] == 0) {
					p[f] = '.';
					p[f + 1] = '0';
					p[f + 2] = 0;
				}
			}
		} else if (f == 16) {
			if (bignum(l) == NIL) {
				if (ivalue(l) >= 0) {
					sprintf(p, "%x", ivalue(l));
				} else {
					sprintf(p, "-%x", -ivalue(l));
				}
			} else {
				int32_t i, j, col = abs(ivalue(l));
				size_t len = col * 8 + 1;
				if (len >= strlength(strbuff)) {
					strbuff = mk_memblock((len + 255) / 256 * 256, &l, &NIL);
					p = strvalue(strbuff);
				}
				p = &p[strlength(strbuff) - 1];
				*p = 0;
				for (i = 0; i < col; i++) {
					for (j = 0; j < 8; j++) {
						uint32_t n = ((uint32_t *)strvalue(bignum(l)))[i] >> (4 * j);
						if (i < col - 1 || n != 0) {
							char c = n & 0xf;
							*--p = (c < 10) ? c + '0' : c - 10 + 'a';
						}
					}
				}
				if (ivalue(l) < 0) *--p = '-';
			}
		} else if (f == 8) {
			if (bignum(l) == NIL) {
				if (ivalue(l) >= 0)
					sprintf(p, "%o", ivalue(l));
				else
					sprintf(p, "-%o", -ivalue(l));
			} else {
				int32_t i, j, k, col = abs(ivalue(l));
				size_t len = col * 11 + 1;
				if (len >= strlength(strbuff)) {
					strbuff = mk_memblock((len + 255) / 256 * 256, &l, &NIL);
					p = strvalue(strbuff);
				}
				p = &p[strlength(strbuff) - 1];
				*p = 0;
				for (i = 0; i < col; i++) {
					uint32_t m = ((uint32_t *)strvalue(bignum(l)))[i];
					if (i % 3 == 0) {
						k = 11;
					} else if (i % 3 == 1) {
						*p += (m & 0x1) << 2;
						k = 11;
					} else {
						*p += (m & 0x3) << 1;
						k = 10;
					}
					for (j = 0; j < k; j++) {
						uint32_t n = m >> (3 * j + i % 3);
						if (i < col - 1 || n != 0) {
							*--p = (n & 0x7) + '0';
						}
					}
				}
				if (ivalue(l) < 0) *--p = '-';
			}
		} else if (f == 2) {
			if (bignum(l) == NIL) {
				uint32_t b = (ivalue(l) < 0) ? -ivalue(l) : ivalue(l);
				p = &p[strlength(strbuff) - 1];
				*p = 0;
				do { *--p = (b & 1) ? '1' : '0'; b >>= 1; } while (b != 0);
				if (ivalue(l) < 0) *--p = '-';
			} else {
				int32_t i, j, col = abs(ivalue(l));
				size_t len = col * 32 + 1;
				if (len >= strlength(strbuff)) {
					strbuff = mk_memblock((len + 255) / 256 * 256, &l, &NIL);
					p = strvalue(strbuff);
				}
				p = &p[strlength(strbuff) - 1];
				*p = 0;
				for (i = 0; i < col; i++) {
					for (j = 0; j < 32; j++) {
						uint32_t n = ((uint32_t *)strvalue(bignum(l)))[i] >> j;
						if (i < col - 1 || n != 0) {
							*--p = (n & 0x1) + '0';
						}
					}
				}
				if (ivalue(l) < 0) *--p = '-';
			}
		} else {
			p = NULL;
		}
	} else if (is_symbol(l)) {
		if (syntaxnum(l) & T_DEFSYNTAX) {
			p = symname(cdr(l));
		} else {
			p = symname(l);
		}
	} else if (is_string(l)) {
		if (!f) {
			p = strvalue(l);
		} else {
			printslashstring((unsigned char *)strvalue(l));
			p = NULL;
		}
	} else if (is_character(l)) {
		int c = (int)ivalue(l);
		p = strvalue(strbuff);
		if (!f) {
			*(p + utf32_to_utf8(c, p)) = '\0';
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
					sprintf(p, "#\\x%x", c < 0 ? -c : c);
				} else {
					p[0] = '#';
					p[1] = '\\';
					*(p + 2 + utf32_to_utf8(c, p + 2)) = '\0';
				}
				break;
			}
		}
	} else if (is_proc(l)) {
		p = strvalue(strbuff);
		sprintf(p, "#<PROCEDURE %d>", procnum(l));
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
	} else if (is_foreign(l)) {
		p = strvalue(strbuff);
		sprintf(p, "#<FOREIGN PROCEDURE %d>", procnum(l));
	} else {
		p = "#<ERROR>";
	}
	return p;
}

/* print atoms */
size_t printatom(pointer l, int f)
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

/* ========== Routines for Numerical operations ========== */

/* greatest common divisor */
static int64_t gcd(int32_t x, int32_t y)
{
	uint32_t ux = (uint32_t)llabs(x), uy = (uint32_t)llabs(y), uz;
	while (ux != 0) {
		uz = ux;
		ux = uy % ux;
		uy = uz;
	}
	return uy;
}

/* least common multiple */
static int64_t lcm(int32_t x, int32_t y)
{
	if (x == 0 || y == 0) {
		return 0;
	}
	return llabs(x / gcd(x, y) * y);
}

/* the first bit1 position */
static int32_t find1_32(uint32_t val)
{
	static const int32_t clz_table_4bit[16] = { 4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 };
	int32_t n = 32;

	if ((val & 0xFFFF0000) == 0) { n -= 16; val <<= 16; }
	if ((val & 0xFF000000) == 0) { n -= 8; val <<= 8; }
	if ((val & 0xF0000000) == 0) { n -= 4; val <<= 4; }

	return n - clz_table_4bit[val >> (32 - 4)];
}

/* if x == y */
static int bn_eq(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (colx != coly) {
		return  0;
	}
	for (i = colx - 1; i >= 0; i--) {
		if (x[i] != y[i]) {
			return 0;
		}
	}
	return 1;
}

/* if x > y */
static int bn_gt(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (colx > coly) {
		return  1;
	} else if (colx < coly) {
		return 0;
	}
	for (i = colx - 1; i >= 0; i--) {
		if (x[i] > y[i]) {
			return 1;
		} else if (x[i] < y[i]) {
			return 0;
		}
	}
	return 0;
}

/* if x >= y */
static int bn_ge(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (colx > coly) {
		return  1;
	} else if (colx < coly) {
		return 0;
	}
	for (i = colx - 1; i >= 0; i--) {
		if (x[i] > y[i]) {
			return 1;
		} else if (x[i] < y[i]) {
			return 0;
		}
	}
	return 1;
}

/* z = x + y */
static int bn_add(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i, col = (colx < coly) ? colx : coly;
	uint64_t t = 0;
	for (i = 0; i < col; i++) {
		t = (uint64_t)x[i] + y[i] + (t >> 32);
		z[i] = (uint32_t)t;
	}
	if (colx > coly) {
		*colz = colx;
		for (; i < colx; i++) {
			t = x[i] + (t >> 32);
			z[i] = (uint32_t)t;
		}
	} else {
		*colz = coly;
		for (; i < coly; i++) {
			t = y[i] + (t >> 32);
			z[i] = (uint32_t)t;
		}
	}
	if (t >>= 32) {
		z[(*colz)++] = (uint32_t)t;
	}
	return 1;
}

/* z = x - y */
static int bn_sub(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	uint32_t carry = 0;
	for (i = 0; i < coly; i++) {
		uint64_t t = (uint64_t)y[i] + carry;
		if (x[i] >= t) {
			z[i] = (uint32_t)(x[i] - t);
			carry = 0;
		} else {
			z[i] = (uint32_t)(((uint64_t)1 << 32) + x[i] - t);
			carry = 1;
		}
	}
	*colz = colx;
	for (; i < colx; i++) {
		if (x[i] >= carry) {
			z[i] = (uint32_t)(x[i] - carry);
			carry = 0;
		} else {
			z[i] = (uint32_t)(((uint64_t)1 << 32) + x[i] - carry);
			carry = 1;
		}
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
	return 1;
}

/* z = x * y */
static int bn_mul(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i, j;
	memset(z, 0, sizeof(uint32_t) * (colx + coly));
	for (i = 0; i < colx; i++) {
		uint64_t t = 0;
		for (j = 0; j < coly; j++) {
			t = z[i + j] + (uint64_t)x[i] * y[j] + (t >> 32);
			z[i + j] = (uint32_t)t;
		}
		if (t >>= 32) {
			z[i + j] = (uint32_t)t;
		}
	}
	*colz = colx + coly;
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
	return 1;
}

/* z = x^2 */
static int bn_sqr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx)
{
	int32_t i, j = 0;
	uint64_t t;
	*colz = 2 * colx;
	memset(z, 0, sizeof(uint32_t) * *colz);
	for (i = 0; i < colx - 1; i++) {
		t = 0;
		for (j = i + 1; j < colx; j++) {
			t = z[i + j] + (uint64_t)x[i] * x[j] + (t >> 32);
			z[i + j] = (uint32_t)t;
		}
		if (t >>= 32) {
			z[i + j] = (uint32_t)t;
		}
	}
	for (i = i + j - 1; i >= 0; i--) {
		z[i + 1] |= z[i] >> 31;
		z[i] = z[i] << 1;
	}
	t = 0;
	for (i = 0; i < colx * 2; i++) {
		t = z[i] + (uint64_t)x[i >> 1] * x[i >> 1] + (t >> 32);
		z[i++] = (uint32_t)t;
		t = z[i] + (t >> 32);
		z[i] = (uint32_t)t;
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
	return 1;
}

/* z = x << n */
static int bn_sftl(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n)
{
	int32_t i, q = n / 32, r = n & 0x1F;

	if (r == 0) {
		for (i = colx - 1; i >= 0; --i) {
			z[i + q] = x[i];
		}
		memset(z, 0, sizeof(int32_t) * q);
		*colz = colx + q;
	} else {
		int32_t col = 0;
		if (x[colx - 1] >> (32 - r)) {
			col++;
			z[colx + q] = 0;
		}
		for (i = colx - 1; i >= 0; --i) {
			z[i + q + 1] |= x[i] >> (32 - r);
			z[i + q] = x[i] << r;
		}
		memset(z, 0, sizeof(int32_t) * q);
		*colz = colx + q + col;
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
	return 1;
}

/* z = x >> n */
static int bn_sftr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n)
{
	int32_t i, q = n / 32, r = n & 0x1F;

	if (r == 0) {
		if (colx - q <= 0) {
			*colz = 0;
		} else {
			for (i = q; i < colx; i++) {
				z[i - q] = x[i];
			}
			*colz = colx - q;
		}
	} else {
		if (colx - q <= 0) {
			*colz = 0;
		} else {
			int32_t col = 0;
			if ((x[colx - 1] >> r) == 0) {
				col--;
			}
			z[0] = x[q] >> r;
			for (i = q + 1; i < colx; i++) {
				z[i - q - 1] |= x[i] << (32 - r);
				z[i - q] = x[i] >> r;
			}
			*colz = colx - q + col;
		}
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
	return 1;
}

/* q = x / y + r */
static int bn_div(uint32_t q[], int32_t *colq, uint32_t r[], int32_t *colr, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (coly == 1) {
		uint64_t t = 0;
		for (i = colx - 1; i >= 0; i--) {
			t = t << 32 | x[i];
			q[i] = (uint32_t)(t / y[0]);
			t = t % y[0];
		}
		*colq = colx;
		while (*colq > 0) {
			if (q[*colq - 1] > 0) {
				break;
			}
			(*colq)--;
		}
		r[0] = (int32_t)t;
		*colr = 1;
	} else {
		uint32_t *t_a = r + *colr, *t_b = t_a + 1 + colx, *t_x = t_b + 1 + coly;
		int32_t cola, colb, d = 32 - find1_32(y[coly - 1]);
		if (d == 0) {
			memcpy(t_a, x, sizeof(uint32_t) * colx);
			memcpy(t_b, y, sizeof(uint32_t) * coly);
			cola = colx;
			colb = coly;
		} else {
			if (!bn_sftl(t_a, &cola, x, colx, d)) {
				return 0;
			}
			if (!bn_sftl(t_b, &colb, y, coly, d)) {
				return 0;
			}
		}
		i = cola - colb;
		if (i > 0) {
			*colq = i;
		} else if (i == 0) {
			if (bn_ge(t_a, cola, t_b, colb)) {
				q[0] = 1;
				*colq = 1;
				if (!bn_sub(t_a, &cola, t_a, cola, t_b, colb)) {
					return 0;
				}
			} else {
				*colq = 0;
			}
		} else {
			*colq = 0;
		}
		while (--i >= 0) {
			if (cola > 1 && bn_gt(t_a, cola, t_b, colb)) {
				uint64_t aa = (uint64_t)t_a[cola - 1] << 32 | t_a[cola - 2];
				uint64_t qq = aa / t_b[colb - 1];
				uint32_t q2[2];
				if (cola > 2 && colb > 1) {
					uint64_t rr = aa % t_b[colb - 1];
					while ((rr << 32 | t_a[cola - 3]) < qq * t_b[colb - 2]) {
						qq--;
						rr += t_b[colb - 2];
						if (rr > UINT32_MAX) break;
					}
				}
				do {
					q2[0] = (uint32_t)qq;
					q2[1] = (uint32_t)(qq >> 32);
					if (!bn_mul(t_x, &colx, t_b, colb, q2, q2[1] > 0 ? 2 : 1)) {
						return 0;
					}
					--qq;
					if (!bn_sftl(t_x, &colx, t_x, colx, i * 32)) {
						return 0;
					}
				} while (bn_gt(t_x, colx, t_a, cola));
				q[i] = (uint32_t)(qq + 1);
				if (qq >= UINT32_MAX) {
					q2[0] = 1;
					bn_add(&q[i + 1], colq, &q[i + 1], *colq - i - 1, q2, 1);
					*colq += i + 1;
				}
				if (!bn_sub(t_a, &cola, t_a, cola, t_x, colx)) {
					return 0;
				}
			} else if (bn_eq(t_a, cola, t_b, colb)) {
				q[i] = 1;
				while (i > 0) q[--i] = 0;
				while (cola > 0) t_a[--cola] = 0;
			} else {
				q[i] = 0;
			}
		}
		if (d > 0) {
			if (!bn_sftr(r, colr, t_a, cola, d)) {
				return 0;
			}
		} else {
			memcpy(r, t_a, cola * 32);
			*colr = cola;
			while (*colr > 0) {
				if (r[*colr - 1] > 0) {
					break;
				}
				(*colr)--;
			}
		}
	}
	return 1;
}

static double get_rvalue(pointer x)
{
	if (x->_isfixnum) {
		if (bignum(x) == NIL) {
			return (double)ivalue(x);
		} else {
			int32_t colx = abs(ivalue(x)), bitx = find1_32(((uint32_t *)strvalue(bignum(x)))[colx - 1]);
			uint64_t d = (uint64_t)((uint32_t *)strvalue(bignum(x)))[colx - 1] << (64 - bitx);
			d += (uint64_t)((uint32_t *)strvalue(bignum(x)))[colx - 2] << (32 - bitx);
			if (bitx < 32 && colx > 2) {
				d += (uint64_t)((uint32_t *)strvalue(bignum(x)))[colx - 3] >> bitx;
			}
			return (ivalue(x) < 0 ? -1 : 1) * ldexp((double)d, bitx + (colx - 3) * 32);
		}
	} else {
		return rvalue(x);
	}
}

static void bignum_from_int64(pointer x, int64_t d)
{
	type(x) = T_NUMBER | T_ATOM;
	set_num_integer(x);
	if (INT32_MIN <= d && d <= INT32_MAX) {
		ivalue(x) = (int32_t)d;
		bignum(x) = NIL;
	} else if (-((int64_t)UINT32_MAX + 1) <= d && d <= UINT32_MAX) {
		ivalue(x) = d < 0 ? -1 : 1;
		bignum(x) = mk_memblock(1 * sizeof(uint32_t), &x, &NIL);
		if (d < 0) d = (uint32_t)~d + 1;
		((uint32_t *)strvalue(bignum(x)))[0] = (uint32_t)d;
	} else {
		ivalue(x) = d < 0 ? -2 : 2;
		bignum(x) = mk_memblock(2 * sizeof(uint32_t), &x, &NIL);
		if (d < 0) d = (uint64_t)~d + 1;
		((uint32_t *)strvalue(bignum(x)))[1] = (uint32_t)(d >> 32);
		((uint32_t *)strvalue(bignum(x)))[0] = (uint32_t)d;
	}
}

static void bignum_adjust(pointer z, pointer m, int32_t col, int32_t sign)
{
	type(z) = T_NUMBER | T_ATOM;
	set_num_integer(z);
	if (col == 0) {
		ivalue(z) = 0;
		bignum(z) = NIL;
	} else if (col == 1) {
		int64_t d = (int64_t)sign * ((uint32_t *)strvalue(m))[0];
		if (INT32_MIN <= d && d <= INT32_MAX) {
			ivalue(z) = (int32_t)d;
			bignum(z) = NIL;
		} else {
			ivalue(z) = sign;
			bignum(z) = m;
		}
	} else {
		ivalue(z) = sign * col;
		bignum(z) = m;
	}
}

/* if |x| == |y| */
static int bignum_eq(pointer x, pointer y)
{
	return bn_eq((uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(bignum(y)), abs(ivalue(y)));
}

/* if |x| > |y| */
static int bignum_gt(pointer x, pointer y)
{
	return bn_gt((uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(bignum(y)), abs(ivalue(y)));
}

/* if |x| >= |y| */
static int bignum_ge(pointer x, pointer y)
{
	return bn_ge((uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(bignum(y)), abs(ivalue(y)));
}

/* z = |x| */
static int bignum_abs(pointer z, pointer x)
{
	if (bignum(x) == NIL) {
		bignum_from_int64(z, ivalue(x) < 0 ? (uint32_t)~ivalue(x) + 1 : ivalue(x));
	} else {
		pointer m = mk_memblock(abs(ivalue(x)) * sizeof(uint32_t), &z, &x);
		memcpy(strvalue(m), strvalue(bignum(x)), abs(ivalue(x)) * sizeof(uint32_t));
		bignum_adjust(z, m, abs(ivalue(x)), 1);
	}
	return 1;
}

/* z = sign * (|x|+|y|) */
static int bignum_add(pointer z, pointer x, pointer y, int32_t sign)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), col = (colx > coly ? colx : coly) + 1;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &y);
	if (bn_add((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)strvalue(bignum(y)), coly) == 0) {
		return 0;
	}
	bignum_adjust(z, m, col, sign);
	return 1;
}

/* z = sign * (|x|+|val|) */
static int bignum_add_imm(pointer z, pointer x, int32_t val, int32_t sign)
{
	uint32_t y = val < 0 ? (uint32_t)~val + 1 : val;
	int32_t colx = abs(ivalue(x)), col = (colx > 1 ? colx : 1) + 1;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &z);
	if (bn_add((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1) == 0) {
		return 0;
	}
	bignum_adjust(z, m, col, sign);
	return 1;
}

/* z = sign * (|x|-|y|) */
static int bignum_sub(pointer z, pointer x, pointer y, int32_t sign)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), col = colx;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &y);
	if (bn_sub((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)strvalue(bignum(y)), coly) == 0) {
		return 0;
	}
	bignum_adjust(z, m, col, sign);
	return 1;
}

/* z = sign * (|x|-|val|) */
static int bignum_sub_imm(pointer z, pointer x, int32_t val, int32_t sign)
{
	uint32_t y = val < 0 ? (uint32_t)~val + 1 : val;
	int32_t colx = abs(ivalue(x)), col = colx;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &z);
	if (bn_sub((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1) == 0) {
		return 0;
	}
	bignum_adjust(z, m, col, sign);
	return 1;
}

/* z = x * y */
static int bignum_mul(pointer z, pointer x, pointer y)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), col = colx + coly;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &y);
	if (bn_mul((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, ((uint32_t *)strvalue(bignum(y))), coly) == 0) {
		return 0;
	}
	bignum_adjust(z, m, col, (ivalue(x) < 0 ? -1 : 1) * (ivalue(y) < 0 ? -1 : 1));
	return 1;
}

/* z = x * val */
static int bignum_mul_imm(pointer z, pointer x, int32_t val)
{
	uint32_t y = val < 0 ? (uint32_t)~val + 1 : val;
	int32_t colx = abs(ivalue(x)), col = colx + 1;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &z);
	if (bn_mul((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1) == 0) {
		return 0;
	}
	bignum_adjust(z, m, col, (ivalue(x) < 0 ? -1 : 1) * (val < 0 ? -1 : 1));
	return 1;
}

/* q = x / y + r*/
static int32_t bignum_div(pointer q, pointer r, pointer x, pointer y)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), colq = colx, colr = coly, signx = ivalue(x) < 0 ? -1 : 1, signy = ivalue(y) < 0 ? -1 : 1;
	pointer a = cons(x, y), m = mk_memblock(colq * sizeof(uint32_t), &a, &NIL);
	pointer b = mk_memblock(2 * (colq + colr + 1) * sizeof(uint32_t), &m, &a);
	uint32_t *t_q = (uint32_t *)strvalue(m), *t_r = (uint32_t *)strvalue(b);
	if (bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(car(a))), colx, (uint32_t *)strvalue(bignum(cdr(a))), coly) == 0) {
		return 0;
	}
	bignum_adjust(q, m, colq, signx * signy);

	m = mk_memblock(colr * sizeof(uint32_t), &q, &b);
	memcpy((uint32_t *)strvalue(m), t_r, colr * sizeof(uint32_t));
	bignum_adjust(r, m, colr, signx);
	return 1;
}

/* q = x / val + r */
static int bignum_div_imm(pointer q, pointer r, pointer x, int32_t val)
{
	int32_t colx = abs(ivalue(x)), colq = colx, colr, sign = (ivalue(x) < 0 ? -1 : 1) * (val < 0 ? -1 : 1);
	pointer m = mk_memblock(colq * sizeof(uint32_t), &x, &NIL);
	uint32_t *t_q = (uint32_t *)strvalue(m), tr, y = val < 0 ? (uint32_t)~val + 1 : val;
	if (bn_div(t_q, &colq, &tr, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1) == 0) {
		return 0;
	}
	bignum_adjust(q, m, colq, sign);

	type(r) = T_NUMBER | T_ATOM;
	set_num_integer(r);
	ivalue(r) = (int32_t)tr * (ivalue(x) < 0 ? -1 : 1);
	bignum(r) = NIL;
	return 1;
}

/* r = gcd(x, y) */
static int bignum_gcd(pointer r, pointer x, pointer y)
{
	int32_t cols = abs(ivalue(x)), colt = abs(ivalue(y));
	pointer s, t, u;
	t = mk_memblock(colt * sizeof(uint32_t), &x, &y);
	memcpy(strvalue(t), strvalue(bignum(y)), colt * sizeof(uint32_t));
	s = mk_memblock(cols * sizeof(uint32_t), &x, &t);
	memcpy(strvalue(s), strvalue(bignum(x)), cols * sizeof(uint32_t));
	u = mk_memblock(2 * (colt + cols + 1) * sizeof(uint32_t), &s, &t);
	while (colt > 1 || (colt == 1 && ((uint32_t *)strvalue(t))[0] > 0)) {
		int32_t colq = cols, colr = colt;
		uint32_t *t_q = (uint32_t *)strvalue(s), *t_r = (uint32_t *)strvalue(u);
		if (bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(s), cols, (uint32_t *)strvalue(t), colt) == 0) {
			return 0;
		}
		memcpy(strvalue(s), strvalue(t), colt * sizeof(uint32_t));
		cols = colt;
		if (bn_sub((uint32_t *)strvalue(t), &colt, (uint32_t *)strvalue(t), colt, t_r, colr) == 0) {
			return 0;
		}
		if (bn_gt((uint32_t *)strvalue(t), colt, t_r, colr)) {
			memcpy(strvalue(t), t_r, colr * sizeof(uint32_t));
			colt = colr;
		}
	}
	bignum_adjust(r, s, cols, 1);
	return 1;
}

/* r = gcd(x, val) */
static int bignum_gcd_imm(pointer r, pointer x, int32_t val)
{
	int32_t colx = abs(ivalue(x)), colq = colx, colr;
	pointer m = mk_memblock(colq * sizeof(uint32_t), &x, &NIL), n = mk_memblock(sizeof(uint32_t), &x, &m);
	uint32_t *t_q = (uint32_t *)strvalue(m), *t_r = (uint32_t *)strvalue(n), y = val < 0 ? (uint32_t)~val + 1 : val;
	if (bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1) == 0) {
		return 0;
	}
	if (t_r[0] > 0) {
		t_r[0] = (uint32_t)gcd(t_r[0], y);
	} else {
		t_r[0] = y;
	}
	bignum_adjust(r, n, 1, 1);
	return 1;
}

/* r = lcm(x, y) */
static int bignum_lcm(pointer r, pointer x, pointer y)
{
	int32_t colq, colr, cols = abs(ivalue(x)), colt = abs(ivalue(y));
	pointer s, t, u;
	uint32_t *t_q, *t_r;
	t = mk_memblock((colt + cols) * sizeof(uint32_t), &x, &y);
	memcpy(strvalue(t), strvalue(bignum(y)), colt * sizeof(uint32_t));
	s = mk_memblock(cols * sizeof(uint32_t), &x, &t);
	memcpy(strvalue(s), strvalue(bignum(x)), cols * sizeof(uint32_t));
	u = mk_memblock(2 * (colt + cols + 1) * sizeof(uint32_t), &s, &t);
	t_q = (uint32_t *)strvalue(s);
	t_r = (uint32_t *)strvalue(u);
	while (colt > 1 || (colt == 1 && ((uint32_t *)strvalue(t))[0] > 0)) {
		colq = cols;
		colr = colt;
		if (bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(s), cols, (uint32_t *)strvalue(t), colt) == 0) {
			return 0;
		}
		memcpy(strvalue(s), strvalue(t), colt * sizeof(uint32_t));
		cols = colt;
		if (bn_sub((uint32_t *)strvalue(t), &colt, (uint32_t *)strvalue(t), colt, t_r, colr) == 0) {
			return 0;
		}
		if (bn_gt((uint32_t *)strvalue(t), colt, t_r, colr)) {
			memcpy(strvalue(t), t_r, colr * sizeof(uint32_t));
			colt = colr;
		}
	}
	if (bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(s), cols) == 0) {
		return 0;
	}
	if (bn_mul((uint32_t *)strvalue(t), &colt, t_q, colq, (uint32_t *)strvalue(bignum(y)), abs(ivalue(y))) == 0) {
		return 0;
	}
	bignum_adjust(r, t, colt, 1);
	return 1;
}

/* r = lcm(x, val) */
static int bignum_lcm_imm(pointer r, pointer x, int32_t val)
{
	int32_t colx = abs(ivalue(x)), colq = colx, colr;
	pointer m = mk_memblock(colq * sizeof(uint32_t), &x, &NIL), n = mk_memblock(sizeof(uint32_t), &x, &m);
	uint32_t *t_q = (uint32_t *)strvalue(m), *t_r = (uint32_t *)strvalue(n), y = val < 0 ? (uint32_t)~val + 1 : val, z;
	if (bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1) == 0) {
		return 0;
	}
	if (t_r[0] > 0) {
		z = (uint32_t)gcd(t_r[0], y);
	} else {
		z = y;
	}
	if (bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&z, 1) == 0) {
		return 0;
	}
	n = mk_memblock((colq + 1) * sizeof(uint32_t), &x, &m);
	if (bn_mul((uint32_t *)strvalue(n), &colr, (uint32_t *)strvalue(m), colq, (uint32_t *)&y, 1) == 0) {
		return 0;
	}
	bignum_adjust(r, n, colr, 1);
	return 1;
}

/* z = pow(x, val) */
static int bignum_pow(pointer z, pointer x, int32_t val)
{
	int32_t colx = bignum(x) == NIL ? 1 : abs(ivalue(x));
	uint32_t v_x = bignum(x) == NIL ? (ivalue(x) < 0 ? (uint32_t)~ivalue(x) + 1 : ivalue(x)) : ((uint32_t *)strvalue(bignum(x)))[colx - 1];
	int32_t colz = ((find1_32(v_x) + 32 * (colx - 1)) * val + 31) / 32;
	int32_t sign = ivalue(x) < 0 && (val & 0x1) ? -1 : 1;
	pointer m_x = mk_memblock(2 * colz * sizeof(uint32_t), &z, &x), m_z;
	uint32_t *t_x = (uint32_t *)strvalue(m_x), *tmp = t_x + colz, *t_z;
	if (bignum(x) == NIL) {
		t_x[0] = v_x;
	} else {
		memcpy(t_x, strvalue(bignum(x)), colx * sizeof(uint32_t));
	}
	m_z = mk_memblock(colz * sizeof(uint32_t), &z, &m_x);
	t_z = (uint32_t *)strvalue(m_z);
	t_z[0] = 1;
	colz = 1;
	while (val > 0) {
		if (val & 0x1) {
			val--;
			if (bn_mul(tmp, &colz, t_z, colz, t_x, colx) == 0) {
				return 0;
			}
			memcpy(t_z, tmp, colz * sizeof(uint32_t));
		} else {
			val >>= 1;
			if (bn_sqr(tmp, &colx, t_x, colx) == 0) {
				return 0;
			}
			memcpy(t_x, tmp, colx * sizeof(uint32_t));
		}
	}
	bignum_adjust(z, m_z, colz, sign);
	return 1;
}

/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
pointer mk_closure(pointer c, pointer e)
{
	pointer x = get_cell(&c, &e);

	type(x) = T_CLOSURE;
	exttype(x) = 0;
	car(x) = c;
	cdr(x) = e;
	return x;
}

/* make continuation. */
pointer mk_continuation(pointer d)
{
	pointer x = get_cell(&NIL, &d);

	type(x) = T_CONTINUATION;
	exttype(x) = 0;
	car(x) = NIL;
	cont_dump(x) = d;
	return x;
}

/* reverse list -- make new cells */
pointer reverse(pointer a) /* a must be checked by gc */
{
	pointer p = NIL;

	for (mark_x = a; is_pair(mark_x); mark_x = cdr(mark_x)) {
		p = cons(car(mark_x), p);
	}
	return p;
}

/* reverse list --- no make new cells */
pointer non_alloc_rev(pointer term, pointer list)
{
	pointer p = list, result = term, q;

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
	pointer q;

	if (a != NIL) {
		mark_y = b;
		a = reverse(a);
		b = mark_y;
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
int eqv(pointer a, pointer b)
{
	if (is_string(a)) {
		if (is_string(b))
			return (strvalue(a) == strvalue(b));
		else
			return 0;
	} else if (is_number(a)) {
		if (is_number(b)) {
			if (a->_isfixnum && b->_isfixnum) {
				if (bignum(a) == NIL && bignum(b) == NIL) {
					return (ivalue(a) == ivalue(b));
				} else if (bignum(a) != NIL && bignum(b) != NIL && (ivalue(a) < 0) == (ivalue(b) < 0)) {
					return bignum_eq(a, b);
				} else {
					return 0;
				}
			} else if (!a->_isfixnum && !b->_isfixnum) {
				return (rvalue(a) == rvalue(b));
			} else {
				return 0;
			}
		} else {
			return 0;
		}
	} else if (is_character(a)) {
		if (is_character(b))
			return (ivalue(a) == ivalue(b));
		else
			return 0;
	} else
		return (a == b);
}

/* equivalence of pairs, vectors and strings recursively */
int equal(pointer a, pointer b)
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

int is_ellipsis(pointer p)
{
	pointer x, y;
	for (x = envir; x != NIL; x = cdr(x)) {
		for (y = car(x); y != NIL; y = cdr(y)) {
			if (cdar(y) == ELLIPSIS) {
				return caar(y) == p;
			}
		}
	}
	return p == ELLIPSIS;
}

int matchpattern(pointer p, pointer f, pointer keyword, int *s)
{
	pointer x;
	if (is_symbol(p)) {
		if (syntaxnum(p) & T_DEFSYNTAX) p = cdr(p);
		for (x = keyword; x != NIL; x = cdr(x)) {
			if (p == car(x)) {
				return p == f;
			}
		}
		(*s)++;
		return 1;
	} else if (is_pair(p)) {
		long len_f;
		if (is_pair(cdr(p)) && is_ellipsis(cadr(p)) && (len_f = list_length(f)) >= 0) {
			len_f -= list_length(cddr(p));
			for (x = f; len_f-- > 0; x = cdr(x)) {
				if (!matchpattern(car(p), car(x), keyword, s)) {
					return 0;
				}
			}
			if (is_pair(x) && !matchpattern(cddr(p), x, keyword, s)) {
				return 0;
			}
			(*s)++;
			return 1;
		} else if (is_pair(f)) {
			return matchpattern(car(p), car(f), keyword, s) && matchpattern(cdr(p), cdr(f), keyword, s);
		} else {
			return 0;
		}
	} else if (is_vector(p)) {
		if (is_vector(f)) {
			long i, j;
			for (i = 0, j = 0; i < ivalue(p) && j < ivalue(f); i++, j++) {
				if (i + 1 < ivalue(p) && is_ellipsis(vector_elem(p, i + 1))) {
					for (; j < ivalue(f) && j - i < 2 + ivalue(f) - ivalue(p); j++) {
						if (!matchpattern(vector_elem(p, i), vector_elem(f, j), keyword, s)) {
							return 0;
						}
					}
					(*s)++;
					if (j < ivalue(f)) { i++; j--; continue; }
					i += 2;
					break;
				}
				if (!matchpattern(vector_elem(p, i), vector_elem(f, j), keyword, s)) {
					return 0;
				}
			}
			if (i != ivalue(p) || j != ivalue(f)) {
				return 0;
			}
			return 1;
		} else {
			return 0;
		}
	} else {
		return equal(p, f);
	}
}

/* note: value = (vector of bindings, list of keywords) */
void bindpattern(pointer p, pointer f, int d, int n, int *s)
{
	pointer x;
	if (is_symbol(p)) {
		if (syntaxnum(p) & T_DEFSYNTAX) p = cdr(p);
		for (x = cdr(value); x != NIL; x = cdr(x)) {
			if (p == car(x)) {
				return;
			}
		}
		set_vector_elem(car(value), (*s)++, p);
		x = vector_elem(car(value), (*s)++);
		ivalue(car(x)) = d;
		ivalue(cdr(x)) = n;
		set_vector_elem(car(value), (*s)++, f);
	} else if (is_pair(p)) {
		long len_f;
		if (is_pair(cdr(p)) && is_ellipsis(cadr(p)) && (len_f = list_length(f)) >= 0) {
			long i = 0;
			set_vector_elem(car(value), (*s)++, car(p));
			x = vector_elem(car(value), (*s)++);
			ivalue(car(x)) = d;
			ivalue(cdr(x)) = n;
			set_vector_elem(car(value), (*s)++, NULL);
			len_f -= list_length(cddr(p));
			for (x = f; len_f-- > 0; x = cdr(x)) {
				bindpattern(car(p), car(x), d + 1, i++, s);
			}
			if (is_pair(x)) {
				bindpattern(cddr(p), x, d, n, s);
			}
		} else if (is_pair(f)) {
			bindpattern(car(p), car(f), d, n, s);
			bindpattern(cdr(p), cdr(f), d, n, s);
		}
	} else if (is_vector(p)) {
		if (is_vector(f)) {
			long i, j;
			for (i = 0, j = 0; i < ivalue(p) && j < ivalue(f); i++, j++) {
				if (i + 1 < ivalue(p) && is_ellipsis(vector_elem(p, i + 1))) {
					set_vector_elem(car(value), (*s)++, vector_elem(p, i));
					x = vector_elem(car(value), (*s)++);
					ivalue(car(x)) = d;
					ivalue(cdr(x)) = n;
					set_vector_elem(car(value), (*s)++, NULL);
					for (; j < ivalue(f) && j - i < 2 + ivalue(f) - ivalue(p); j++) {
						bindpattern(vector_elem(p, i), vector_elem(f, j), d + 1, j - i, s);
					}
					if (j < ivalue(f)) { i++; j--; continue; }
					i += 2;
					break;
				}
				bindpattern(vector_elem(p, i), vector_elem(f, j), d, n, s);
			}
		}
	}
}

pointer expandsymbol(pointer p)
{
	pointer x, y;
	if (is_symbol(p)) {
		if (syntaxnum(p) & T_DEFSYNTAX) {
			car(p) = cons(cadr(args), car(p));
			syntaxnum(car(p)) |= T_DEFSYNTAX;
		} else {
			p = cons(cdr(args), p);
			type(p) = type(cdr(p));
			syntaxnum(p) |= T_DEFSYNTAX;
		}
		return p;
	} else if (is_pair(p)) {
		mark_x = cons(p, mark_x);
		x = expandsymbol(caar(mark_x));
		if (is_symbol(x) && is_syntax(cdr(x)) && !strcmp(symname(cdr(x)), "quote")) {
			y = cdar(mark_x);
			mark_x = cdr(mark_x);
			return cons(x, y);
		}
		mark_y = cons(x, mark_y);
		y = expandsymbol(cdar(mark_x));
		mark_x = cdr(mark_x);
		x = car(mark_y);
		mark_y = cdr(mark_y);
		return cons(x, y);
	} else if (is_vector(p)) {
		int i, len = (int)ivalue(p);
		mark_x = cons(p, mark_x);
		y = NIL;
		for (i = 0; i < len; i++) {
			mark_y = cons(y, mark_y);
			x = expandsymbol(vector_elem(car(mark_x), i));
			y = car(mark_y);
			mark_y = cdr(mark_y);
			y = cons(x, y);
		}
		mark_x = cdr(mark_x);
		mark_y = cons(y, mark_y);
		i = list_length(y);
		x = mk_vector(i);
		y = car(mark_y);
		mark_y = cdr(mark_y);
		for (; y != NIL; y = cdr(y)) {
			set_vector_elem(x, --i, car(y));
		}
		return x;
	} else {
		return p;
	}
}

pointer expandpattern(pointer p, int d, int n, int *e)
{
	pointer x, y = NULL;
	int i, j;
	*((int *)strvalue(car(code)) + d) = n;
	if (is_symbol(p)) {
		int find = 0;
		if (syntaxnum(p) & T_DEFSYNTAX) p = cdr(p);
		for (x = cdr(value); x != NIL; x = cdr(x)) {
			if (p == car(x)) {
				return p;
			}
		}
		for (i = 0; i < ivalue(car(value)); i += 3) {
			int e_d, e_n;
			x = vector_elem(car(value), i);
			if (x == p) {
				find = 1;
			}
			e_d = (int)ivalue(car(vector_elem(car(value), i + 1)));
			e_n = (int)ivalue(cdr(vector_elem(car(value), i + 1)));
			if (e_d < d) {
				if (*((int *)strvalue(car(code)) + e_d) == e_n) {
					*((int *)strvalue(cdr(code)) + e_d) = 1;
					if (p == x) {
						for (j = 0; j < e_d; j++) {
							if (*((int *)strvalue(cdr(code)) + j) == 0) break;
						}
						if (j < e_d) continue;
						if (*e < e_d) {
							*e = e_d;
						}
						y = vector_elem(car(value), i + 2);
						if (y == NULL) continue;
						y = expandsymbol(y);
					}
				} else {
					*((long *)strvalue(cdr(code)) + e_d) = 0;
				}
			} else if (p == x && e_d == d && e_n == n) {
				for (j = 0; j < d; j++) {
					if (*((int *)strvalue(cdr(code)) + j) == 0) break;
				}
				if (j < d) continue;
				if (*e < d) {
					*e = d;
				}
				y = vector_elem(car(value), i + 2);
				if (y == NULL) return NIL;
				return expandsymbol(y);
			}
		}
		if (d > 0 && find) {
			return y;
		}
		return p;
	} else if (is_pair(p)) {
		mark_x = cons(p, mark_x);
		if (is_pair(cdar(mark_x)) && is_ellipsis(car(cdar(mark_x)))) {
			if (expandpattern(caar(mark_x), d, n, e) == NULL) {
				mark_x = cdr(mark_x);
				return NULL;
			}
			y = NIL;
			for (i = 0; ; i++) {
				mark_y = cons(y, mark_y);
				*e = 0;
				x = expandpattern(caar(mark_x), d + 1, i, e);
				y = car(mark_y);
				mark_y = cdr(mark_y);
				if (x == NULL || *e < d + 1) break;
				y = cons(x, y);
			}
			if (is_pair(cdar(mark_x))) {
				mark_y = cons(y, mark_y);
				x = expandpattern(cdr(cdar(mark_x)), d, n, e);
				y = car(mark_y);
				mark_y = cdr(mark_y);
			}
			mark_x = cdr(mark_x);
			if (x == NULL) {
				x = NIL;
			}
			return non_alloc_rev(x, y);
		}
		x = expandpattern(caar(mark_x), d, n, e);
		mark_y = cons(x, mark_y);
		y = expandpattern(cdar(mark_x), d, n, e);
		mark_x = cdr(mark_x);
		x = car(mark_y);
		mark_y = cdr(mark_y);
		if (x == NULL || y == NULL) {
			return NULL;
		}
		return cons(x, y);
	} else if (is_vector(p)) {
		int len = (int)ivalue(p);
		mark_x = cons(p, mark_x);
		y = NIL;
		for (i = 0; i < len; i++) {
			if (i + 1 < len && is_ellipsis(vector_elem(car(mark_x), i + 1))) {
				if (expandpattern(vector_elem(car(mark_x), i), d, n, e) == NULL) {
					mark_x = cdr(mark_x);
					return NULL;
				}
				for (j = 0; ; j++) {
					p = car(mark_x);
					mark_y = cons(y, mark_y);
					*e = 0;
					x = expandpattern(vector_elem(car(mark_x), i), d + 1, j, e);
					y = car(mark_y);
					mark_y = cdr(mark_y);
					if (x == NULL || *e < d + 1) break;
					y = cons(x, y);
				}
				i++;
				continue;
			}
			mark_y = cons(y, mark_y);
			x = expandpattern(vector_elem(car(mark_x), i), d, n, e);
			y = car(mark_y);
			mark_y = cdr(mark_y);
			if (x == NULL) {
				mark_x = cdr(mark_x);
				return NULL;
			}
			y = cons(x, y);
		}
		mark_x = cdr(mark_x);
		i = list_length(y);
		mark_y = cons(y, mark_y);
		x = mk_vector(i);
		y = car(mark_y);
		mark_y = cdr(mark_y);
		for (; y != NIL; y = cdr(y)) {
			set_vector_elem(x, --i, car(y));
		}
		return x;
	} else {
		return p;
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
		args = cons(args, x);
		x = mk_symbol("cons");
		return cons(x, args);
	}
}

/* make append list for quasiquote */
pointer mappend(pointer f, pointer l, pointer r)
{
	pointer x;

	if (car(f) == NIL ||
		(is_pair(r) && car(l) == QUOTE && cadr(r) == NIL)) {
		return l;
	} else {
		args = l;
		x = cons(r, NIL);
		args = cons(args, x);
		x = mk_symbol("append");
		return cons(x, args);
	}
}

/* true or false value macro */
#define istrue(p)       ((p) != F)
#define isfalse(p)      ((p) == F)

/* Error macro */
#define	BEGIN	do {
#define	END	} while (0)

#define Error_0(s) BEGIN                       \
	args = cons(mk_string((s)), NIL);          \
	operator = OP_ERR0;                        \
	goto LOOP; END

#define Error_1(s, a) BEGIN                    \
	args = cons((a), NIL);                     \
	code = mk_string(s);                       \
	args = cons(code, args);                   \
	operator = OP_ERR0;                        \
	goto LOOP; END

/* control macros for Eval_Cycle */
#define s_goto(a) BEGIN                        \
	operator = (int)(a);                       \
	goto a; END

#ifndef USE_SCHEME_STACK

#define s_save(a, b, c) BEGIN                  \
	if (dump_prev(dump) == NIL) {              \
		pointer d = mk_dumpstack(dump);        \
		dump_prev(dump) = d;                   \
	}                                          \
	dump_op(dump) = (pointer)(intptr_t)(a);    \
	dump_args(dump) = (b);                     \
	dump_envir(dump) = envir;                  \
	dump_code(dump) = (c);                     \
	dump = dump_prev(dump); END

#define s_return(a) BEGIN                      \
	value = (a);                               \
	if (dump == dump_base) return 0;           \
	dump = dump_next(dump);                    \
	operator = (int)(intptr_t)dump_op(dump);   \
	args = dump_args(dump);                    \
	envir = dump_envir(dump);                  \
	code = dump_code(dump);                    \
	goto LOOP; END

#define s_next_op() ((int)(intptr_t)dump_op(dump_next(dump)))

pointer s_clone(pointer d) {
	pointer p;

	if (d == NIL) return dump_base;

	p = s_clone(cddddr(d));
	dump_op(p) = (pointer)(intptr_t)ivalue(car(d));
	dump_args(p) = cadr(d);
	dump_envir(p) = caddr(d);
	dump_code(p) = cadddr(d);
	return dump_prev(p);
}

pointer s_clone_save(void) {
	pointer p = NIL;

	for (mark_x = dump_base; mark_x != dump; mark_x = dump_prev(mark_x)) {
		p = cons(dump_code(mark_x), p);
		p = cons(dump_envir(mark_x), p);
		args = cons(dump_args(mark_x), p);
		p = mk_integer((int32_t)(intptr_t)dump_op(mark_x));
		p = cons(p, args);
	}
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
	if (dump == NIL) return 0;                 \
	operator = (intptr_t)ivalue(car(dump));    \
	args = cadr(dump);                         \
	envir = caddr(dump);                       \
	code = cadddr(dump);                       \
	dump = cddddr(dump);                       \
	goto LOOP; END

#define s_next_op() ((int)ivalue(car(dump)))

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
	OP_LET0AST,
	OP_LET1AST,
	OP_LET2AST,
	OP_LET0REC,
	OP_LET1REC,
	OP_LETRECAST0,
	OP_LETRECAST1,
	OP_DO0,
	OP_DO1,
	OP_DO2,
	OP_DO3,
	OP_DO4,
	OP_DO5,
	OP_COND0,
	OP_COND1,
	OP_COND2,
	OP_ELSE,
	OP_FEEDTO,
	OP_DELAY,
	OP_LAZY,
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
	OP_DEFSYNTAX0,
	OP_DEFSYNTAX1,
	OP_LETSYNTAX0,
	OP_LETSYNTAX1,
	OP_LETRECSYNTAX0,
	OP_LETRECSYNTAX1,
	OP_SYNTAXRULES,
	OP_EXPANDPATTERN,
	OP_CASE0,
	OP_CASE1,
	OP_CASE2,
	OP_WHEN0,
	OP_WHEN1,
	OP_UNLESS0,
	OP_UNLESS1,
	OP_RECEIVE0,
	OP_RECEIVE1,

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
	OP_EAGER,
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
int Eval_Cycle(int operator)
{
	FILE *tmpfp = NULL;
	int tok = 0;
	int print_flag = 0;
	pointer x, y;
	struct cell v;
	int64_t w;

LOOP:
	c_sink = NIL;

	switch (operator) {
	case OP_EVAL:		/* main part of evalution */
OP_EVAL:
		if (is_symbol(code)) {	/* symbol */
			if (syntaxnum(code) & T_DEFSYNTAX) {
				args = car(code);
				code = cdr(code);
				for (x = car(envir); x != NIL; x = cdr(x)) {
					if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
						s_return(cdar(x));
					}
				}
				for (x = args; x != NIL; x = cdr(x)) {
					if (syntaxnum(x) & T_DEFSYNTAX) {
						for (y = car(x); y != NIL; y = cdr(y)) {
							if ((syntaxnum(caar(y)) & T_DEFSYNTAX) && cdr(caar(y)) == code) {
								s_return(cdar(y));
							}
						}
					} else {
						for (y = car(x); y != NIL; y = cdr(y)) {
							if ((syntaxnum(caar(y)) & T_DEFSYNTAX ? cdr(caar(y)) : caar(y)) == code) {
								s_return(cdar(y));
							}
						}
					}
				}
				for (x = cdr(envir); x != NIL; x = cdr(x)) {
					for (y = car(x); y != NIL; y = cdr(y)) {
						if (caar(y) == code) {
							s_return(cdar(y));
						}
					}
				}
			} else {
				for (x = envir; x != NIL; x = cdr(x)) {
					pointer z = NIL;
					for (y = car(x); y != NIL; z = y, y = cdr(y)) {
						if (caar(y) == code && cdar(y) != UNDEF) {
							if (z != NIL) {
								cdr(z) = cdr(y);
								cdr(y) = car(x);
								car(x) = y;
							}
							s_return(cdar(y));
						}
					}
				}
			}
			Error_1("Unbounded variable", code);
		} else if (is_pair(code)) {
			s_save(OP_E0ARGS, NIL, code);
			code = car(code);
			s_goto(OP_EVAL);
		} else {
			s_return(code);
		}

	case OP_E0ARGS:	/* eval arguments */
		if (is_syntax(value)) {
			if (syntaxnum(value) & T_DEFSYNTAX) {
				value = cdr(value);
			}
			code = cdr(code);
			operator = syntaxnum(value) & T_SYNTAXNUM;
			goto LOOP;
		}
		if (is_closure(value) && is_macro(value)) {	/* macro expansion */
			if (syntaxnum(value) & T_DEFSYNTAX) {
				args = cons(NIL, envir);
				envir = closure_env(value);
				s_save(OP_DOMACRO, NIL, NIL);
				if (is_symbol(caar(value))) {
					x = cons(caar(value), ELLIPSIS);
					x = cons(x, NIL);
					envir = cons(x, envir);
					setenvironment(envir);
					car(value) = cdar(value);
				}
				s_goto(OP_EXPANDPATTERN);
			} else if (exttype(value) & T_DEFMACRO) {
				args = cdr(code);
			} else {
				args = cons(code, NIL);
			}
			code = value;
			s_save(OP_DOMACRO, NIL, NIL);
			s_goto(OP_APPLY);
		}
		code = cdr(code);
		/* fall through */

	case OP_E1ARGS:	/* eval arguments */
		args = cons(value, args);
		if (is_pair(code)) {	/* continue */
			s_save(OP_E1ARGS, args, cdr(code));
			code = car(code);
			args = NIL;
			s_goto(OP_EVAL);
		}
		/* end */
		args = reverse(args);
		code = car(args);
		args = cdr(args);
		/* fall through */

	case OP_APPLY:		/* apply 'code' to 'args' */
OP_APPLY:
		if (is_proc(code)) {	/* PROCEDURE */
			operator = procnum(code);
			goto LOOP;
		} else if (is_foreign(code)) {	/* FOREIGN */
			push_recent_alloc(args);
			x = (foreignfnc(code))(args);
			s_return(x);
		} else if (is_closure(code)) {	/* CLOSURE */
			/* make environment */
			envir = cons(NIL, closure_env(code));
			setenvironment(envir);
			for (mark_x = car(closure_code(code));
			     is_pair(mark_x); mark_x = cdr(mark_x), args = cdr(args)) {
				if (args == NIL) {
					Error_0("Few arguments");
				} else {
					y = cons(car(mark_x), car(args));
					y = cons(y, car(envir));
					car(envir) = y;
				}
			}
			if (mark_x == NIL) {
				/*--
				 * if (args != NIL) {
				 * 	Error_0("Many arguments");
				 * }
				 */
			} else if (is_symbol(mark_x)) {
				mark_x = cons(mark_x, args);
				mark_x = cons(mark_x, car(envir));
				car(envir) = mark_x;
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
		w = s_next_op();
		if (w == OP_WITHVALUES1 || w == OP_RECEIVE1) {
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
		if (load_files == MAXFIL) {
			Error_1("Unable to open", car(args));
		}
		if ((tmpfp = fopen(strvalue(car(args)), "rb")) == NULL) {
			Error_1("Unable to open", car(args));
		}
		load_stack[load_files++] = inport;
		inport = mk_port(tmpfp, port_input);
		/* fall through */

	case OP_T0LVL:	/* top level */
OP_T0LVL:
		if (port_file(inport) == NULL || is_eofport(inport)) {
			if (load_files == 1) {
				break;
			}
			inport = load_stack[--load_files];
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
		} else {
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
				x = cons(vector_elem(code, (int)w), x);
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
		args = cons(value, NIL);
		x = mk_symbol("vector");
		args = cons(x, args);
		x = mk_symbol("apply");
		s_return(cons(x, args));

	case OP_QQUOTE3:	/* quasiquote -- 'quasiquote */
		x = cons(QQUOTE, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, value));

	case OP_QQUOTE4:	/* quasiquote -- 'unquote */
		x = cons(UNQUOTE, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, value));

	case OP_QQUOTE5:	/* quasiquote -- 'unquote-splicing */
		x = cons(UNQUOTESP, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, value));

	case OP_QQUOTE6:	/* quasiquote -- 'cons */
		s_save(OP_QQUOTE7, value, code);
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
		if (syntaxnum(code) & T_DEFSYNTAX) {
			args = car(code);
			code = cdr(code);
			for (x = car(envir); x != NIL; x = cdr(x)) {
				if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
					break;
				}
			}
			if (x == NIL) {
				envir = args;
				if (syntaxnum(envir) & T_DEFSYNTAX) {
					for (x = car(envir); x != NIL; x = cdr(x)) {
						if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
							break;
						}
					}
				} else {
					for (x = car(envir); x != NIL; x = cdr(x)) {
						if ((syntaxnum(caar(x)) & T_DEFSYNTAX ? cdr(caar(x)) : caar(x)) == code) {
							break;
						}
					}
				}
			}
		} else {
			for (x = car(envir); x != NIL; x = cdr(x)) {
				if (caar(x) == code) {
					break;
				}
			}
		}
		if (x != NIL) {
			cdar(x) = value;
		} else {
			x = cons(code, value);
			x = cons(x, car(envir));
			car(envir) = x;
		}
		s_return(code);

	case OP_DEFP:	/* defined? */
		if (!validargs("defined?", 1, 2, TST_SYMBOL TST_ENVIRONMENT)) Error_0(msg);
		code = car(args);
		if (syntaxnum(code) & T_DEFSYNTAX) {
			if (cdr(args) != NIL) {
				args = cadr(args);
			} else {
				args = envir;
			}
			if (args == envir) {
				for (x = car(envir); x != NIL; x = cdr(x)) {
					if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == cdr(code)) {
						s_return(T);
					}
				}
			}
			if (args == envir || args == caar(code)) {
				for (x = car(code); x != NIL; x = cdr(x)) {
					if (syntaxnum(x) & T_DEFSYNTAX) {
						for (y = car(x); y != NIL; y = cdr(y)) {
							if ((syntaxnum(caar(y)) & T_DEFSYNTAX) && cdr(caar(y)) == code) {
								s_return(T);
							}
						}
					} else {
						for (y = car(x); y != NIL; y = cdr(y)) {
							if ((syntaxnum(caar(y)) & T_DEFSYNTAX ? cdr(caar(y)) : caar(y)) == cdr(code)) {
								s_return(T);
							}
						}
					}
				}
				args = cdr(envir);
			}
			for (x = args; x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == cdr(code)) {
						s_return(T);
					}
				}
			}
		} else {
			if (cdr(args) != NIL) {
				envir = cadr(args);
			}
			for (x = envir; x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == code) {
						s_return(T);
					}
				}
			}
		}
		s_return(F);

	case OP_SET0:		/* set! */
		s_save(OP_SET1, NIL, car(code));
		code = cadr(code);
		s_goto(OP_EVAL);

	case OP_SET1:		/* set! */
		if (syntaxnum(code) & T_DEFSYNTAX) {
			args = car(code);
			code = cdr(code);
			for (x = car(envir); x != NIL; x = cdr(x)) {
				if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
					s_return(cdar(x) = value);
				}
			}
			for (x = args; x != NIL; x = cdr(x)) {
				if (syntaxnum(x) & T_DEFSYNTAX) {
					for (y = car(x); y != NIL; y = cdr(y)) {
						if ((syntaxnum(caar(y)) & T_DEFSYNTAX) && cdr(caar(y)) == code) {
							s_return(cdar(y) = value);
						}
					}
				} else {
					for (y = car(x); y != NIL; y = cdr(y)) {
						if ((syntaxnum(caar(y)) & T_DEFSYNTAX ? cdr(caar(y)) : caar(y)) == code) {
							s_return(cdar(y) = value);
						}
					}
				}
			}
			for (x = cdr(envir); x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == code) {
						s_return(cdar(y) = value);
					}
				}
			}
		} else {
			for (x = envir; x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == code) {
						s_return(cdar(y) = value);
					}
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
		if (code != NIL && !is_pair(code)) {
			Error_1("Bad syntax of binding spec in let :", code);
		}
		/* fall through */

	case OP_LET1:		/* let (caluculate parameters) */
		args = cons(value, args);
		if (is_pair(code)) {	/* continue */
			s_save(OP_LET1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in let :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		}
		/* end */
		args = reverse(args);
		code = car(args);
		args = cdr(args);

		envir = cons(NIL, envir);
		setenvironment(envir);
		for (mark_x = is_symbol(car(code)) ? cadr(code) : car(code);
		     args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		if (is_symbol(car(code))) {	/* named let */
			for (mark_x = cadr(code), y = NIL; mark_x != NIL; mark_x = cdr(mark_x))
				y = cons(caar(mark_x), y);
			y = cons(non_alloc_rev(NIL, y), cddr(code));
			y = mk_closure(y, envir);
			y = cons(car(code), y);
			y = cons(y, car(envir));
			car(envir) = y;
			code = cddr(code);
		} else {
			code = cdr(code);
		}
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_LET0AST:	/* let* */
		if (car(code) == NIL) {
			envir = cons(NIL, envir);
			setenvironment(envir);
			code = cdr(code);
			s_goto(OP_BEGIN);
		}
		if (!is_pair(car(code)) || !is_pair(caar(code))) {
			Error_1("Bad syntax of binding spec in let* :", car(code));
		}
		s_save(OP_LET1AST, cdr(code), car(code));
		code = cadaar(code);
		s_goto(OP_EVAL);

	case OP_LET1AST:	/* let* (make new frame) */
		envir = cons(NIL, envir);
		setenvironment(envir);
		/* fall through */

	case OP_LET2AST:	/* let* (caluculate parameters) */
		x = cons(caar(code), value);
		x = cons(x, car(envir));
		car(envir) = x;
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
		if (!is_pair(car(code))) {
			Error_1("Bad syntax of binding spec in letrec :", car(code));
		}
		for (mark_x = car(code); is_pair(mark_x); mark_x = cdr(mark_x)) {
			if (!is_pair(car(mark_x))) {
				Error_1("Bad syntax of binding spec in letrec :", car(mark_x));
			}
			y = cons(caar(mark_x), UNDEF);
			car(envir) = cons(y, car(envir));
		}
		args = NIL;
		value = code;
		code = car(code);
		/* fall through */

	case OP_LET1REC:	/* letrec (caluculate parameters) */
		args = cons(value, args);
		if (is_pair(code)) {	/* continue */
			s_save(OP_LET1REC, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in letrec :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		}
		/* end */
		args = reverse(args);
		code = car(args);
		args = cdr(args);

		for (x = car(code); args != NIL; x = cdr(x), args = cdr(args)) {
			for (y = car(envir); y != NIL; y = cdr(y)) {
				if (caar(y) == caar(x)) {
					cdar(y) = car(args);
				}
			}
		}
		code = cdr(code);
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_LETRECAST0:	/* letrec* */
		envir = cons(NIL, envir);
		setenvironment(envir);
		if (car(code) == NIL) {
			code = cdr(code);
			s_goto(OP_BEGIN);
		}
		if (!is_pair(car(code))) {
			Error_1("Bad syntax of binding spec in letrec* :", car(code));
		}
		for (mark_x = car(code); is_pair(mark_x); mark_x = cdr(mark_x)) {
			if (!is_pair(car(mark_x))) {
				Error_1("Bad syntax of binding spec in letrec* :", car(mark_x));
			}
			y = cons(caar(mark_x), UNDEF);
			car(envir) = cons(y, car(envir));
		}
		if (!is_pair(caar(code)) || !is_pair(cdr(caar(code)))) {
			Error_1("Bad syntax of binding spec in letrec* :", caar(code));
		}
		s_save(OP_LETRECAST1, cdr(code), car(code));
		code = cadaar(code);
		s_goto(OP_EVAL);

	case OP_LETRECAST1:	/* letrec* (caluculate parameters) */
		for (y = car(envir); y != NIL; y = cdr(y)) {
			if (caar(y) == caar(code)) {
				cdar(y) = value;
			}
		}
		code = cdr(code);
		if (is_pair(code)) {	/* continue */
			s_save(OP_LETRECAST1, args, code);
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in letrec* :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		}
		/* end */
		code = args;
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_DO0:		/* do */
		envir = cons(NIL, envir);
		setenvironment(envir);
		args = NIL;
		value = code;
		code = car(code);
		if (!is_pair(code)) {
			Error_1("Bad syntax of binding spec in do :", code);
		}
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
		for (mark_x = car(code); args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		s_save(OP_DO3, NIL, code);
		if (!is_pair(cadr(code))) {
			Error_1("Bad syntax of binding spec in do :", cadr(code));
		}
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
		if (!is_pair(car(code))) {
			Error_1("Syntax error in cond :", car(code));
		}
		s_save(OP_COND1, NIL, code);
		code = caar(code);
		s_goto(OP_EVAL);

	case OP_COND1:		/* cond */
		if (istrue(value)) {
			if ((code = cdar(code)) == NIL) {
				s_return(value);
			}
			if (cdr(code) == NIL || (is_syntax(value) && (syntaxnum(value) & T_SYNTAXNUM) == OP_ELSE)) {
				s_goto(OP_BEGIN);
			}
			s_save(OP_COND2, value, cdr(code));
			code = car(code);
			s_goto(OP_EVAL);
		}
		if ((code = cdr(code)) == NIL) {
			s_return(NIL);
		} else {
			if (!is_pair(car(code))) {
				Error_1("Syntax error in cond :", car(code));
			}
			s_save(OP_COND1, NIL, code);
			code = caar(code);
			s_goto(OP_EVAL);
		}

	case OP_COND2:		/* cond */
		if (is_syntax(value) && (syntaxnum(value) & T_SYNTAXNUM) == OP_FEEDTO) {
			x = cons(args, NIL);
			x = cons(QUOTE, x);
			x = cons(x, NIL);
			code = cons(car(code), x);
			args = NIL;
			s_goto(OP_EVAL);
		}
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_ELSE:		/* else */
		Error_0("Syntax error in else");

	case OP_FEEDTO:		/* => */
		Error_0("Syntax error in =>");

	case OP_DELAY:		/* delay */
		x = cons(NIL, code);
		code = mk_closure(x, envir);
		setpromise(code);
		setresultready(code);
		/* fall through */

	case OP_LAZY:		/* lazy */
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
			x = cons(x, car(envir));
			car(envir) = x;
		}
		s_return(code);

	case OP_SYNTAXRULES:	/* syntax-rules */
		w = s_next_op();
		if (w != OP_DEFSYNTAX1 && w != OP_LETSYNTAX1 && w != OP_LETRECSYNTAX1) {
			Error_0("Malformed syntax of syntax-rules");
		}
		s_return(mk_closure(code, envir));

	case OP_EXPANDPATTERN:	/* expand pattern */
OP_EXPANDPATTERN:
		if (!is_pair(code)) {
			Error_0("Syntax error in syntax-rules");
		}
		for (x = cdar(value); x != NIL; x = cdr(x)) {
			if (car(x) == NIL) {
				Error_0("Syntax error in syntax-rules");
			}
			w = 0;
			if (matchpattern(cdr(caar(x)), cdr(code), caar(value), (int *)&w)) {
				int i, j, m = 0;
				car(args) = car(x);
				x = mk_vector((int)w * 3);
				value = cons(x, caar(value));
				for (i = 0; i < ivalue(car(value)); i += 3) {
					mark_x = mk_integer(0);
					mark_y = mk_integer(0);
					set_vector_elem(car(value), i + 1, cons(mark_x, mark_y));
				}
				w = 0;
				bindpattern(cdr(caar(args)), cdr(code), 0, 0, (int *)&w);
				for (i = 0; i < ivalue(car(value)); i += 3) {
					j = (int)ivalue(car(vector_elem(car(value), i + 1)));
					if (j > m) m = j;
				}
				mark_x = mk_memblock(m * sizeof(int), &NIL, &NIL);
				mark_y = mk_memblock(m * sizeof(int), &NIL, &NIL);
				code = cons(mark_x, mark_y);
				s_return(car(expandpattern(cdar(args), 0, 0, (int *)&w)));
			}
		}
		s_return(NIL);

	case OP_DEFSYNTAX0:		/* define-syntax */
		if (!is_symbol(car(code))) {
			Error_0("Variable is not symbol");
		}
		args = car(code);
		code = cadr(code);
		s_save(OP_DEFSYNTAX1, NIL, args);
		args = NIL;
		s_goto(OP_EVAL);

	case OP_DEFSYNTAX1:		/* define-syntax */
		exttype(value) |= T_MACRO;
		syntaxnum(value) |= T_DEFSYNTAX;
		for (x = car(envir); x != NIL; x = cdr(x))
			if (caar(x) == code)
				break;
		if (x != NIL) {
			cdar(x) = value;
		} else {
			x = cons(code, value);
			x = cons(x, car(envir));
			car(envir) = x;
		}
		s_return(code);

	case OP_LETSYNTAX0:		/* let-syntax */
		args = NIL;
		value = code;
		code = car(code);
		/* fall through */

	case OP_LETSYNTAX1:		/* let-syntax */
		args = cons(value, args);
		if (is_pair(code)) {
			/* continue */
			s_save(OP_LETSYNTAX1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in let-syntax :", car(code));
			}
			if (!is_symbol(caar(code))) {
				Error_0("Variable is not symbol");
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		}
		/* end */
		args = reverse(args);
		code = car(args);
		args = cdr(args);

		envir = cons(NIL, envir);
		setenvironment(envir);
		for (mark_x = car(code); args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			exttype(car(args)) |= T_MACRO;
			syntaxnum(car(args)) |= T_DEFSYNTAX;
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		code = cdr(code);
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_LETRECSYNTAX0:	/* letrec-syntax */
		envir = cons(NIL, envir);
		setenvironment(envir);
		args = NIL;
		value = code;
		code = car(code);
		/* fall through */

	case OP_LETRECSYNTAX1:	/* letrec-syntax */
		args = cons(value, args);
		if (is_pair(code)) {
			/* continue */
			s_save(OP_LETRECSYNTAX1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("Bad syntax of binding spec in letrec-syntax :", car(code));
			}
			if (!is_symbol(caar(code))) {
				Error_0("Variable is not symbol");
			}
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		}
		/* end */
		args = reverse(args);
		code = car(args);
		args = cdr(args);

		for (mark_x = car(code); args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			exttype(car(args)) |= T_MACRO;
			syntaxnum(car(args)) |= T_DEFSYNTAX;
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		code = cdr(code);
		args = NIL;
		s_goto(OP_BEGIN);

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
		if (is_syntax(value) && (syntaxnum(value) & T_SYNTAXNUM) == OP_ELSE) {
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

	case OP_RECEIVE0:	/* receive */
		s_save(OP_RECEIVE1, NIL, code);
		code = cadr(code);
		s_goto(OP_EVAL);

	case OP_RECEIVE1:	/* receive */
		if (type(value) & T_VALUES) {
			type(value) &= ~T_VALUES;
			args = value;
		} else {
			args = cons(value, NIL);
		}
		envir = cons(NIL, envir);
		setenvironment(envir);
		for (mark_x = car(code); is_pair(mark_x); mark_x = cdr(mark_x), args = cdr(args)) {
			if (args == NIL) {
				Error_0("Few arguments");
			} else {
				y = cons(car(mark_x), car(args));
				y = cons(y, car(envir));
				car(envir) = y;
			}
		}
		if (is_symbol(mark_x)) {
			mark_x = cons(mark_x, args);
			mark_x = cons(mark_x, car(envir));
			car(envir) = mark_x;
		}
		code = cddr(code);
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_PAPPLY:	/* apply */
		if (!validargs("apply", 2, 65535, TST_NONE)) Error_0(msg);
		code = car(args);
		args = cdr(args);
		for (x = args, y = car(x); cdr(x) != NIL; x = cdr(x)) {
			if (cddr(x) == NIL) {
				y = cadr(x);
				cdr(x) = NIL;
				x = NIL;
				break;
			}
		}
		if (list_length(y) < 0) {
			Error_0("apply: last argument is not a proper list");
		}
		if (x != NIL) {
			args = y;
		} else {
			args = append(args, y);
		}
		s_goto(OP_APPLY);

	case OP_PEVAL:	/* eval */
		if (!validargs("eval", 1, 1, TST_ANY)) Error_0(msg);
		code = car(args);
		args = NIL;
		s_goto(OP_EVAL);

	case OP_MAP0:	/* map */
		if (!validargs("map", 2, 65535, TST_ANY TST_LIST)) Error_0(msg);
		code = car(args);
		/* fall through */

	case OP_MAP1:	/* map */
		if (operator == OP_MAP0) {
			car(args) = NIL;
		} else {
			car(args) = cons(value, car(args));
		}
		mark_y = NIL;
		for (mark_x = cdr(args); mark_x != NIL; mark_x = cdr(mark_x)) {
			if (car(mark_x) == NIL) {
				s_return(non_alloc_rev(NIL, car(args)));
			}
			mark_y = cons(caar(mark_x), mark_y);
			car(mark_x) = cdar(mark_x);
		}
		s_save(OP_MAP1, args, code);
		args = non_alloc_rev(NIL, mark_y);
		s_goto(OP_APPLY);

	case OP_FOREACH0:	/* for-each */
		if (!validargs("for-each", 2, 65535, TST_ANY TST_LIST)) Error_0(msg);
		code = car(args);
		args = cdr(args);

	case OP_FOREACH1:	/* for-each */
		mark_y = NIL;
		for (mark_x = args; mark_x != NIL; mark_x = cdr(mark_x)) {
			if (car(mark_x) == NIL) {
				s_return(T);
			}
			mark_y = cons(caar(mark_x), mark_y);
			car(mark_x) = cdar(mark_x);
		}
		s_save(OP_FOREACH1, args, code);
		args = non_alloc_rev(NIL, mark_y);
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
		w = s_next_op();
		if (w == OP_WITHVALUES1 || w == OP_RECEIVE1) {
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
		if (type(value) & T_VALUES) {
			type(value) &= ~T_VALUES;
			args = value;
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
		x = cons(car(args), caddr(args));
		winders = cons(x, winders);
		s_save(OP_DYNAMICWIND2, args, code);
		code = cadr(args);
		args = NIL;
		s_goto(OP_APPLY);

	case OP_DYNAMICWIND2:	/* dynamic-wind -- after */
		winders = cdr(winders);
		value = cons(value, args);
		s_save(OP_DYNAMICWIND3, value, code);
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
		for (mark_x = mk_number(&_ZERO); args != NIL; args = cdr(args)) {
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) + ivalue(car(args)));
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
							if (signx == signy) {
								bignum_add_imm(mark_x, car(args), ivalue(mark_x), signx);
							} else {
								bignum_sub_imm(mark_x, car(args), ivalue(mark_x), -signx);
							}
						}
					} else {
						int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
						int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
						if (bignum(car(args)) == NIL) {
							if (signx == signy) {
								bignum_add_imm(mark_x, mark_x, ivalue(car(args)), signx);
							} else {
								bignum_sub_imm(mark_x, mark_x, ivalue(car(args)), signx);
							}
						} else {
							if (signx == signy) {
								bignum_add(&v, mark_x, car(args), signx);
							} else {
								if (bignum_gt(car(args), mark_x)) {
									bignum_sub(&v, car(args), mark_x, -signx);
								} else {
									bignum_sub(&v, mark_x, car(args), signx);
								}
							}
							*mark_x = v;
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) + rvalue(car(args));
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) += get_rvalue(car(args));
			}
		}
		s_return(mark_x);

	case OP_SUB:		/* - */
		if (!validargs("-", 1, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			mark_x = mk_number(&_ZERO);
		} else {
			mark_x = mk_number(car(args));
			args = cdr(args);
		}
		for ( ; args != NIL; args = cdr(args)) {
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) - ivalue(car(args)));
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
							if (signx == signy) {
								bignum_sub_imm(mark_x, car(args), ivalue(mark_x), -signx);
							} else {
								bignum_add_imm(mark_x, car(args), ivalue(mark_x), signx);
							}
						}
					} else {
						int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
						int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
						if (bignum(car(args)) == NIL) {
							if (signx == signy) {
								bignum_sub_imm(mark_x, mark_x, ivalue(car(args)), signx);
							} else {
								bignum_add_imm(mark_x, mark_x, ivalue(car(args)), signx);
							}
						} else {
							if (signx == signy) {
								if (bignum_gt(car(args), mark_x)) {
									bignum_sub(&v, car(args), mark_x, -signx);
								} else {
									bignum_sub(&v, mark_x, car(args), signx);
								}
							} else {
								bignum_add(&v, mark_x, car(args), signx);
							}
							*mark_x = v;
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) - rvalue(car(args));
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) -= get_rvalue(car(args));
			}
		}
		s_return(mark_x);

	case OP_MUL:		/* * */
		if (!validargs("*", 0, 65535, TST_NUMBER)) Error_0(msg);
		for (mark_x = mk_number(&_ONE); args != NIL; args = cdr(args)) {
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) * ivalue(car(args)));
						} else {
							bignum_mul_imm(mark_x, car(args), ivalue(mark_x));
						}
					} else {
						if (bignum(car(args)) == NIL) {
							bignum_mul_imm(mark_x, mark_x, ivalue(car(args)));
						} else {
							bignum_mul(&v, mark_x, car(args));
							*mark_x = v;
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) * rvalue(car(args));
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) *= get_rvalue(car(args));
			}
		}
		s_return(mark_x);

	case OP_DIV:		/* / */
		if (!validargs("/", 1, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			mark_x = mk_number(&_ONE);
		} else {
			mark_x = mk_number(car(args));
			args = cdr(args);
		}
		for ( ; args != NIL; args = cdr(args)) {
			double d = get_rvalue(car(args));
			if (-DBL_MIN < d && d < DBL_MIN) {
				Error_0("Divided by zero");
			}
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							if ((int64_t)ivalue(mark_x) % ivalue(car(args)) == 0) {
								bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) / ivalue(car(args)));
							} else {
								rvalue(mark_x) = ivalue(mark_x) / d;
								set_num_real(mark_x);
							}
						} else {
							if (ivalue(mark_x) == INT32_MIN
								&& ivalue(car(args)) == 1 && ((uint32_t *)strvalue(bignum(car(args))))[0] == (uint32_t)1 << 31) {
								ivalue(mark_x) = -1;
								bignum(mark_x) = NIL;
							} else {
								rvalue(mark_x) = ivalue(mark_x) / d;
								set_num_real(mark_x);
							}
						}
					} else {
						struct cell q, r;
						if (bignum(car(args)) == NIL) {
							bignum_div_imm(&q, &r, mark_x, ivalue(car(args)));
							if (ivalue(&r) == 0) {
								*mark_x = q;
							} else {
								rvalue(mark_x) = get_rvalue(mark_x) / d;
								set_num_real(mark_x);
							}
						} else {
							bignum_div(&q, &r, mark_x, car(args));
							if (ivalue(&r) == 0) {
								*mark_x = q;
							} else {
								rvalue(mark_x) = get_rvalue(mark_x) / d;
								set_num_real(mark_x);
							}
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) / d;
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) /= d;
			}
		}
		s_return(mark_x);

	case OP_ABS:		/* abs */
		if (!validargs("abs", 1, 1, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		if (mark_x->_isfixnum) {
			bignum_abs(mark_x, mark_x);
		} else {
			rvalue(mark_x) = fabs(rvalue(mark_x));
		}
		s_return(mark_x);

	case OP_QUO:		/* quotient */
		if (!validargs("quotient", 2, 2, TST_INTEGER)) Error_0(msg);
		mark_x = mk_number(car(args));
		mark_y = cadr(args);
		w = mark_y->_isfixnum ? ivalue(mark_y) : (int32_t)rvalue(mark_y);
		if (w == 0) {
			Error_0("Divided by zero");
		}
		if (mark_x->_isfixnum) {
			if (mark_y->_isfixnum) {
				if (bignum(mark_x) == NIL) {
					if (bignum(mark_y) == NIL) {
						bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) / ivalue(mark_y));
					} else {
						if (ivalue(mark_x) == INT32_MIN
							&& ivalue(mark_y) == 1 && ((uint32_t *)strvalue(bignum(mark_y)))[0] == (uint32_t)1 << 31) {
							ivalue(mark_x) = -1;
							bignum(mark_x) = NIL;
						} else {
							ivalue(mark_x) = 0;
							bignum(mark_x) = NIL;
						}
					}
				} else {
					struct cell q, r;
					if (bignum(mark_y) == NIL) {
						bignum_div_imm(&q, &r, mark_x, ivalue(mark_y));
					} else {
						bignum_div(&q, &r, mark_x, mark_y);
					}
					*mark_x = q;
				}
			} else {
				rvalue(mark_x) = get_rvalue(mark_x) / rvalue(mark_y);
				rvalue(mark_x) = (rvalue(mark_x) < 0) ? ceil(rvalue(mark_x)) : floor(rvalue(mark_x));
				set_num_real(mark_x);
			}
		} else {
			rvalue(mark_x) = rvalue(mark_x) / get_rvalue(mark_y);
			rvalue(mark_x) = (rvalue(mark_x) < 0) ? ceil(rvalue(mark_x)) : floor(rvalue(mark_x));
		}
		s_return(mark_x);

	case OP_REM:		/* remainder */
		if (!validargs("remainder", 2, 2, TST_INTEGER)) Error_0(msg);
		mark_x = mk_number(car(args));
		mark_y = cadr(args);
		w = mark_y->_isfixnum ? ivalue(mark_y) : (int32_t)rvalue(mark_y);
		if (w == 0) {
			Error_0("Divided by zero");
		}
		if (mark_x->_isfixnum) {
			if (mark_y->_isfixnum) {
				if (bignum(mark_x) == NIL) {
					if (bignum(mark_y) == NIL) {
						bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) % ivalue(mark_y));
					} else {
						if (ivalue(mark_x) == INT32_MIN
							&& ivalue(mark_y) == 1 && ((uint32_t *)strvalue(bignum(mark_y)))[0] == (uint32_t)1 << 31) {
							ivalue(mark_x) = 0;
							bignum(mark_x) = NIL;
						}
					}
				} else {
					struct cell q, r;
					if (bignum(mark_y) == NIL) {
						bignum_div_imm(&q, &r, mark_x, ivalue(mark_y));
					} else {
						bignum_div(&q, &r, mark_x, mark_y);
					}
					*mark_x = r;
				}
			} else {
				rvalue(mark_x) = fmod(get_rvalue(mark_x), rvalue(mark_y));
				set_num_real(mark_x);
			}
		} else {
			rvalue(mark_x) = fmod(rvalue(mark_x), get_rvalue(mark_y));
		}
		s_return(mark_x);

	case OP_MOD:		/* modulo */
		if (!validargs("modulo", 2, 2, TST_INTEGER)) Error_0(msg);
		mark_x = mk_number(car(args));
		mark_y = cadr(args);
		w = mark_y->_isfixnum ? ivalue(mark_y) : (int32_t)rvalue(mark_y);
		if (w == 0) {
			Error_0("Divided by zero");
		}
		if (mark_x->_isfixnum) {
			if (mark_y->_isfixnum) {
				if (bignum(mark_x) == NIL) {
					if (bignum(mark_y) == NIL) {
						int64_t r = (int64_t)ivalue(mark_x) % ivalue(mark_y);
						if (r * ivalue(mark_y) < 0) {
							r += ivalue(mark_y);
						}
						bignum_from_int64(mark_x, r);
					} else {
						if (ivalue(mark_x) == INT32_MIN
							&& ivalue(mark_y) == 1 && ((uint32_t *)strvalue(bignum(mark_y)))[0] == (uint32_t)1 << 31) {
							ivalue(mark_x) = 0;
							bignum(mark_x) = NIL;
						}
						if (ivalue(mark_x) * w < 0) {
							bignum_sub_imm(mark_x, mark_y, ivalue(mark_x), ivalue(mark_y) < 0 ? -1 : 1);
						}
					}
				} else {
					struct cell q, r;
					if (bignum(mark_y) == NIL) {
						bignum_div_imm(&q, &r, mark_x, ivalue(mark_y));
						if (ivalue(&r) * w < 0) {
							ivalue(&r) += ivalue(mark_y);
						}
					} else {
						bignum_div(&q, &r, mark_x, mark_y);
						if (ivalue(&r) * w < 0) {
							if (bignum(&r) == NIL) {
								bignum_sub_imm(&r, mark_y, ivalue(&r), ivalue(mark_y) < 0 ? -1 : 1);
							} else {
								bignum_sub(&r, mark_y, &r, ivalue(mark_y) < 0 ? -1 : 1);
							}
						}
					}
					*mark_x = r;
				}
			} else {
				rvalue(mark_x) = fmod(get_rvalue(mark_x), rvalue(mark_y));
				set_num_real(mark_x);
				if (rvalue(mark_x) * w < 0) {
					rvalue(mark_x) += w;
				}
			}
		} else {
			rvalue(mark_x) = fmod(rvalue(mark_x), get_rvalue(mark_y));
			if (rvalue(mark_x) * w < 0) {
				rvalue(mark_x) += w;
			}
		}
		s_return(mark_x);

	case OP_GCD:		/* gcd */
		if (!validargs("gcd", 0, 65535, TST_NUMBER)) Error_0(msg);
		if (args == NIL) {
			mark_x = mk_number(&_ZERO);
		} else if (cdr(args) == NIL) {
			mark_x = mk_number(car(args));
			if (mark_x->_isfixnum) {
				bignum_abs(mark_x, mark_x);
			} else {
				rvalue(mark_x) = fabs(rvalue(mark_x));
			}
		} else {
			mark_x = mk_number(car(args));
			for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
				if (mark_x->_isfixnum) {
					if (car(mark_y)->_isfixnum) {
						if (bignum(mark_x) == NIL) {
							if (bignum(car(mark_y)) == NIL) {
								bignum_from_int64(mark_x, gcd(ivalue(mark_x), ivalue(car(mark_y))));
							} else {
								if (ivalue(mark_x) != 0) {
									struct cell r;
									bignum_gcd_imm(&r, car(mark_y), ivalue(mark_x));
									*mark_x = r;
								}
							}
						} else {
							if (bignum(car(mark_y)) == NIL) {
								if (ivalue(car(mark_y)) != 0) {
									struct cell r;
									bignum_gcd_imm(&r, mark_x, ivalue(car(mark_y)));
									*mark_x = r;
								}
							} else {
								struct cell r;
								if (bignum_gt(mark_x, car(mark_y))) {
									bignum_gcd(&r, mark_x, car(mark_y));
								} else {
									bignum_gcd(&r, car(mark_y), mark_x);
								}
								*mark_x = r;
							}
						}
					} else {
						rvalue(mark_x) = (double)gcd((int32_t)get_rvalue(mark_x), (int32_t)rvalue(car(mark_y)));
						set_num_real(mark_x);
					}
				} else {
					rvalue(mark_x) = (double)gcd((int32_t)rvalue(mark_x), (int32_t)get_rvalue(car(mark_y)));
				}
			}			
		}
		s_return(mark_x);

	case OP_LCM:		/* lcm */
		if (!validargs("lcm", 0, 65535, TST_NUMBER)) Error_0(msg);
		if (args == NIL) {
			mark_x = mk_number(&_ONE);
		} else if (cdr(args) == NIL) {
			mark_x = mk_number(car(args));
			if (mark_x->_isfixnum) {
				bignum_abs(mark_x, mark_x);
			} else {
				rvalue(mark_x) = fabs(rvalue(mark_x));
			}
		} else {
			mark_x = mk_number(car(args));
			for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
				if (mark_x->_isfixnum) {
					if (car(mark_y)->_isfixnum) {
						if (bignum(mark_x) == NIL) {
							if (bignum(car(mark_y)) == NIL) {
								bignum_from_int64(mark_x, lcm(ivalue(mark_x), ivalue(car(mark_y))));
							} else {
								if (ivalue(mark_x) == 0) {
									mark_x = mk_number(&_ZERO);
									break;
								} else {
									struct cell r;
									bignum_lcm_imm(&r, car(mark_y), ivalue(mark_x));
									*mark_x = r;
								}
							}
						} else {
							if (bignum(car(mark_y)) == NIL) {
								if (ivalue(car(mark_y)) == 0) {
									mark_x = mk_number(&_ZERO);
									break;
								} else {
									struct cell r;
									bignum_lcm_imm(&r, mark_x, ivalue(car(mark_y)));
									*mark_x = r;
								}
							} else {
								struct cell r;
								if (bignum_gt(mark_x, car(mark_y))) {
									bignum_lcm(&r, mark_x, car(mark_y));
								} else {
									bignum_lcm(&r, car(mark_y), mark_x);
								}
								*mark_x = r;
							}
						}
					} else {
						rvalue(mark_x) = (double)lcm((int32_t)get_rvalue(mark_x), (int32_t)rvalue(car(mark_y)));
						set_num_real(mark_x);
					}
				} else {
					rvalue(mark_x) = (double)lcm((int32_t)rvalue(mark_x), (int32_t)get_rvalue(car(mark_y)));
				}
			}
		}
		s_return(mark_x);

	case OP_FLOOR:		/* floor */
		if (!validargs("floor", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(floor(get_rvalue(car(args)))));

	case OP_CEILING:	/* ceiling */
		if (!validargs("ceiling", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(ceil(get_rvalue(car(args)))));

	case OP_TRUNCATE:	/* truncate */
		if (!validargs("truncate", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (get_rvalue(x) > 0) {
			s_return(mk_real(floor(get_rvalue(x))));
		} else {
			s_return(mk_real(ceil(get_rvalue(x))));
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
		s_return(mk_real(exp(get_rvalue(car(args)))));

	case OP_LOG:		/* log */
		if (!validargs("log", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(log(get_rvalue(car(args)))));

	case OP_SIN:		/* sin */
		if (!validargs("sin", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(sin(get_rvalue(car(args)))));

	case OP_COS:		/* cos */
		if (!validargs("cos", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(cos(get_rvalue(car(args)))));

	case OP_TAN:		/* tan */
		if (!validargs("tan", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(tan(get_rvalue(car(args)))));

	case OP_ASIN:		/* asin */
		if (!validargs("asin", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(asin(get_rvalue(car(args)))));

	case OP_ACOS:		/* acos */
		if (!validargs("acos", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(acos(get_rvalue(car(args)))));

	case OP_ATAN:		/* atan */
		if (!validargs("atan", 1, 2, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			s_return(mk_real(atan(get_rvalue(car(args)))));
		} else {
			s_return(mk_real(atan2(get_rvalue(car(args)), get_rvalue(cadr(args)))));
		}

	case OP_SQRT:		/* sqrt */
		if (!validargs("sqrt", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(sqrt(get_rvalue(car(args)))));

	case OP_EXPT:		/* expt */
		if (!validargs("expt", 2, 2, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		y = cadr(args);
		if (mark_x->_isfixnum && y->_isfixnum && ((bignum(mark_x) == NIL && ivalue(mark_x) == 1) || ivalue(y) == 0)) {
			*mark_x = _ONE;
		} else if (mark_x->_isfixnum && y->_isfixnum && ivalue(mark_x) == 0) {
			*mark_x = _ZERO;
		} else if (mark_x->_isfixnum) {
			if (y->_isfixnum && bignum(y) == NIL && ivalue(y) > 0) {
				bignum_pow(mark_x, mark_x, ivalue(y));
			} else {
				rvalue(mark_x) = pow(get_rvalue(mark_x), get_rvalue(y));
				set_num_real(mark_x);
			}
		} else {
			rvalue(mark_x) = pow(rvalue(mark_x), get_rvalue(y));
		}
		s_return(mark_x);

	case OP_EX2INEX:	/* exact->inexact */
		if (!validargs("exact->inexact", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(get_rvalue(car(args))));

	case OP_INEX2EX:	/* inexact->exact */
		if (!validargs("inexact->exact", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (x->_isfixnum) {
			s_return(x);
		} else if (rvalue(x) == (int64_t)rvalue(x)) {
			bignum_from_int64(&v, (int64_t)rvalue(x));
			s_return(mk_number(&v));
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
		s_return(mk_string(atom2str(car(args), (int)w)));

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
			s_return(mk_integer_from_str(strvalue(car(args)), strlen(strvalue(car(args))), (int)w));
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
		args = cadr(args);
		if (w < ivalue(args)) {
			Error_1("list-tail: index too large:", args);
		}
		for (w = 0; w < ivalue(args); w++) {
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
		args = cadr(args);
		if (w <= ivalue(args)) {
			Error_1("list-ref: index too large:", args);
		}
		for (w = 0; w < ivalue(args); w++) {
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
		s_return(mk_character((int)ivalue(car(args))));

	case OP_CHARUPCASE:	/* char-upcase */
		if (!validargs("char-upcase", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_character(utf32_toupper((int)ivalue(car(args)))));

	case OP_CHARDNCASE:	/* char-downcase */
		if (!validargs("char-downcase", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_character(utf32_tolower((int)ivalue(car(args)))));

	case OP_MKSTRING:	/* make-string */
		if (!validargs("make-string", 1, 2, TST_NATURAL TST_CHAR)) Error_0(msg);
		if (cdr(args) != NIL) {
			s_return(mk_empty_string((size_t)ivalue(car(args)), (int)ivalue(cadr(args))));
		} else {
			s_return(mk_empty_string((size_t)ivalue(car(args)), ' '));
		}

	case OP_STRING:		/* string */
		if (!validargs("string", 0, 65535, TST_CHAR)) Error_0(msg);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			w += utf32_to_utf8((int)ivalue(car(x)), NULL);
		}
		y = mk_counted_string("", (size_t)w);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			w += utf32_to_utf8((int)ivalue(car(x)), strvalue(y) + w);
		}
		strvalue(y)[w] = '\0';
		s_return(y);

	case OP_STRLEN:		/* string-length */
		if (!validargs("string-length", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_integer((int32_t)utf8_strlen(strvalue(car(args)))));

	case OP_STRREF:		/* string-ref */
		if (!validargs("string-ref", 2, 2, TST_STRING TST_NATURAL)) Error_0(msg);
		w = utf8_strref(strvalue(car(args)), (size_t)ivalue(cadr(args)));
		if (w == -1) {
			Error_1("string-ref: out of bounds:", cadr(args));
		}
		s_return(mk_character((int)w));

	case OP_STRSET:		/* string-set! */
		if (!validargs("string-set!", 3, 3, TST_STRING TST_NATURAL TST_CHAR)) Error_0(msg);
		w = utf8_strpos(strvalue(car(args)), (size_t)ivalue(cadr(args)));
		if (w == -1 || strvalue(car(args))[w] == '\0') {
			Error_1("string-set!: out of bounds:", cadr(args));
		} else {
			char utf8[4];
			size_t len1 = utf8_get_next(strvalue(car(args)) + w, NULL);
			size_t len2 = utf32_to_utf8((int)ivalue(caddr(args)), utf8);
			if (len1 == len2) {
				memcpy(strvalue(car(args)) + w, utf8, len2);
			} else {
				size_t n = strlength(car(args)) + len2 - len1;
				x = mk_memblock(n, &NIL, &NIL);
				memcpy(strvalue(x), strvalue(car(args)), (size_t)w);
				memcpy(strvalue(x) + w, utf8, len2);
				memcpy(strvalue(x) + w + len2, strvalue(car(args)) + w + len1, (size_t)(n - w - len2));
				strvalue(x)[n] = '\0';
				strvalue(car(args)) = strvalue(x);
				strlength(car(args)) = strlength(x);
			}
		}
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
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) == 0);
	case OP_STRCILSS:		/* string-ci<? */
		if (!validargs("string-ci<?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) < 0);
	case OP_STRCIGTR:		/* string-ci>? */
		if (!validargs("string-ci>?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) > 0);
	case OP_STRCILEQ:		/* string-ci<=? */
		if (!validargs("string-ci<=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) <= 0);
	case OP_STRCIGEQ:		/* string-ci>=? */
		if (!validargs("string-ci>=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) >= 0);

	case OP_SUBSTR:		/* substring */
		if (!validargs("substring", 2, 3, TST_STRING TST_NATURAL)) Error_0(msg);
		w = utf8_strlen(strvalue(car(args)));
		if (ivalue(cadr(args)) > w) {
			Error_1("substring: start out of bounds:", cadr(args));
		} else {
			int start = utf8_strpos(strvalue(car(args)), (size_t)ivalue(cadr(args))), n;
			if (cddr(args) != NIL) {
				if (ivalue(caddr(args)) > w || ivalue(caddr(args)) < ivalue(cadr(args))) {
					Error_1("substring: end out of bounds:", caddr(args));
				}
				n = utf8_strpos(strvalue(car(args)), (size_t)ivalue(caddr(args))) - start;
			} else {
				n = (int)strlength(car(args)) - start;
			}
			x = mk_counted_string("", n);
			memcpy(strvalue(x), strvalue(car(args)) + start, n);
			strvalue(x)[n] = '\0';
		}
		s_return(x);

	case OP_STRAPPEND:	/* string-append */
		if (!validargs("string-append", 0, 65535, TST_STRING)) Error_0(msg);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			w += strlength(car(x));
		}
		y = mk_counted_string("", (size_t)w);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			memcpy(strvalue(y) + w, strvalue(car(x)), strlength(car(x)));
			w += strlength(car(x));
		}
		strvalue(y)[w] = '\0';
		s_return(y);

	case OP_STR2LIST:	/* string->list */
		if (!validargs("string->list", 1, 1, TST_STRING)) Error_0(msg);
		code = NIL;
		w = 0;
		while ((size_t)w < strlength(car(args))) {
			int c;
			w += utf8_get_next(strvalue(car(args)) + w, &c);
			x = mk_character(c);
			code = cons(x, code);
		}
		s_return(non_alloc_rev(NIL, code));

	case OP_LIST2STR:	/* list->string */
		if (!validargs("list->string", 1, 1, TST_LIST)) Error_0(msg);
		if (list_length(car(args)) < 0) {
			Error_1("list->string: not a proper list:", car(args));
		}
		for (w = 0, x = car(args); x != NIL; x = cdr(x)) {
			if (!is_character(car(x))) {
				Error_1("list->string: not a charactor:", car(x));
			}
			w += utf32_to_utf8((int)ivalue(car(x)), NULL);
		}
		y = mk_counted_string("", (size_t)w);
		for (w = 0, x = car(args); x != NIL; x = cdr(x)) {
			w += utf32_to_utf8((int)ivalue(car(x)), strvalue(y) + w);
		}
		strvalue(y)[w] = '\0';
		s_return(y);

	case OP_STRCOPY:	/* string-copy */
		if (!validargs("string-copy", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_string(strvalue(car(args))));

	case OP_STRFILL:	/* string-fill! */
		if (!validargs("string-fill!", 2, 2, TST_STRING TST_CHAR)) Error_0(msg);
		x = car(args);
		w = utf8_strlen(strvalue(x));
		if (w > 0) {
			char utf8[4];
			size_t len = utf32_to_utf8((int)ivalue(cadr(args)), utf8);
			if (strlength(x) != (size_t)w * len) {
				y = mk_memblock((size_t)w * len, &x, &NIL);
				strvalue(x) = strvalue(y);
				strlength(x) = strlength(y);
			}
			strvalue(x)[w * len] = '\0';
			while (w > 0) {
				memcpy(strvalue(x) + --w * len, utf8, len);
			}
		}
		s_return(x);

	case OP_VECTOR:		/* vector */
OP_VECTOR:
		if (!validargs("vector", 0, 65535, TST_NONE)) Error_0(msg);
		w = list_length(args);
		if (w < 0) {
			Error_1("vector: not a proper list:", args);
		}
		y = mk_vector((int)w);
		for (x = args, w = 0; is_pair(x); x = cdr(x)) {
			set_vector_elem(y, (int)w++, car(x));
		}
		s_return(y);

	case OP_MKVECTOR:	/* make-vector */
		if (!validargs("make-vector", 1, 2, TST_NATURAL TST_ANY)) Error_0(msg);
		w = ivalue(car(args));
		x = mk_vector((int)w);
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
		s_return(vector_elem(car(args), (int)w));

	case OP_VECSET:		/* vector-set! */
		if (!validargs("vector-set!", 3, 3, TST_VECTOR TST_NATURAL TST_ANY)) Error_0(msg);
		w = ivalue(cadr(args));
		if (w >= ivalue(car(args))) {
		   Error_1("vector-set!: out of bounds:", cadr(args));
		}
		set_vector_elem(car(args), (int)w, caddr(args));
		s_return(car(args));

	case OP_VEC2LIST:		/* vector->list */
		if (!validargs("vector->list", 1, 1, TST_VECTOR)) Error_0(msg);
		y = NIL;
		for (w = ivalue(car(args)) - 1; w >= 0; w--) {
			y = cons(vector_elem(car(args), (int)w), y);
		}
		s_return(y);

	case OP_LIST2VEC:		/* list->vector */
		if (!validargs("list->vector", 1, 1, TST_LIST)) Error_0(msg);
		args = car(args);
		w = list_length(args);
		if (w < 0) {
			Error_1("list->vector: not a proper list:", args);
		}
		y = mk_vector((int)w);
		for (w = 0; args != NIL; args = cdr(args)) {
			set_vector_elem(y, (int)w++, car(args));
		}
		s_return(y);

	case OP_VECFILL:	/* vector-fill! */
		if (!validargs("vector-fill!", 2, 2, TST_VECTOR TST_ANY)) Error_0(msg);
		for (x = car(args), w = ivalue(x) - 1; w >= 0; w--) {
			set_vector_elem(car(args), (int)w, cadr(args));
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
		if (car(args)->_isfixnum) {
			if (bignum(car(args)) == NIL) {
				s_retbool(ivalue(car(args)) % 2 != 0);
			} else {
				s_retbool(((uint32_t *)strvalue(bignum(car(args))))[0] % 2 != 0);
			}
		} else {
			s_retbool((int64_t)rvalue(car(args)) % 2 != 0);
		}
	case OP_EVEN:		/* even? */
		if (!validargs("even?", 1, 1, TST_INTEGER)) Error_0(msg);
		if (car(args)->_isfixnum) {
			if (bignum(car(args)) == NIL) {
				s_retbool(ivalue(car(args)) % 2 == 0);
			} else {
				s_retbool(((uint32_t *)strvalue(bignum(car(args))))[0] % 2 == 0);
			}
		} else {
			s_retbool((int64_t)rvalue(car(args)) % 2 == 0);
		}
	case OP_NEQ:		/* = */
		if (!validargs("=", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) != ivalue(y)) { s_return(F); }
					} else {
						s_return(F);
					}
				} else {
					if (bignum(y) == NIL) {
						s_return(F);
					} else {
						if ((ivalue(x) < 0) == (ivalue(y) < 0)) {
							if (!bignum_eq(x, y)) { s_return(F); }
						} else {
							s_return(F);
						}
					}
				}
			} else {
				if (get_rvalue(x) != get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case OP_LESS:		/* < */
		if (!validargs("<", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) >= ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) <= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) >= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx > 0) {
								if (bignum_ge(x, y)) { s_return(F); }
							} else {
								if (bignum_ge(y, x)) { s_return(F); }
							}
						} else {
							if (signx >= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) >= get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case OP_GRE:		/* > */
		if (!validargs(">", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) <= ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) >= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) <= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx < 0) {
								if (bignum_ge(x, y)) { s_return(F); }
							} else {
								if (bignum_ge(y, x)) { s_return(F); }
							}
						} else {
							if (signx <= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) <= get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case OP_LEQ:		/* <= */
		if (!validargs("<=", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) > ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) <= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) >= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx > 0) {
								if (bignum_gt(x, y)) { s_return(F); }
							} else {
								if (bignum_gt(y, x)) { s_return(F); }
							}
						} else {
							if (signx >= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) > get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case OP_GEQ:		/* >= */
		if (!validargs(">=", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) < ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) >= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) <= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx < 0) {
								if (bignum_gt(x, y)) { s_return(F); }
							} else {
								if (bignum_gt(y, x)) { s_return(F); }
							}
						} else {
							if (signx <= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) < get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case OP_MAX:		/* max */
		if (!validargs("max", 1, 65535, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
			if (mark_x->_isfixnum) {
				if (car(mark_y)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) < ivalue(car(mark_y))) {
								ivalue(mark_x) = ivalue(car(mark_y));
							}
						} else {
							if (ivalue(car(mark_y)) > 0) {
								pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
							}
						}
					} else {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) < 0) {
								bignum_from_int64(mark_x, ivalue(car(mark_y)));
							}
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(mark_y)) < 0 ? -1 : 1;
							if (signx == signy) {
								if (signx > 0) {
									if (bignum_gt(car(mark_y), mark_x)) {
										pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
									}
								} else {
									if (bignum_gt(mark_x, car(mark_y))) {
										pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
									}
								}
							} else if (signx < signy) {
								pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
							}
						}
					}
				} else {
					if (get_rvalue(mark_x) < rvalue(car(mark_y))) {
						rvalue(mark_x) = rvalue(car(mark_y));
					} else {
						rvalue(mark_x) = get_rvalue(mark_x);
					}
					set_num_real(mark_x);
				}
			} else {
				if (rvalue(mark_x) < get_rvalue(car(mark_y))) {
					rvalue(mark_x) = get_rvalue(car(mark_y));
				}
			}
		}
		s_return(mark_x);

	case OP_MIN:		/* min */
		if (!validargs("min", 1, 65535, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
			if (mark_x->_isfixnum) {
				if (car(mark_y)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) > ivalue(car(mark_y))) {
								ivalue(mark_x) = ivalue(car(mark_y));
							}
						} else {
							if (ivalue(car(mark_y)) < 0) {
								pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
							}
						}
					} else {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) > 0) {
								bignum_from_int64(mark_x, ivalue(car(mark_y)));
							}
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(mark_y)) < 0 ? -1 : 1;
							if (signx == signy) {
								if (signx < 0) {
									if (bignum_gt(car(mark_y), mark_x)) {
										pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
									}
								} else {
									if (bignum_gt(mark_x, car(mark_y))) {
										pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
									}
								}
							} else if (signx > signy) {
								pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
							}
						}
					}
				} else {
					if (get_rvalue(mark_x) > rvalue(car(mark_y))) {
						rvalue(mark_x) = rvalue(car(mark_y));
					} else {
						rvalue(mark_x) = get_rvalue(mark_x);
					}
					set_num_real(mark_x);
				}
			} else {
				if (rvalue(mark_x) > get_rvalue(car(mark_y))) {
					rvalue(mark_x) = get_rvalue(car(mark_y));
				}
			}
		}
		s_return(mark_x);

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
		s_retbool(utf32_tolower((int)ivalue(car(args))) == utf32_tolower((int)ivalue(cadr(args))));
	case OP_CHARCILSS:	/* char-ci<? */
		if (!validargs("char-ci<?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) < utf32_tolower((int)ivalue(cadr(args))));
	case OP_CHARCIGTR:	/* char-ci>? */
		if (!validargs("char-ci>?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) > utf32_tolower((int)ivalue(cadr(args))));
	case OP_CHARCILEQ:	/* char-ci<=? */
		if (!validargs("char-ci<=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) <= utf32_tolower((int)ivalue(cadr(args))));
	case OP_CHARCIGEQ:	/* char-ci>=? */
		if (!validargs("char-ci>=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) >= utf32_tolower((int)ivalue(cadr(args))));
	case OP_CHARAP:		/* char-alphabetic? */
		if (!validargs("char-alphabetic?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isalpha((int)ivalue(car(args))));
	case OP_CHARNP:		/* char-numeric? */
		if (!validargs("char-numeric?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isdigit((int)ivalue(car(args))));
	case OP_CHARWP:		/* char-whitespace? */
		if (!validargs("char-whitespace?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isspace((int)ivalue(car(args))));
	case OP_CHARUP:		/* char-upper-case? */
		if (!validargs("char-upper-case?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isupper((int)ivalue(car(args))));
	case OP_CHARLP:		/* char-lower-case? */
		if (!validargs("char-lower-case?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_islower((int)ivalue(car(args))));
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

	case OP_EAGER:		/* eagar */
		if (!validargs("eagar", 1, 1, TST_ANY)) Error_0(msg);
		x = cons(NIL, car(args));
		x = mk_closure(x, envir);
		setpromise(x);
		setresultready(x);
		s_return(x);

	case OP_FORCE:		/* force */
		if (!validargs("force", 1, 1, TST_ANY)) Error_0(msg);
		code = car(args);
	OP_FORCE:
		if (is_promise(code)) {
			if (is_resultready(code)) {
				s_return(cdar(code));
			}
			s_save(OP_FORCED, NIL, code);
			args = NIL;
			if (is_promise(cdar(code))) {
				code = cdar(code);
			}
			s_goto(OP_APPLY);
		}
		s_return(code);

	case OP_FORCED:		/* force */
		if (is_resultready(code)) {
			s_return(cdar(code));
		}
		if (is_promise(cdar(code)) || !is_promise(value)) {
			cdar(code) = value;
			setresultready(code);
		} else {
			cdar(code) = cdar(value);
			cdr(code) = cdr(value);
			car(value) = car(code);
			if (is_resultready(value)) {
				setresultready(code);
			} else {
				setresultready(value);
			}
		}
		s_goto(OP_FORCE);

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
			if (!interactive_repl) {
				return -1;
			}
			s_goto(OP_T0LVL);
		}

	case OP_REVERSE:	/* reverse */
		if (!validargs("reverse", 1, 1, TST_LIST)) Error_0(msg);
		s_return(reverse(car(args)));

	case OP_APPEND:	/* append */
		if (!validargs("append", 0, 65535, TST_NONE)) Error_0(msg);
		s_return(append(car(args), cadr(args)));

	case OP_QUIT:		/* quit */
		if (!validargs("quit", 0, 1, TST_INTEGER)) Error_0(msg);
		if (is_pair(args)) {
			return (int)ivalue(car(args));
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
			backchar((int)w);
		}
		s_return(mk_character((int)w));

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
		if (tok == TOK_EOF) {
			s_return(EOF_OBJ);
		}
		switch (tok & 0x0f) {
		case TOK_VEC:
			s_save(OP_RDVEC, NIL, code);
			/* fall through */
		case TOK_LPAREN:
			w = tok >> 4;
			tok = token();
			if ((tok & 0x0f) == TOK_RPAREN) {
				if (w == (tok >> 4)) {
					s_return(NIL);
				}
				Error_0("syntax error -- unexpected parenthesis");
			} else if (tok == TOK_DOT) {
				Error_0("syntax error -- illegal dot expression");
			} else {
				code = mk_integer((int32_t)w);
				s_save(OP_RDLIST, NIL, code);
				s_goto(OP_RDSEXPR);
			}
		case TOK_QUOTE:
			s_save(OP_RDQUOTE, NIL, code);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_BQUOTE:
			tok = token();
			if ((tok & 0x0f) == TOK_VEC) {
				s_save(OP_RDQQUOTEVEC, NIL, code);
				tok = TOK_LPAREN | (tok >> 4);
			} else {
				s_save(OP_RDQQUOTE, NIL, code);
			}
			s_goto(OP_RDSEXPR);
		case TOK_COMMA:
			s_save(OP_RDUNQUOTE, NIL, code);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_ATMARK:
			s_save(OP_RDUQTSP, NIL, code);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_ATOM:
			s_return(mk_atom(readstr("()[]{};\t\n\r ")));
		case TOK_DQUOTE:
			s_return(readstrexp());
		case TOK_SHARP:
			if ((x = mk_const(readstr("()[]{};\t\n\r "))) == NIL) {
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
		} else if ((tok & 0x0f) == TOK_RPAREN) {
			if (ivalue(code) == (tok >> 4)) {
				s_return(non_alloc_rev(NIL, args));
			}
			Error_0("syntax error -- unexpected parenthesis");
		} else if (tok == TOK_DOT) {
			s_save(OP_RDDOT, args, code);
			tok = token();
			s_goto(OP_RDSEXPR);
		} else {
			s_save(OP_RDLIST, args, code);
			s_goto(OP_RDSEXPR);
		}

	case OP_RDDOT:
		tok = token();
		if ((tok & 0x0f) == TOK_RPAREN) {
			if (ivalue(code) == (tok >> 4)) {
				s_return(non_alloc_rev(value, args));
			}
			Error_0("syntax error -- unexpected parenthesis");
		} else {
			Error_0("syntax error -- illegal dot expression");
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
		args = cons(x, NIL);
		x = mk_symbol("vector");
		args = cons(x, args);
		x = mk_symbol("apply");
		s_return(cons(x, args));

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
			x = mk_integer(0);
			args = cons(args, x);
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
			ivalue(cdr(args)) = (int32_t)w + 1;
			s_save(OP_PVECFROM, args, NIL);
			args = vector_elem(car(args), (int)w);
			if (w > 0) putstr(" ");
			s_goto(OP_P0LIST);
		}

	case OP_LIST_LENGTH:	/* length */	/* a.k */
		if (!validargs("length", 1, 1, TST_LIST)) Error_0(msg);
		w = list_length(car(args));
		if (w < 0) {
			Error_1("length: not a list:", car(args));
		}
		s_return(mk_integer((int32_t)w));

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
		if (syntaxnum(code) & T_DEFSYNTAX) {
			value = code;
			code = car(args);
			s_goto(OP_EXPANDPATTERN);
		} else if (exttype(code) & T_DEFMACRO) {
			args = cdar(args);
		}
		code = value;
		s_goto(OP_APPLY);

	case OP_ATOMP:		/* atom? */
		if (!validargs("atom?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(is_atom(car(args)));

	default:
		sprintf(msg, "%d is illegal operator", operator);
		Error_0(msg);
	}

	return 0;
}

/* ========== Initialization of internal keywords ========== */

void mk_syntax(int op, char *name)
{
	pointer x;

	x = mk_string(name);
	type(x) |= T_SYMBOL | T_SYNTAX;
	syntaxnum(x) = (short)op;
	oblist = cons(x, oblist);
	x = cons(x, x);
	x = cons(x, car(global_env));
	car(global_env) = x;
}

void mk_proc(int op, char *name)
{
	pointer x, y;

	x = mk_symbol(name);
	y = get_cell(&x, &NIL);
	type(y) = (T_PROC | T_ATOM);
	ivalue(y) = op;
	set_num_integer(y);
	x = cons(x, y);
	x = cons(x, car(global_env));
	car(global_env) = x;
}


void init_vars_global(void)
{
	oblist = NIL;
	winders = NIL;
#ifdef USE_COPYING_GC
	gcell_list = NIL;
#endif
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
	/* init UNDEF */
	type(UNDEF) = (T_ATOM | MARK);
	car(UNDEF) = cdr(UNDEF) = UNDEF;
	/* init global_env */
	global_env = cons(NIL, NIL);
	setenvironment(global_env);
	type(&_ZERO) = T_NUMBER | T_ATOM;
	set_num_integer(&_ZERO);
	ivalue(&_ZERO) = 0;
	bignum(&_ZERO) = NIL;
	type(&_ONE) = T_NUMBER | T_ATOM;
	set_num_integer(&_ONE);
	ivalue(&_ONE) = 1;
	bignum(&_ONE) = NIL;
	load_stack[0] = mk_port(stdin, port_input);
	load_files = 1;
	/* init output file */
	outport = mk_port(stdout, port_output);
	strbuff = mk_memblock(256, &NIL, &NIL);
}


void init_syntax(void)
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
	mk_syntax(OP_LETRECAST0, "letrec*");
	mk_syntax(OP_DO0, "do");
	mk_syntax(OP_COND0, "cond");
	mk_syntax(OP_ELSE, "else");
	mk_syntax(OP_FEEDTO, "=>");
	mk_syntax(OP_DELAY, "delay");
	mk_syntax(OP_LAZY, "lazy");
	mk_syntax(OP_AND0, "and");
	mk_syntax(OP_OR0, "or");
	mk_syntax(OP_C0STREAM, "cons-stream");
	mk_syntax(OP_0MACRO, "macro");
	mk_syntax(OP_DEFMACRO0, "define-macro");
	mk_syntax(OP_CASE0, "case");
	mk_syntax(OP_WHEN0, "when");
	mk_syntax(OP_UNLESS0, "unless");
	mk_syntax(OP_SYNTAXRULES, "syntax-rules");
	mk_syntax(OP_DEFSYNTAX0, "define-syntax");
	mk_syntax(OP_LETSYNTAX0, "let-syntax");
	mk_syntax(OP_LETRECSYNTAX0, "letrec-syntax");
	mk_syntax(OP_RECEIVE0, "receive");
}


void init_procs(void)
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
	mk_proc(OP_EAGER, "eager");
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


/* initialization of Mini-Scheme */
void scheme_init(void)
{
	alloc_cellseg();
	gc_verbose = 0;

	/* initialize several globals */
	init_vars_global();
	init_syntax();
	init_procs();
	/* intialization of global pointers to special symbols */
	LAMBDA = mk_symbol("lambda");
	QUOTE = mk_symbol("quote");
	QQUOTE = mk_symbol("quasiquote");
	UNQUOTE = mk_symbol("unquote");
	UNQUOTESP = mk_symbol("unquote-splicing");
	ELLIPSIS = mk_symbol("...");
#ifndef USE_SCHEME_STACK
	dump_base = mk_dumpstack(NIL);
	dump = dump_base;
#else
	dump = NIL;
#endif
	envir = global_env;
	code = NIL;
	value = NIL;
	mark_x = NIL;
	mark_y = NIL;
	c_nest = NIL;
	c_sink = NIL;
}

void scheme_deinit(void)
{
	oblist = NIL;
	inport = NIL;
	outport = NIL;
	global_env = NIL;
	winders = NIL;
#ifdef USE_COPYING_GC
	gcell_list = NIL;
#endif
	args = NIL;
	envir = NIL;
	code = NIL;
#ifndef USE_SCHEME_STACK
	dump_base = NIL;
#endif
	dump = NIL;
	value = NIL;
	mark_x = NIL;
	mark_y = NIL;
	c_nest = NIL;
	c_sink = NIL;

	gc_verbose = 0;
	gc(&NIL, &NIL);
}

int scheme_load_file(FILE *fin)
{
	int op;

	if (fin == stdin) {
		interactive_repl = 1;
	}
	inport = mk_port(fin, port_input);
	if (setjmp(error_jmp) == 0) {
		op = OP_T0LVL;
	} else {
		op = OP_QUIT;
	}
	return Eval_Cycle(op);
}

int scheme_load_string(const char *cmd)
{
	int op;

	interactive_repl = 0;
	inport = port_from_string(cmd, port_input);
	if (setjmp(error_jmp) == 0) {
		op = OP_T0LVL;
	} else {
		op = OP_QUIT;
	}
	return Eval_Cycle(op);
}

void scheme_register_foreign_func(const char *name, foreign_func ff)
{
	pointer s = mk_symbol(name);
	pointer f = mk_foreign_func(ff, &s), x;

	for (x = car(global_env); x != NIL; x = cdr(x)) {
		if (caar(x) == s) {
			cdar(x) = f;
			return;
		}
	}
	x = cons(s, f);
	x = cons(x, car(global_env));
	car(global_env) = x;
}

void save_from_C_call(void)
{
	pointer x;
#ifndef USE_SCHEME_STACK
	x = cons(dump, dump_base);
	x = cons(envir, x);
	x = cons(c_sink, x);
	c_nest = cons(x, c_nest);
	dump_base = mk_dumpstack(NIL);
	dump = dump_base;
#else
	x = cons(envir, dump);
	x = cons(c_sink, x);
	c_nest = cons(x, c_nest);
	dump = NIL;
#endif
	envir = global_env;
}

void restore_from_C_call(void)
{
#ifndef USE_SCHEME_STACK
	c_sink = caar(c_nest);
	envir = car(cdar(c_nest));
	dump = cadr(cdar(c_nest));
	dump_base = cddr(cdar(c_nest));
#else
	c_sink = caar(c_nest);
	envir = car(cdar(c_nest));
	dump = cdr(cdar(c_nest));
#endif
	c_nest = cdr(c_nest);
}

pointer scheme_call(pointer func, pointer argslist)
{
	int old_repl = interactive_repl;
	interactive_repl = 0;
	save_from_C_call();
	envir = global_env;
	args = argslist;	/* assumed to be already eval'ed. */
	code = func;		/* assumed to be already eval'ed. */
	Eval_Cycle(OP_APPLY);
	interactive_repl = old_repl;
	restore_from_C_call();
	return value;
}

pointer scheme_eval(pointer obj)
{
	int old_repl = interactive_repl;
	interactive_repl = 0;
	save_from_C_call();
	args = NIL;
	code = obj;
	Eval_Cycle(OP_EVAL);
	interactive_repl = old_repl;
	restore_from_C_call();
	return value;
}

pointer scheme_apply0(const char *procname)
{
	pointer x = mk_symbol(procname);
	return scheme_eval(cons(x, NIL));
}

pointer scheme_apply1(const char *procname, pointer argslist)
{
	pointer x;
	mark_x = argslist;
	x = mk_symbol(procname);
	return scheme_eval(cons(x, mark_x));
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

#if STANDALONE

int main(int argc, char *argv[])
{
	int ret;
	FILE *fin;

	scheme_init();

	/* Load "init.scm" */
	if ((fin = fopen(InitFile, "rb")) != NULL) {
		ret = scheme_load_file(fin);
	} else {
		fprintf(stderr, "Unable to open %s\n", InitFile);
	}

	if (argc > 1) {
		fin = fopen(argv[1], "rb");
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

#endif
