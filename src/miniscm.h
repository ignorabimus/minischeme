#ifndef MINISCHEME_H
#define MINISCHEME_H

#include <stdio.h>
#include <inttypes.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Define or undefine following symbols as you need.
 */
#ifndef STANDALONE
#define STANDALONE 1	/* define 0 if you want to build as a Library */
#endif
/* #define USE_SCHEME_STACK */	/* define this if you want original-Stack */
#define USE_COPYING_GC	/* undef this if you do not want to use Copying GC */

/*
 *  Basic memory allocation units
 */
#define CELL_SEGSIZE 500000	/* # of cells in one segment */

#define MAXFIL 64	/* stack size of loading files */

typedef struct cell *pointer;
typedef pointer (*foreign_func)(pointer);

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
		union {
			struct {
				int32_t _ivalue;
				struct cell *_bignum;
			} _integer;
			double  _rvalue;
		} _number;
		struct {
			FILE   *_file;
			char   *_curr;
		} _port;
		foreign_func _ff;
		struct {
			struct cell *_car;
			struct cell *_cdr;
		} _cons;
#ifdef USE_COPYING_GC
		struct cell *_forwarded;
#endif
	} _object;
};

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
#define T_FOREIGN     2048	/* 0000100000000000 */
#define T_MEMBLOCK    4096	/* 0001000000000000 */
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
#define T_DEFSYNTAX  32768	/* 1000000000000000 */	/* for define-syntax */
#define T_SYNTAXNUM  32767	/* 0111111111111111 */	/* for define-syntax */

/* macros for cell operations */
#define type(p)         ((p)->_flag)
#define exttype(p)      ((p)->_extflag)

#define is_string(p)    (type(p)&T_STRING)
#define strvalue(p)     ((p)->_object._string._svalue)
#define strlength(p)    ((p)->_object._string._length)

#define is_number(p)    (type(p)&T_NUMBER)
#define ivalue(p)       ((p)->_object._number._integer._ivalue)
#define bignum(p)       ((p)->_object._number._integer._bignum)
#define rvalue(p)       ((p)->_object._number._rvalue)
#define nvalue(p)       ((p)->_isfixnum ? ivalue(p) : rvalue(p))
#define is_integer(p)   (is_number(p) && ((p)->_isfixnum || floor(rvalue(p) + 0.5) == rvalue(p)))
#define set_num_integer(p)   ((p)->_isfixnum = 1)
#define set_num_real(p)      ((p)->_isfixnum = 0)

#define is_pair(p)      (type(p)&T_PAIR)
#define car(p)          ((p)->_object._cons._car)
#define cdr(p)          ((p)->_object._cons._cdr)

#define is_symbol(p)    (type(p)&T_SYMBOL)
#define symname(p)      strvalue(p)

#define is_syntax(p)    (type(p)&T_SYNTAX)
#define is_proc(p)      (type(p)&T_PROC)
#define syntaxnum(p)    (*(short *)&(p)->_extflag)
#define procnum(p)      (int)ivalue(p)

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
	port_eof = 16,
};
#define is_port(p)      (type(p) & T_PORT)
#define is_inport(p)    (is_port(p) && ((p)->_isfixnum & port_input))
#define is_outport(p)   (is_port(p) && ((p)->_isfixnum & port_output))
#define is_fileport(p)  (is_port(p) && ((p)->_isfixnum & port_file))
#define is_strport(p)   (is_port(p) && ((p)->_isfixnum & port_string))
#define is_eofport(p)   (is_port(p) && ((p)->_isfixnum & port_eof))
#define port_file(p)    ((p)->_object._port._file)
#define port_curr(p)    ((p)->_object._port._curr)

#define is_vector(p)    (type(p) & T_VECTOR)

#define is_foreign(p)   (type(p) & T_FOREIGN)
#define foreignfnc(p)   ((p)->_object._ff)

#define is_memblock(p)  (type(p) & T_MEMBLOCK)

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

extern pointer NIL;
extern pointer T;
extern pointer F;
extern pointer EOF_OBJ;
extern pointer mark_x;
extern pointer mark_y;
extern jmp_buf error_jmp;

pointer cons(pointer a, pointer b);
pointer mk_character(int c);
pointer mk_integer(int32_t num);
pointer mk_real(double num);
pointer mk_number(pointer v);
pointer mk_string(const char *str);
pointer mk_empty_string(size_t len, int fill);
pointer mk_symbol(const char *name);
pointer mk_uninterned_symbol(const char *name);
pointer gensym(void);
pointer mk_atom(const char *q);
pointer mk_const(const char *name);
pointer mk_port(FILE *fp, int prop);
pointer mk_port_string(pointer p, int prop);
pointer mk_vector(int len);
pointer vector_elem(pointer v, int i);
pointer set_vector_elem(pointer v, int i, pointer a);
int list_length(pointer a);

void scheme_init(void);
void scheme_deinit(void);
int scheme_load_file(FILE *fin);
int scheme_load_string(const char *cmd);
void scheme_register_foreign_func(const char *name, foreign_func ff);
pointer scheme_apply0(const char *procname);
pointer scheme_apply1(const char *procname, pointer argslist);

#ifdef __cplusplus
}
#endif

#endif /* MINISCHEME_H */
