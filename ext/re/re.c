/* re.c */
/* Henry Spencer's implementation of Regular Expressions,
   used for MiniScheme */
/* Refurbished by Stephen Gildea */
#include <sys/types.h>
#include <stdlib.h>
#include "regex.h"
#include "miniscm.h"

pointer foreign_re_match(pointer args)
{
	pointer retval = F;
	int retcode;
	regex_t rt;
	pointer first_arg, second_arg;
	pointer third_arg = NIL;
	char *string;
	char *pattern;
	int num = 0;

	if (!((args != NIL) && is_string((first_arg = car(args)))
		  && (args=cdr(args))
		  && is_pair(args) && is_string((second_arg = car(args))))) {
		return F;
	}
	pattern = strvalue(first_arg);
	string = strvalue(second_arg);

	args = cdr(args);
	if (args != NIL) {
		if (!(is_pair(args) && is_vector((third_arg = car(args))))) {
			return F;
		} else {
			num = ivalue(third_arg);
		}
	}

	if (regcomp(&rt, pattern, REG_EXTENDED) != 0) {
		return F;
	}

	if (num == 0) {
		retcode = regexec(&rt, string, 0, 0, 0);
	} else {
		regmatch_t *pmatch = malloc((num + 1) * sizeof(regmatch_t));
		if (pmatch != 0) {
			retcode = regexec(&rt, string, num + 1, pmatch, 0);
			if (retcode == 0) {
				int i;
				for (i = 0; i < num; i++) {
					mark_x = mk_integer(pmatch[i].rm_so);
					mark_y = mk_integer(pmatch[i].rm_eo);
					set_vector_elem(third_arg, i, cons(mark_x, mark_y));
				}
			}
			free(pmatch);
		} else {
			retcode = -1;
		}
	}

	if (retcode == 0) {
		retval = T;
	}

	regfree(&rt);

	return retval;
}

void init_re(void)
{
	scheme_register_foreign_func("re-match", foreign_re_match);
}
