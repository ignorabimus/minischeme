#include "miniscm.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#ifdef _WIN32
#include <winsock2.h>
#endif

#include "replxx.h"
void init_re(void);
void init_tsx(void);
extern pointer oblist;

static int get_input_length(const char *input)
{
	int escaped = 0, dquotes = 0;
	int n_paren = 0, s_paren = 0, c_paren = 0;
	const char *p;

	if (input[0] == '\0') {
		return 0;
	}

	for (p = input; *p == '\t' || *p == ' '; p++);

	while (*p) {
		if (escaped) {
			escaped = 0;
			p++;
			continue;
		}
		switch (*p) {
		case '\\':
			escaped = 1;
			break;
		case '"':
			dquotes = !dquotes;
			break;
		case ';':
			if (dquotes == 0) {
				do { p++; } while (*p != '\r' && *p != '\n' && *p != '\0');
				continue;
			}
			break;
		case '\t':
		case ' ':
			if (dquotes == 0 && n_paren <= 0 && s_paren <= 0 && c_paren <= 0) {
				return p - input;
			}
			break;
		case '(':
		case '[':
		case '{':
			if (dquotes == 0) {
				if (p - input > 0 && n_paren <= 0 && s_paren <= 0 && c_paren <= 0) {
					return p - input;
				}
				if (*p == '(') n_paren++;
				else if (*p == '[') s_paren++;
				else c_paren++;
			}
			break;
		case ')':
		case ']':
		case '}':
			if (dquotes == 0) {
				if (*p == ')') n_paren--;
				else if (*p == ']') s_paren--;
				else c_paren--;
				if (n_paren <= 0 && s_paren <= 0 && c_paren <= 0) {
					return p - input + 1;
				}
			}
			break;
		default:
			break;
		}
		p++;
	}

	return (n_paren > 0 || s_paren > 0 || c_paren > 0) ? -1 : p - input;
}

static void completionHook(char const* prefix, int bp, replxx_completions* lc, void* ud)
{
	char **examples = (char **)ud;
	size_t i;
	for (i = 0; examples[i] != NULL; ++i) {
		if (strncmp(prefix + bp, examples[i], strlen(prefix) - bp) == 0) {
			replxx_add_completion(lc, examples[i]);
		}
	}
}

static void hintHook(char const* prefix, int bp, replxx_hints* lc, ReplxxColor* c, void* ud)
{
	char **examples = (char **)ud;
	size_t i;
	int len = strlen(prefix);
	if (len > bp) {
		for (i = 0; examples[i] != NULL; ++i) {
			if (strncmp(prefix + bp, examples[i], strlen(prefix) - bp) == 0) {
				replxx_add_hint(lc, examples[i] + len - bp);
			}
		}
	}
}

static void colorHook(char const* str_, ReplxxColor* colors_, int size_, void* ud)
{
	int i = 0;
	for (; i < size_; ++i) {
		if (isdigit(str_[i])) {
			colors_[i] = BRIGHTMAGENTA;
		}
	}
}

static int compare_hints(const void *a, const void *b)
{
	return strcmp(*(char **)a, *(char **)b);
}

int main(int argc, char *argv[])
{
	int ret;
	pointer x;
	char *examples_base = NULL, **examples = NULL;
	size_t hints_size = 0, hints_num = 0;

#ifdef _WIN32
	WSADATA wsaData;
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
		return 1;
	}
#endif

	scheme_init();
	init_re();
	init_tsx();

	ret = scheme_load_string("(define call/cc call-with-current-continuation)");
	if (ret != 0) {
		fprintf(stderr, "Unable to init.\n");
		return 1;
	}

	for (x = oblist; x != NIL; x = cdr(x)) {
		hints_size += strlen(symname(car(x))) + 1;
		hints_num++;
	}

	examples_base = (char *)malloc(hints_size + 1);
	if (examples_base == NULL) {
		fprintf(stderr, "Fatal: malloc failed (%u bytes).\n", hints_size + 1);
		return 1;
	}

	examples = (char **)malloc((hints_num + 1) * sizeof (char *));
	if (examples == NULL) {
		fprintf(stderr, "Fatal: malloc failed (%u bytes).\n", (hints_num + 1) * sizeof (char *));
		return 1;
	}

	hints_size = hints_num = 0;
	examples[0] = examples_base;
	for (x = oblist; x != NIL; x = cdr(x)) {
		hints_size = strlen(symname(car(x))) + 1;
		memcpy(examples[hints_num], symname(car(x)), hints_size);
		examples[hints_num + 1] = examples[hints_num] + hints_size;
		hints_num++;
	}
	examples[hints_num] = NULL;

	qsort(examples, hints_num, sizeof (char *), compare_hints);

	char const* prompt0 = "\x1b[1;32m>>>\x1b[0m ";
	char const* prompt1 = "\x1b[1;32m...\x1b[0m ";

	Replxx* replxx = replxx_init();
	replxx_install_window_change_handler(replxx);
	replxx_set_completion_callback(replxx, completionHook, (void *)examples);
	replxx_set_highlighter_callback(replxx, colorHook, NULL);
	replxx_set_hint_callback(replxx, hintHook, (void *)examples);
	replxx_set_word_break_characters(replxx, " \\\"'`;@{([])}");

	if (argc > 1) {
		FILE *fin = fopen(argv[1], "r");
		if (fin == NULL) {
			fprintf(stderr, "Unable to open %s\n", argv[1]);
			return 1;
		}
		scheme_load_file(fin);
	}

	printf("Hello, This is Mini-Scheme REPL.\n");

	ret = 0;
	while (ret <= 0) {
		char const* result;
		do {
			result = replxx_input(replxx, prompt0);
		} while ((result == NULL) && (errno == EAGAIN));

		if (result == NULL) {
			break;
		}
		replxx_history_add(replxx, result);

		char *line1 = (char *)malloc(strlen(result) + 2);
		if (line1 == NULL) {
			fprintf(stderr, "Fatal: malloc failed (%u bytes).\n", strlen(result) + 2);
			goto EXIT;
		}
		sprintf(line1, "%s\n", result);

		do {
			int i, len;
			char *line2, *line3;
			while ((len = get_input_length(line1)) == -1) {
				do {
					result = replxx_input(replxx, prompt1);
				} while ((result == NULL) && (errno == EAGAIN));

				if (result == NULL) {
					goto EXIT;
				}
				replxx_history_add(replxx, result);

				line3 = (char *)malloc(strlen(line1) + strlen(result) + 2);
				if (line3 == NULL) {
					fprintf(stderr, "Fatal: malloc failed (%u bytes).\n", strlen(line1) + strlen(result) + 2);
					goto EXIT;
				}

				sprintf(line3, "%s%s\n", line1, result);
				free(line1);
				line1 = line3;
			}

			line2 = (char *)malloc(strlen(line1) - len + 1);
			if (line2 == NULL) {
				fprintf(stderr, "Fatal: malloc failed (%u bytes).\n", strlen(line1) - len + 1);
				goto EXIT;
			}
			strcpy(line2, line1 + len);

			for (i = 0; i < len; i++) {
				if (line1[i] == ';') {
					i = len;
					break;
				} else if (line1[i] != '\r' && line1[i] != '\n' && line1[i] != '\t' && line1[i] != ' ') {
					break;
				}
			}
			if (i < len) {
				line1[len] = '\0';
				line3 = (char *)malloc(len + 20);
				if (line3 == NULL) {
					fprintf(stderr, "Fatal: malloc failed (%u bytes).\n", len + 20);
					goto EXIT;
				}
				sprintf(line3, "(display %s)(newline)", line1);

				ret = scheme_load_string(line3);
				free(line3);
			}

			strcpy(line1, line2);
			free(line2);
		} while (strlen(line1) > 0 && ret <= 0);

		free(line1);
	}

EXIT:
	replxx_end(replxx);
	free(examples);
	free(examples_base);
	scheme_deinit();
#ifdef _WIN32
	WSACleanup();
#endif

	return ret;
}
