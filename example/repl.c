#include "miniscm.h"
#ifdef _WIN32
#include <winsock2.h>
#endif

void init_re(void);
void init_tsx(void);

static char *getline(const char *prompt)
{
	char buf[1024], *p;

	fputs(prompt, stdout);

	if (fgets(buf, sizeof(buf), stdin) == NULL) {
		return NULL;
	}

	p = (char *)malloc(strlen(buf) + 1);
	if (p == NULL) {
		return NULL;
	}
	strcpy(p, buf);

	return p;
}

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
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
			if (dquotes == 0) {
				if (*p == '(') n_paren++;
				else if (*p == ')') n_paren--;
				else if (*p == '[') s_paren++;
				else if (*p == ']') s_paren--;
				else if (*p == '{') c_paren++;
				else c_paren--;
			}
			break;
		default:
			break;
		}
		p++;
	}

	return (n_paren > 0 || s_paren > 0 || c_paren > 0) ? -1 : p - input;
}

int main(int argc, char *argv[])
{
	int ret;

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
		char *line1 = getline("> ");
		if (line1 == NULL) {
			break;
		}

		do {
			int i, len;
			char *line2, *line3;
			while ((len = get_input_length(line1)) == -1) {
				line2 = getline("  ");
				if (line2 == NULL) {
					goto EXIT;
				}

				line3 = (char *)malloc(strlen(line1) + strlen(line2) + 1);
				if (line3 == NULL) {
					goto EXIT;
				}

				sprintf(line3, "%s%s", line1, line2);
				free(line1);
				free(line2);
				line1 = line3;
			}

			line2 = (char *)malloc(strlen(line1) - len + 1);
			if (line2 == NULL) {
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
	scheme_deinit();

#ifdef _WIN32
	WSACleanup();
#endif

	return ret;
}
