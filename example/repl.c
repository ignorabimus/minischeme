#include "miniscm.h"
#ifdef _WIN32
#include <winsock2.h>
#endif

void init_re(void);
void init_tsx(void);

int main(int argc, char *argv[])
{
	int ret;

#ifdef _WIN32
	WSADATA wsaData;
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
		return;
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

	ret = scheme_load_file(stdin);

	scheme_deinit();

#ifdef _WIN32
	WSACleanup();
#endif

	return ret;
}
