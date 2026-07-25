ROOT := ..
REPLXX_DIR := ../../replxx
REPLXX_BUILD_DIR := $(REPLXX_DIR)/build

BIN_DIR := bin
OBJ_DIR := .obj
TARGET := $(BIN_DIR)/repl

CC ?= gcc
CXX ?= g++

CPPFLAGS := -DSTANDALONE=0 -I$(ROOT)/src -I$(ROOT)/ext/re -I$(ROOT)/ext/tsx -I$(REPLXX_DIR)/include
CFLAGS := -O2 -std=gnu11
LDFLAGS :=
LDLIBS := -pthread -ldl -lm

WARN_REPL := -Wall -Wextra -Wno-format -Wno-unused-parameter
WARN_CORE := -Wno-sign-compare -Wno-implicit-fallthrough
WARN_LEGACY_RE := -Wno-old-style-declaration -Wno-missing-braces -Wno-unused-value -Wno-unused-function -Wno-unused-variable -Wno-maybe-uninitialized
WARN_LEGACY_TSX := -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -Wno-unused-parameter -include math.h

SRCS := \
	repl.c \
	$(ROOT)/src/miniscm.c \
	$(ROOT)/src/bignum.c \
	$(ROOT)/ext/re/re.c \
	$(ROOT)/ext/re/regcomp.c \
	$(ROOT)/ext/re/regexec.c \
	$(ROOT)/ext/re/regerror.c \
	$(ROOT)/ext/re/regfree.c \
	$(ROOT)/ext/tsx/tsx.c

OBJS := $(addprefix $(OBJ_DIR)/,$(notdir $(SRCS:.c=.o)))

CORE_OBJS := $(OBJ_DIR)/miniscm.o $(OBJ_DIR)/bignum.o
RE_OBJS := $(OBJ_DIR)/re.o $(OBJ_DIR)/regcomp.o $(OBJ_DIR)/regexec.o $(OBJ_DIR)/regerror.o $(OBJ_DIR)/regfree.o

VPATH := .:$(ROOT)/src:$(ROOT)/ext/re:$(ROOT)/ext/tsx

.PHONY: all clean replxx

all: $(TARGET)

$(TARGET): $(OBJS) $(REPLXX_BUILD_DIR)/libreplxx.a | $(BIN_DIR)
	$(CXX) $(LDFLAGS) -o $@ $(OBJS) $(REPLXX_BUILD_DIR)/libreplxx.a $(LDLIBS)


$(REPLXX_BUILD_DIR)/libreplxx.a:
	@cmake -S $(REPLXX_DIR) -B $(REPLXX_BUILD_DIR) -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF
	@cmake --build $(REPLXX_BUILD_DIR) --target replxx

replxx: $(REPLXX_BUILD_DIR)/libreplxx.a

$(BIN_DIR) $(OBJ_DIR):
	mkdir -p $@

$(OBJ_DIR)/repl.o: EXTRA_CFLAGS := $(WARN_REPL)
$(CORE_OBJS): EXTRA_CFLAGS := $(WARN_CORE)
$(RE_OBJS): EXTRA_CFLAGS := $(WARN_LEGACY_RE)
$(OBJ_DIR)/tsx.o: EXTRA_CFLAGS := $(WARN_LEGACY_TSX)

$(OBJ_DIR)/%.o: %.c | $(OBJ_DIR)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(EXTRA_CFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ_DIR) $(TARGET)
