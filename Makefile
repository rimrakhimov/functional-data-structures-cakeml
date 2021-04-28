# Change this directory if necessary  -- or
# provide the directory for your machine on the make command-line, e.g.
# make -n   CAKE_DIR="/someOtherLocation/cake-x64-64"
CAKE_DIR = ~/cake-x64-64
CAKEC = $(CAKE_DIR)/cake
BASIS = $(CAKE_DIR)/basis_ffi.c

OS ?= $(shell uname)

ifeq ($(OS),Windows_NT)
	PREF =
	SUFF = .exe
else
	PREF = ./
	SUFF =
endif

ifeq ($(OS),Darwin)
	# These options avoid linker warnings on macOS
	LDFLAGS += -Wl,-no_pie
endif

DEBUG = true
ifeq ($(DEBUG), true)
	CFLAGS += -ggdb3
else
	CFLAGS = -DNDEBUG
endif

CC = gcc

# Using the CakeML compiler

# The conventions used here for extensions, namely,
# *.cml          # CakeML source program
# *.cake.S       # CakeML-generated machine-code
# *.cake$(SUFF)  # CakeML-generated executable
# are not required in general when using CakeML.
# (They are not used here for the compiler itself.)

%.cake.S : src/%.sml
	$(CAKEC) $(CAKEFLAGS) <$< >$@

%.cake$(SUFF) : %.cake.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	$(RM) basis_ffi.o cake$(SUFF) *.cake.S *.cake$(SUFF)
