#
# generic makefile for Duel 1.10
# nothing fancy is required: yacc/bison of parse.y, compile all other modules,
# and put all of the OBJ= modules into duel.a library.
# gdb is compiled with its own makefile (you add duel.a and duelgdb.c)
# selfduel, for testing, is compiled and linked with duel.a

# NOTE:
# for sunOS4.x you must use gcc or another ANSI compiler. cc doesnt
# support prototypes!
# Also for sunOS4.x, yacc inserts a declaration of malloc() at the first line
# of y.tab.c, which is WRONG (return char*). just delete it.

CFLAGS= -g
CC= cc                  # won't work on sunOS. need ANSI C prototypes support.
#CC= gcc
YACC=yacc
RANLIB=ranlib           # for BSD-derived systems

#CC = gcc -U__GNUC__    #ONLY if you get __eprintf undefined (assert.h problem)
#RANLIB=echo            # for System V, there is no 'ranlib'

OBJ= duel.o types.o eval.o misc.o y.tab.o error.o evalops.o print.o output.o

all:    duel.a duelself

duelself: duel.a duelself.o
	$(CC) duelself.o duel.a -o duelself

duel.a:  $(OBJ)
	ar ru duel.a $(OBJ)
	$(RANLIB) duel.a

y.tab.c: parse.y
	$(YACC) parse.y
clean:
	rm -f $(OBJ) y.tab.c duelself.o duelself duel.a


test:   duelself
	duelself <tsuite.self >self.out
	@echo NOTE: diffs of the @xxxx values are ok
	diff tsuite.self.out self.out
