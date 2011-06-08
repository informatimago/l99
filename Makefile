LISP=clisp -ansi -E utf-8
all:
	$(LISP) compile-all.lisp
clean:
	rm -f *.lib *.fas *.fasl *.x86f

