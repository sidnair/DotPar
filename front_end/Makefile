all:
	lex lexer.l && yacc -d --verbose dotpar.y && gcc lex.yy.c y.tab.c -ly

clean:
	rm -f a.out *.c *.h *.output
