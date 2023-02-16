SRC = md2html.c

md2html: $(SRC)
	c89 $(SRC) -g -o $@

clean:
	rm md2html
