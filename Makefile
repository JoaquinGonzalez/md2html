SRC = md2html.c

md2html: $(SRC)
	$(CC) $(SRC) -std=c89 -pedantic -march=native -Wall -O0 -g -o $@

clean:
	rm md2html
