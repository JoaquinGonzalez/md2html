#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHAR_IS_NEWLINE(x)  x == 10
#define CHAR_IS_HEADER(x)   x == 35
#define CHAR_IS_ASTERISK(x) x == 42
#define CHAR_IS_SPACE(x)    x == 32
#define CHAR_IS_TEXT(x)     x >= 32 && x <= 41 || x >= 43 && x <= 126

enum
{
    TOKEN_TYPE_HEADER,
    TOKEN_TYPE_IMAGE,
    TOKEN_TYPE_PARAGRAPH,
    TOKEN_TYPE_ITALIC,
    TOKEN_TYPE_BOLD,
    TOKEN_TYPE_CODE,
    TOKEN_TYPE_HTML,
    TOKEN_TYPE_TEXT,
    TOKEN_TYPE_NEWLINE
};

typedef struct t_List
{
    void *ptr;
    struct t_List *next;
} List;

typedef struct
{
    unsigned int type;
    union {
        int i;
        char *s;
        struct {
            char *alt;
            char *src;
        } img;
    } value;
} Token;

static char* itoa(int, int);
static char* createstring(char*);
static char* createstring2(int);
static char* stringcat(char*, char*);
static void freestring(char*);
static List* createlist();
static void listadd(List*, void*);
static void printlist(List*);
static void freelist(List*);
static void stackpush(void*);
static void* stackpop();
static void freestack();
static void rendertag(Token*, int);
static void renderstacktop(int);
static void renderlist(List*);
static void freerenderl(List*);
static void render(List*);
static Token* createtoken(int);
static void freetoken(Token*);
static void freetokenlist(List*);
static void tokenize(char*);
char* md2html(char*);

static List *tokens;
static List *renderl;
static List *st = NULL;

static char *headertag[] = {
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6"
};
static char *taglist[] = {
    0,
    "img",
    "p"
};
static char buff[1<<25];
static char *out;

static char* itoa(int val, int base)
{
	static char buf[32] = {0};
	int i = 30;

	for(; val && i ; --i, val /= base)
		buf[i] = "0123456789abcdef"[val % base];

	return &buf[i+1];
}

static char* createstring(char* str)
{
    const unsigned int size = sizeof(unsigned int) + strlen(str) +  256;
    void *head = (void*)calloc(size, sizeof(char));
    char *headstr = (char*)(head + sizeof(unsigned int) + 1);

    *(unsigned int*)head = size;
    strcpy(headstr, str);

    return headstr;
}

static char* createstring2(int size)
{
    const unsigned int nsize = sizeof(unsigned int) + size;
    void *head = (void*)calloc(nsize, sizeof(char));
    char *headstr = (char*)(head + sizeof(unsigned int) + 1);

    *(unsigned int*)head = size;

    return headstr;
}

static char* stringcat(char *from, char *to)
{
    void *head = (void*)(to - sizeof(unsigned int) - 1);
    const unsigned int size = *(unsigned int*)head;

    if (strlen(from) + strlen(to) > size) {
        char *c = createstring2(strlen(from) + strlen(to) + 256);
        strcat(c, to);
        strcat(c, from);
        free(head);
        return c;
    }

    strcat(to, from);

    return to;
}

static void freestring(char *str)
{
    void *head = (void*)(str - sizeof(unsigned int) - 1);
    free(head);
}

static List* createlist()
{
    List *l = (List*)malloc(sizeof(List));
    l->ptr = NULL;
    l->next = NULL;
    return l;
}

static void listadd(List *list, void *ptr)
{
    if (list->next == NULL) {
        if (list->ptr == NULL) {
            list->ptr = ptr;
        } else {
            List *l = createlist();
            l->ptr = ptr;
            list->next = l;
        }
    } else {
        listadd(list->next, ptr);
    }
}

static void freelist(List *list)
{
    if (list->next != NULL) {
        freelist(list->next);
        free(list->next);

        list->next = NULL;
    }
}

static void printlist(List *list)
{
    Token *t;

    if (list->ptr != NULL) {
        t = (Token*)list->ptr;

        switch (t->type) {
            case TOKEN_TYPE_HEADER:
                printf("TOKEN_TYPE_HEADER { n = %i }\n", t->value.i);
                break;
            case TOKEN_TYPE_ITALIC:
                printf("TOKEN_TYPE_ITALIC\n");
                break;
            case TOKEN_TYPE_BOLD:
                printf("TOKEN_TYPE_BOLD\n");
                break;
            case TOKEN_TYPE_TEXT:
                printf("TOKEN_TYPE_TEXT { text = %s }\n", t->value.s);
                break;
            case TOKEN_TYPE_NEWLINE:
                printf("TOKEN_TYPE_NEWLINE\n");
                break;
        }
    }

    if (list->next != NULL)
        printlist(list->next);
}

static void stackpush(void *ptr)
{
    List *nst;

    nst = (List*)malloc(sizeof(List));
    nst->ptr = ptr;
    nst->next = st;
    st = nst;
}

static void* stackpop()
{
    if (st == NULL) return NULL;
    
    List *ost = st;
    st = ost->next;
    free(ost);

    return ost->ptr;
}

static void* stacktop()
{
    if (st == NULL) return NULL;
    return st->ptr;
}

static void freestack()
{
    while (st != NULL) stackpop();
}

static void rendertag(Token *t, int close)
{
    char tag[100];
    char *tagname;

    switch (t->type) {
        case TOKEN_TYPE_HEADER:
            tagname = headertag[t->value.i - 1];
            break;
        case TOKEN_TYPE_PARAGRAPH:
            tagname = "p";
            break;
        case TOKEN_TYPE_BOLD:
            tagname = "strong";
            break;
        case TOKEN_TYPE_ITALIC:
            tagname = "em";
            break;
    }

    if (close == 0) {
        sprintf(tag, "<%s>", tagname);
    } else {
        sprintf(tag, "</%s>", tagname);
    }

    out = stringcat(tag, out);
}

static void renderstacktop(int close)
{
    Token *sttoken = (Token*)stacktop();

    if (sttoken != NULL)
        rendertag(sttoken, close);
}

static void renderlist(List* list)
{
    if (list->ptr != NULL) {
        Token *t = (Token*)list->ptr;

        if (t->type == TOKEN_TYPE_TEXT)
            out = stringcat(t->value.s, out);
    }

    if (list->next != NULL)
        renderlist(list->next);
}

static void freerenderl(List *list)
{
    list->ptr = NULL;
    if (list->next != NULL)
        freerenderl(list->next);
}

static void render(List *list)
{
    if (list->ptr != NULL) {
        Token *t = (Token*)list->ptr;

        switch (t->type) {
            case TOKEN_TYPE_HEADER:
            case TOKEN_TYPE_BOLD:
            case TOKEN_TYPE_ITALIC:
                if (st != NULL) {
                    Token *top = (Token*)stacktop();

                    if (top->type == t->type) {
                        renderstacktop(1);
                        stackpop();
                    }
                } else {
                    stackpush(t);
                    renderstacktop(0);
                }
                break;
            case TOKEN_TYPE_TEXT:
                /* listadd(renderl, t); */
                if (st == NULL) {
                    Token *nt = createtoken(TOKEN_TYPE_PARAGRAPH);
                    stackpush(nt);
                    renderstacktop(0);
                }
                out = stringcat(t->value.s, out);
                break;
            case TOKEN_TYPE_NEWLINE:
                if (st != NULL) {
                    renderstacktop(1);
                    stackpop();
                    out = stringcat("\n", out);
                }
                break;
        }
    }

    if (list->next != NULL) {
        render(list->next);
    }
}

static Token* createtoken(int type)
{
    Token *t = (Token*)malloc(sizeof(Token));

    t->type = type;

    switch (t->type) {
        case TOKEN_TYPE_TEXT:
        case TOKEN_TYPE_BOLD:
        case TOKEN_TYPE_CODE:
        case TOKEN_TYPE_HTML:
            t->value.s = createstring("");
            break;
        case TOKEN_TYPE_IMAGE:
            t->value.img.src = createstring("");
            t->value.img.alt = createstring("");
            break;
    }

    return t;
}

static void freetoken(Token *token)
{
    switch (token->type) {
        case TOKEN_TYPE_TEXT:
        case TOKEN_TYPE_BOLD:
        case TOKEN_TYPE_CODE:
        case TOKEN_TYPE_HTML:
            freestring(token->value.s);
            break;
        case TOKEN_TYPE_IMAGE:
            freestring(token->value.img.alt);
            freestring(token->value.img.src);
            break;
    }

    free(token);
}

static void freetokenlist(List *list)
{
    if (list->ptr != NULL) {
        freetoken((Token*)list->ptr);
    }

    if (list->next != NULL) {
        freetokenlist(list->next);
    }
}

static void tokenize(char *md)
{
    Token *t;
    unsigned int ch = 0;

    while (md[ch]) {
        if (CHAR_IS_NEWLINE(md[ch])) {
            t = createtoken(TOKEN_TYPE_NEWLINE);
            listadd(tokens, t);
            ++ch;
        } else if (CHAR_IS_HEADER(md[ch])) {
            unsigned int n = 1;

            while(CHAR_IS_HEADER(md[++ch])) ++n;

            t = createtoken(TOKEN_TYPE_HEADER);
            t->value.i = n;

            listadd(tokens, t);

            if (CHAR_IS_SPACE(md[ch]))
                ++ch;
        } else if (CHAR_IS_ASTERISK(md[ch])) {
            unsigned int n = 1;

            while (CHAR_IS_ASTERISK(md[++ch])) ++n;

            if (n == 1) {
                t = createtoken(TOKEN_TYPE_ITALIC);
            } else if (n == 2) {
                t = createtoken(TOKEN_TYPE_BOLD);
            }

            listadd(tokens, t);
        } else if (CHAR_IS_TEXT(md[ch])) {
            char c[1];

            t = createtoken(TOKEN_TYPE_TEXT);

            while (CHAR_IS_TEXT(md[ch])) {
                c[0] = md[ch];
                t->value.s = stringcat(c, t->value.s);
                ch++;
            }

            listadd(tokens, t);
        } else {
            ++ch;
        }
    }
}

char* md2html(char *md)
{
    char *html;

    tokens = createlist();

    tokenize(md);
    printlist(tokens);

    out = createstring("");
    renderl = createlist();

    render(tokens);

    html = (char*)calloc(strlen(out) + 1, sizeof(char));
    strcat(html, out);

    freestring(out);
    freestack();
    freelist(tokens);
    freetokenlist(tokens);

    return html;
}

int main(int argc, char **argv)
{
    FILE *f;
    char *html;

    if (argc < 2) {
        printf("Usage: md2html <filename>\n");
        return -1;
    }

    f = fopen(argv[1], "r");

    if (f == NULL) {
        printf("Couldn't open file or doesn't exists.\n");
        return -1;
    }

    fread(buff, 1, 1<<25, f);
    fclose(f);

    html = md2html(buff);
    printf("%s", html);
    free(html);

    return 0;
}
