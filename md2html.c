#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHAR_IS_EOF(x)         x == 0
#define CHAR_IS_NEWLINE(x)     x == 10
#define CHAR_IS_HEADER(x)      x == 35
#define CHAR_IS_ASTERISK(x)    x == 42
#define CHAR_IS_SPACE(x)       x == 32
#define CHAR_IS_EXCLAMATION(x) x == 33
#define CHAR_IS_TEXT(x)        x >= 32 && x <= 41 || x >= 43 && x <= 126

enum
{
    TOKEN_TYPE_HEADER,
    TOKEN_TYPE_IMAGE,
    TOKEN_TYPE_LINK,
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
        } link;
    } value;
} Token;

static char* itoa(int, int);
static char* createstring(char*);
static char* createstring2(int);
static char* stringcat(char*, char*);
static void freestring(char*);
static List* createlist();
static void listadd(List*, void*);
static void printtoken(Token*);
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

static void printtoken(Token *t)
{
    switch (t->type) {
        case TOKEN_TYPE_HEADER:
            printf("TOKEN_TYPE_HEADER { n = %i }\n", t->value.i);
            break;
        case TOKEN_TYPE_IMAGE:
            printf("TOKEN_TYHPE_IMAGE { alt = %s, src = %s }\n",
                    t->value.link.alt, t->value.link.src);
            break;
        case TOKEN_TYPE_LINK:
            printf("TOKEN_TYHPE_LINK { alt = %s, src = %s }\n",
                    t->value.link.alt, t->value.link.src);
            break;
        case TOKEN_TYPE_ITALIC:
            printf("TOKEN_TYPE_ITALIC\n");
            break;
        case TOKEN_TYPE_BOLD:
            printf("TOKEN_TYPE_BOLD\n");
            break;
        case TOKEN_TYPE_PARAGRAPH:
            printf("TOKEN_TYPE_PARAGRAPH\n");
            break;
        case TOKEN_TYPE_TEXT:
            printf("TOKEN_TYPE_TEXT { text = %s }\n", t->value.s);
            break;
        case TOKEN_TYPE_CODE:
            printf("TOKEN_TYPE_CODE { code = %s }\n", t->value.s);
            break;
        case TOKEN_TYPE_HTML:
            printf("TOKEN_TYPE_HTML { html = %s }\n", t->value.s);
            break;
        case TOKEN_TYPE_NEWLINE:
            printf("TOKEN_TYPE_NEWLINE\n");
            break;
    }
}

static void printlist(List *list)
{
    Token *t;

    if (list->ptr != NULL)
        printtoken((Token*)list->ptr);

    if (list->next != NULL)
        printlist(list->next);
}

static void stackpush(void *ptr)
{
    List *nst;

#ifdef DEBUG
    printf("PUSH: ");
    printtoken((Token*)ptr);
#endif

    nst = (List*)malloc(sizeof(List));
    nst->ptr = ptr;
    nst->next = st;
    st = nst;
}

static void* stackpop()
{
    if (st == NULL) return NULL;

    void *optr = st->ptr;

#ifdef DEBUG
    printf("POP: ");
    printtoken((Token*)optr);
#endif
    
    List *ost = st;
    st = ost->next;
    free(ost);

    return optr;
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
        case TOKEN_TYPE_CODE:
            tagname = "code";
            break;
    }

    if (close == 0)
        sprintf(tag, "<%s>", tagname);
    else
        sprintf(tag, "</%s>", tagname);

    out = stringcat(tag, out);
}

static void rendertag2(char *tagname, int close)
{
    char tag[100];

    if (close == 0)
        sprintf(tag, "<%s>", tagname);
    else
        sprintf(tag, "</%s>", tagname);

    out = stringcat(tag, out);
}

static void renderlink(Token *t)
{
    char *tag = createstring("<");

    if (t->type == TOKEN_TYPE_IMAGE)
        tag = stringcat("img src=\"", tag);
    else if (t->type == TOKEN_TYPE_LINK)
        tag = stringcat("a href=\"", tag);

    tag = stringcat(t->value.link.src, tag);
    tag = stringcat("\" alt=\"", tag);
    tag = stringcat(t->value.link.alt, tag);

    if (t->type == TOKEN_TYPE_IMAGE)
        tag = stringcat("\" \\>", tag);
    else if (t->type == TOKEN_TYPE_LINK) {
        tag = stringcat("\">", tag);

        if (strlen(t->value.link.alt) == 0)
            tag = stringcat(t->value.link.src, tag);
        else
            tag = stringcat(t->value.link.alt, tag);

        tag = stringcat("</a>", tag);
    }

    out = stringcat(tag, out);
    freestring(tag);
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

static void renderaddp()
{
    Token *nt = createtoken(TOKEN_TYPE_PARAGRAPH);
    stackpush(nt);
    renderstacktop(0);
}

static void rendertext(Token *t)
{
    out = stringcat(t->value.s, out);
}

static void render(List *list)
{
    Token *t, *top, *prev = NULL;
    List *next = list;
    unsigned char nl = 0;

    while (next != NULL) {
        if (next->ptr != NULL) {
            t = (Token*)next->ptr;

#ifdef DEBUG
            printtoken(t);
#endif

            switch (t->type) {
                case TOKEN_TYPE_BOLD:
                case TOKEN_TYPE_ITALIC:
                case TOKEN_TYPE_HEADER:
                    if (st != NULL) {
                        top = (Token*)stacktop();

                        if (top->type == t->type) {
                            renderstacktop(1);
                            stackpop();
                        } else {
                            if (top->type == TOKEN_TYPE_PARAGRAPH) {
                                stackpush(t);
                                renderstacktop(0);
                            }
                        }
                    } else {
                        if (t->type == TOKEN_TYPE_BOLD
                            || t->type == TOKEN_TYPE_ITALIC)
                            renderaddp();

                        stackpush(t);
                        renderstacktop(0);
                    }
                    break;
                case TOKEN_TYPE_IMAGE:
                case TOKEN_TYPE_LINK:
                    renderlink(t);
                    break;
                case TOKEN_TYPE_TEXT:
                    if (st == NULL) {
                        if (prev == NULL || prev->type != TOKEN_TYPE_HTML)
                            renderaddp();
                    }

                    rendertext(t);
                    break;
                case TOKEN_TYPE_CODE:
                    rendertag2("pre", 0);
                    rendertag(t, 0);
                    rendertext(t);
                    rendertag(t, 1);
                    rendertag2("pre", 1);
                    break;
                case TOKEN_TYPE_HTML:
                    rendertext(t);
                    break;
                case TOKEN_TYPE_NEWLINE:
                    if (st != NULL) {
                        top = (Token*)stacktop();

                        if (top->type == TOKEN_TYPE_HEADER) {
                            renderstacktop(1);
                            stackpop();
                        }

                        if (nl == 2 || next->next == NULL) {
                            renderstacktop(1);
                            stackpop();
                            nl = 0;
                        }

                        out = stringcat("\n", out);
                        ++nl;
                    }
                    break;
            }
        }

        prev = t;
        next = next->next;
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
        case TOKEN_TYPE_LINK:
            t->value.link.src = createstring("");
            t->value.link.alt = createstring("");
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
        case TOKEN_TYPE_LINK:
            freestring(token->value.link.alt);
            freestring(token->value.link.src);
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
    unsigned int ch = 0, bch;
    unsigned char valid;
    char c[1];

    while (md[ch]) {
        valid = 0;

        if (CHAR_IS_SPACE(md[ch])) {
            ++ch;
        } else if (CHAR_IS_NEWLINE(md[ch])) {
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
        } else if (md[ch] == '<') {
            bch = ch;
            t = createtoken(TOKEN_TYPE_HTML);

            while (md[ch] != '>' && !CHAR_IS_EOF(md[ch])) {
                c[0] = md[ch];
                stringcat(c, t->value.s);
                ++ch;
            }

            if (md[ch] != '>') {
                ch = bch;
                goto createtext;
            } else {
                c[0] = md[ch];
                stringcat(c, t->value.s);
                listadd(tokens, t);
                ++ch;
            }
        } else if (md[ch] == '`') {
            bch = ch;
            t = createtoken(TOKEN_TYPE_CODE);

            if (md[++ch] == '`' && md[++ch] == '`') {
                while (md[++ch] != '`' && !CHAR_IS_EOF(md[ch])) {
                    c[0] = md[ch];
                    stringcat(c, t->value.s);
                }

                if (md[ch++] == '`' && md[ch++] == '`' && md[ch++] == '`')
                    valid = 1;
            }

            if (!valid) {
                freetoken(t);
                ch = bch;
                goto createtext;
            } else {
                listadd(tokens, t);
            }
        } else if (md[ch] == '[') {
            t = createtoken(TOKEN_TYPE_LINK);
            bch = ch;
            goto createlink;
        } else if (CHAR_IS_EXCLAMATION(md[ch])) {
            if (md[++ch] == '[') {
                t = createtoken(TOKEN_TYPE_IMAGE);

createlink:
                while (md[++ch] != ']' && !CHAR_IS_EOF(md[ch])) {
                    c[0] = md[ch];
                    stringcat(c, t->value.link.alt);
                }

                if (md[++ch] == '(') {
                    while (md[++ch] != ')' && !CHAR_IS_EOF(md[ch])) {
                        c[0] = md[ch];
                        stringcat(c, t->value.link.src);
                    }

                    if (md[ch++] == ')')
                        valid = 1;
                }

                if (!valid)
                    freetoken(t);
            }

            if (!valid) {
                ch = bch;
                goto createtext;
            } else {
                listadd(tokens, t);
            }
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
createtext:
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

#ifdef DEBUG
    /*printlist(tokens);*/
#endif

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
