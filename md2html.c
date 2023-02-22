#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHAR_IS_EOF(x)         x == 0
#define CHAR_IS_NEWLINE(x)     x == 10
#define CHAR_IS_HEADER(x)      x == 35
#define CHAR_IS_ASTERISK(x)    x == 42
#define CHAR_IS_SPACE(x)       x == 32
#define CHAR_IS_EXCLAMATION(x) x == 33
#define CHAR_IS_TEXT(x)        (x >= 32 && x <= 41) || (x >= 43 && x <= 126)

#define DEBUG_PARSER

enum MDTokenType
{
    MD_HEADER,
    MD_IMAGE,
    MD_LINK,
    MD_PARAGRAPH,
    MD_ITALIC,
    MD_BOLD,
    MD_CODE,
    MD_HTML,
    MD_TEXT,
    MD_NEWLINE
};

struct MDList
{
    void *ptr;
    struct MDList *next;
};

struct MDToken
{
    enum MDTokenType type;
    union MDTokenValue {
        int i;
        char *s;
        struct MDTokenLinkValue {
            char *alt;
            char *src;
        } link;
    } value;
};

static char* createstring(char*);
static char* createstring2(size_t);
static char* stringcat(char*, char*);
static void freestring(char*);
static struct MDList* createlist();
static void listadd(struct MDList*, void*);
static void printtoken(struct MDToken*);
static void printlist(struct MDList*);
static void freelist(struct MDList*);
static void stackpush(void*);
static void* stackpop();
static void freestack();
static void rendertag(struct MDToken*, int);
static void renderstacktop(int);
static void render(struct MDList*);
static struct MDToken* createtoken(int);
static void freetoken(struct MDToken*);
static void freetokenlist(struct MDList*);
static void tokenize(char*);
char* md2html(char*);

static struct MDList *tokens;
static struct MDList *renderl;
static struct MDList *st = NULL;

static const char* TOKEN_NAMES[] = {
    "MD_HEADER",
    "MD_IMAGE",
    "MD_LINK",
    "MD_PARAGRAPH",
    "MD_ITALIC",
    "MD_BOLD",
    "MD_CODE",
    "MD_HTML",
    "MD_TEXT",
    "MD_NEWLINE"
};
static char *headertag[] = {
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6"
};
static char buff[1<<25];
static char *out;

static char* createstring(char* str)
{
    const size_t size = sizeof(size_t) + strlen(str) +  256;
    char *head = (char*)calloc(size, sizeof(char));
    char *headstr = (char*)(head + sizeof(size_t) + 1);

    *(size_t*)head = size;
    strcpy(headstr, str);

    return headstr;
}

static char* createstring2(size_t size)
{
    const size_t nsize = sizeof(size_t) + size;
    char *head = (char*)calloc(nsize, sizeof(char));
    char *headstr = (char*)(head + sizeof(size_t) + 1);

    *(size_t*)head = size;

    return headstr;
}

static char* stringcat(char *from, char *to)
{
    void *head = (void*)(to - sizeof(size_t) - 1);
    const size_t size = *(size_t*)head;

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
    void *head = (void*)(str - sizeof(size_t) - 1);
    free(head);
}

static struct MDList* createlist()
{
    struct MDList *l = (struct MDList*)malloc(sizeof(struct MDList));
    l->ptr = NULL;
    l->next = NULL;
    return l;
}

static void listadd(struct MDList *list, void *ptr)
{
    if (list->next == NULL) {
        if (list->ptr == NULL) {
            list->ptr = ptr;
        } else {
            struct MDList *l = createlist();
            l->ptr = ptr;
            list->next = l;
        }
    } else {
        listadd(list->next, ptr);
    }
}

static void freelist(struct MDList *list)
{
    if (list->next != NULL) {
        freelist(list->next);
        free(list->next);

        list->next = NULL;
    }
}

static void printtoken(struct MDToken *t)
{
    switch (t->type) {
        case MD_HEADER:
            printf("%s { n = %i }\n", TOKEN_NAMES[t->type], t->value.i);
            break;
        case MD_IMAGE:
        case MD_LINK:
            printf("%s { alt = %s, src = %s }\n",
                    TOKEN_NAMES[t->type], t->value.link.alt, t->value.link.src);
            break;
        case MD_ITALIC:
        case MD_BOLD:
        case MD_PARAGRAPH:
        case MD_NEWLINE:
            printf("%s\n", TOKEN_NAMES[t->type]);
            break;
        case MD_TEXT:
        case MD_CODE:
        case MD_HTML:
            printf("%s { text = %s }\n", TOKEN_NAMES[t->type], t->value.s);
            break;
    }
}

static void printlist(struct MDList *list)
{
    if (list->ptr != NULL)
        printtoken((struct MDToken*)list->ptr);

    if (list->next != NULL)
        printlist(list->next);
}

static void stackpush(void *ptr)
{
    struct MDList *nst;

#ifdef DEBUG
    printf("PUSH: ");
    printtoken((struct MDToken*)ptr);
#endif

    nst = (struct MDList*)malloc(sizeof(struct MDList));
    nst->ptr = ptr;
    nst->next = st;
    st = nst;
}

static void* stackpop()
{
    struct MDList *ost;
    void *optr;

    if (st == NULL) return NULL;

    optr = st->ptr;

#ifdef DEBUG
    printf("POP: ");
    printtoken((struct MDToken*)optr);
#endif

    ost = st;
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

static void rendertag(struct MDToken *t, int close)
{
    char tag[100];
    char *tagname;

    switch (t->type) {
        case MD_HEADER:
            tagname = headertag[t->value.i - 1];
            break;
        case MD_PARAGRAPH:
            tagname = "p";
            break;
        case MD_BOLD:
            tagname = "strong";
            break;
        case MD_ITALIC:
            tagname = "em";
            break;
        case MD_CODE:
            tagname = "code";
            break;
        case MD_IMAGE:
        case MD_LINK:
        case MD_TEXT:
        case MD_NEWLINE:
        case MD_HTML:
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

static void renderlink(struct MDToken *t)
{
    char *tag = createstring("<");

    if (t->type == MD_IMAGE)
        tag = stringcat("img src=\"", tag);
    else if (t->type == MD_LINK)
        tag = stringcat("a href=\"", tag);

    tag = stringcat(t->value.link.src, tag);
    tag = stringcat("\" alt=\"", tag);
    tag = stringcat(t->value.link.alt, tag);

    if (t->type == MD_IMAGE)
        tag = stringcat("\" \\>", tag);
    else if (t->type == MD_LINK) {
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
    struct MDToken *sttoken = (struct MDToken*)stacktop();

    if (sttoken != NULL)
        rendertag(sttoken, close);
}

static void renderaddp()
{
    struct MDToken *nt = createtoken(MD_PARAGRAPH);
    stackpush(nt);
    renderstacktop(0);
}

static void rendertext(struct MDToken *t)
{
    out = stringcat(t->value.s, out);
}

static void render(struct MDList *list)
{
    struct MDToken *t, *top, *prev = NULL;
    struct MDList *next = list;
    unsigned char nl = 0;

    while (next != NULL) {
        if (next->ptr != NULL) {
            t = (struct MDToken*)next->ptr;

#ifdef DEBUG
            printtoken(t);
#endif

            switch (t->type) {
                case MD_BOLD:
                case MD_ITALIC:
                case MD_HEADER:
                    if (st != NULL) {
                        top = (struct MDToken*)stacktop();

                        if (top->type == t->type) {
                            renderstacktop(1);
                            stackpop();
                        } else {
                            if (top->type == MD_PARAGRAPH) {
                                stackpush(t);
                                renderstacktop(0);
                            }
                        }
                    } else {
                        if (t->type == MD_BOLD
                            || t->type == MD_ITALIC)
                            renderaddp();

                        stackpush(t);
                        renderstacktop(0);
                    }
                    break;
                case MD_IMAGE:
                case MD_LINK:
                    renderlink(t);
                    break;
                case MD_TEXT:
                    if (st == NULL) {
                        if (prev == NULL || prev->type != MD_HTML)
                            renderaddp();
                    }

                    rendertext(t);
                    break;
                case MD_CODE:
                    rendertag2("pre", 0);
                    rendertag(t, 0);
                    rendertext(t);
                    rendertag(t, 1);
                    rendertag2("pre", 1);
                    break;
                case MD_HTML:
                    rendertext(t);
                    break;
                case MD_NEWLINE:
                    if (st != NULL) {
                        top = (struct MDToken*)stacktop();

                        if (top->type == MD_HEADER) {
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
                case MD_PARAGRAPH:
                    break;
            }
        }

        prev = t;
        next = next->next;
    }
}

static struct MDToken* createtoken(int type)
{
    struct MDToken *t = (struct MDToken*)malloc(sizeof(struct MDToken));

    t->type = type;

    switch (t->type) {
        case MD_TEXT:
        case MD_BOLD:
        case MD_CODE:
        case MD_HTML:
            t->value.s = createstring("");
            break;
        case MD_IMAGE:
        case MD_LINK:
            t->value.link.src = createstring("");
            t->value.link.alt = createstring("");
            break;
        case MD_HEADER:
        case MD_PARAGRAPH:
        case MD_ITALIC:
        case MD_NEWLINE:
            break;
    }

    return t;
}

static void freetoken(struct MDToken *token)
{
    switch (token->type) {
        case MD_TEXT:
        case MD_BOLD:
        case MD_CODE:
        case MD_HTML:
            freestring(token->value.s);
            break;
        case MD_IMAGE:
        case MD_LINK:
            freestring(token->value.link.alt);
            freestring(token->value.link.src);
            break;
        case MD_HEADER:
        case MD_PARAGRAPH:
        case MD_ITALIC:
        case MD_NEWLINE:
            break;
    }

    free(token);
}

static void freetokenlist(struct MDList *list)
{
    if (list->ptr != NULL) {
        freetoken((struct MDToken*)list->ptr);
    }

    if (list->next != NULL) {
        freetokenlist(list->next);
    }
}

static void tokenize(char *md)
{
    struct MDToken *t;
    unsigned int ch = 0, bch;
    unsigned char valid;
    char c[1];

    while (md[ch]) {
        valid = 0;

        if (CHAR_IS_SPACE(md[ch])) {
            ++ch;
        } else if (CHAR_IS_NEWLINE(md[ch])) {
            t = createtoken(MD_NEWLINE);
            listadd(tokens, t);
            ++ch;
        } else if (CHAR_IS_HEADER(md[ch])) {
            unsigned int n = 1;

            while(CHAR_IS_HEADER(md[++ch])) ++n;

            t = createtoken(MD_HEADER);
            t->value.i = n;

            listadd(tokens, t);

            if (CHAR_IS_SPACE(md[ch]))
                ++ch;
        } else if (md[ch] == '<') {
            bch = ch;
            t = createtoken(MD_HTML);

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
            t = createtoken(MD_CODE);

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
            t = createtoken(MD_LINK);
            bch = ch;
            goto createlink;
        } else if (CHAR_IS_EXCLAMATION(md[ch])) {
            if (md[++ch] == '[') {
                t = createtoken(MD_IMAGE);

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
                t = createtoken(MD_ITALIC);
            } else if (n == 2) {
                t = createtoken(MD_BOLD);
            }

            listadd(tokens, t);
        } else if (CHAR_IS_TEXT(md[ch])) {
createtext:
            t = createtoken(MD_TEXT);

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

#ifdef DEBUG_PARSER
    printlist(tokens);
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
