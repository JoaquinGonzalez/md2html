#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_STRING_LEN 256

#define CHAR_IS_EOF(x)         x == 0
#define CHAR_IS_NEWLINE(x)     x == 10
#define CHAR_IS_HEADER(x)      x == 35
#define CHAR_IS_STAR(x)        x == 42
#define CHAR_IS_SPACE(x)       x == 32
#define CHAR_IS_TEXT(x)        (x >= 33 && x <= 126)

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

struct MDString
{
    char *str;
    size_t size;
};

struct MDToken
{
    enum MDTokenType type;
    union MDTokenValue {
        int i;
        struct MDString *s;
        struct MDTokenLinkValue {
            struct MDString *alt;
            struct MDString *src;
        } link;
    } value;
};

struct MDRenderer
{
    struct MDList *renderl;
    struct MDList *st;
    struct MDString *out;
};

struct MDTokenizer
{
    char *source;
    unsigned int index;
    unsigned int save;
    struct MDList *tokens;
};

static struct MDString* createstring(char*, size_t);
static struct MDString* stringcat(char*, struct MDString*);
static void freestring(struct MDString*);
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
static void appendout(char *str);
static void render(struct MDList*);
static struct MDToken* createtoken(int);
static void freetoken(struct MDToken*);
static void freetokenlist(struct MDList*);
static void backtrack();
static void save();
static char currentchr();
static char advancechr();
static void tokenize(char*);
char* md2html(char*);

static struct MDTokenizer tokenizer;
static struct MDRenderer renderer;

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

static struct MDString* createstring(char* str, size_t len)
{
    struct MDString *s = (struct MDString*)malloc(sizeof(struct MDString));
    s->size = strlen(str) + len;
    s->str = (char*)calloc(s->size, sizeof(char));

    strcat(s->str, str);

    return s;
}

static struct MDString* stringcat(char *from, struct MDString *to)
{
    size_t flen = strlen(from);
    size_t tlen = strlen(to->str);
    struct MDString *ns;

    if (flen + tlen + 1 >= to->size) {
        ns = createstring("", flen + tlen + DEFAULT_STRING_LEN);
        strcat(ns->str, to->str);
        strcat(ns->str, from);
        freestring(to);
        return ns;
    }

    strcat(to->str, from);

    return to;
}

static void freestring(struct MDString *s)
{
    free(s->str);
    free(s);
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
    }
    free(list);
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
                    TOKEN_NAMES[t->type], t->value.link.alt->str, t->value.link.src->str);
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
            printf("%s { text = %s }\n", TOKEN_NAMES[t->type], t->value.s->str);
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
    nst->next = renderer.st;
    renderer.st = nst;
}

static void* stackpop()
{
    struct MDList *ost;
    void *optr;

    if (renderer.st == NULL) return NULL;

    optr = renderer.st->ptr;

#ifdef DEBUG
    printf("POP: ");
    printtoken((struct MDToken*)optr);
#endif

    ost = renderer.st;
    renderer.st = ost->next;
    free(ost);

    return optr;
}

static void* stacktop()
{
    if (renderer.st == NULL) return NULL;
    return renderer.st->ptr;
}

static void freestack()
{
    while (renderer.st != NULL) stackpop();
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

    appendout(tag);
}

static void rendertag2(char *tagname, int close)
{
    char tag[100];

    if (close == 0)
        sprintf(tag, "<%s>", tagname);
    else
        sprintf(tag, "</%s>", tagname);

    appendout(tag);
}

static void renderlink(struct MDToken *t)
{
    struct MDString *tag = createstring("<", DEFAULT_STRING_LEN);

    if (t->type == MD_IMAGE)
        tag = stringcat("img src=\"", tag);
    else if (t->type == MD_LINK)
        tag = stringcat("a href=\"", tag);

    tag = stringcat(t->value.link.src->str, tag);
    tag = stringcat("\" alt=\"", tag);
    tag = stringcat(t->value.link.alt->str, tag);

    if (t->type == MD_IMAGE)
        tag = stringcat("\" \\>", tag);
    else if (t->type == MD_LINK) {
        tag = stringcat("\">", tag);

        if (strlen(t->value.link.alt->str) == 0)
            tag = stringcat(t->value.link.src->str, tag);
        else
            tag = stringcat(t->value.link.alt->str, tag);

        tag = stringcat("</a>", tag);
    }

    appendout(tag->str);
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
    appendout(t->value.s->str);
}

static void appendout(char *str)
{
    renderer.out = stringcat(str, renderer.out);
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
                    if (renderer.st != NULL) {
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
                    if (renderer.st == NULL) {
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
                    if (renderer.st != NULL) {
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

                        appendout("\n");
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
            t->value.s = createstring("", DEFAULT_STRING_LEN);
            break;
        case MD_IMAGE:
        case MD_LINK:
            t->value.link.src = createstring("", DEFAULT_STRING_LEN);
            t->value.link.alt = createstring("", DEFAULT_STRING_LEN);
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
        list->ptr = NULL;
    }

    if (list->next != NULL) {
        freetokenlist(list->next);
    }
}

static int parsetext()
{
    struct MDToken *t = createtoken(MD_TEXT);
    char c[1];

    if (!CHAR_IS_TEXT(currentchr())) {
        freetoken(t);
        return 0;
    }

    while (CHAR_IS_TEXT(currentchr()) || CHAR_IS_SPACE(currentchr())) {
        c[0] = currentchr();
        t->value.s = stringcat(c, t->value.s);
        advancechr();
    }

    listadd(tokenizer.tokens, t);
    return 1;
}

static void parsestar()
{
    struct MDToken *t;
    unsigned int n = 1;

    while (CHAR_IS_STAR(advancechr()) || n >= 2) ++n;

    if (n == 1)
        t = createtoken(MD_ITALIC);
    else if (n == 2)
        t = createtoken(MD_BOLD);

    listadd(tokenizer.tokens, t);
}

static void parselink(enum MDTokenType type)
{
    struct MDToken *t = createtoken(type);
    char c[1];
    unsigned int valid = 0;

    save();
    while (advancechr() != ']' && !CHAR_IS_EOF(currentchr())) {
        c[0] = currentchr();
        t->value.link.alt = stringcat(c, t->value.link.alt);
    }

    if (advancechr() == '(') {
        while (advancechr() != ')' && !CHAR_IS_EOF(currentchr())) {
            c[0] = currentchr();
            t->value.link.src = stringcat(c, t->value.link.src);
        }

        if (currentchr() == ')')
            valid = 1;
    }

    if (valid) {
        listadd(tokenizer.tokens, t);
        advancechr();
    } else {
        freetoken(t);
        backtrack();
        parsetext();
    }
}

static void parsecode()
{
    struct MDToken *t = createtoken(MD_CODE);
    char c[1];

    save();
    if (advancechr() == '`' && advancechr() == '`') {
        while (advancechr() != '`' && !CHAR_IS_EOF(currentchr())) {
            c[0] = currentchr();
            t->value.s = stringcat(c, t->value.s);
        }

        if (currentchr() == '`' && advancechr() == '`' && advancechr() == '`') {
            listadd(tokenizer.tokens, t);
            advancechr();
        } else {
            freetoken(t);
            backtrack();
            parsetext();
        }
    }
}

static void parsehtml()
{
    struct MDToken *t = createtoken(MD_HTML);
    char c[1];

    save();
    while (currentchr() != '>' && !CHAR_IS_EOF(currentchr())) {
        c[0] = currentchr();
        t->value.s = stringcat(c, t->value.s);
        advancechr();
    }

    if (currentchr() != '>') {
        backtrack();
        freetoken(t);
        parsetext();
    } else {
        c[0] = currentchr();
        t->value.s = stringcat(c, t->value.s);
        listadd(tokenizer.tokens, t);
        advancechr();
    }
}

static void parseheader()
{
    struct MDToken *t = createtoken(MD_HEADER);
    t->value.i = 1;

    while (CHAR_IS_HEADER(advancechr())) t->value.i++;

    listadd(tokenizer.tokens, t);
}

static void backtrack()
{
    tokenizer.index = tokenizer.save;
}

static void save()
{
    tokenizer.save = tokenizer.index;
}

static char currentchr()
{
    return tokenizer.source[tokenizer.index];
}

static char advancechr()
{
    return tokenizer.source[++tokenizer.index];
}

static void tokenize(char *md)
{
    struct MDToken *t;

    tokenizer.index = 0;
    tokenizer.source = md;
    tokenizer.tokens = createlist();

    while (currentchr()) {
        switch (currentchr()) {
            case ' ':
                advancechr();
                break;
            case '\n':
                t = createtoken(MD_NEWLINE);
                listadd(tokenizer.tokens, t);
                advancechr();
                break;
            case '#':
                parseheader();
                break;
            case '<':
                parsehtml();
                break;
            case '`':
                parsecode();
                break;
            case '!':
                save();
                if (advancechr() != '[') {
                    backtrack();
                    parsetext();
                } else {
                    parselink(MD_IMAGE);
                }
                break;
            case '[':
                parselink(MD_LINK);
                break;
            case '*':
                parsestar();
                break;
            default:
                if (!parsetext())
                    advancechr();
                break;
        }
    }
}

char* md2html(char *md)
{
    char *html;

    tokenize(md);

#ifdef DEBUG_PARSER
    printlist(tokenizer.tokens);
#endif

    renderer.out = createstring("", DEFAULT_STRING_LEN);
    renderer.st = NULL;

    render(tokenizer.tokens);

    html = (char*)calloc(strlen(renderer.out->str) + 1, sizeof(char));
    strcat(html, renderer.out->str);

    freestring(renderer.out);
    freestack();
    free(renderer.st);
    freetokenlist(tokenizer.tokens);
    freelist(tokenizer.tokens);

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
