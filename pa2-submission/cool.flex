/*
*  The scanner definition for COOL.
*/

/*
*  Stuff enclosed in %{ %} in the first section is copied verbatim to the
*  output, so headers and global definitions are placed here to be visible
* to the code in the file.  Don't remove anything that was here initially
*/

%option noyywrap

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
// #include <iostream> // DEBUG
// #include <string> // DEBUG

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
        if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
                YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

/* comment_level keeps track of comment nesting */
int comment_level = 0;
%}

/*
 * Define names for regular expressions here.
 */

        /* cool-manual: 3.1.5 White Space */
WHITESPACE      [ \f\r\t\v]
        /* cool-manual: 3.1.2 Strings */
    /* had to use | because [] is for a character class and not regular expressions, which \\b is */
ESC_SEQ_STRING   \\b|\\t|\\f|\\n
OPERATORS       (?x:[\= \@ \( \) \{ \} \; \: \; \+ \- \* \/ \< \~ \, \.])
DARROW          =>

        /* start conditions */
%x STRINGS
    /* when a string is invalidated by a null character or if too long */
%x STRINGS_ERROR
    /* of the form "(* comment \n *)" */
%x COMMENTS
    /* of the form "-- comment \n" or "-- comment EOF" */
%x DASH_COMMENTS

%%
\n                       { curr_lineno++; }

        /* cool-manual: 3.1.4 
            Keywords */
    /* "Except for the constants true and false, keywords are case insensitive." */
(?i:IF)                          { return (IF); }
(?i:THEN)                        { return (THEN); }
(?i:ELSE)                        { return (ELSE); }
(?i:FI)                          { return (FI); }
(?i:INHERITS)                    { return (INHERITS); }
(?i:IN)                          { return (IN); }
(?i:CLASS)                       { return (CLASS); }
(?i:ISVOID)                      { return (ISVOID); }
(?i:LET)                         { return (LET); }
(?i:WHILE)                       { return (WHILE); }
(?i:LOOP)                        { return (LOOP); }
(?i:POOL)                        { return (POOL); }
(?i:CASE)                        { return (CASE); }
(?i:ESAC)                        { return (ESAC); }
(?i:NEW)                         { return (NEW); }
(?i:OF)                          { return (OF); }
(?i:NOT)                         { return (NOT); }
    /* "first letter of true and false must be lowercase; the trailing letters may be upper or lower case" */
t(?i:rue)                        { yylval.boolean = true; return (BOOL_CONST); }
f(?i:alse)                       { yylval.boolean = false; return (BOOL_CONST); }

        /* cool-manual: 3.1.4 
            White Space */
{WHITESPACE}                    { /* do nothing */ }

        /* cool-manual: 3.1.4 
            Integers, Identifiers, and Special Notation */
    /* "Integers are non-empty strings of digits 0-9" */
    /* pattern: 1 or more digits */
[[:digit:]]+                     { yylval.symbol = inttable.add_string(yytext, yyleng); 
                                    return (INT_CONST); }
    /* Identifiers are strings (other than keywords) consisting of letters, digits, and the underscore character. */
    /* Vince: rule is placed after keywords since: "If it finds two or more matches of the same length, the rule listed first in the flex input file is chosen"  */
    /* "Type identifiers begin with a capital letter" */
    /* pattern: lower case letter followed by any number of upper or lower case letters or digits or underscores */
[[:lower:]][[:alnum:]_]*         { yylval.symbol = idtable.add_string(yytext, yyleng); 
                                    return (OBJECTID); }
    /* object identifiers begin with a lower case letter */
    /* pattern: upper case letter followed by any number of upper or lower case letters or digits or underscores */
[[:upper:]][[:alnum:]_]*         { yylval.symbol = idtable.add_string(yytext, yyleng); 
                                    return (TYPEID); }

        /* cool-manual: Figure 1 of Chapter 3
            Operators */
    /*
    *  The multiple-character operators.
    */
{DARROW}                { return (DARROW); }
    /* NOTE: "=" is used for comparison */
"<-"                             { return (ASSIGN); }
"<="                             { return (LE); }
    /* single character operators */
{OPERATORS}                      { return yytext[0]; }

    /* string constants */
\"                             { string_buf_ptr = string_buf;
                                    BEGIN(STRINGS); }
<STRINGS>{
    /* pattern matches end quote and returns to INITAL start condition */
\"                    { *string_buf_ptr = '\0';  
                        // std::cout << string_buf_ptr - string_buf << ", " << string_buf << endl; // DEBUG
                        yylval.symbol = stringtable.add_string(string_buf, string_buf_ptr - string_buf); 
                        BEGIN(INITIAL); 
                        return (STR_CONST); }  
    /*
    *  String constants (C syntax)
    *  Escape sequence \c is accepted for all characters c. Except for 
    *  \n \t \b \f, the result is c.
    */
{ESC_SEQ_STRING} {
                        char esc_seq;
                        switch (yytext[yyleng-1]) {
                            case 'b':
                                esc_seq = '\b';
                                break;
                            case 't':
                                esc_seq = '\t';
                                break;
                            case 'f':
                                esc_seq = '\f';
                                break;
                            case 'n':
                                esc_seq = '\n';
                                break;
                            // default:
                            //     std::cout << "Error matching esc seq"; // DEBUG
                        }
                        *string_buf_ptr++ = esc_seq;
                    }
    /* If a string contains an unescaped newline, report that error as “Unterminated string constant” and resume scanning at the beginning of the next line. */
    /* push the INITIAL state since "after" the newline is immediately after this \n */
\n                  { yylval.error_msg = "Unterminated string constant"; BEGIN(INITIAL); curr_lineno++; return ERROR; }
    /* If the string contains invalid characters (i.e., the null character), report this as “String contains null character”. */    
\0                    { yylval.error_msg = "String contains null character"; BEGIN(STRINGS_ERROR); return ERROR; }
    /* add c from \c */
\\.                    { *string_buf_ptr++ = yytext[1]; }
    /* QUESTION: are all characters following a backslash an escape character? */
<<EOF>>               { yylval.error_msg = "EOF in string constant"; BEGIN(INITIAL); return ERROR; }
    /* consume all input */
.                     { // check for string length
                        if ((string_buf_ptr - string_buf + 1) == (MAX_STR_CONST)) {  // == because final index is for null terminator
                            yylval.error_msg = "String constant too long"; 
                            BEGIN(STRINGS_ERROR);
                            return ERROR;
                        }
                        *string_buf_ptr++ = yytext[0]; }
}
    /* STRINGS_ERROR is triggered when a string is invalidated */
<STRINGS_ERROR>{
    /* end of string: "after the quotation mark" */
\"          { BEGIN(INITIAL); }
    /* end of string: "the beginning of the next line if an unescaped newline occurs after these errors are encountered" */
\n          { curr_lineno++; BEGIN(INITIAL); }
    /* consume all input */
.           { /* do nothing */ }
}

        /* cool-manual: 3.1.3
            comments */
    /* dash comments are of the form "-- comment \n" or "-- comment EOF" */
--          { BEGIN(DASH_COMMENTS); }
<DASH_COMMENTS>{
\n          { curr_lineno++; BEGIN(INITIAL); }
    /* BEGIN(INITIAL) triggers default flex behavior for EOF and ends scanning */
<<EOF>>     { BEGIN(INITIAL); }
.           { /* do nothing */ }
}
    /*
    *  Nested comments
    */
    /* comments of the form "(* comment *)", can be nested */
<INITIAL,COMMENTS>\(\*     { comment_level++; BEGIN(COMMENTS); }
\*\)        { yylval.error_msg = "Unmatched *)"; return ERROR; }

<COMMENTS>{
\*\)         { comment_level--;
               if (comment_level == 0) { BEGIN(INITIAL); } }
\n           { curr_lineno++; }
    /* BEGIN(INITIAL) triggers default flex behavior for EOF and ends scanning */
<<EOF>>      { yylval.error_msg = "EOF in comment"; BEGIN(INITIAL); return ERROR; }
    /* consume all input */
.            { /* do nothing */ }
}

    /* invalid chars, i.e. those that weren't matched by the previous rules */
.                           { yylval.error_msg = &yytext[0]; return ERROR; }
%%
