/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

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

%}

%x comment
%x stringconst

/*
 * Define names for regular expressions here.
 */

DARROW          =>
DIGIT [0-9]
LWR_ALPH [a-z]
UPR_ALPH [A-Z]
CLASS_KYWRD [cC][lL][aA][sS][sS]
ELSE_KYWRD [eE][lL][sS][eE]
FI_KYWRD [fF][iI]
IF_KYWRD [iI][fF]
IN_KYWRD [iI][nN]
INHERITS_KYWRD [iI][nN][hH][iI][rR][iI][tT][sS]
ISVOID_KYWRD [iI][sS][vV][oO][iI][dD]
LOOP_KYWRD [lL][oO][oO][pP]
POOL_KYWRD [pP][oO][oO][lL]
THEN_KYWRD [tT][hH][eE][nN]
WHILE_KYWRD [wW][hH][iI][lL][eE]
CASE_KYWRD [cC][aA][sS][eE]
ESAC_KYWRD [eE][sS][aA][cC]
NEW_KYWRD [nN][eE][wW]
OF_KYWRD [oO][fF]
NOT_KYWRD [nN][oO][tT]

FALSE_KYWRD f[aA][lL][sS][eE]
TRUE_KYWRD t[rR][uU][eE]

%%

 /*
  *  Nested comments
  */


{CLASS_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return CLASS;
		}

{ELSE_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return ELSE;
		}

{FI_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return FI;
		}

{IF_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return IF;
		}

{INHERITS_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return INHERITS;
		}

{IN_KYWRD}	{
				printf("keyword: %s\n", yytext);
				return IN;
			}

{ISVOID_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return ISVOID;
		}

{LOOP_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return LOOP;
		}

{POOL_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return POOL;
		}

{THEN_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return THEN;
		}

{WHILE_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return WHILE;
		}

{CASE_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return CASE;
		}

{ESAC_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return ESAC;
		}

{NEW_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return NEW;
		}

{OF_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return OF;
		}

{NOT_KYWRD}	{
			printf("keyword: %s\n", yytext);
			return NOT;
		}

{FALSE_KYWRD}	{
			printf("keyword: %s\n", yytext);
			cool_yylval.boolean = false;
			return BOOL_CONST;
		}

{TRUE_KYWRD}	{
			printf("keyword: %s\n", yytext);
			cool_yylval.boolean = true;
			return BOOL_CONST;
		}

{DIGIT}+	{
			printf("integer %s\n", yytext);
			cool_yylval.symbol = inttable.add_string(yytext);
			return INT_CONST;
		}

{LWR_ALPH}[A-Za-z\_]*	{
				printf("keyword %s\n", yytext);
				return OBJECTID;
			}

{UPR_ALPH}[A-Za-z\_]*	{
				return TYPEID;	
			}


"("+"*"			BEGIN(comment);
\n			++curr_lineno;
<comment>"*"+")"	BEGIN(0);

\"			BEGIN(stringconst);
<stringconst>[^"\0]*	printf("in string const %s\n", yytext);
<stringconst>\"		{
				BEGIN(0);
				cool_yylval.symbol = stringtable.add_string(yytext);
			}
<stringconst><<EOF>>	{
				cool_yylval.error_msg = "EOF in string constant";
				yyterminate();
				return ERROR;
			}

<comment><<EOF>>	{
				cool_yylval.error_msg = "EOF in comment";
				yyterminate();
				return ERROR;
			}

<stringconst>\0		{
				cool_yylval.error_msg = "String contains null character";
				printf("string contains null character\n");
				yyterminate();
				return ERROR;
}


 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
