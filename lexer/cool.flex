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
static int NUM_COMMENT_CNTR = 0;
static int STRING_BUF_IDX = 0;
extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

void remove_escape_chars(char* buf);

/*
 *  Add Your own definitions here
 */

%}

%x multilinecomment
%x singlelinecomment
%x stringconst
%x recoverystringerror

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
INHERITS_KYWRD [iI][nN][hH][eE][rR][iI][tT][sS]
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

LET_KYWRD [lL][eE][tT]

LEQ	<=
ASSIGN_KYWRD <-
BEGIN_ML_COMMENT \(\*
END_ML_COMMENT \*\)

SL_COMMENT_KYWRD \-\-

INVALID_CHARS .|\n

%%

 /*
  *  Nested comments
  */

{BEGIN_ML_COMMENT}	{
				BEGIN(multilinecomment);
				NUM_COMMENT_CNTR++;
			}

\n                      		++curr_lineno;
<multilinecomment>\n             	++curr_lineno;

<multilinecomment>{BEGIN_ML_COMMENT}    {
                                		NUM_COMMENT_CNTR++;
                        		}


<multilinecomment>{END_ML_COMMENT}	{
						NUM_COMMENT_CNTR--;
						if(NUM_COMMENT_CNTR == 0)
							BEGIN(0);
					}
<multilinecomment>.

{SL_COMMENT_KYWRD}	BEGIN(singlelinecomment);



<singlelinecomment><<EOF>>	
<singlelinecomment>\n	{
				BEGIN(0);
				++curr_lineno;
			}
<singlelinecomment>.

[ \f\r\t\v]+

{LEQ}	return LE; 
{ASSIGN_KYWRD} return ASSIGN;

\=	return '=';
\+	return '+';
\*	return '*';
\-	return '-';
\;	return ';';
\,	return ',';
\~	return '~';
\<	return '<';
\(	return '(';
\)	return ')';
\/	return '/';
\{	return '{';
\}	return '}';
\:	return ':';
\.	return '.';
\@	return '@';

{CLASS_KYWRD}	{
			return CLASS;
		}

{ELSE_KYWRD}	{
			return ELSE;
		}

{FI_KYWRD}	{
			return FI;
		}

{IF_KYWRD}	{
			return IF;
		}

{INHERITS_KYWRD}	{
			return INHERITS;
		}

{IN_KYWRD}	{
			return IN;
		}

{ISVOID_KYWRD}	{
			return ISVOID;
		}

{LOOP_KYWRD}	{
			return LOOP;
		}

{POOL_KYWRD}	{
			return POOL;
		}

{THEN_KYWRD}	{
			return THEN;
		}

{WHILE_KYWRD}	{
			return WHILE;
		}

{CASE_KYWRD}	{
			return CASE;
		}

{ESAC_KYWRD}	{
			return ESAC;
		}

{NEW_KYWRD}	{
			return NEW;
		}

{OF_KYWRD}	{
			return OF;
		}

{NOT_KYWRD}	{
			return NOT;
		}

{FALSE_KYWRD}	{
			cool_yylval.boolean = false;
			return BOOL_CONST;
		}

{TRUE_KYWRD}	{
			cool_yylval.boolean = true;
			return BOOL_CONST;
		}

{LET_KYWRD}	{
			return LET;
		}

{DIGIT}+	{
			cool_yylval.symbol = inttable.add_string(yytext);
			return INT_CONST;
		}

{LWR_ALPH}[A-Za-z0-9\_]*	{
				cool_yylval.symbol = idtable.add_string(yytext);
				return OBJECTID;
			}

{UPR_ALPH}[A-Za-z0-9\_]*	{
				cool_yylval.symbol = idtable.add_string(yytext);
				return TYPEID;	
			}

{END_ML_COMMENT}		{
				cool_yylval.error_msg = "Unmatched *)";
				return ERROR;
			}

\"			{
				BEGIN(stringconst);
				STRING_BUF_IDX = 0;
				memset(string_buf, 0, MAX_STR_CONST);
			}

<stringconst>\\\n	{
                                if (STRING_BUF_IDX+1 > (MAX_STR_CONST-1) )
                                {
                                        cool_yylval.error_msg = "String constant too long";
                                        return ERROR;
                                }
                                string_buf[STRING_BUF_IDX] = yytext[0];
				string_buf[STRING_BUF_IDX+1] = yytext[1];
                       		string_buf[STRING_BUF_IDX+2] = '\0';
				STRING_BUF_IDX++;
				++curr_lineno;
			 }	

<stringconst>\\\"|\\\\	{
                          	// escaped backslashes
			        if (STRING_BUF_IDX+1 > (MAX_STR_CONST-1) )
                                {
                                        cool_yylval.error_msg = "String constant too long";
                                        return ERROR;
                                }
                                string_buf[STRING_BUF_IDX] = yytext[0];
                                string_buf[STRING_BUF_IDX+1] = yytext[1];
                                string_buf[STRING_BUF_IDX+2] = '\0';
                                STRING_BUF_IDX += 2;
                         }


<stringconst>[^"\0\n]	{
				if (STRING_BUF_IDX+1 > (MAX_STR_CONST-1) ) 
				{
					cool_yylval.error_msg = "String constant too long";
					return ERROR;
				}
				string_buf[STRING_BUF_IDX] = yytext[0];
				string_buf[STRING_BUF_IDX+1] = '\0';
				STRING_BUF_IDX++;
			}
	


<stringconst>\n		{
				BEGIN(recoverystringerror);
			}

<stringconst>\"		{
				BEGIN(0);
				if( STRING_BUF_IDX > 0)
					remove_escape_chars(string_buf);
				cool_yylval.symbol = stringtable.add_string(string_buf);
				return STR_CONST;
			}

<stringconst><<EOF>>	{
				cool_yylval.error_msg = "EOF in string constant";
				yyterminate();
				return ERROR;
			}

<recoverystringerror>\n	{
				cool_yylval.error_msg = "Unterminated string constant";
				BEGIN(0);
        return ERROR;		
			}

<recoverystringerror>.		

<multilinecomment><<EOF>>	{
					cool_yylval.error_msg = "EOF in comment";
					yyterminate();
					return ERROR;
				}

<stringconst>\0		{
				cool_yylval.error_msg = "String contains null character";
				BEGIN(recoverystringerror);
				return ERROR;
			}


 {INVALID_CHARS}  { 
  cool_yylval.error_msg = yytext;
  return ERROR;
  }

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }


%%

bool verify_last_char_replaced(int i,char *buf)
{
	return i==strlen(buf)-1;
}

void remove_escape_chars(char* buf)
{
int i;
bool last_char_replaced = false;
int offset = 0;
for (i = 0; i < strlen(buf)-1; i++){
	if ( buf[i] == '\\' ){
		if ( buf[i+1] == 'n' ){
			buf[i-offset] = 0x0A;
			offset++;
			i++;
			last_char_replaced = verify_last_char_replaced(i,buf);
		}else if ( buf[i+1] == 't' ){
			buf[i-offset] = 0x09;
			offset++;
			i++;
			last_char_replaced = verify_last_char_replaced(i,buf);
		}else if ( buf[i+1] == 'b' ){
			buf[i-offset] = 0x08;
			offset++;
			i++;
      last_char_replaced = verify_last_char_replaced(i,buf);
		}else if ( buf[i+1] == 'f' ){
			buf[i-offset] = 0x0C;
			offset++;
			i++;
			last_char_replaced = verify_last_char_replaced(i,buf);
		}else if ( buf[i+1] == '0' ){
			buf[i-offset] = '0';
			offset++;
			i++;
			last_char_replaced = verify_last_char_replaced(i,buf);
		}else{ 
		     buf[i-offset] = buf[i];
		}
	
	}else{
		buf[i-offset] = buf[i];
	}
}
if(!last_char_replaced) {
	i = strlen(buf)-1;
	buf[i-offset] = buf[i];
}
  buf[strlen(buf)-offset] = '\0';
}


