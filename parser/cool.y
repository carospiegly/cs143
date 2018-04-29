/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
   /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    %type <features> feature_list
    %type <features> empty_feature_list
    %type <feature> feature
    %type <formals> formal_list
    %type <formal> formal
    %type <expressions> comma_expr_list
    %type <expressions> semicolon_expr_list
    %type <expression> expr
    %type <case_> branch
    %type <cases> case_list
    %type <expression> let_chunk_list

    /* Precedence declarations go here. */
    %right IN
    %right ASSIGN
    %precedence NOT
    %nonassoc '<' '=' LE 
    %left '+' '-'
    %left '*' '/'
    %precedence ISVOID
    %precedence '~'
    %precedence '@'
    %precedence '.'
    
    %%

    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program : class_list 
    { @$ = @1; ast_root = program($1); }
    ;



    
    class_list
    : error ';'
    {
    	$$ = nil_Classes();
        printf("class list with no classes\n");
	parse_results = $$;}
    |
    class    /* single class */
    { 	printf("class list, with 1 class\n"); 
	$$ = single_Classes($1);
    	parse_results = $$; }
    | class_list class  /* several classes */
    { 	printf("class list with multiple classes, recurses\n"); 
	$$ = append_Classes($1,single_Classes($2)); 
    	parse_results = $$; }
    | class_list class error ';'   /* several classes */
    { printf(" class list class ERROR; \n"); yyerrok; }
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class : 
    CLASS TYPEID '{' empty_feature_list '}' ';'
    { 	printf("CLASS TYPEID { empty feature list }\n"); 
	$$ = class_($2,idtable.add_string("Object"), $4,
    	stringtable.add_string(curr_filename)); }



    | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
    { 	printf("CLASS TYPEID INHERITS TYPEID { feature list };\n"); 
	$$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }


    | CLASS TYPEID '{' feature_list '}' ';'
    { 	printf("CLASS TYPEID { feature list }; \n"); 
	$$ = class_($2,idtable.add_string("Object"),$4,
    	stringtable.add_string(curr_filename)); }



    | CLASS TYPEID INHERITS TYPEID '{' empty_feature_list '}' ';'
    { 	printf("CLASS TYPEID INHERITS TYPEID { empty feature list };\n"); 
	$$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
    ;
    

    /* Feature list may be empty, but no empty features in list. */
    feature_list: feature /* single feature */
    { printf("1 in feature list\n"); $$ = single_Features($1); } 
    | feature_list feature /* several features */
    { printf("multiple in feature list\n"); $$ = append_Features($1, single_Features($2));}
    ;

    empty_feature_list:
    { printf("empty feature list\n"); nil_Features(); }

    feature: OBJECTID ':' TYPEID ';'
    { printf("vanilla feature\n"); $$ = attr($1, $3, no_expr());}
    | OBJECTID ':' TYPEID ASSIGN expr ';'
    { printf("FEATURE iwth NO formal list\n"); $$ = attr($1, $3, $5);}
    | OBJECTID '(' formal_list ')' ':'TYPEID '{' expr '}' ';'
    { printf("FEATURE iwth formal list\n"); $$ = method($1, $3, $6, $8);}
    ;

    formal_list: /* empty */
    { printf("empty formal list\n"); $$ = nil_Formals(); }
    | formal /* single formal */
    { printf("formal list with 1 thing\n"); $$ = single_Formals($1); } 
    | formal_list ',' formal /* several formals */
    { printf("formal list with multiple things\n"); $$ = append_Formals($1, single_Formals($3));}
    ;

    formal: OBJECTID ':' TYPEID
    { printf("formal, OBJID : TYPEID\n"); $$ = formal($1, $3);}
    ;

    comma_expr_list: /* empty */
    { printf("comma expr list, with 0 exprs \n"); $$ = nil_Expressions(); }
    | expr /* single expression */
    { printf("comma expr list, that is 1 expr \n"); $$ = single_Expressions($1); } 
    | comma_expr_list ',' expr 
    { printf("comma expr list, expr \n"); $$ = append_Expressions($1, single_Expressions($3));}
    ;

    semicolon_expr_list: /* empty */
    { printf("semicolon expr list, with no expressions inside\n"); $$ = nil_Expressions(); }
    | expr /* single expression */
    { printf("semicolon expr list,  1 expr\n"); $$ = single_Expressions($1); } 
    | semicolon_expr_list ';' expr 
    { printf(" semicolon expr list; expr \n"); $$ = append_Expressions($1, single_Expressions($3));}
    ;
   
    branch: OBJECTID ':' TYPEID DARROW expr ';'
    { printf("OBJ : TYPEID >= expr ; \n"); branch( $1, $3, $5); }
    ;
    case_list: branch 
    { printf(" case list with 1 case\n"); $$ = single_Cases($1); } 
    | case_list ';' branch
    { printf(" case list with multiple cases\n"); $$ = append_Cases($1, single_Cases($3));}
    ;
    
    

    let_chunk_list: OBJECTID ':' TYPEID let_chunk_list
    { printf(" let chunk list with right recursion, OBJID : TYPEID lcl \n"); let($1, $3, no_expr(), $4);}
    | OBJECTID ':' TYPEID ASSIGN expr let_chunk_list
    { printf(" let chunk list with right recursion, OBJID : TYPEID <- expr lcl \n"); let($1, $3, $5, $6);}
    | OBJECTID ':' TYPEID IN expr
    { printf("OBJID : TYPEID in expr \n"); let($1, $3, no_expr(), $5);}
    | OBJECTID ':' TYPEID ASSIGN expr IN expr
    { printf("OBJID : TYPEID <- expr in expr \n"); let($1, $3, $5, $7);}
    ;
    

    expr: OBJECTID ASSIGN expr
    { printf("in vanilla,  OBJECTID <- expr\n"); $$ = assign($1, $3); }
    | expr '.' OBJECTID '(' comma_expr_list ')' 
    { printf("dispath, expr.OBJECTID ( comma expr list )\n"); $$ = dispatch($1, $3, $5); }
    | OBJECTID '(' comma_expr_list ')' 
    { printf("self dispatch, OBJECTID ( comma expr list )\n"); $$ = dispatch(object(idtable.add_string("self")), $1, $3);}
    | expr '@' TYPEID '.' OBJECTID '(' comma_expr_list ')' 
    { printf("static dispatch expr @ TYPEID.OBJECTID ( comma expr list) \n"); $$ = static_dispatch($1, $3, $5, $7);}
    | IF expr THEN expr ELSE expr FI
    { printf("IF expr THEN expr ELSE expr FI \n"); $$ = cond($2, $4, $6);}
    | WHILE expr LOOP expr POOL
    { printf("WHILE expr LOOP expr POOL\n"); $$ = loop($2, $4);}

    /* TODO CHECK BLOCK CODE WITH TA */
    | '{' semicolon_expr_list '}'
    { printf("{ semicolon expr list } \n"); $$ = block($2);}


    | LET OBJECTID ':' TYPEID let_chunk_list
    { printf("let, with right recursion because of multiple interior arugments, no optional assign\n"); $$ = let ($2, $4, no_expr(), $5); }
    | LET OBJECTID ':' TYPEID IN expr
    { printf("let, with a single interior argument and no optional assign\n"); $$ = let($2, $4, no_expr(), $6);}

    | LET OBJECTID ':' TYPEID ASSIGN expr let_chunk_list
    { printf("let, with right recursion because of multiple interior arguments, with optional assign\n"); $$ = let ($2, $4, $6, $7); }
    | LET OBJECTID ':' TYPEID ASSIGN expr IN expr
    { printf("let, with a single interior argument and optional assign\n"); $$ = let($2, $4, $6, $8);}
  


    /* TODO CASE */
    | CASE expr OF case_list ESAC
    { printf("typcase, assemble case expr of caselist esac\n"); typcase( $2, $4 );}




    | NEW TYPEID
    { printf("NEW TYPEID\n"); $$ = new_($2);}
    | ISVOID expr
    { printf(" isvoid expr\n"); $$ = isvoid($2);}
    | expr '+' expr
    { printf(" expr + expr\n"); $$ = plus($1 , $3);}
    | expr '-' expr
    { printf( " expr - expr\n"); $$ = sub($1, $3);}
    | expr '*' expr
    { printf(" expr * expr\n"); $$ = mul($1, $3);}
    | expr '/' expr
    { printf("expr / expr\n"); $$ = divide($1, $3);}
    | '~' expr
    { printf("tilde ~ expr\n"); $$ = neg($2);}
    | expr '<' expr
    { printf("expr < expr\n"); $$ = lt($1, $3);}
    | expr LE expr
    { printf("expr LE expr\n"); $$ = leq($1, $3);}
    | expr '=' expr
    { printf("expr = expr\n"); $$ = eq($1 , $3);}
    | NOT expr
    { printf("NOT expr\n"); $$ = comp($2);}
    | '(' expr ')'
    { printf("parent ( expr ) paren\n"); $$ = $2;}
    | OBJECTID
    { printf("objectid\n"); $$ = object($1);}
    | INT_CONST
    { printf("int const\n"); $$ = int_const($1);}
    | STR_CONST
    { printf("str const\n"); $$ = string_const($1);} 
    | BOOL_CONST 
    { printf("bool const\n"); $$ = bool_const($1);}
    ;



    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
