(*
 *  A contribution from Anne Sheets (sheets@cory)
 *
 *  Tests the arithmetic operations and various other things
 *)

class A {

   var : Int <- 0;

   value() : Int { var };

   set_var(num : Int) : SELF_TYPE {
      {
         var <- num;
         self;
      }
   };

   method1(num : Int) : SELF_TYPE {  -- same
      self
   };

   method2(num1 : Int, num2 : Int) : B {  -- plus
      (let x : Int in
	 {
            x <- num1 + num2;
	    (new B).set_var(x);
	 }
      )
   };

   method3(num : Int) : C {  -- negate
      (let x : Int in
	 {
            x <- ~num;
	    (new C).set_var(x);
	 }
      )
   };

   method4(num1 : Int, num2 : Int) : D {  -- diff
            if num2 < num1 then
               (let x : Int in
		  {
                     x <- num1 - num2;
	             (new D).set_var(x);
	          }
               )
            else
               (let x : Int in
		  {
	             x <- num2 - num1;
	             (new D).set_var(x);
		  }
               )
            fi
   };

   method5(num : Int) : E {  -- factorial
      (let x : Int <- 1 in
	 {
	    (let y : Int <- 1 in
	       while y <= num loop
	          {
                     x <- x * y;
	             y <- y + 1;
	          }
	       pool
	    );
	    (new E).set_var(x);
	 }
      )
   };

};

class B inherits A {  -- B is a number squared

   method5(num : Int) : E { -- square
      (let x : Int in
	 {
            x <- num * num;
	    (new E).set_var(x);
	 }
      )
   };

};

class C inherits B {

   method6(num : Int) : A { -- negate
      (let x : Int in
         {
            x <- ~num;
	    (new A).set_var(x);
         }
      )
   };

   method5(num : Int) : E {  -- cube
      (let x : Int in
	 {
            x <- num * num * num;
	    (new E).set_var(x);
	 }
      )
   };

};

class D inherits B {  
		
   method7(num : Int) : Bool {  -- divisible by 3
      (let x : Int <- num in
            if x < 0 then method7(~x) else
            if 0 = x then true else
            if 1 = x then false else
	    if 2 = x then false else
	       method7(x - 3)
	    fi fi fi fi
      )
   };

};

class E inherits D {

   method6(num : Int) : A {  -- division
      (let x : Int in
         {
            x <- num / 8;
	    (new A).set_var(x);
         }
      )
   };

};

(* The following code is from atoi.cl in ~cs164/examples *)

(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simpl write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

     c2i(char : String) : Int {
	if char = "0" then 0 else
	if char = "1" then 1 else
	if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  (* the 0 is needed to satisfy the
				  typchecker *)
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
	if i = 0 then "0" else
	if i = 1 then "1" else
	if i = 2 then "2" else
	if i = 3 then "3" else
	if i = 4 then "4" else
	if i = 5 then "5" else
	if i = 6 then "6" else
	if i = 7 then "7" else
	if i = 8 then "8" else
	if i = 9 then "9" else
	{ abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(* a2i_aux converts the usigned portion of the string.  As a
   programming example, this method is written iteratively.  *)


     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			{
			    int <- int * 10 + c2i(s.substr(i,1));
			    i <- i + 1;
			}
		    pool
		  )
	       );
              int;
	    }
        )
     };

(* i2a converts an integer to a string.  Positive and negative 
   numbers are handled correctly.  *)

    i2a(i : Int) : String {
	if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };
	
(* i2a_aux is an example using recursion.  *)		

    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
	    (let next : Int <- i / 10 in
		i2a_aux(next).concat(i2c(i - next * 10))
	    )
        fi
    };

};

class Main inherits IO {
   
   char : String;
   avar : A; 
   a_var : A;
   flag : Bool <- true;


   menu() : String {
      {
         out_string("\n\tTo add a number to ");
         print(avar);
         out_string("...enter a:\n");
         out_string("\tTo negate ");
         print(avar);
         out_string("...enter b:\n");
         out_string("\tTo find the difference between ");
         print(avar);
         out_string("and another number...enter c:\n");
         out_string("\tTo find the factorial of ");
         print(avar);
         out_string("...enter d:\n");
         out_string("\tTo square ");
         print(avar);
         out_string("...enter e:\n");
         out_string("\tTo cube ");
         print(avar);
         out_string("...enter f:\n");
         out_string("\tTo find out if ");
         print(avar);
         out_string("is a multiple of 3...enter g:\n");
         out_string("\tTo divide ");
         print(avar);
         out_string("by 8...enter h:\n");
	 out_string("\tTo get a new number...enter j:\n");
	 out_string("\tTo quit...enter q:\n\n");
         in_string();
      }
   };

   prompt() : String {
      {
         out_string("\n");
         out_string("Please enter a number...  ");
         in_string();
      }
   };

   get_int() : Int {
      {
	 (let z : A2I <- new A2I in
	    (let s : String <- prompt() in
	       z.a2i(s)
	    )
         );
      }
   };

   is_even(num : Int) : Bool {
      (let x : Int <- num in
            if x < 0 then is_even(~x) else
            if 0 = x then true else
	    if 1 = x then false else
	          is_even(x - 2)
	    fi fi fi
      )
   };

   class_type(var : A) : SELF_TYPE {
      case var of
	 a : A => out_string("Class type is now A\n");
	 b : B => out_string("Class type is now B\n");
	 c : C => out_string("Class type is now C\n");
	 d : D => out_string("Class type is now D\n");
	 e : E => out_string("Class type is now E\n");
	 o : Object => out_string("Oooops\n");
      esac
   };
 
   print(var : A) : SELF_TYPE {
     (let z : A2I <- new A2I in
	{
	   out_string(z.i2a(var.value()));
	   out_string(" ");
	}
     )
   };

   main() : Object {
      {
         avar <- (new A);
         while flag loop
            {
	       -- avar <- (new A).set_var(get_int());
	       out_string("number ");
	       print(avar);
	       if is_even(avar.value()) then
	          out_string("is even!\n")
	       else
	          out_string("is odd!\n")
	       fi;
	       -- print(avar); -- prints out answer
	       class_type(avar);
	       char <- menu();
                  if char = "a" then -- add
                     {
                        a_var <- (new A).set_var(get_int());
	                avar <- (new B).method2(avar.value(), a_var.value());
	             } else
                  if char = "b" then -- negate
                     case avar of
	                   c : C => avar <- c.method6(c.value());
	                   a : A => avar <- a.method3(a.value());
	                   o : Object => {
		                  out_string("Oooops\n");
		                  abort(); 0;
		               };
                     esac else
                  if char = "c" then -- diff
                     {
                        a_var <- (new A).set_var(get_int());
	                avar <- (new D).method4(avar.value(), a_var.value());
	             } else
                  if char = "d" then avar <- (new C)@A.method5(avar.value()) else
		          -- factorial
                  if char = "e" then avar <- (new C)@B.method5(avar.value()) else
			  -- square
                  if char = "f" then avar <- (new C)@C.method5(avar.value()) else
			  -- cube
                  if char = "g" then -- multiple of 3?
		      if ((new D).method7(avar.value()))
		                       then -- avar <- (new A).method1(avar.value())
			 {
	                    out_string("number ");
	                    print(avar);
	                    out_string("is divisible by 3.\n");
			 }
			 else  -- avar <- (new A).set_var(0)
			 {
	                    out_string("number ");
	                    print(avar);
	                    out_string("is not divisible by 3.\n");
			 }
		      fi else
                  if char = "h" then 
		      (let x : A in
			 {
		            x <- (new E).method6(avar.value());
			    (let r : Int <- (avar.value() - (x.value() * 8)) in
			       {
			          out_string("number ");
			          print(avar);
			          out_string("is equal to ");
			          print(x);
			          out_string("times 8 with a remainder of ");
				  (let a : A2I <- new A2I in
				     {
			                out_string(a.i2a(r));
			                out_string("\n");
				     }
				  ); -- end let a:
			       }
                            ); -- end let r:
			    avar <- x;
		         } 
		      )  -- end let x:
		      else
                  if char = "j" then avar <- (new A)
		      else
                  if char = "q" then flag <- false
		      else
                      avar <- (new A).method1(avar.value()) -- divide/8
                  fi fi fi fi fi fi fi fi fi fi;
            }
         pool;
       }
   };

};

(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simpl write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

     c2i(char : String) : Int {
	if char = "0" then 0 else
	if char = "1" then 1 else
	if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
	if i = 0 then "0" else
	if i = 1 then "1" else
	if i = 2 then "2" else
	if i = 3 then "3" else
	if i = 4 then "4" else
	if i = 5 then "5" else
	if i = 6 then "6" else
	if i = 7 then "7" else
	if i = 8 then "8" else
	if i = 9 then "9" else
	{ abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			{
			    int <- int * 10 + c2i(s.substr(i,1));
			    i <- i + 1;
			}
		    pool
		  )
	       );
              int;
	    }
        )
     };

(*
    i2a converts an integer to a string.  Positive and negative 
numbers are handled correctly.  
*)
    i2a(i : Int) : String {
	if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };
	
(*
    i2a_aux is an example using recursion.
*)		
    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
	    (let next : Int <- i / 10 in
		i2a_aux(next).concat(i2c(i - next * 10))
	    )
        fi
    };

};
(*
   This method implements a driver for testing the ATOI class.
The program repeatedly asks the user to enter a number, which
is then coverted from its string form to an integer and back
again to a string.  The results of both conversions are printed
on the screen.  Typing "stop" at the prompt exits the program.
*)

class Main inherits IO {
   newline() : Object {
	out_string("\n")
   };

   prompt() : String {
	{
	   out_string("Enter a number>");
	   in_string();
	}
   };

   main() : Object {
   (* Since we didn't bother to inherit from the A2I class, we have
	to have an object of type A2I in order to access the
	methods of that class. *)
     (let z : A2I <- new A2I in
	while true loop  
	   (let s : String <- prompt() in
		if s = "stop" then 
		    abort() -- we don't bother to terminate gracefully
		else
		   (let i : Int <- z.a2i(s) in
			(let news : String <- z.i2a(i) in
			   {
			     out_int(i);
			     newline();
			     out_string(news);
			     newline();
			   }
	                )
                  )
		fi
	   )
        pool
     )
   };
};
-- example of static and dynamic type differing for a dispatch

Class Book inherits IO {
    title : String;
    author : String;

    initBook(title_p : String, author_p : String) : Book {
        {
            title <- title_p;
            author <- author_p;
            self;
        }
    };

    print() : Book {
        {
            out_string("title:      ").out_string(title).out_string("\n");
            out_string("author:     ").out_string(author).out_string("\n");
            self;
        }
    };
};

Class Article inherits Book {
    per_title : String;

    initArticle(title_p : String, author_p : String,
		per_title_p : String) : Article {
        {
            initBook(title_p, author_p);
            per_title <- per_title_p;
            self;
        }
    };

    print() : Book {
        {
	    self@Book.print();
            out_string("periodical:  ").out_string(per_title).out_string("\n");
            self;
        }
    };
};

Class BookList inherits IO { 
    (* Since abort "returns" type Object, we have to add
       an expression of type Bool here to satisfy the typechecker.
       This code is unreachable, since abort() halts the program.
    *)
    isNil() : Bool { { abort(); true; } };
    
    cons(hd : Book) : Cons {
        (let new_cell : Cons <- new Cons in
            new_cell.init(hd,self)
        )
    };

    (* Since abort "returns" type Object, we have to add
       an expression of type Book here to satisfy the typechecker.
       This code is unreachable, since abort() halts the program.
    *)
    car() : Book { { abort(); new Book; } };
    
    (* Since abort "returns" type Object, we have to add
       an expression of type BookList here to satisfy the typechecker.
       This code is unreachable, since abort() halts the program.
    *)
    cdr() : BookList { { abort(); new BookList; } };
    
    print_list() : Object { abort() };
};

Class Cons inherits BookList {
    xcar : Book;  -- We keep the car and cdr in attributes.
    xcdr : BookList; -- Because methods and features must have different names,
    -- we use xcar and xcdr for the attributes and reserve
    -- car and cdr for the features.
    
    isNil() : Bool { false };
    
    init(hd : Book, tl : BookList) : Cons {
        {
            xcar <- hd;
            xcdr <- tl;
            self;
        }
    };

    car() : Book { xcar };

    cdr() : BookList { xcdr };
    
    print_list() : Object {
        {
            case xcar.print() of
                dummy : Book => out_string("- dynamic type was Book -\n");
                dummy : Article => out_string("- dynamic type was Article -\n");
            esac;
            xcdr.print_list();
        }
    };
};

Class Nil inherits BookList {
    isNil() : Bool { true };

    print_list() : Object { true };
};


Class Main {

    books : BookList;

    main() : Object {
        (let a_book : Book <-
            (new Book).initBook("Compilers, Principles, Techniques, and Tools",
                                "Aho, Sethi, and Ullman")
        in
            (let an_article : Article <-
                (new Article).initArticle("The Top 100 CD_ROMs",
                                          "Ulanoff",
                                          "PC Magazine")
            in
                {
                    books <- (new Nil).cons(a_book).cons(an_article);
                    books.print_list();
                }
            )  -- end let an_article
        )  -- end let a_book
    };
};
(* models one-dimensional cellular automaton on a circle of finite radius
   arrays are faked as Strings,
   X's respresent live cells, dots represent dead cells,
   no error checking is done *)
class CellularAutomaton inherits IO {
    population_map : String;
   
    init(map : String) : SELF_TYPE {
        {
            population_map <- map;
            self;
        }
    };
   
    print() : SELF_TYPE {
        {
            out_string(population_map.concat("\n"));
            self;
        }
    };
   
    num_cells() : Int {
        population_map.length()
    };
   
    cell(position : Int) : String {
        population_map.substr(position, 1)
    };
   
    cell_left_neighbor(position : Int) : String {
        if position = 0 then
            cell(num_cells() - 1)
        else
            cell(position - 1)
        fi
    };
   
    cell_right_neighbor(position : Int) : String {
        if position = num_cells() - 1 then
            cell(0)
        else
            cell(position + 1)
        fi
    };
   
    (* a cell will live if exactly 1 of itself and it's immediate
       neighbors are alive *)
    cell_at_next_evolution(position : Int) : String {
        if (if cell(position) = "X" then 1 else 0 fi
            + if cell_left_neighbor(position) = "X" then 1 else 0 fi
            + if cell_right_neighbor(position) = "X" then 1 else 0 fi
            = 1)
        then
            "X"
        else
            "."
        fi
    };
   
    evolve() : SELF_TYPE {
        (let position : Int in
        (let num : Int <- num_cells() in
        (let temp : String in
            {
                while position < num loop
                    {
                        temp <- temp.concat(cell_at_next_evolution(position));
                        position <- position + 1;
                    }
                pool;
                population_map <- temp;
                self;
            }
        ) ) )
    };
};

class Main {
    cells : CellularAutomaton;
   
    main() : SELF_TYPE {
        {
            cells <- (new CellularAutomaton).init("         X         ");
            cells.print();
            (let countdown : Int <- 20 in
                while 0 < countdown loop
                    {
                        cells.evolve();
                        cells.print();
                        countdown <- countdown - 1;
                    }
                pool
            );
            self;
        }
    };
};
class Main inherits IO {
    main() : SELF_TYPE {
	(let c : Complex <- (new Complex).init(1, 1) in
	    if c.reflect_X().reflect_Y() = c.reflect_0()
	    then out_string("=)\n")
	    else out_string("=(\n")
	    fi
	)
    };
};

class Complex inherits IO {
    x : Int;
    y : Int;

    init(a : Int, b : Int) : Complex {
	{
	    x = a;
	    y = b;
	    self;
	}
    };

    print() : Object {
	if y = 0
	then out_int(x)
	else out_int(x).out_string("+").out_int(y).out_string("I")
	fi
    };

    reflect_0() : Complex {
	{
	    x = ~x;
	    y = ~y;
	    self;
	}
    };

    reflect_X() : Complex {
	{
	    y = ~y;
	    self;
	}
    };

    reflect_Y() : Complex {
	{
	    x = ~x;
	    self;
	}
    };
};
class Main inherits IO {
    main() : SELF_TYPE {
	{
	    out_string((new Object).type_name().substr(4,1)).
	    out_string((isvoid self).type_name().substr(1,3));
	    out_string("\n");
	}
    };
};
(*
 *   Cool program reading descriptions of weighted directed graphs
 *   from stdin. It builds up a graph objects with a list of vertices
 *   and a list of edges. Every vertice has a list of outgoing edges.
 *
 *  INPUT FORMAT
 *      Every line has the form		vertice successor*
 *      Where vertice is an int, and successor is   vertice,weight
 *
 *      An empty line or EOF terminates the input.
 *
 *   The list of vertices and the edge list is printed out by the Main
 *   class. 
 *
 *  TEST
 *     Once compiled, the file g1.graph can be fed to the program.
 *     The output should look like this:

nautilus.CS.Berkeley.EDU 53# spim -file graph.s <g1.graph 
SPIM Version 5.4 of Jan. 17, 1994
Copyright 1990-1994 by James R. Larus (larus@cs.wisc.edu).
All Rights Reserved.
See the file README a full copyright notice.
Loaded: /home/n/cs164/lib/trap.handler
5 (5,5)5 (5,4)4 (5,3)3 (5,2)2 (5,1)1
4 (4,5)100 (4,3)55
3 (3,2)10
2 (2,1)150 (2,3)200
1 (1,2)100

 (5,5)5 (5,4)4 (5,3)3 (5,2)2 (5,1)1 (4,5)100 (4,3)55 (3,2)10 (2,1)150 (2,3)200 (1,2)100
COOL program successfully executed

 *)



class Graph {

   vertices : VList <- new VList;
   edges    : EList <- new EList;

   add_vertice(v : Vertice) : Object { {
      edges <- v.outgoing().append(edges);
      vertices <- vertices.cons(v);
   } };

   print_E() : Object { edges.print() };
   print_V() : Object { vertices.print() };

};

class Vertice inherits IO { 

   num  : Int;
   out  : EList <- new EList;

   outgoing() : EList { out };

   number() : Int { num };

   init(n : Int) : SELF_TYPE {
      {
         num <- n;
         self;
      }
   };


   add_out(s : Edge) : SELF_TYPE {
      {
	 out <- out.cons(s);
         self;
      }
   };

   print() : Object {
      {
         out_int(num);
	 out.print();
      }
   };

};

class Edge inherits IO {

   from   : Int;
   to     : Int;
   weight : Int;

   init(f : Int, t : Int, w : Int) : SELF_TYPE {
      {
         from <- f;
	 to <- t;
	 weight <- w;
	 self;
      }
   };

   print() : Object {
      {
         out_string(" (");
	 out_int(from);
	 out_string(",");
	 out_int(to);
	 out_string(")");
	 out_int(weight);
      }
   };

};



class EList inherits IO {
   -- Define operations on empty lists of Edges.

   car : Edge;

   isNil() : Bool { true };

   head()  : Edge { { abort(); car; } };

   tail()  : EList { { abort(); self; } };

   -- When we cons and element onto the empty list we get a non-empty
   -- list. The (new Cons) expression creates a new list cell of class
   -- Cons, which is initialized by a dispatch to init().
   -- The result of init() is an element of class Cons, but it
   -- conforms to the return type List, because Cons is a subclass of
   -- List.

   cons(e : Edge) : EList {
      (new ECons).init(e, self)
   };

   append(l : EList) : EList {
     if self.isNil() then l
     else tail().append(l).cons(head())
     fi
   };

   print() : Object {
     out_string("\n")
   };

};


(*
 *  Cons inherits all operations from List. We can reuse only the cons
 *  method though, because adding an element to the front of an emtpy
 *  list is the same as adding it to the front of a non empty
 *  list. All other methods have to be redefined, since the behaviour
 *  for them is different from the empty list.
 *
 *  Cons needs an extra attribute to hold the rest of the list.
 *
 *  The init() method is used by the cons() method to initialize the
 *  cell.
 *)

class ECons inherits EList {

   cdr : EList;	-- The rest of the list

   isNil() : Bool { false };

   head()  : Edge { car };

   tail()  : EList { cdr };

   init(e : Edge, rest : EList) : EList {
      {
	 car <- e;
	 cdr <- rest;
	 self;
      }
   };

   print() : Object {
     {
       car.print();
       cdr.print();
     } 
   };

};




class VList inherits IO {
   -- Define operations on empty lists of vertices.

   car : Vertice;

   isNil() : Bool { true };

   head()  : Vertice { { abort(); car; } };

   tail()  : VList { { abort(); self; } };

   -- When we cons and element onto the empty list we get a non-empty
   -- list. The (new Cons) expression creates a new list cell of class
   -- ECons, which is initialized by a dispatch to init().
   -- The result of init() is an element of class Cons, but it
   -- conforms to the return type List, because Cons is a subclass of
   -- List.

   cons(v : Vertice) : VList {
      (new VCons).init(v, self)
   };

   print() : Object { out_string("\n") };

};


class VCons inherits VList {

   cdr : VList;	-- The rest of the list

   isNil() : Bool { false };

   head()  : Vertice { car };

   tail()  : VList { cdr };

   init(v : Vertice, rest : VList) : VList {
      {
	 car <- v;
	 cdr <- rest;
	 self;
      }
   };

   print() : Object {
     {
       car.print();
       cdr.print();
     } 
   };

};


class Parse inherits IO {


   boolop : BoolOp <- new BoolOp;

   -- Reads the input and parses the fields

   read_input() : Graph {

      (let g : Graph <- new Graph in {
         (let line : String <- in_string() in
            while (boolop.and(not line="\n", not line="")) loop {
		-- out_string(line);
		-- out_string("\n");
		g.add_vertice(parse_line(line));
		line <- in_string();
	    } pool
         );
	 g;
      } )
   };


   parse_line(s : String) : Vertice {
      (let v : Vertice <- (new Vertice).init(a2i(s)) in {
	 while (not rest.length() = 0) loop {
	       -- out_string(rest);
	       -- out_string("\n");
	       (let succ : Int <- a2i(rest) in (let
	           weight : Int <- a2i(rest)
               in
	          v.add_out(new Edge.init(v.number(), 
                                          succ,
					  weight))
	       ) );
	 } pool;
	 v;
         }
      )
   };

     c2i(char : String) : Int {
	if char = "0" then 0 else
	if char = "1" then 1 else
	if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

     rest : String;

     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = " " then a2i(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
  The conversion stops at a space or comma.
  As a side effect, r is set to the remaining string (without the comma).
*)
     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			(let c : String <- s.substr(i,1) in
			    if (c = " ") then
			       {
				  rest <- s.substr(i+1,s.length()-i-1);
				  i <- j;
			       }
			    else if (c = ",") then
		               {
				  rest <- s.substr(i+1, s.length()-i-1);
				  i <- j;
		               }
			    else
			       {
				 int <- int * 10 + c2i(s.substr(i,1));
				 i <- i + 1;
				 if i=j then rest <- "" else "" fi;
			       }
			    fi fi
			)
		    pool
		  )
	       );
              int;
	    }
        )
     };

};


class Main inherits Parse {

   g : Graph <- read_input();

   main() : Object {
      {
	 g.print_V();
         g.print_E();
      }
   };

};

class BoolOp {

  and(b1 : Bool, b2 : Bool) : Bool {
     if b1 then b2 else false fi
  };


  or(b1 : Bool, b2 : Bool) : Bool {
     if b1 then true else b2 fi
  };

};
(* hairy  . . .*)

class Foo inherits Bazz {
     a : Razz <- case self of
		      n : Razz => (new Bar);
		      n : Foo => (new Razz);
		      n : Bar => n;
   	         esac;

     b : Int <- a.doh() + g.doh() + doh() + printh();

     doh() : Int { (let i : Int <- h in { h <- h + 2; i; } ) };

};

class Bar inherits Razz {

     c : Int <- doh();

     d : Object <- printh();
};


class Razz inherits Foo {

     e : Bar <- case self of
		  n : Razz => (new Bar);
		  n : Bar => n;
		esac;

     f : Int <- a@Bazz.doh() + g.doh() + e.doh() + doh() + printh();

};

class Bazz inherits IO {

     h : Int <- 1;

     g : Foo  <- case self of
		     	n : Bazz => (new Foo);
		     	n : Razz => (new Bar);
			n : Foo  => (new Razz);
			n : Bar => n;
		  esac;

     i : Object <- printh();

     printh() : Int { { out_int(h); 0; } };

     doh() : Int { (let i: Int <- h in { h <- h + 1; i; } ) };
};

(* scary . . . *)
class Main {
  a : Bazz <- new Bazz;
  b : Foo <- new Foo;
  c : Razz <- new Razz;
  d : Bar <- new Bar;

  main(): String { "do nothing" };

};





class Main inherits IO {
   main(): SELF_TYPE {
	out_string("Hello, World.\n")
   };
};
(*
 *  The IO class is predefined and has 4 methods:
 *
 *    out_string(s : String) : SELF_TYPE
 *    out_int(i : Int) : SELF_TYPE
 *    in_string() : String
 *    in_int() : Int
 *
 *    The out operations print their argument to the terminal. The
 *    in_string method reads an entire line from the terminal and returns a
 *    string not containing the new line. The in_int method also reads
 *    an entire line from the terminal and returns the integer
 *    corresponding to the first non blank word on the line. If that
 *    word is not an integer, it returns 0.
 *
 *
 *  Because our language is object oriented, we need an object of type
 *  IO in order to call any of these methods.
 *
 *  There are basically two ways of getting access to IO in a class C.
 *
 *   1) Define C to Inherit from IO. This way the IO methods become
 *      methods of C, and they can be called using the abbreviated
 *      dispatch, i.e.
 *
 *      class C inherits IO is
 *          ...
 *          out_string("Hello world\n")
 *          ...
 *      end;
 *
 *   2) If your class C does not directly or indirectly inherit from
 *      IO, the best way to access IO is through an initialized
 *      attribute of type IO. 
 *
 *      class C inherits Foo is
 *         io : IO <- new IO;
 *         ...
 *             io.out_string("Hello world\n");
 *         ...
 *      end;
 *
 *  Approach 1) is most often used, in particular when you need IO
 *  functions in the Main class.
 *
 *)


class A {

   -- Let's assume that we don't want A to not inherit from IO.

   io : IO <- new IO;

   out_a() : Object { io.out_string("A: Hello world\n") };

};


class B inherits A {

   -- B does not have to an extra attribute, since it inherits io from A.

   out_b() : Object { io.out_string("B: Hello world\n") };

};


class C inherits IO {

   -- Now the IO methods are part of C.

   out_c() : Object { out_string("C: Hello world\n") };

   -- Note that out_string(...) is just a shorthand for self.out_string(...)

};


class D inherits C {

   -- Inherits IO methods from C.

   out_d() : Object { out_string("D: Hello world\n") };

};


class Main inherits IO {

   -- Same case as class C.

   main() : Object {
      {
	 (new A).out_a();
	 (new B).out_b();
	 (new C).out_c();
	 (new D).out_d();
	 out_string("Done.\n");
      }
   };

};
(* A program for

   1. Representing lambda terms
   2. Interpreting lambda terms
   3. Compiling lambda calculus programs to Cool

   The lambda calculus is described by the following grammar:

   e ::= x	       a variable
      |  \x.e	       a function with argument x
      |  e1@e2	       apply function e1 to argument e2

  Jeff Foster (jfoster@cs.berkeley.edu)
  March 24, 2000
*)

(*
 * A list of variables.  We use this to do de Bruijn numbering
 *
 *)
class VarList inherits IO {
  isNil() : Bool { true };
  head()  : Variable { { abort(); new Variable; } };
  tail()  : VarList { { abort(); new VarList; } };
  add(x : Variable) : VarList { (new VarListNE).init(x, self) };
  print() : SELF_TYPE { out_string("\n") };
};

class VarListNE inherits VarList {
  x : Variable;
  rest : VarList;
  isNil() : Bool { false };
  head()  : Variable { x };
  tail()  : VarList { rest };
  init(y : Variable, r : VarList) : VarListNE { { x <- y; rest <- r; self; } };
  print() : SELF_TYPE { { x.print_self(); out_string(" ");
	                  rest.print(); self; } };
};

(*
 * A list of closures we need to build.  We need to number (well, name)
 * the closures uniquely.
 *)
class LambdaList {
  isNil() : Bool { true };
  headE() : VarList { { abort(); new VarList; } };
  headC() : Lambda { { abort(); new Lambda; } };
  headN() : Int { { abort(); 0; } };
  tail()  : LambdaList { { abort(); new LambdaList; } };
  add(e : VarList, x : Lambda, n : Int) : LambdaList {
    (new LambdaListNE).init(e, x, n, self)
  };
};

class LambdaListNE inherits LambdaList {
  lam : Lambda;
  num : Int;
  env : VarList;
  rest : LambdaList;
  isNil() : Bool { false };
  headE() : VarList { env };
  headC() : Lambda { lam };
  headN() : Int { num };
  tail()  : LambdaList { rest };
  init(e : VarList, l : Lambda, n : Int, r : LambdaList) : LambdaListNE {
    {
      env <- e;
      lam <- l;
      num <- n;
      rest <- r;
      self;
    }
  };
};

class LambdaListRef {
  nextNum : Int <- 0;
  l : LambdaList;
  isNil() : Bool { l.isNil() };
  headE() : VarList { l.headE() };
  headC() : Lambda { l.headC() };
  headN() : Int { l.headN() };
  reset() : SELF_TYPE {
    {
      nextNum <- 0;
      l <- new LambdaList;
      self;
    }
  };
  add(env : VarList, c : Lambda) : Int {
    {
      l <- l.add(env, c, nextNum);
      nextNum <- nextNum + 1;
      nextNum - 1;
    }
  };
  removeHead() : SELF_TYPE {
    {
      l <- l.tail();
      self;
    }
  };
};

(*
 * Lambda expressions
 *
 *)

-- A pure virtual class representing any expression
class Expr inherits IO {

  -- Print this lambda term
  print_self() : SELF_TYPE {
    {
      out_string("\nError: Expr is pure virtual; can't print self\n");
      abort();
      self;
    }
  };

  -- Do one step of (outermost) beta reduction to this term
  beta() : Expr {
    {
      out_string("\nError: Expr is pure virtual; can't beta-reduce\n");
      abort();
      self;
    }
  };

  -- Replace all occurrences of x by e
  substitute(x : Variable, e : Expr) : Expr {
    {
      out_string("\nError: Expr is pure virtual; can't substitute\n");
      abort();
      self;
    }
  };

  -- Generate Cool code to evaluate this expression
  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("\nError: Expr is pure virtual; can't gen_code\n");
      abort();
      self;
    }
  };
};

(*
 * Variables
 *)
class Variable inherits Expr {
  name : String;

  init(n:String) : Variable {
    {
      name <- n;
      self;
    }
  };

  print_self() : SELF_TYPE {
    out_string(name)
  };

  beta() : Expr { self };
  
  substitute(x : Variable, e : Expr) : Expr {
    if x = self then e else self fi
  };

  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    let cur_env : VarList <- env in
      { while (if cur_env.isNil() then
	          false
	       else
	         not (cur_env.head() = self)
	       fi) loop
	  { out_string("get_parent().");
	    cur_env <- cur_env.tail();
          }
        pool;
        if cur_env.isNil() then
          { out_string("Error:  free occurrence of ");
            print_self();
            out_string("\n");
            abort();
            self;
          }
        else
          out_string("get_x()")
        fi;
      }
  };
};

(*
 * Functions
 *)
class Lambda inherits Expr {
  arg : Variable;
  body : Expr;

  init(a:Variable, b:Expr) : Lambda {
    {
      arg <- a;
      body <- b;
      self;
    }
  };

  print_self() : SELF_TYPE {
    {
      out_string("\\");
      arg.print_self();
      out_string(".");
      body.print_self();
      self;
    }
  };

  beta() : Expr { self };

  apply(actual : Expr) : Expr {
    body.substitute(arg, actual)
  };

  -- We allow variables to be reused
  substitute(x : Variable, e : Expr) : Expr {
    if x = arg then
      self
    else
      let new_body : Expr <- body.substitute(x, e),
	  new_lam : Lambda <- new Lambda in
	new_lam.init(arg, new_body)
    fi
  };

  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("((new Closure");
      out_int(closures.add(env, self));
      out_string(").init(");
      if env.isNil() then
        out_string("new Closure))")
      else
	out_string("self))") fi;
      self;
    }
  };

  gen_closure_code(n : Int, env : VarList,
		   closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("class Closure");
      out_int(n);
      out_string(" inherits Closure {\n");
      out_string("  apply(y : EvalObject) : EvalObject {\n");
      out_string("    { out_string(\"Applying closure ");
      out_int(n);
      out_string("\\n\");\n");
      out_string("      x <- y;\n");
      body.gen_code(env.add(arg), closures);
      out_string(";}};\n");
      out_string("};\n");
    }
  };
};

(*
 * Applications
 *)
class App inherits Expr {
  fun : Expr;
  arg : Expr;

  init(f : Expr, a : Expr) : App {
    {
      fun <- f;
      arg <- a;
      self;
    }
  };

  print_self() : SELF_TYPE {
    {
      out_string("((");
      fun.print_self();
      out_string(")@(");
      arg.print_self();
      out_string("))");
      self;
    }
  };

  beta() : Expr {
    case fun of
      l : Lambda => l.apply(arg);     -- Lazy evaluation
      e : Expr =>
	let new_fun : Expr <- fun.beta(),
	    new_app : App <- new App in
	  new_app.init(new_fun, arg);
    esac
  };

  substitute(x : Variable, e : Expr) : Expr {
    let new_fun : Expr <- fun.substitute(x, e),
        new_arg : Expr <- arg.substitute(x, e),
        new_app : App <- new App in
      new_app.init(new_fun, new_arg)
  };

  gen_code(env : VarList, closures : LambdaListRef) : SELF_TYPE {
    {
      out_string("(let x : EvalObject <- ");
      fun.gen_code(env, closures);
      out_string(",\n");
      out_string("     y : EvalObject <- ");
      arg.gen_code(env, closures);
      out_string(" in\n");
      out_string("  case x of\n");
      out_string("    c : Closure => c.apply(y);\n");
      out_string("    o : Object => { abort(); new EvalObject; };\n");
      out_string("  esac)");
    }
  };
};

(*
 * Term: A class for building up terms
 *
 *)

class Term inherits IO {
  (*
   * The basics
   *)
  var(x : String) : Variable {
    let v : Variable <- new Variable in
      v.init(x)
  };

  lam(x : Variable, e : Expr) : Lambda {
    let l : Lambda <- new Lambda in
      l.init(x, e)
  };

  app(e1 : Expr, e2 : Expr) : App {
    let a : App <- new App in
      a.init(e1, e2)
  };

  (*
   * Some useful terms
   *)
  i() : Expr {
    let x : Variable <- var("x") in
      lam(x,x)
  };

  k() : Expr {
    let x : Variable <- var("x"),
        y : Variable <- var("y") in
    lam(x,lam(y,x))
  };

  s() : Expr {
    let x : Variable <- var("x"),
        y : Variable <- var("y"),
        z : Variable <- var("z") in
      lam(x,lam(y,lam(z,app(app(x,z),app(y,z)))))
  };

};

(*
 *
 * The main method -- build up some lambda terms and try things out
 *
 *)

class Main inherits Term {
  -- Beta-reduce an expression, printing out the term at each step
  beta_reduce(e : Expr) : Expr {
    {
      out_string("beta-reduce: ");
      e.print_self();
      let done : Bool <- false,
          new_expr : Expr in
        {
	  while (not done) loop
	    {
	      new_expr <- e.beta();
	      if (new_expr = e) then
		done <- true
	      else
		{
		  e <- new_expr;
		  out_string(" =>\n");
		  e.print_self();
		}
	      fi;
	    }
          pool;
	  out_string("\n");
          e;
	};
    }
  };

  eval_class() : SELF_TYPE {
    {
      out_string("class EvalObject inherits IO {\n");
      out_string("  eval() : EvalObject { { abort(); self; } };\n");
      out_string("};\n");
    }
  };

  closure_class() : SELF_TYPE {
    {
      out_string("class Closure inherits EvalObject {\n");
      out_string("  parent : Closure;\n");
      out_string("  x : EvalObject;\n");
      out_string("  get_parent() : Closure { parent };\n");
      out_string("  get_x() : EvalObject { x };\n");
      out_string("  init(p : Closure) : Closure {{ parent <- p; self; }};\n");
      out_string("  apply(y : EvalObject) : EvalObject { { abort(); self; } };\n");
      out_string("};\n");
    }
  };

  gen_code(e : Expr) : SELF_TYPE {
    let cl : LambdaListRef <- (new LambdaListRef).reset() in
      {
	out_string("Generating code for ");
	e.print_self();
	out_string("\n------------------cut here------------------\n");
	out_string("(*Generated by lam.cl (Jeff Foster, March 2000)*)\n");
	eval_class();
	closure_class();
	out_string("class Main {\n");
	out_string("  main() : EvalObject {\n");
	e.gen_code(new VarList, cl);
	out_string("\n};\n};\n");
	while (not (cl.isNil())) loop
	  let e : VarList <- cl.headE(),
	      c : Lambda <- cl.headC(),
	      n : Int <- cl.headN() in
	    {
	      cl.removeHead();
	      c.gen_closure_code(n, e, cl);
	    }
	pool;
	out_string("\n------------------cut here------------------\n");
      }
  };

  main() : Int {
    {
      i().print_self();
      out_string("\n");
      k().print_self();
      out_string("\n");
      s().print_self();
      out_string("\n");
      beta_reduce(app(app(app(s(), k()), i()), i()));
      beta_reduce(app(app(k(),i()),i()));
      gen_code(app(i(), i()));
      gen_code(app(app(app(s(), k()), i()), i()));
      gen_code(app(app(app(app(app(app(app(app(i(), k()), s()), s()),
                                   k()), s()), i()), k()), i()));
      gen_code(app(app(i(), app(k(), s())), app(k(), app(s(), s()))));
      0;
    }
  };
};
(* The Game of Life 
   Tendo Kayiira, Summer '95
   With code taken from /private/cool/class/examples/cells.cl

 This introduction was taken off the internet. It gives a brief 
 description of the Game Of Life. It also gives the rules by which 
 this particular game follows.

	Introduction

   John Conway's Game of Life is a mathematical amusement, but it 
   is also much more: an insight into how a system of simple 
   cellualar automata can create complex, odd, and often aesthetically 
   pleasing patterns. It is played on a cartesian grid of cells
   which are either 'on' or 'off' The game gets it's name from the 
   similarity between the behaviour of these cells and the behaviour 
   of living organisms.

 The Rules

  The playfield is a cartesian grid of arbitrary size. Each cell in 
  this grid can be in an 'on' state or an 'off' state. On each 'turn' 
  (called a generation,) the state of each cell changes simultaneously 
  depending on it's state and the state of all cells adjacent to it.

   For 'on' cells, 
      If the cell has 0 or 1 neighbours which are 'on', the cell turns 
        'off'. ('dies of loneliness') 
      If the cell has 2 or 3 neighbours which are 'on', the cell stays 
        'on'. (nothing happens to that cell) 
      If the cell has 4, 5, 6, 7, 8, or 9 neighbours which are 'on', 
        the cell turns 'off'. ('dies of overcrowding') 

   For 'off' cells, 
      If the cell has 0, 1, 2, 4, 5, 6, 7, 8, or 9 neighbours which 
        are 'on', the cell stays 'off'. (nothing happens to that cell) 
      If the cell has 3 neighbours which are 'on', the cell turns 
        'on'. (3 neighbouring 'alive' cells 'give birth' to a fourth.) 

   Repeat for as many generations as desired. 

 *)
 

class Board inherits IO { 
 
 rows : Int;
 columns : Int;
 board_size : Int;

 size_of_board(initial : String) : Int {
   initial.length()
 };

 board_init(start : String) : SELF_TYPE {
   (let size :Int  <- size_of_board(start) in
    {
	if size = 15 then
	 {
	  rows <- 3;
	  columns <- 5;
	  board_size <- size;
	 }
	else if size = 16 then
	  {
	  rows <- 4;
	  columns <- 4;
	  board_size <- size;
	 }
	else if size = 20 then
	 {
	  rows <- 4;
	  columns <- 5;
	  board_size <- size;
	 }
	else if size = 21 then
	 {
	  rows <- 3;
	  columns <- 7;
	  board_size <- size;
	 }
	else if size = 25 then
	 {
	  rows <- 5;
	  columns <- 5;
	  board_size <- size;
	 }
	else if size = 28 then
	 {
	  rows <- 7;
	  columns <- 4;
	  board_size <- size;
	 }
	else 	-- If none of the above fit, then just give 
	 {  -- the configuration of the most common board
	  rows <- 5;
	  columns <- 5;
	  board_size <- size;
	 }
	fi fi fi fi fi fi;
	self;
    }
   )
 };

};



class CellularAutomaton inherits Board {
    population_map : String;
   
    init(map : String) : SELF_TYPE {
        {
            population_map <- map;
	    board_init(map);
            self;
        }
    };



   
    print() : SELF_TYPE {
        
	(let i : Int <- 0 in
	(let num : Int <- board_size in
	{
 	out_string("\n");
	 while i < num loop
           {
	    out_string(population_map.substr(i,columns));
	    out_string("\n"); 
	    i <- i + columns;
	   }
	 pool;
 	out_string("\n");
	self;
	}
	) ) 
    };
   
    num_cells() : Int {
        population_map.length()
    };
   
    cell(position : Int) : String {
	if board_size - 1 < position then
		" "
	else 
        	population_map.substr(position, 1)
	fi
    };
   
 north(position : Int): String {
	if (position - columns) < 0 then
	      " "	                       
	else
	   cell(position - columns)
	fi
 };

 south(position : Int): String {
	if board_size < (position + columns) then
	      " "                     
	else
	   cell(position + columns)
	fi
 };

 east(position : Int): String {
	if (((position + 1) /columns ) * columns) = (position + 1) then
	      " "                
	else
	   cell(position + 1)
	fi 
 };

 west(position : Int): String {
	if position = 0 then
	      " "
	else 
	   if ((position / columns) * columns) = position then
	      " "
	   else
	      cell(position - 1)
	fi fi
 };

 northwest(position : Int): String {
	if (position - columns) < 0 then
	      " "	                       
	else  if ((position / columns) * columns) = position then
	      " "
	      else
		north(position - 1)
	fi fi
 };

 northeast(position : Int): String {
	if (position - columns) < 0 then
	      " "	
	else if (((position + 1) /columns ) * columns) = (position + 1) then
	      " "     
	     else
	       north(position + 1)
	fi fi
 };

 southeast(position : Int): String {
	if board_size < (position + columns) then
	      " "                     
	else if (((position + 1) /columns ) * columns) = (position + 1) then
	       " "                
	     else
	       south(position + 1)
	fi fi
 };

 southwest(position : Int): String {
	if board_size < (position + columns) then
	      " "                     
	else  if ((position / columns) * columns) = position then
	      " "
	      else
	       south(position - 1)
	fi fi
 };

 neighbors(position: Int): Int { 
 	{
	     if north(position) = "X" then 1 else 0 fi
	     + if south(position) = "X" then 1 else 0 fi
 	     + if east(position) = "X" then 1 else 0 fi
 	     + if west(position) = "X" then 1 else 0 fi
	     + if northeast(position) = "X" then 1 else 0 fi
	     + if northwest(position) = "X" then 1 else 0 fi
 	     + if southeast(position) = "X" then 1 else 0 fi
	     + if southwest(position) = "X" then 1 else 0 fi;
	 }
 };

 
(* A cell will live if 2 or 3 of it's neighbors are alive. It dies 
   otherwise. A cell is born if only 3 of it's neighbors are alive. *)
    
    cell_at_next_evolution(position : Int) : String {

	if neighbors(position) = 3 then
		"X"
	else
	   if neighbors(position) = 2 then
		if cell(position) = "X" then
			"X"
		else
			"-"
	        fi
	   else
		"-"
	fi fi
    };
  

    evolve() : SELF_TYPE {
        (let position : Int <- 0 in
        (let num : Int <- num_cells() in
        (let temp : String in
            {
                while position < num loop
                    {
                        temp <- temp.concat(cell_at_next_evolution(position));
                        position <- position + 1;
                    }
                pool;
                population_map <- temp;
                self;
            }
        ) ) )
    };

(* This is where the background pattern is detremined by the user. More 
   patterns can be added as long as whoever adds keeps the board either
   3x5, 4x5, 5x5, 3x7, 7x4, 4x4 with the row first then column. *) 
 option(): String {
 {
  (let num : Int in
   {
   out_string("\nPlease chose a number:\n");
   out_string("\t1: A cross\n"); 
   out_string("\t2: A slash from the upper left to lower right\n");
   out_string("\t3: A slash from the upper right to lower left\n"); 
   out_string("\t4: An X\n"); 
   out_string("\t5: A greater than sign \n"); 
   out_string("\t6: A less than sign\n"); 
   out_string("\t7: Two greater than signs\n"); 
   out_string("\t8: Two less than signs\n"); 
   out_string("\t9: A 'V'\n"); 
   out_string("\t10: An inverse 'V'\n"); 
   out_string("\t11: Numbers 9 and 10 combined\n"); 
   out_string("\t12: A full grid\n"); 
   out_string("\t13: A 'T'\n");
   out_string("\t14: A plus '+'\n");
   out_string("\t15: A 'W'\n");
   out_string("\t16: An 'M'\n");
   out_string("\t17: An 'E'\n");
   out_string("\t18: A '3'\n");
   out_string("\t19: An 'O'\n");
   out_string("\t20: An '8'\n");
   out_string("\t21: An 'S'\n");
   out_string("Your choice => ");
   num <- in_int();
   out_string("\n");
   if num = 1 then
    	" XX  XXXX XXXX  XX  "
   else if num = 2 then
    	"    X   X   X   X   X    "
   else if num = 3 then
    	"X     X     X     X     X"
   else if num = 4 then
	"X   X X X   X   X X X   X"
   else if num = 5 then
	"X     X     X   X   X    "
   else if num = 6 then
	"    X   X   X     X     X"
   else if num = 7 then
	"X  X  X  XX  X      "
   else if num = 8 then
	" X  XX  X  X  X     "
   else if num = 9 then
	"X   X X X   X  "
   else if num = 10 then
	"  X   X X X   X"
   else if num = 11 then
	"X X X X X X X X"
   else if num = 12 then
	"XXXXXXXXXXXXXXXXXXXXXXXXX"
   else if num = 13 then
    	"XXXXX  X    X    X    X  "
   else if num = 14 then
    	"  X    X  XXXXX  X    X  "
   else if num = 15 then
    	"X     X X X X   X X  "
   else if num = 16 then
    	"  X X   X X X X     X"
   else if num = 17 then
	"XXXXX   X   XXXXX   X   XXXX"
   else if num = 18 then
	"XXX    X   X  X    X   XXXX "
   else if num = 19 then
	" XX X  XX  X XX "
   else if num = 20 then
	" XX X  XX  X XX X  XX  X XX "
   else if num = 21 then
	" XXXX   X    XX    X   XXXX "
   else
	"                         "
  fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi;
    }
   );
 }
 };




 prompt() : Bool { 
 {
  (let ans : String in
   {
   out_string("Would you like to continue with the next generation? \n");
   out_string("Please use lowercase y or n for your answer [y]: ");
   ans <- in_string();
   out_string("\n");
   if ans = "n" then 
	false
   else
	true
   fi;
   }
  );
 }
 };


 prompt2() : Bool { 
  (let ans : String in
   {
   out_string("\n\n");
   out_string("Would you like to choose a background pattern? \n");
   out_string("Please use lowercase y or n for your answer [n]: ");
   ans <- in_string();
   if ans = "y" then 
	true
   else
	false
   fi;
   }
  )
 };


};

class Main inherits CellularAutomaton {
    cells : CellularAutomaton;
   
    main() : SELF_TYPE {
        {
	 (let continue : Bool  in
	  (let choice : String  in
	   {
	   out_string("Welcome to the Game of Life.\n");
	   out_string("There are many initial states to choose from. \n");
	   while prompt2() loop
	    {
	     continue <- true;
	     choice <- option();
	     cells <- (new CellularAutomaton).init(choice);
	     cells.print();
             while continue loop
		if prompt() then
                    {
                        cells.evolve();
                        cells.print();
                    }
		else
		    continue <- false
	      fi 
                pool;
            }
            pool;
	    self;
      }  ) ); }
    };
};

(*
 *  This file shows how to implement a list data type for lists of integers.
 *  It makes use of INHERITANCE and DYNAMIC DISPATCH.
 *
 *  The List class has 4 operations defined on List objects. If 'l' is
 *  a list, then the methods dispatched on 'l' have the following effects:
 *
 *    isNil() : Bool		Returns true if 'l' is empty, false otherwise.
 *    head()  : Int		Returns the integer at the head of 'l'.
 *				If 'l' is empty, execution aborts.
 *    tail()  : List		Returns the remainder of the 'l',
 *				i.e. without the first element.
 *    cons(i : Int) : List	Return a new list containing i as the
 *				first element, followed by the
 *				elements in 'l'.
 *
 *  There are 2 kinds of lists, the empty list and a non-empty
 *  list. We can think of the non-empty list as a specialization of
 *  the empty list.
 *  The class List defines the operations on empty list. The class
 *  Cons inherits from List and redefines things to handle non-empty
 *  lists.
 *)


class List {
   -- Define operations on empty lists.

   isNil() : Bool { true };

   -- Since abort() has return type Object and head() has return type
   -- Int, we need to have an Int as the result of the method body,
   -- even though abort() never returns.

   head()  : Int { { abort(); 0; } };

   -- As for head(), the self is just to make sure the return type of
   -- tail() is correct.

   tail()  : List { { abort(); self; } };

   -- When we cons and element onto the empty list we get a non-empty
   -- list. The (new Cons) expression creates a new list cell of class
   -- Cons, which is initialized by a dispatch to init().
   -- The result of init() is an element of class Cons, but it
   -- conforms to the return type List, because Cons is a subclass of
   -- List.

   cons(i : Int) : List {
      (new Cons).init(i, self)
   };

};


(*
 *  Cons inherits all operations from List. We can reuse only the cons
 *  method though, because adding an element to the front of an emtpy
 *  list is the same as adding it to the front of a non empty
 *  list. All other methods have to be redefined, since the behaviour
 *  for them is different from the empty list.
 *
 *  Cons needs two attributes to hold the integer of this list
 *  cell and to hold the rest of the list.
 *
 *  The init() method is used by the cons() method to initialize the
 *  cell.
 *)

class Cons inherits List {

   car : Int;	-- The element in this list cell

   cdr : List;	-- The rest of the list

   isNil() : Bool { false };

   head()  : Int { car };

   tail()  : List { cdr };

   init(i : Int, rest : List) : List {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };

};



(*
 *  The Main class shows how to use the List class. It creates a small
 *  list and then repeatedly prints out its elements and takes off the
 *  first element of the list.
 *)

class Main inherits IO {

   mylist : List;

   -- Print all elements of the list. Calls itself recursively with
   -- the tail of the list, until the end of the list is reached.

   print_list(l : List) : Object {
      if l.isNil() then out_string("\n")
                   else {
			   out_int(l.head());
			   out_string(" ");
			   print_list(l.tail());
		        }
      fi
   };

   -- Note how the dynamic dispatch mechanism is responsible to end
   -- the while loop. As long as mylist is bound to an object of 
   -- dynamic type Cons, the dispatch to isNil calls the isNil method of
   -- the Cons class, which returns false. However when we reach the
   -- end of the list, mylist gets bound to the object that was
   -- created by the (new List) expression. This object is of dynamic type
   -- List, and thus the method isNil in the List class is called and
   -- returns true.

   main() : Object {
      {
	 mylist <- new List.cons(1).cons(2).cons(3).cons(4).cons(5);
	 while (not mylist.isNil()) loop
	    {
	       print_list(mylist);
	       mylist <- mylist.tail();
	    }
	 pool;
      }
   };

};



class Main inherits IO {
    main() : SELF_TYPE {
	(let c : Complex <- (new Complex).init(1, 1) in
	    {
	        -- trivially equal (see CoolAid)
	        if c.reflect_X() = c.reflect_0()
	        then out_string("=)\n")
	        else out_string("=(\n")
	        fi;
		-- equal
	        if c.reflect_X().reflect_Y().equal(c.reflect_0())
	        then out_string("=)\n")
	        else out_string("=(\n")
	        fi;
	    }
	)
    };
};

class Complex inherits IO {
    x : Int;
    y : Int;

    init(a : Int, b : Int) : Complex {
	{
	    x = a;
	    y = b;
	    self;
	}
    };

    print() : Object {
	if y = 0
	then out_int(x)
	else out_int(x).out_string("+").out_int(y).out_string("I")
	fi
    };

    reflect_0() : Complex {
	{
	    x = ~x;
	    y = ~y;
	    self;
	}
    };

    reflect_X() : Complex {
	{
	    y = ~y;
	    self;
	}
    };

    reflect_Y() : Complex {
	{
	    x = ~x;
	    self;
	}
    };

    equal(d : Complex) : Bool {
	if x = d.x_value()
	then
	    if y = d.y_value()
	    then true
	    else false
	    fi
	else false
	fi
    };

    x_value() : Int {
	x
    };

    y_value() : Int {
	y
    };
};
class Main inherits IO {
    pal(s : String) : Bool {
	if s.length() = 0
	then true
	else if s.length() = 1
	then true
	else if s.substr(0, 1) = s.substr(s.length() - 1, 1)
	then pal(s.substr(1, s.length() -2))
	else false
	fi fi fi
    };

    i : Int;

    main() : SELF_TYPE {
	{
            i <- ~1;
	    out_string("enter a string\n");
	    if pal(in_string())
	    then out_string("that was a palindrome\n")
	    else out_string("that was not a palindrome\n")
	    fi;
	}
    };
};

(*
 * methodless-primes.cl
 *
 * Designed by Jesse H. Willett, jhw@cory, 11103234, with 
 *             Istvan Siposs, isiposs@cory, 12342921.
 *
 * This program generates primes in order without using any methods.
 * Actually, it does use three methods: those of IO to print out each prime, and
 * abort() to halt the program.  These methods are incidental, however,
 * to the information-processing functionality of the program.  We
 * could regard the attribute 'out's sequential values as our output,
 * and the string "halt" as our terminate signal.
 *
 * Naturally, using Cool this way is a real waste, basically reducing it 
 * to assembly without the benefit of compilation.  
 *
 * There could even be a subroutine-like construction, in that different
 * code could be in the assign fields of attributes of other classes,
 * and it could be executed by calling 'new Sub', but no parameters
 * could be passed to the subroutine, and it could only return itself.
 * but returning itself would be useless since we couldn't call methods
 * and the only operators we have are for Int and Bool, which do nothing
 * interesting when we initialize them!
 *)

class Main inherits IO {

  main() : Int {	-- main() is an atrophied method so we can parse. 
    0 
  };

  out : Int <-		-- out is our 'output'.  It's values are the primes.
    {
      out_string("2 is trivially prime.\n");
      2;
    };

  testee : Int <- out;	-- testee is a number to be tested for primeness.   

  divisor : Int;	-- divisor is a number which may factor testee.

  stop : Int <- 500;	-- stop is an arbitrary value limiting testee. 	

  m : Object <-		-- m supplants the main method.
    while true loop 
      {

        testee <- testee + 1;
        divisor <- 2;

        while 
          if testee < divisor * divisor 
            then false 		-- can stop if divisor > sqrt(testee).
	  else if testee - divisor*(testee/divisor) = 0 
            then false 		-- can stop if divisor divides testee. 
            else true
          fi fi     
        loop 
          divisor <- divisor + 1
        pool;        

        if testee < divisor * divisor	-- which reason did we stop for?
        then 	-- testee has no factors less than sqrt(testee).
          {
            out <- testee;	-- we could think of out itself as the output.
            out_int(out); 
            out_string(" is prime.\n");
          }
        else	-- the loop halted on testee/divisor = 0, testee isn't prime.
          0	-- testee isn't prime, do nothing.
	fi;   	

        if stop <= testee then 
          "halt".abort()	-- we could think of "halt" as SIGTERM.
        else 
          "continue"
        fi;       

      } 
    pool;

}; (* end of Main *)

(*
   This file presents a fairly large example of Cool programming.  The
class List defines the names of standard list operations ala Scheme:
car, cdr, cons, isNil, rev, sort, rcons (add an element to the end of
the list), and print_list.  In the List class most of these functions
are just stubs that abort if ever called.  The classes Nil and Cons
inherit from List and define the same operations, but now as
appropriate to the empty list (for the Nil class) and for cons cells (for
the Cons class).

The Main class puts all of this code through the following silly 
test exercise:

   1. prompt for a number N
   2. generate a list of numbers 0..N-1
   3. reverse the list
   4. sort the list
   5. print the sorted list

Because the sort used is a quadratic space insertion sort, sorting
moderately large lists will cause spim to run out of memory.
*)

Class List inherits IO { 
        (* Since abort() returns Object, we need something of
	   type Bool at the end of the block to satisfy the typechecker. 
           This code is unreachable, since abort() halts the program. *)
	isNil() : Bool { { abort(); true; } };

	cons(hd : Int) : Cons {
	  (let new_cell : Cons <- new Cons in
		new_cell.init(hd,self)
	  )
	};

	(* 
	   Since abort "returns" type Object, we have to add
	   an expression of type Int here to satisfy the typechecker.
	   This code is, of course, unreachable.
        *)
	car() : Int { { abort(); new Int; } };

	cdr() : List { { abort(); new List; } };

	rev() : List { cdr() };

	sort() : List { cdr() };

	insert(i : Int) : List { cdr() };

	rcons(i : Int) : List { cdr() };
	
	print_list() : Object { abort() };
};

Class Cons inherits List {
	xcar : Int;  -- We keep the car in cdr in attributes.
	xcdr : List; -- Because methods and features must have different names,
		     -- we use xcar and xcdr for the attributes and reserve
		     -- cons and car for the features.

	isNil() : Bool { false };

	init(hd : Int, tl : List) : Cons {
	  {
	    xcar <- hd;
	    xcdr <- tl;
	    self;
	  }
	};
	  
	car() : Int { xcar };

	cdr() : List { xcdr };

	rev() : List { (xcdr.rev()).rcons(xcar) };

	sort() : List { (xcdr.sort()).insert(xcar) };

	insert(i : Int) : List {
		if i < xcar then
			(new Cons).init(i,self)
		else
			(new Cons).init(xcar,xcdr.insert(i))
		fi
	};


	rcons(i : Int) : List { (new Cons).init(xcar, xcdr.rcons(i)) };

	print_list() : Object {
		{
		     out_int(xcar);
		     out_string("\n");
		     xcdr.print_list();
		}
	};
};

Class Nil inherits List {
	isNil() : Bool { true };

        rev() : List { self };

	sort() : List { self };

	insert(i : Int) : List { rcons(i) };

	rcons(i : Int) : List { (new Cons).init(i,self) };

	print_list() : Object { true };

};


Class Main inherits IO {

	l : List;

	(* iota maps its integer argument n into the list 0..n-1 *)
	iota(i : Int) : List {
	    {
		l <- new Nil;
		(let j : Int <- 0 in
		   while j < i 
		   loop 
		     {
		       l <- (new Cons).init(j,l);
		       j <- j + 1;
		     } 
		   pool
		);
		l;
	    }
	};		

	main() : Object {
	   {
	     out_string("How many numbers to sort?");
	     iota(in_int()).rev().sort().print_list();
	   }
	};
};			    





class A {
ana(): Int {
(let x:Int <- 1 in 2)+3
};
};

Class BB__ inherits A {
};
