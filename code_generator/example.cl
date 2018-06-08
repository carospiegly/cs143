class Z {
        a : Int;
        b : Bool;
        init(x : Int, y : Bool) : C {
           {
                a <- x;
                b <- y;
                self;
           }
        };
};

Class A {
        main2():C {
          foo(new C)
        };
        foo(a:C):C {
          a.init(1,true)
        };
};

Class B {
	b:Int <- 5;
	c:Int <- 10;
	d:Int <- b + c;
        main3():Object {
		(new IO).out_int( d )
        };
};
Class C {
        main4():Int {
        	5
	};
};
class D {
        a : Int;
        b : Bool;
        init(x : Int, y : Bool) : C {
           {
                a <- x;
                b <- y;
                self;
           }
        };
};

Class E {
        main5():C {
          foo(new C)
        };
        foo(a:C):C {
          a.init(1,true)
        };
};

Class F {
	a:IO;
        main6():Object {
		(new IO).out_int(5)  
        };

Class Y inherits A{
        a : Int;
=======
Class C2 {
        foo(x : Int) : Int {
           {

              Int a <- x;
           }
        };
};

Class W {

b : Int; 

};


Class Main {
        main():Int {
                5 + 7 
        };
};
