
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
Class C {
        a : Int;
        b : Bool;
        d : String;
        e : C;
        foo(x : Int) : Int {
           {
                a <- x;
           }
        };
};
Class A inherits C{
i: Int;
};

Class Main {
        b : C;
        main():Int {
          (new C).foo(1)
        };
};
