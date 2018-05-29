
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
Class C {
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

Class Main {
        main():C {
          (new C).init(1,true)
        };
};
