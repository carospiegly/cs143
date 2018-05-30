
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
Class C {
        a : Int;
        b : Bool;
        d : String;
        e : C;
        init(x : Int, y : Bool) : C {
           {
                a <- x;
                b <- y;
                self;
           }
        };
       foo(): SELF_TYPE { self };
};
Class A inherits C{
i: Int;
foo(): SELF_TYPE { self };
};

Class Main {
        b : C;
        main():C {
          (new C).init(1,true)
        };
};
