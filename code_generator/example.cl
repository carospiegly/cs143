
Class C inherits A{
        a : Int;
        foo(x : Int) : Int {
           {
                a <- x;
           }
        };
};

Class A {

b : Int; 

};


Class Main {
        main():Int {
          (new C).foo(1)
        };
};
