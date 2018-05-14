Class A {
var : Int <- 0;
value() : Int { var };
set_var(num : Int) : SELF_TYPE {
{
var <- num;
self;
}
};
method1(num : Int) : SELF_TYPE {  -- same
self
};

};
