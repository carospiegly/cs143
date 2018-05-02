
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* Begin Class List Tests *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};


(* Begin Feature List Tests *)

(* error: Bad feature *)
Class E inherits A {

value( : Int { }; 

};

(* error: Bad feature in middle of list of features *)
Class E inherits A {

value() : Int { }; 
value( : Int { };
value() : Int { };

};

(* error: Bad feature at end of list of features *)
Class E inherits A {

value() : Int { }; 
value() : Int { };
value( : Int { };

};

(* Begin Let Binding Tests *)

(* error: Singular bad let binding *)
Class A {
	
value() : Int { let caroline : sdfadf in caroline <- 8 + 8 }; 

};

(* error: Bad let binding begins list of let bindings *)
Class A {
	
value() : Int { let caroline : sdfadf, caroline : Int, john : Int in 8 }; 

};

(* error: Bad let binding ends list of let bindings *)
Class A {
	
value() : Int { let caroline : A, caroline : Int, john : dsaidj in 8 }; 

};

(* error: Bad let binding in the middle of a list of let bindings *)
Class A {
	
value() : Int { let caroline : Int, caroline : sdfadf, john : Int in 8 }; 

};

(* error: Multiple bad let bindings *)
Class A {
	
value() : Int { let caroline : dsfd, caroline : sdfadf, john : Int in 8 }; 

};

(* Begin Block Tests *)

(* error: Singular bad block *)
Class A {
	
value() : Int { { foo bla; } }; 

};

(* error: Bad block begins *)
Class A {
	
value() : Int { { foo bla; 8; 7; } }; 

};

(* error: Bad block ends *)
Class A {
	
value() : Int { { 8; 7; foo bla;} }; 

};

(* error: Bad block in middle *)
Class A {
	
value() : Int { { 8; foo bla; 7;} }; 

};

(* error: No blocks *)

Class A {
	
value() : Int { { ;} }; 

};
