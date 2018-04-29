(*
 *  A contribution from Anne Sheets (sheets@cory)
 *
 *  Tests the arithmetic operations and various other things
 *)

class A {

   var : Int <- 0;

   value() : Int { var };
};

class Main inherits IO {
   is_even(num : Int) : Bool {
      (let x : Int <- num in
	if 
		x < 0
	then 
		is_even(~x) 
	else
		if 
			0 = x 
		then 
			true 
		else
			if 
				1 = x 
			then 
				false 
			else
				is_even(x - 2)
			fi 
		fi 
	fi
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
};
