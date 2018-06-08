

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


      } 
    pool;

}; (* end of Main *)

