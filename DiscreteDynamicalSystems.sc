object DiscreteDynamicalSystems {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 1
  def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
  	// - If the state is the halting state, return that state
  	if (halt(state, cycle)) state
  	// - Otherwise, keep updating the system with this recursive call
  	else controlLoop(update(state, cycle), cycle + 1, halt, update)
                                                  //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S
                                                  //| , Int) => S)S
  	// - Must use tail recursion b/c this can be a huge call which will
  	// create a stack overflow error if we use iteration or classical recursion
  	
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 2
  	val finalPop = controlLoop(1, 0, (p: Int, t: Int) => p >= 10000, (p: Int, t: Int) => 2 * p)
                                                  //> finalPop  : Int = 16384
  	finalPop                                  //> res0: Int = 16384
  	
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 3
  
  	val delta = 1e-5                          //> delta  : Double = 1.0E-5
  	
  	def deriv(f: Double => Double): Double => Double =
  	{
  		def df(x: Double) = (f(x + delta) - f(x)) / delta
  		// return the function
  		df _
  	}                                         //> deriv: (f: Double => Double)Double => Double
  	
  	def foo(x: Double) = 3 * x * x            //> foo: (x: Double)Double
  	val dfoo = deriv(foo)                     //> dfoo  : Double => Double = DiscreteDynamicalSystems$$$Lambda$11/873415566@1
                                                  //| 9dfb72a
  	dfoo(10)                                  //> res1: Double = 60.00002999826392
  	
  	def solve(f: Double => Double):  Double =
  	{
  		def df = deriv(f)
  		// this function will help you figure out if your guess is close enough to 0 when you do f(guess)
  		def goodEnuf(guess: Double, c: Int) = math.abs(f(guess)) <= delta
  		def improve(guess: Double, c: Int) = guess - f(guess)/df(guess)
  		controlLoop(1.0, 0, goodEnuf, improve)
  	}                                         //> solve: (f: Double => Double)Double
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 4
  	def squareRoot(n: Double) = solve((x: Double) => x * x - n)
                                                  //> squareRoot: (n: Double)Double
  	squareRoot(81)                            //> res2: Double = 9.000000000013383
  	squareRoot(49)                            //> res3: Double = 7.000000142285558
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 5
  def cubeRoot(n: Double) = solve((x: Double) => x * x * x - n)
                                                  //> cubeRoot: (n: Double)Double
  
  cubeRoot(8)                                     //> res4: Double = 2.000000000036784
  cubeRoot(27)                                    //> res5: Double = 3.0000000000019176
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 6
  def nthRoot(x: Double, n: Int) = solve((z: Double) => Math.pow(z, n) - x)
                                                  //> nthRoot: (x: Double, n: Int)Double
  nthRoot(9,2)                                    //> res6: Double = 3.0000000015508212
  nthRoot(4,2)                                    //> res7: Double = 2.0000000944796694
  nthRoot(27, 3)                                  //> res8: Double = 3.000000000001917
  nthRoot(81, 2)
}
