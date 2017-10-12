object FunctionalProgramming {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 1
  def compose[T](f: T => T, g: T => T): T => T =
  {
 		def r(x: T): T = f(g(x));
  	r _
  }                                               //> compose: [T](f: T => T, g: T => T)T => T
  //def compose(f: Int=>Int, g: Int=>Int): Int=>Int = {
   //def r(x: Int): Int = f(g(x));
  // r _
	//}
	def mul3(n: Int) = n * 3                  //> mul3: (n: Int)Int
	def sq(n: Int) = n * n                    //> sq: (n: Int)Int
	val derp = compose(sq _, mul3 _)          //> derp  : Int => Int = FunctionalProgramming$$$Lambda$10/1209271652@6e8dacdf
	val pred = compose(mul3 _, sq _)          //> pred  : Int => Int = FunctionalProgramming$$$Lambda$10/1209271652@b684286
	derp(2)                                   //> res0: Int = 36
	pred(2)                                   //> res1: Int = 12
	def append5(s: String) = s + "5"          //> append5: (s: String)String
	def append0(s: String) = s + "0"          //> append0: (s: String)String
	val app50 = compose(append0 _, append5 _) //> app50  : String => String = FunctionalProgramming$$$Lambda$10/1209271652@7f6
                                                  //| 3425a
	app50("Wtf")                              //> res2: String = Wtf50
	
	//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 2
  def selfIter[T](f: T => T, n: Int) =
  {
  	def loop(count: Int, result: T => T): T => T =
  		if (count == n) result else loop(count + 1, compose(f, result))
  	loop(1, f)
  }                                               //> selfIter: [T](f: T => T, n: Int)T => T
  def inc(x: Double) = 1 + x                      //> inc: (x: Double)Double
  def double(x: Double) = 2 * x                   //> double: (x: Double)Double
  
 	val inc2 = selfIter(inc _, 2)             //> inc2  : Double => Double = FunctionalProgramming$$$Lambda$10/1209271652@59a
                                                  //| 6e353
 	inc2(0)                                   //> res3: Double = 2.0
 	inc2(1)                                   //> res4: Double = 3.0
 	inc2(2)                                   //> res5: Double = 4.0
 	
 	val double2 = selfIter(double _, 2)       //> double2  : Double => Double = FunctionalProgramming$$$Lambda$10/1209271652@
                                                  //| 71be98f5
 	double2(0)                                //> res6: Double = 0.0
 	double2(1)                                //> res7: Double = 4.0
 	double2(2)                                //> res8: Double = 8.0
  
  val double3 = selfIter(double _, 3)             //> double3  : Double => Double = FunctionalProgramming$$$Lambda$10/1209271652@
                                                  //| 17f6480
  
  double3(0)                                      //> res9: Double = 0.0
  double3(1)                                      //> res10: Double = 8.0
  double3(2)                                      //> res11: Double = 16.0
  
  val append55 = selfIter(append5 _, 10)          //> append55  : String => String = FunctionalProgramming$$$Lambda$10/1209271652
                                                  //| @2812cbfa
  append55("Hello")                               //> res12: String = Hello5555555555
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 3
  def countPass[T](f: T => Boolean, l: Array[T]) =
  {
  	var count = 0;
  	for (elem <- l if f(elem) == true)
  	{
  		count += 1
  	}
  	count
  }                                               //> countPass: [T](f: T => Boolean, l: Array[T])Int
  val a1 = Array(1, 2, 3, 4, 5)                   //> a1  : Array[Int] = Array(1, 2, 3, 4, 5)
  val a2 = Array(2, 4, 6, 8, 10)                  //> a2  : Array[Int] = Array(2, 4, 6, 8, 10)
  def isEven(i: Int) =
  {
  	if (i % 2 == 0) true
  	else false
  }                                               //> isEven: (i: Int)Boolean
 
 	countPass(isEven _, a1)                   //> res13: Int = 2
 	countPass(isEven _, a2)                   //> res14: Int = 5
 	//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 4
  def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int =
  {
  	if (baseVal == 1) ((x: Int) => 1)
  	else recur(baseVal - 1, combiner)
  }                                               //> recur: (baseVal: Int, combiner: (Int, Int) => Int)Int => Int
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 5
  def deOptionize[T] (f: T => Option[T]) =
  {
  	//def toDeOptionize (x: Option[T]): T =
  	//compose(f _, toDeOptionize _)
  }                                               //> deOptionize: [T](f: T => Option[T])Unit
}