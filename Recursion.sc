object Recursion {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def inc(n: Int) = n + 1                         //> inc: (n: Int)Int
  def dec(n: Int) = n - 1                         //> dec: (n: Int)Int
  def isZero(n: Int) = n == 0                     //> isZero: (n: Int)Boolean
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 1
  def add(n: Int, m: Int) =
  {
  	def loop(count: Int, result: Int): Int =
  		if (n+m < count) result else	loop(inc(count), count)
  	loop(0,0)
  }                                               //> add: (n: Int, m: Int)Int
  // Test
  add(2, 4)                                       //> res0: Int = 6
  add(10, 1)                                      //> res1: Int = 11
  add(20, 20)                                     //> res2: Int = 40
  add(0, 0)                                       //> res3: Int = 0
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 2
  def mul(n: Int, m: Int) =
  {
  	def loop(count: Int, result: Int): Int =
  		if (n*m < count) result else loop(add(count, n), count)
  	loop(0,0)
  }                                               //> mul: (n: Int, m: Int)Int
  // Test
  mul(3, 1)                                       //> res4: Int = 3
  mul(5, 0)                                       //> res5: Int = 0
  mul(10, 10)                                     //> res6: Int = 100
  mul(10000,1000)                                 //> res7: Int = 10000000
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 3
  def exp2(m: Int) =
  {
  	def loop(count: Int, result: Int): Int =
  		if (Math.pow(2, m) < count) result else loop((mul(2,1) * count), count)
  	loop(1,1)
  }                                               //> exp2: (m: Int)Int
  // Test
  exp2(0)                                         //> res8: Int = 1
  exp2(1)                                         //> res9: Int = 2
  exp2(4)                                         //> res10: Int = 16
  exp2(10)                                        //> res11: Int = 1024
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 4
  def hyperExp(n: Int) =
  {
  	def loop(count: Int, result: Int): Int =
  		if (n < count) result else loop(count + 1, exp2(result))
  	loop(1,1)
  }                                               //> hyperExp: (n: Int)Int
  // Test
  hyperExp(0)                                     //> res12: Int = 1
  hyperExp(1)                                     //> res13: Int = 2
  hyperExp(2)                                     //> res14: Int = 4
  hyperExp(3)                                     //> res15: Int = 16
  hyperExp(4)                                     //> res16: Int = 65536
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 5
  // Everything above is written in Tail Recursion
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 9
  // Classic Recursion
  def fib(n: Int): Int =
  	if (n == 0) 0
  	else if (n == 1) 1
  	else fib(n-2) + fib(n-1)                  //> fib: (n: Int)Int
  
  // Tail Recursion
  def fibTail(n: Int) =
  {
  	def loop(count: Int, result: Int): Int =
  		if (n < count) result else if (n == 1) 1 else loop(count + 1, fib(count))
  	loop(0,0)
  }                                               //> fibTail: (n: Int)Int
  // Test
  fib(0)                                          //> res17: Int = 0
  fib(2)                                          //> res18: Int = 1
  fib(3)                                          //> res19: Int = 2
  fib(10)                                         //> res20: Int = 55
  
  fibTail(0)                                      //> res21: Int = 0
  fibTail(20)                                     //> res22: Int = 6765
  fibTail(2)                                      //> res23: Int = 1
  fibTail(3)                                      //> res24: Int = 2
  fibTail(10)                                     //> res25: Int = 55
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 10
  def choose(n: Int, m: Int) =
  {
  	def fact(n: Int): Int = if (n == 0 || n ==1 ) 1 else n * fact(n - 1)
  	if (m == 1) n
  	else if (m == 0) 1
  	else fact(n) / fact(m) * fact(n-m)
  }                                               //> choose: (n: Int, m: Int)Int
  // Test
  choose(4, 0)                                    //> res26: Int = 1
  choose(3, 2)                                    //> res27: Int = 3
  choose(5, 1)                                    //> res28: Int = 5
}