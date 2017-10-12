import scala.math._
object Mathematics
{
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 1
  def solve(a: Double, b: Double, c: Double) =
  {
  	val discriminant = b * b - (4 * a * c)
  		if (a == 0) None
  		else if (discriminant < 0) None
  		else
  			{
  				val root1 = (-b + sqrt(discriminant)) / (2 * a)
  				val root2 = (-b - sqrt(discriminant)) / (2 * a)
  				Some((root1, root2))
  			}
  }                                               //> solve: (a: Double, b: Double, c: Double)Option[(Double, Double)]
  // Test
  solve(2, -2, -4)                                //> res0: Option[(Double, Double)] = Some((2.0,-1.0))
  solve(1, 0, 1)                                  //> res1: Option[(Double, Double)] = None
  solve(1, 0, -1)                                 //> res2: Option[(Double, Double)] = Some((1.0,-1.0))
  solve(0, 1, 1)                                  //> res3: Option[(Double, Double)] = None
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 2
  def dist(p1: (Double, Double), p2: (Double, Double)) =
  {
  	val (x1, y1) = p1
  	val (x2, y2) = p2
  	val distance = sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2))
  	distance
  }                                               //> dist: (p1: (Double, Double), p2: (Double, Double))Double
  // Test
  dist((1, 1), (0, 0))                            //> res4: Double = 1.4142135623730951
  dist((3, 0), (0, 0))                            //> res5: Double = 3.0
  dist((23, 1), (23, 1))                          //> res6: Double = 0.0
  dist((-2, 1), (-3, -1))                         //> res7: Double = 2.23606797749979
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 3
  def dot(dim1: (Double, Double, Double), dim2: (Double, Double, Double)) =
  {
  	val (x1, y1, z1) = dim1
  	val (x2, y2, z2) = dim2
  	val dotProd = (x1 * x2) + (y1 * y2) + (z1 * z2)
  	dotProd
  }                                               //> dot: (dim1: (Double, Double, Double), dim2: (Double, Double, Double))Double
                                                  //| 
 	// Test
 	dot((2.0, 3, 4), (2, 2.0, 2))             //> res8: Double = 18.0
 	dot((9.0, 2, 7), (4, 8.0, 10))            //> res9: Double = 122.0
 	
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 // Problem 6
 def isPrime(x: Double) =
 	x match
 	{
 		case x if x < 0 => throw new Exception ("Input must be non-negative")
 		case x if (x > 1 && x < 4) => true
 		case x if (x == 5 || x == 7) => true
 		case x if (x % 2 == 0 || x % 3 == 0 || x % 5 == 0 || x % 7 == 0) => false
 		case _ => true
 	}                                         //> isPrime: (x: Double)Boolean
 // Test
  isPrime(21377)                                  //> res10: Boolean = true
 	isPrime(1)                                //> res11: Boolean = true
 	isPrime(3)                                //> res12: Boolean = true
 	isPrime(5)                                //> res13: Boolean = true
 	isPrime(7)                                //> res14: Boolean = true
 	isPrime(49)                               //> res15: Boolean = false
 try
 {
 	isPrime(-1212)
 }
 catch
 {
 	case e: Exception => println(e)
 }                                                //> java.lang.Exception: Input must be non-negative
                                                  //| res16: AnyVal = ()
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 // Problem 7

		def phi(n: Int) =
		{
			var phiValue = 0
			val prime = isPrime(n)
			if (prime == true)
			{
				phiValue = n - 1
			}
			else
			{
				phiValue = 1
				for (i <- 2 to n)
				{
					var remainder = n % i
					if ((n % 2 == 0 && remainder == 2) || (n % 2 == 1 && remainder == 1))
					{
							phiValue += 1
					}
					else if (remainder != 0)
					{
						if (n % remainder != 0)
						{
								remainder = n % remainder
						}
					}
				}
			}
			phiValue
		}                                 //> phi: (n: Int)Int
		// Test
		phi(9)                            //> res17: Int = 4
		phi(10)                           //> res18: Int = 3
		phi(8)                            //> res19: Int = 3
		phi(7)                            //> res20: Int = 6
		
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 // Problem 8
 def rollDice() =
 {
 		val start = 1
 		val end = 6
 		val rand = new scala.util.Random
 		val val1 = start + rand.nextInt( (end - start) + 1)
 		val val2 = start + rand.nextInt( (end - start) + 1)
 		(val1, val2)
 }                                                //> rollDice: ()(Int, Int)
 		// Test
 		rollDice()                        //> res21: (Int, Int) = (3,5)
 		rollDice()                        //> res22: (Int, Int) = (1,2)
		rollDice()                        //> res23: (Int, Int) = (2,1)
		rollDice()                        //> res24: (Int, Int) = (4,4)
		rollDice()                        //> res25: (Int, Int) = (6,2)
		rollDice()                        //> res26: (Int, Int) = (1,3)
		rollDice()                        //> res27: (Int, Int) = (4,5)
		rollDice()                        //> res28: (Int, Int) = (4,4)
		rollDice()                        //> res29: (Int, Int) = (1,1)
		rollDice()                        //> res30: (Int, Int) = (3,1)
		rollDice()                        //> res31: (Int, Int) = (4,4)
		rollDice()                        //> res32: (Int, Int) = (3,6)
		rollDice()                        //> res33: (Int, Int) = (3,3)
		rollDice()                        //> res34: (Int, Int) = (5,1)
		rollDice()                        //> res35: (Int, Int) = (6,6)
		rollDice()                        //> res36: (Int, Int) = (6,2)
}