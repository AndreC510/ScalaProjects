import java.lang.Comparable
import scala.util.control._
object ListProcessing {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //problem 1 1 2 3 6 7 8 10 13
  def square(n: Int) = n * n                      //> square: (n: Int)Int
  def cube(n: Int) = n * n * n                    //> cube: (n: Int)Int
  def isEven(n: Int) = n % 2 == 0                 //> isEven: (n: Int)Boolean
  def isOdd(n: Int) = n % 2 != 0                  //> isOdd: (n: Int)Boolean
  def isPal(s: String) = s == s.reverse           //> isPal: (s: String)Boolean
  def isPrime(n: Int) =
  {
  	if (n < 2) false
  	else
  	{
  		var result = true
  		for (i <- 2 until n if result)
  		{
  			result = n % i != 0
  		}
  		result
  	}
  }                                               //> isPrime: (n: Int)Boolean
  val nums1 = List(1,2,3,4,5)                     //> nums1  : List[Int] = List(1, 2, 3, 4, 5)
  val nums2 = List(6,7,8,9,10)                    //> nums2  : List[Int] = List(6, 7, 8, 9, 10)
  val list = List(nums1, nums2)                   //> list  : List[List[Int]] = List(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10))
  val list2 = List(nums1, List())                 //> list2  : List[List[Int]] = List(List(1, 2, 3, 4, 5), List())
  val list3 = List(nums2, List(10, 11, 12))       //> list3  : List[List[Int]] = List(List(6, 7, 8, 9, 10), List(10, 11, 12))
  val nums3 = List(1)                             //> nums3  : List[Int] = List(1)
  val nums4 = List(2, 4)                          //> nums4  : List[Int] = List(2, 4)
  val list7 = List(nums3, nums4)                  //> list7  : List[List[Int]] = List(List(1), List(2, 4))
  val strings1 = List("Hello", "BRB", "mom")      //> strings1  : List[String] = List(Hello, BRB, mom)
  val strings2 = List("Hello", "123", "owls")     //> strings2  : List[String] = List(Hello, 123, owls)
  val strings3 = List("121", "racecar", "wow")    //> strings3  : List[String] = List(121, racecar, wow)
  
  //iterative solution
  def iter_SumOfOddCubes(nums: List[Int]) = {
  var result = 0
  for (i <- nums if isOdd(i)) result += cube(i)
  result
  }                                               //> iter_SumOfOddCubes: (nums: List[Int])Int
  // Test
  iter_SumOfOddCubes(nums1)                       //> res0: Int = 153
  iter_SumOfOddCubes(nums2)                       //> res1: Int = 1072
  iter_SumOfOddCubes(nums3)                       //> res2: Int = 1
  iter_SumOfOddCubes(nums4)                       //> res3: Int = 0
  
  //recursive solution
  def recur_SumOfOddCubes(nums: List[Int]): Int =
  	if (nums == Nil) 0
  	else if (isOdd(nums.head)) cube(nums.head) + recur_SumOfOddCubes(nums.tail)
  	else recur_SumOfOddCubes(nums.tail)       //> recur_SumOfOddCubes: (nums: List[Int])Int
  // Test
  	recur_SumOfOddCubes(nums1)                //> res4: Int = 153
  	recur_SumOfOddCubes(nums2)                //> res5: Int = 1072
  	recur_SumOfOddCubes(nums3)                //> res6: Int = 1
  	recur_SumOfOddCubes(nums4)                //> res7: Int = 0
  	
  //tail recursive solution
  def tailrecur_SumOfOddCubes(nums: List[Int]) =
  {
  	def helper(count: Int, result: Int): Int =
  		if (count == nums.length) result
  		else
  		{
  			if (isOdd(nums(count))) helper(count + 1, result + cube(nums(count)))
  			else helper(count + 1, result)
  		}
  		helper(0,0)
  }                                               //> tailrecur_SumOfOddCubes: (nums: List[Int])Int
  // Test
  tailrecur_SumOfOddCubes(nums1)                  //> res8: Int = 153
  tailrecur_SumOfOddCubes(nums2)                  //> res9: Int = 1072
  tailrecur_SumOfOddCubes(nums3)                  //> res10: Int = 1
  tailrecur_SumOfOddCubes(nums4)                  //> res11: Int = 0
  
  //map-filter-reduce solution
  def mfr_SumOfOddCubes(nums: List[Int]) = nums.filter(isOdd _).map(cube _).reduce(_ + _)
                                                  //> mfr_SumOfOddCubes: (nums: List[Int])Int
// Test
  mfr_SumOfOddCubes(nums1)                        //> res12: Int = 153
  mfr_SumOfOddCubes(nums2)                        //> res13: Int = 1072
  mfr_SumOfOddCubes(nums3)                        //> res14: Int = 1
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 2
  //iterative solution
  def iter_SumOfSums(list: List[List[Int]]) =
  {
  	var sum = 0;
  	for (i <- 0 until list.length)
  	{
  		for (j <- 0 until list(i).length) sum = sum + list(i)(j)
  	}
  	sum
  }                                               //> iter_SumOfSums: (list: List[List[Int]])Int
  // Test
  iter_SumOfSums(list)                            //> res15: Int = 55
  iter_SumOfSums(list2)                           //> res16: Int = 15
  iter_SumOfSums(list3)                           //> res17: Int = 73
  iter_SumOfSums(list7)                           //> res18: Int = 7
  iter_SumOfSums(List(List(), List()))            //> res19: Int = 0
  iter_SumOfSums(List())                          //> res20: Int = 0
  
  //recursive solution
  def recur_SumOfSums(list: List[List[Int]]): Int =
  	if (list == Nil || (list.head == Nil && list.tail.head == Nil)) {
  	0
  	}
  	else if (list.head == Nil){
  	 list.tail.head.head + recur_SumOfSums(Nil :: list.tail.head.tail :: Nil)
  	 } //list.head ::
  	else if (list.tail.head == Nil)
  	{
  	list.head.head + recur_SumOfSums(list.head.tail :: list.tail)
  	}
  	else {
  	list.head.head + list.tail.head.head + recur_SumOfSums(list.head.tail :: list.tail.head.tail :: Nil)
  	}                                         //> recur_SumOfSums: (list: List[List[Int]])Int
	// Test
  recur_SumOfSums(list)                           //> res21: Int = 55
  recur_SumOfSums(list2)                          //> res22: Int = 15
  recur_SumOfSums(list3)                          //> res23: Int = 73
  recur_SumOfSums(list7)                          //> res24: Int = 7
  recur_SumOfSums(List(List(), List()))           //> res25: Int = 0
  recur_SumOfSums(List())                         //> res26: Int = 0
  
  //tail recursive solution
  def tailrecur_SumOfSums(list: List[List[Int]]) =
  {
  	def loop(outerIndex: Int, innerIndex: Int, result: Int): Int =
  		if (list == Nil || (list.head == Nil && list.tail == Nil)) result
  		else if (outerIndex == list.size - 1 && innerIndex == list.tail.head.size - 1) result + list(outerIndex)(innerIndex)
  		else if (innerIndex == list(outerIndex).size - 1 && list(1).size == 0) result + list(outerIndex)(innerIndex)
  		else if (innerIndex == list(outerIndex).size - 1 && list(outerIndex + 1)(0) != Nil) loop(outerIndex + 1, 0, result + list(outerIndex)(innerIndex))
  		else loop(outerIndex, innerIndex + 1, result + list(outerIndex)(innerIndex))
		loop(0,0,0)
  }                                               //> tailrecur_SumOfSums: (list: List[List[Int]])Int
  //Test
  tailrecur_SumOfSums(List())                     //> res27: Int = 0
  tailrecur_SumOfSums(List(List()))               //> res28: Int = 0
  tailrecur_SumOfSums(list)                       //> res29: Int = 55
  tailrecur_SumOfSums(list2)                      //> res30: Int = 15
  tailrecur_SumOfSums(list3)                      //> res31: Int = 73
  tailrecur_SumOfSums(list7)                      //> res32: Int = 7
  
  //map-filer-reduce solution
  def mfr_SumOfSums(list: List[List[Int]]) =
  if (list == Nil || (list.head == Nil && list.tail == Nil)) 0
  else if (list.head == Nil && list.tail.head != Nil) list.tail.head.reduce(_ + _)
  else if (list.head != Nil && list.tail.head == Nil) list.head.reduce(_ + _)
  else list.head.reduce(_ + _) + list.tail.head.reduce(_ + _)
                                                  //> mfr_SumOfSums: (list: List[List[Int]])Int
  
  mfr_SumOfSums(List())                           //> res33: Int = 0
  mfr_SumOfSums(List(List()))                     //> res34: Int = 0
  mfr_SumOfSums(list)                             //> res35: Int = 55
  mfr_SumOfSums(list2)                            //> res36: Int = 15
  mfr_SumOfSums(list3)                            //> res37: Int = 73
  mfr_SumOfSums(list7)                            //> res38: Int = 7
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 3
  // no iterative or map-reduce version of this problem
  val depthList1 = List(List(3, List(4)), List(List(5)), 6, List(7))
                                                  //> depthList1  : List[Any] = List(List(3, List(4)), List(List(5)), 6, List(7))
                                                  //| 
  val depthList2 = List(List(3, List(4, List(5))), List(List(List(List(List(6))))))
                                                  //> depthList2  : List[List[Any]] = List(List(3, List(4, List(5))), List(List(L
                                                  //| ist(List(List(6))))))
  val depthList3 = List(1)                        //> depthList3  : List[Int] = List(1)
  val depthList4 = List(1, List(2))               //> depthList4  : List[Any] = List(1, List(2))
  def depth(value: Any): Int =
  {
  	value match {
  		case first :: rest => math.max(depth(first) + 1, depth(rest)) //this is a non-empty list (something created by a cons operator)
  		case _ => 0
  		}
  }                                               //> depth: (value: Any)Int
  // Test
  depth(depthList1)                               //> res39: Int = 3
  depth(depthList2)                               //> res40: Int = 6
  depth(depthList3)                               //> res41: Int = 1
  depth(depthList4)                               //> res42: Int = 2
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 6
  // iterative solution
  def iter_NumTrueElements[T](pred: T => Boolean, vals: List[T]) =
  {
  	var result = 0
  	for (i <- 0 until vals.size)
  		if (pred(vals(i))) result += 1
  	result
  }                                               //> iter_NumTrueElements: [T](pred: T => Boolean, vals: List[T])Int
  // Test
  iter_NumTrueElements(isOdd _, List())           //> res43: Int = 0
  iter_NumTrueElements(isOdd _, nums1)            //> res44: Int = 3
  iter_NumTrueElements(isPrime _, nums1)          //> res45: Int = 3
  iter_NumTrueElements(isPal _, strings1)         //> res46: Int = 2
  iter_NumTrueElements(isPal _, strings2)         //> res47: Int = 0
  iter_NumTrueElements(isPal _, strings3)         //> res48: Int = 3
  
  // recursive solution
  def recur_NumTrueElements[T](pred: T => Boolean, vals: List[T]): Int =
  {
  	if (vals == Nil) 0
  	else if (vals.head == Nil) 0
  	else if (pred(vals.head)) 1 + recur_NumTrueElements(pred, vals.tail)
  	else recur_NumTrueElements(pred, vals.tail)
  }                                               //> recur_NumTrueElements: [T](pred: T => Boolean, vals: List[T])Int
  // Test
 	recur_NumTrueElements(isOdd _, List())    //> res49: Int = 0
  recur_NumTrueElements(isOdd _, nums1)           //> res50: Int = 3
  recur_NumTrueElements(isPrime _, nums1)         //> res51: Int = 3
  recur_NumTrueElements(isPal _, strings1)        //> res52: Int = 2
  recur_NumTrueElements(isPal _, strings2)        //> res53: Int = 0
  recur_NumTrueElements(isPal _, strings3)        //> res54: Int = 3
  
  // tail recursive solution
  def tailrecur_NumTrueElements[T](pred: T => Boolean, vals: List[T]) =
  {
  	def helper(list: List[T], result: Int) : Int =
  		if (list == Nil || list.head == Nil) result
  		else
  		{
  			if (pred(list.head)) helper(list.tail, result + 1)
  			else helper(list.tail, result)
  		}
  	helper(vals, 0)
  }                                               //> tailrecur_NumTrueElements: [T](pred: T => Boolean, vals: List[T])Int
  // Test
  tailrecur_NumTrueElements(isOdd, List())        //> res55: Int = 0
  tailrecur_NumTrueElements(isOdd _, nums1)       //> res56: Int = 3
  tailrecur_NumTrueElements(isPrime _, nums1)     //> res57: Int = 3
  tailrecur_NumTrueElements(isPal _, strings1)    //> res58: Int = 2
  tailrecur_NumTrueElements(isPal _, strings2)    //> res59: Int = 0
  tailrecur_NumTrueElements(isPal _, strings3)    //> res60: Int = 3
 
 	// mfr solution
 	def mfr_NumTrueElements[T](pred: T => Boolean, vals: List[T]) = vals.filter(pred(_)).size
                                                  //> mfr_NumTrueElements: [T](pred: T => Boolean, vals: List[T])Int
 	// Test
 	mfr_NumTrueElements(isOdd, List())        //> res61: Int = 0
 	mfr_NumTrueElements(isOdd _, nums1)       //> res62: Int = 3
 	mfr_NumTrueElements(isPrime, nums1)       //> res63: Int = 3
 	mfr_NumTrueElements(isPal _, strings1)    //> res64: Int = 2
 	mfr_NumTrueElements(isPal _, strings2)    //> res65: Int = 0
 	mfr_NumTrueElements(isPal _, strings3)    //> res66: Int = 3
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 7
  // iterative solution
	def iter_IsAllTrue[T](test: T => Boolean, vals: List[T]) =
	{
		var result = true
		for (v <- vals if result) result = result && test(v)
		result
	}                                         //> iter_IsAllTrue: [T](test: T => Boolean, vals: List[T])Boolean
	// Test
	iter_IsAllTrue(isPrime _, List(2, 3, 4, 5, 7, 11))
                                                  //> res67: Boolean = false
	iter_IsAllTrue(isOdd _, List())           //> res68: Boolean = true
	iter_IsAllTrue(isOdd, nums1)              //> res69: Boolean = false
	iter_IsAllTrue(isPrime, nums1)            //> res70: Boolean = false
	iter_IsAllTrue(isPrime, List(2,3,5,7,11)) //> res71: Boolean = true
	iter_IsAllTrue(isPal, strings1)           //> res72: Boolean = false
	iter_IsAllTrue(isPal, strings2)           //> res73: Boolean = false
	iter_IsAllTrue(isPal, strings3)           //> res74: Boolean = true
  
  // recursive solution
  def recur_IsAllTrue[T](test: T => Boolean, vals: List[T]): Boolean =
  {
  	if (vals == Nil) true
  	else test(vals.head) && recur_IsAllTrue(test, vals.tail)
  }                                               //> recur_IsAllTrue: [T](test: T => Boolean, vals: List[T])Boolean
  // Test
  recur_IsAllTrue(isPrime _, List(2, 3, 5, 7, 11))//> res75: Boolean = true
  recur_IsAllTrue(isPal _, List("mom", "rotator", "dad"))
                                                  //> res76: Boolean = true
	recur_IsAllTrue(isOdd _, nums1)           //> res77: Boolean = false
	recur_IsAllTrue(isOdd _, List())          //> res78: Boolean = true
	recur_IsAllTrue(isPrime _, nums1)         //> res79: Boolean = false
	recur_IsAllTrue(isPal _, strings1)        //> res80: Boolean = false
	recur_IsAllTrue(isPal _, strings2)        //> res81: Boolean = false
	recur_IsAllTrue(isPal _, strings3)        //> res82: Boolean = true
	
	// tail recursive solution
	def tailrecur_IsAllTrue[T](pred: T => Boolean, vals: List[T]) =
	{
		def helper(list: List[T], result: Boolean): Boolean =
			if (list == Nil) true
			else if (pred(list.head) && result == true) helper(list.tail, result)
			else false
		helper(vals, true)
	}                                         //> tailrecur_IsAllTrue: [T](pred: T => Boolean, vals: List[T])Boolean
	// Test
	tailrecur_IsAllTrue(isOdd _, List())      //> res83: Boolean = true
	tailrecur_IsAllTrue(isOdd _, nums1)       //> res84: Boolean = false
	tailrecur_IsAllTrue(isPrime _, List(2,3,5,7,11))
                                                  //> res85: Boolean = true
	tailrecur_IsAllTrue(isPal _, strings1)    //> res86: Boolean = false
	tailrecur_IsAllTrue(isPal _, strings2)    //> res87: Boolean = false
	tailrecur_IsAllTrue(isPal _, strings3)    //> res88: Boolean = true
	
	
	// mfr solution
	def mfr_IsAllTrue[T](pred: T => Boolean, vals: List[T]) = vals.filter(pred(_)).size == vals.size
                                                  //> mfr_IsAllTrue: [T](pred: T => Boolean, vals: List[T])Boolean
	// Test
	mfr_IsAllTrue(isOdd _, nums1)             //> res89: Boolean = false
	mfr_IsAllTrue(isPrime _, List(2,3,5,7))   //> res90: Boolean = true
	mfr_IsAllTrue(isOdd _, nums2)             //> res91: Boolean = false
	mfr_IsAllTrue(isPal _, strings1)          //> res92: Boolean = false
	mfr_IsAllTrue(isPal _, strings2)          //> res93: Boolean = false
	mfr_IsAllTrue(isPal _, strings3)          //> res94: Boolean = true
	
	//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 8
  // iterative solution
  def iter_IsAnyTrue[T](pred: T => Boolean, vals: List[T]) =
  {
  	var anyTrues = false
  	val loop = new Breaks
  	loop.breakable{
  	for (i <- 0 until vals.size) if (pred(vals(i))){ anyTrues = true; loop.break}
  	}
  	anyTrues
  }                                               //> iter_IsAnyTrue: [T](pred: T => Boolean, vals: List[T])Boolean
  // Test
  iter_IsAnyTrue(isOdd _, List(0, 2, 4))          //> res95: Boolean = false
  iter_IsAnyTrue(isOdd _, nums1)                  //> res96: Boolean = true
  iter_IsAnyTrue(isOdd _, List(0, 2, 4, 5))       //> res97: Boolean = true
  iter_IsAnyTrue(isPrime _, nums2)                //> res98: Boolean = true
  iter_IsAnyTrue(isOdd _, List())                 //> res99: Boolean = false
  iter_IsAnyTrue(isPal _, strings1)               //> res100: Boolean = true
  iter_IsAnyTrue(isPal _, strings2)               //> res101: Boolean = false
  iter_IsAnyTrue(isPal _, strings3)               //> res102: Boolean = true
  
  // recursive solution
  def recur_IsAnyTrue[T](pred: T => Boolean, vals: List[T]): Boolean =
  {
  	if (vals == Nil) false
  	else if ((pred(vals.head))) true
  	else recur_IsAnyTrue(pred, vals.tail)
  }                                               //> recur_IsAnyTrue: [T](pred: T => Boolean, vals: List[T])Boolean
  // Test
  recur_IsAnyTrue(isOdd _, List(0,2,4))           //> res103: Boolean = false
  recur_IsAnyTrue(isOdd _, nums1)                 //> res104: Boolean = true
  recur_IsAnyTrue(isPrime _, nums1)               //> res105: Boolean = true
  recur_IsAnyTrue(isPal _, strings1)              //> res106: Boolean = true
  recur_IsAnyTrue(isPal _, strings2)              //> res107: Boolean = false
  recur_IsAnyTrue(isPal _, strings3)              //> res108: Boolean = true
  
  // tail recursive solution
  def tailrecur_IsAnyTrue[T](pred: T => Boolean, vals: List[T]) =
  {
  	def helper(list: List[T], result: Boolean): Boolean =
  		if (list == Nil) result
  		else if (pred(list.head)) true
  		else helper(list.tail, result && pred(list.head))
  	helper(vals, false)
  }                                               //> tailrecur_IsAnyTrue: [T](pred: T => Boolean, vals: List[T])Boolean
  // Test
  tailrecur_IsAnyTrue(isPal _,  List("Golden", "State"))
                                                  //> res109: Boolean = false
  tailrecur_IsAnyTrue(isOdd _, nums1)             //> res110: Boolean = true
  tailrecur_IsAnyTrue(isPrime _, nums1)           //> res111: Boolean = true
  tailrecur_IsAnyTrue(isPrime _, nums2)           //> res112: Boolean = true
  tailrecur_IsAnyTrue(isPal _, strings1)          //> res113: Boolean = true
  tailrecur_IsAnyTrue(isPal _, strings2)          //> res114: Boolean = false
  tailrecur_IsAnyTrue(isPal _, strings3)          //> res115: Boolean = true
  
  // mfr solution
  def mfr_IsAnyTrue[T](pred: T => Boolean, vals: List[T]) = vals.filter(pred(_)).size >= 1
                                                  //> mfr_IsAnyTrue: [T](pred: T => Boolean, vals: List[T])Boolean
  // Test
	mfr_IsAnyTrue(isOdd _, nums1)             //> res116: Boolean = true
	mfr_IsAnyTrue(isPrime _, nums1)           //> res117: Boolean = true
	mfr_IsAnyTrue(isPrime _, nums2)           //> res118: Boolean = true
	mfr_IsAnyTrue(isPal _, strings1)          //> res119: Boolean = true
	mfr_IsAnyTrue(isPal _, strings2)          //> res120: Boolean = false
	mfr_IsAnyTrue(isPal _, strings3)          //> res121: Boolean = true
	
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 10
  def isSorted (list: List[Int]) =
  {
  	def helper(l: List[Int], sorted: Boolean): Boolean =
  	{
  		if (l.head == Nil) sorted
  		else if (l.tail != Nil) helper(l.tail, sorted && (l.head <= l.tail.head))
			else sorted
  	}
  	helper(list, true)
  }                                               //> isSorted: (list: List[Int])Boolean
  //Test
  isSorted(List(1,2,3,4,5))                       //> res122: Boolean = true
  isSorted(List(2,1,3,4,5))                       //> res123: Boolean = false
  isSorted(List(9,4,2,1,0))                       //> res124: Boolean = false
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 13
  // stream of infinite 1's
  def make1s(from: Int): Stream[Int] = from #:: make1s(from / from)
                                                  //> make1s: (from: Int)Stream[Int]
  val ones = make1s(1)                            //> ones  : Stream[Int] = Stream(1, ?)
  ones(10)                                        //> res125: Int = 1
  ones                                            //> res126: Stream[Int] = Stream(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ?)
  
  // stream of non-negative integers
  def makeNats(from: Int): Stream[Int] = from #:: makeNats(from + 1)
                                                  //> makeNats: (from: Int)Stream[Int]
  val nats = makeNats(0)                          //> nats  : Stream[Int] = Stream(0, ?)
  nats(5)                                         //> res127: Int = 5
  nats                                            //> res128: Stream[Int] = Stream(0, 1, 2, 3, 4, 5, ?)
  
  // stream of non-negative even integers
  val evenNats = nats.filter(isEven _)            //> evenNats  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
  evenNats(1)                                     //> res129: Int = 2
  evenNats(4)                                     //> res130: Int = 8
  evenNats                                        //> res131: scala.collection.immutable.Stream[Int] = Stream(0, 2, 4, 6, 8, ?)
  
  // stream of all squares of integers
  val squaredNats = nats.map((x: Int) => square(x))
                                                  //> squaredNats  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
  squaredNats(5)                                  //> res132: Int = 25
  squaredNats                                     //> res133: scala.collection.immutable.Stream[Int] = Stream(0, 1, 4, 9, 16, 25
                                                  //| , ?)
  //val primes = nats.filter(isPrime _)
  //primes(5)
  //val cubedPrimes = primes.map((x: Int) => cube(x))
  //cubedPrimes(5)
}