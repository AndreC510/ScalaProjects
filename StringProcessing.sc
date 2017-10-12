import scala.collection.mutable.Stack
import scala.util.control.Breaks._
import scala.util.Random
object StringProcessing {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ rewrite this to only have one str1 val
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 1, 2, 4, 5, 8, 9
  def isPal(str: String) =
  {
  	val stack = Stack[Char]()
  	val str1 = str.toLowerCase
  	var isPalindrome = true
  	for (i <- 0 until str1.length) { stack.push(str1(i)) }
  	for (i <- 0 until str1.length)
  	{
  		breakable
  		{
  			if (!stack.pop.equals(str1(i)))
  			{
  			 	isPalindrome = false
  			 	break
  			}
  		}
  	}
  	if (!isPalindrome) false
  	else true
  }                                               //> isPal: (str: String)Boolean
  // Test
  isPal("11")                                     //> res0: Boolean = true
  isPal("friend")                                 //> res1: Boolean = false
  isPal("")                                       //> res2: Boolean = true
  isPal("h")                                      //> res3: Boolean = true
  isPal("Racecar")                                //> res4: Boolean = true
  isPal("1112")                                   //> res5: Boolean = false
  isPal("WreckeD")                                //> res6: Boolean = false
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 2
  def isPal2(str: String) =
  {
  	var isPalindrome = true
  	var a1 = Array[String]()
  	var s1 = Stack[Char]()
  	var allWords = ""
  	a1 = str.split("[ .,!?']+")
  	for (i <- 0 until a1.size)
  	{
  		var a1STR = a1(i).toLowerCase
  		allWords += a1STR
  	}
  	for (i <- 0 until allWords.length) { s1.push(allWords(i)) }
		for (i <- 0 until allWords.length)
		{
			breakable
			{
				if (!s1.pop.equals(allWords(i)))
				{
				  isPalindrome = false
				  break
				}
			}
		}
		if (!isPalindrome) false
		else true
	}                                         //> isPal2: (str: String)Boolean
  
  // Test
  isPal2("A MAN, a plan, a canal, Panama!")       //> res7: Boolean = true
  isPal2("What tahw?")                            //> res8: Boolean = true
  isPal2("Cc")                                    //> res9: Boolean = true
  isPal2("")                                      //> res10: Boolean = true
  isPal2("a")                                     //> res11: Boolean = true
  isPal2("China china")                           //> res12: Boolean = false
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 // Problem 4
 def mkWord(num: Int = 5) =
 {
 	val alpha = "abcdefghijklmnopqrstuvwxyz"
 	var result = ""
 	val r = new scala.util.Random
 	for (i <- 1 to num)
 	{
 		val alphaIndex = r.nextInt(26);
 		result += alpha(alphaIndex)
 	}
 	result
 }                                                //> mkWord: (num: Int)String
 
 // Test
 mkWord()                                         //> res13: String = atxsq
 mkWord(10)                                       //> res14: String = pbkohofdgg
 mkWord(10)                                       //> res15: String = mqlzhjohmi
 mkWord(20)                                       //> res16: String = otkolwoztpxioqzwjneu
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 // Problem 5
 def mkSentence(num: Int = 10) =
 {
 	 val r_WordLength = new scala.util.Random
 	 var result = ""
 	 if (num == 1)
 	 {
 	 		var word = mkWord()
 	 		word = word.substring(0,1).toUpperCase + word.substring(1) + "."
 	 		result += word
 	 }
 	 else
 	 {
 	 	for (i <- 1 to num)
 	 	{
 	 		if (i == 1)
 	 		{
 	 				var first = mkWord()
 	 				first = first.substring(0,1).toUpperCase + first.substring(1)
 	 				result = first + " "
 	 		}
 	 		else if (i == num)
 	 		{
 	 				var last = mkWord()
 	 				last = last + "."
 	 				result += last
 	 		}
 	 		else
 	 		{
 	 		 		val word = mkWord()
 	 		 		result = result + word + " "
 	 		}
 	 	}
 	 }
 	 result
 }                                                //> mkSentence: (num: Int)String
 // Test
 mkSentence(5)                                    //> res17: String = Gvqxf nmldw lvwim acgzm tdxkv.
 mkSentence(1)                                    //> res18: String = Jhdct.
 mkSentence(11)                                   //> res19: String = Vrabs gzmps kixpu ndxku fgxqr trlcz akgxk vbvgt dkmwp wfedg
                                                  //|  lgkwj.
 mkSentence(0)                                    //> res20: String = ""
 mkSentence()                                     //> res21: String = Akdin esrsa lkjgn lpzzq bcnej ihcca pbpyr khyyf dbnfm jpqvi
                                                  //| .
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Problem 8
def eval(exp: String) =
{
	var result = 0.0
	var array = Array[String]()
	array = exp.split("\\s+|[+]")

	exp match
	{
		case exp if !exp.contains("+")  => throw new Exception("Missing '+' operator")
		case exp if exp.containsSlice("[a-zA-Z]+") => throw new NumberFormatException
		case _ =>
							for (i <- 0 until array.size)
							{
									if (!array(i).equals(""))
									{
											result += array(i).toDouble
									}
							}
	}
	result
}                                                 //> eval: (exp: String)Double
// Test
eval("3.14+42")                                   //> res22: Double = 45.14
eval("  -26  +  -49.99  ")                        //> res23: Double = -75.99000000000001
eval("32 + -23")                                  //> res24: Double = 9.0
eval("-2323 + 1")                                 //> res25: Double = -2322.0
try
{
	eval("abc + 3")
}
catch {case e: Exception => println(e) }          //> java.lang.NumberFormatException: For input string: "abc"
                                                  //| res26: AnyVal = ()
try
{
	eval("7 * 3")
}
catch {case e: Exception => println(e) }          //> java.lang.Exception: Missing '+' operator
                                                  //| res27: AnyVal = ()
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Problem 9
def eval2(exp: String) =
{
	var result = 0.0
	var array = Array[String]()
	exp match
	{
		case exp if !(exp.contains("*") || exp.contains("+")) => throw new Exception("Missing '+ or *' operator")
		case exp if exp.containsSlice("[a-zA-Z]+") => throw new NumberFormatException
		case _ =>
							array = exp.split("\\s+")
							for (i <- 0 until array.size)
							{
									if (array(i).equals("*"))
									{
											result = result + (array(i-1).toDouble * array(i+1).toDouble)
									}
									else if (array(i).equals("+"))
									{
											result = result + array(i-1).toDouble + array(i+1).toDouble
									}
							}
	}
	result
}                                                 //> eval2: (exp: String)Double
// Test
eval2("3.121 * 12321")                            //> res28: Double = 38453.841
eval2("      2.2 * 9.2  ")                        //> res29: Double = 20.24
eval2("7 + 3")                                    //> res30: Double = 10.0
try{
eval2("7 * 3")
}
catch{
case e: Exception => println(e)}                  //> res31: AnyVal = 21.0

try{
eval2("abcd * 3")
}
catch{
case e: Exception => println(e)}                  //> java.lang.NumberFormatException: For input string: "abcd"
                                                  //| res32: AnyVal = ()
}