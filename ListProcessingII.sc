import scala.collection.mutable.ListBuffer

object ListProcessingII {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 1 2 3 4
  val cs152 = List(List(93.0, 89.0, 90.0), List(75.0, 40.0, 68.0), List(88.0, 82.0, 78.0))
                                                  //> cs152  : List[List[Double]] = List(List(93.0, 89.0, 90.0), List(75.0, 40.0, 
                                                  //| 68.0), List(88.0, 82.0, 78.0))
  val cs149 = List(List(60.0, 67.0, 67,0), List(79.0, 80.0, 81,0), List(99.0, 98.0, 99.0), List(80.0, 85.0, 86.8))
                                                  //> cs149  : List[List[Double]] = List(List(60.0, 67.0, 67.0, 0.0), List(79.0, 8
                                                  //| 0.0, 81.0, 0.0), List(99.0, 98.0, 99.0), List(80.0, 85.0, 86.8))
  val doc1 = List("Please", "excuse", "my", "dear", "Aunt", "Sally")
                                                  //> doc1  : List[String] = List(Please, excuse, my, dear, Aunt, Sally)
  val doc2 = List("My" , "very", "eggcellent", "mother", "just", "served", "us", "nine", "pizzas")
                                                  //> doc2  : List[String] = List(My, very, eggcellent, mother, just, served, us, 
                                                  //| nine, pizzas)
  val dict1 = List("Please", "excuse", "my", "daer", "Aunt", "Sally")
                                                  //> dict1  : List[String] = List(Please, excuse, my, daer, Aunt, Sally)
  val dict2 = List("My" , "very", "excellent", "mother", "just", "served", "us", "nine", "pizzas")
                                                  //> dict2  : List[String] = List(My, very, excellent, mother, just, served, us, 
                                                  //| nine, pizzas)
  def avg(scores: List[Double]) = scores.reduce(_ + _) / scores.size
                                                  //> avg: (scores: List[Double])Double
  
  avg(cs152.head)                                 //> res0: Double = 90.66666666666667
  avg(cs152.tail.head)                            //> res1: Double = 61.0
  avg(cs152.tail.tail.head)                       //> res2: Double = 82.66666666666667
  avg(cs149.head)                                 //> res3: Double = 48.5
  avg(cs149.tail.head)                            //> res4: Double = 60.0
  
  def avgAvg(scores: List[List[Double]]) = scores.map(avg _)
                                                  //> avgAvg: (scores: List[List[Double]])List[Double]
  
  avgAvg(cs152)                                   //> res5: List[Double] = List(90.66666666666667, 61.0, 82.66666666666667)
  avgAvg(cs149)                                   //> res6: List[Double] = List(48.5, 60.0, 98.66666666666667, 83.93333333333334)
                                                  //| 
 
  def atLeast70(avg: Double) = avg >= 70          //> atLeast70: (avg: Double)Boolean
  def passing(scores: List[List[Double]]) =
  {
  	val indices = new ListBuffer[Int]
  	val avgs = scores.map(avg _)
  	for (i <- 0 until avgs.size)
  		if (atLeast70(avgs(i))) indices ++= Seq(i)
  	indices
  }                                               //> passing: (scores: List[List[Double]])scala.collection.mutable.ListBuffer[In
                                                  //| t]
  passing(cs152)                                  //> res7: scala.collection.mutable.ListBuffer[Int] = ListBuffer(0, 2)
  passing(cs149)                                  //> res8: scala.collection.mutable.ListBuffer[Int] = ListBuffer(2, 3)
  
  def sumSums(scores: List[List[Double]]) =
  {
  	var sum = 0.0
  	for (i <- 0 until scores.size)
  		sum = sum + (avg(scores(i)) * scores(i).size)
  	sum
  }                                               //> sumSums: (scores: List[List[Double]])Double
  sumSums(cs152)                                  //> res9: Double = 703.0
  sumSums(cs149)                                  //> res10: Double = 981.8
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 2 3 4
  def spellCheck(doc: List[String], dictionary: List[String]) =
  {
  	val notInDict = new ListBuffer[String]
  	for (i <- doc)
  	{
  	  if (!dictionary.contains(i)) { notInDict.append(i) }
  	}
  	notInDict
  }                                               //> spellCheck: (doc: List[String], dictionary: List[String])scala.collection.m
                                                  //| utable.ListBuffer[String]
  spellCheck(List("I", "am", "king"), List("king"))
                                                  //> res11: scala.collection.mutable.ListBuffer[String] = ListBuffer(I, am)
  spellCheck(doc1, dict1)                         //> res12: scala.collection.mutable.ListBuffer[String] = ListBuffer(dear)
  spellCheck(doc2, dict2)                         //> res13: scala.collection.mutable.ListBuffer[String] = ListBuffer(eggcellent)
                                                  //| 
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 3 4
  def mfr_spellCheck(doc: List[String], dictionary: List[String]) =
  {
  	def notInDict(word: String) = { if (!dictionary.contains(word)) true else false }
  	val notInDictionary = new ListBuffer[String]
  	var string = doc.filter(notInDict _)
  	string
  }                                               //> mfr_spellCheck: (doc: List[String], dictionary: List[String])List[String]

	mfr_spellCheck(List("I", "am", "king"), List("king"))
                                                  //> res14: List[String] = List(I, am)
	mfr_spellCheck(doc1, dict1)               //> res15: List[String] = List(dear)
	mfr_spellCheck(doc2, dict2)               //> res16: List[String] = List(eggcellent)
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 4
  def evalMono(mono: (Double, Double), x: Double) = Math.pow(x, mono._2) * mono._1
                                                  //> evalMono: (mono: (Double, Double), x: Double)Double
 	// Test
 	evalMono((3.0, 2.0), 4)                   //> res17: Double = 48.0
 	evalMono((5.0, 3.0), 10)                  //> res18: Double = 5000.0
 	evalMono((10.0, 2.0), 1.39)               //> res19: Double = 19.320999999999998
 	evalMono((2.0, 3.0), 6.7)                 //> res20: Double = 601.5260000000001
 	
  def evalPoly(poly: List[(Double, Double)], x: Double): Double =
  {
		var sum = 0.0
		for (i <- 0 until poly.size)
		{
			sum = sum + evalMono(poly(i), x)
		}
		sum
  }                                               //> evalPoly: (poly: List[(Double, Double)], x: Double)Double
  
  // Test
  evalPoly(List((3.0, 2.0), (1.0, 4.0), (4.0, 3.0)), 2)
                                                  //> res21: Double = 60.0
  evalPoly(List((2.0, 2.0), (9.0, 0.5), (-4.0, 3.0)), 3)
                                                  //> res22: Double = -74.4115427318801
  evalPoly(List((7.0, -1.0), (3.0, 2.0), (11.0, 1.0)), 5)
                                                  //> res23: Double = 131.4
}