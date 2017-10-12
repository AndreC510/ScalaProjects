object SequenceControl {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Problem 1
  def taxCalculator(income: Double) =
  {
  	var tax = 0.0
  	income match
  	{
  		case income if income < 0 => throw new Exception("Income must be non-negative")
  		case income if (income < 20000 && income >= 0) => tax = 0
  		case income if income < 30000 => tax = income * 0.05
  		case income if income < 40000 => tax = income * 0.11
  		case income if income < 60000 => tax = income * 0.23
  		case income if income < 100000 => tax = income * 0.32
  		case _ => tax = income * 0.5
  	}
  	tax
  }                                               //> taxCalculator: (income: Double)Double
  // Test
  taxCalculator(39999)                            //> res0: Double = 4399.89
  taxCalculator(10000)                            //> res1: Double = 0.0
  taxCalculator(19999)                            //> res2: Double = 0.0
  taxCalculator(100000)                           //> res3: Double = 50000.0
  taxCalculator(60000)                            //> res4: Double = 19200.0
  taxCalculator(99999)                            //> res5: Double = 31999.68
  taxCalculator(59999)                            //> res6: Double = 13799.77
  taxCalculator(25000)                            //> res7: Double = 1250.0
  taxCalculator(0)                                //> res8: Double = 0.0
  try
 	{ taxCalculator(-943000) }
 	catch
 	{	case e: Exception => println(e) } //> java.lang.Exception: Income must be non-negative
                                                  //| res9: AnyVal = ()
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
}