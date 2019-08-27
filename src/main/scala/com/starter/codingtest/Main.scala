package com.starter.codingtest


object Main {
  def main(args: Array[String]) {
    while (true) {
      Console.println("Please input a number less than trillion without decimals...")
      val exceptionMessage="Invalid Input! "
      try {
        val input = scala.io.StdIn.readLong()
        if (input>999999999999L || input< -999999999999L) Console.println (exceptionMessage)
        else Console.println(s"The Word representation  of $input is :  '${SpeakNumbers.numbersToWords(input)}'" )
      }
      catch
        {
          case nfex: NumberFormatException => Console.println(exceptionMessage)
          case gex:Exception => Console.println("Unknown Exception, Please try again later") ; Console.flush()
        }
    }
  }
}
