package com.starter.codingtest

object SpeakNumbers {

  def numbersToWords(num: Long, printZero: Boolean = true): String = {
    ////Setting Upper and Lower bounds as 1 trillion (1000000000000)
    if (num>999999999999L || num< -999999999999L) return s"Please enter a number less than 1000000000000(one trillion)"
    if (num < 0) s"negative ${numbersToWords(-num)}"
    else if (num >= 1000000000) s"${numbersToWords(num / 1000000000)} billion ${numbersToWords(num % 1000000000, printZero = false)}"
    else if (num >= 1000000) s"${numbersToWords(num / 1000000)} million ${numbersToWords(num % 1000000, printZero = false)}"
    else if (num >= 1000) s"${numbersToWords(num / 1000)} thousand ${numbersToWords(num % 1000, printZero = false)}"
    else if (num >= 100) s"${numbersToWords(num / 100)} hundred ${numbersToWords(num % 100, printZero = false)}"
    else if (num >= 20) num/10 match {
      case 2 => s"twenty ${numbersToWords(num % 10, printZero = false)}"
      case 3 => s"thirty ${numbersToWords(num % 10, printZero = false)}"
      case 5 => s"fifty ${numbersToWords(num % 10, printZero = false)}"
      case n@_ => s"${numbersToWords(n).stripSuffix("t")}ty ${numbersToWords(num % 10, printZero = false)}"
    }
    else num match {
      case 0 => if (printZero) "zero" else ""
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 15 =>"fifteen";
      case n@_ => s"${numbersToWords(n-10).stripSuffix("t")}teen"
    }
  }

}
