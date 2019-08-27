package com.starter.codingtest

import org.scalatest.{FlatSpec, Matchers}

class NumbersToWordsTest extends FlatSpec with Matchers {

  it  should "run a sample test" in {
    7 + 9 should be(16)
  }

   it should "print 0 as zero if specified to print zero" in {
    "zero" should equal(SpeakNumbers.numbersToWords(0))
  }

  it should "print empty string if specified to not print zero" in {
    "" should equal(SpeakNumbers.numbersToWords(0, printZero = false))
  }

  it should "give a friendly message when it receives a number greater than 999999999999" in {
    "Please enter a number less than 1000000000000(one trillion)" should equal(SpeakNumbers.numbersToWords(1000000000000L, printZero = false))
  }

  it should "give a friendly message when it receives a number less than -999999999999" in {
    "Please enter a number less than 1000000000000(one trillion)" should equal(SpeakNumbers.numbersToWords(-1000000000000L, printZero = false))
  }

  it should "correctly print out a billion " in {
    "one billion" should equal(SpeakNumbers.numbersToWords(1000000000, printZero = false))
  }

  it should "correctly print out a million " in {
    "one million" should equal(SpeakNumbers.numbersToWords(1000000, printZero = false))
  }

  it should "correctly print out a thousand " in {
    "one million" should equal(SpeakNumbers.numbersToWords(1000, printZero = false))
  }

  it should "correctly print out a hundred " in {
    "one hundred" should equal(SpeakNumbers.numbersToWords(100, printZero = false))
  }

  it should "correctly print out a ten " in {
    "ten" should equal(SpeakNumbers.numbersToWords(100, printZero = false))
  }

  it should "correctly print out a number less than ten " in {
    "seven" should equal(SpeakNumbers.numbersToWords(7, printZero = false))
  }

  it should "correctly print out numbers in billions" in {
    "nine hundred ninety nine billion nine hundred ninety nine million nine hundred ninety nine thousand nine hundred ninety nine" should equal(SpeakNumbers.numbersToWords(999999999999L, printZero = false))
  }

  it should "correctly print out negative numbers in billions" in {
    "negative nine hundred ninety nine billion nine hundred ninety nine million nine hundred ninety nine thousand nine hundred ninety nine" should equal(SpeakNumbers.numbersToWords(-999999999999L, printZero = false))
  }

  ////Lets test out some big  numbers...
  it should "correctly print out  numbers with 8's in them" in {
    "eight hundred eighty eight billion eight hundred eighty eight million eight hundred eighty eight thousand eight hundred eighty eight" should equal(SpeakNumbers.numbersToWords(888888888888L, printZero = false))
  }

  it should "correctly print out numbers with 7's in them" in {
    "seven hundred seventy seven billion seven hundred seventy seven million seven hundred seventy seven thousand seven hundred seventy seven" should equal(SpeakNumbers.numbersToWords(777777777777L, printZero = false))
  }

  it should "correctly print out numbers with 1's in them" in {
    "one hundred eleven billion one hundred eleven million one hundred eleven thousand one hundred eleven" should equal(SpeakNumbers.numbersToWords(111111111111L, printZero = false))
  }

}
