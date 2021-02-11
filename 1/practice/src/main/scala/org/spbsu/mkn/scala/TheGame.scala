package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

    sealed trait GuessResult

    case class Correct(numTries: Int) extends GuessResult
    case class Incorrect(bulls: Int, cows: Int) extends GuessResult

    class RepeatingDigitsException extends RuntimeException
    class WrongNumberLengthException(val expected: Int, val actual: Int) extends RuntimeException


    val defaultLength = 4
    val characters: String = ('0' to '9').mkString("") + ('A' to 'Z').mkString("")

    def generateNumberString(length: Int): String = {
        Random.shuffle(characters).take(length).toString()
    }

    def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
        val secretSet = secret.toSet
        val userInputSet = userInput.toSet

        if (secret.length != userInput.length) {
            throw new WrongNumberLengthException(secret.length, userInput.length)
        }
        if (secretSet.size != secret.length || userInputSet.size != userInput.length) {
            throw new RepeatingDigitsException
        }

        val bulls = (0 until secret.length).count(i =>
            secret(i) == userInput(i)
        )
        val cows = userInputSet.intersect(secretSet).size - bulls

        if (bulls == secret.length) {
            return Correct(numTries)
        }
        Incorrect(bulls, cows)
    }

    def readLength(): Int = {
        readLine("Enter the length of the number: ").toIntOption match {
            case None        =>
                println("Length must be an integer, so now the length = 4 (default value)")
            case Some(value) =>
                if (value < 0) {
                    println("Length must be a positive number, so now the length = 4 (default value)")
                }
                else {
                    return value
                }
        }
        defaultLength
    }


    def main(args: Array[String]): Unit = {
        val name = readLine("Enter your name: ")
        println(s"Hello, $name!")

        val length = readLength()
        val secret = generateNumberString(length)
        println("I made a number. Try to guess it!")

        println(secret)

        LazyList.from(1).foreach(step => {
            try {
                val userInput = readLine(s"Enter your ($step) attempt: ")
                validate(secret, userInput, step) match {
                    case Correct(numTries) =>
                        println(s"Right! You guessed the number in $numTries attempts")
                        return // from main
                    case Incorrect(bulls, cows) =>
                        println(s"Nope. Bulls = $bulls, Cows = $cows")
                }
            }
            catch {
                case _: RepeatingDigitsException   =>
                    println("Your can't use repeating digits in your number")
                case e: WrongNumberLengthException =>
                    println(s"Your number must have the same length as the secret number. " +
                            s"Expected: ${e.expected}, Actual: ${e.actual}")
                case error: Exception              =>
                    println(s"Oops! ${error.getMessage}")
            }
        })
    }
}
