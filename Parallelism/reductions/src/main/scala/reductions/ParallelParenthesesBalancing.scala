package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceHelper(chars: Array[Char], index: Int, theCheck: Int): Boolean = {
      if (index == chars.length) theCheck == 0
      else if (theCheck < 0) false
      else if (chars(index) == '(') balanceHelper(chars, index + 1, theCheck + 1)
      else if (chars(index) == ')') balanceHelper(chars, index + 1, theCheck - 1)
      else balanceHelper(chars, index + 1, theCheck)
    }
    balanceHelper(chars, 0, 0)
  }

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, somme: Int, valMin: Int): (Int, Int) = {
      if (idx == until) (somme, valMin)
      else if (chars(idx) == '(') traverse(idx + 1, until, somme + 1, valMin)
      else if (chars(idx) == ')') traverse(idx + 1, until, somme - 1, Math.min(valMin, somme - 1))
      else traverse(idx + 1, until, somme, valMin)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if ((until - from) <= threshold) traverse(from, until, 0, 0)
      else {
        val (e1, e2) = parallel(reduce(from, Math.ceil(((from + until) / 2)).toInt), reduce(Math.ceil(((from + until) / 2)).toInt, until))
        (e1._1 + e2._1, Math.min(e1._1 + e2._2, e1._2))
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
