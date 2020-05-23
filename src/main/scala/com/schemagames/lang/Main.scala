package com.schemagames.lang

import com.schemagames.lang.compiler.{Compiler, DefaultCompiler, RunInfo}

object test {
  val n1 =
    """def testProgram = {
      |  def floop = 123
      |  def bloop = floop; def ploop = snoop;
      |  def noop = "nope"
      |
      |  noop
      |}
      |
      |def S = \x -> (\y -> (\z -> (x y) (y z)))
      |def K = \x -> (\y -> x)
      |def I = \x -> x
      |
      |def KI = K I
      |
      |def testIfStatement = KI "true" "false"
      |
      |def main = testIfStatement
      |""".stripMargin
  val n2 = "def test = a b c d"
}

object Main {
  def main(args: Array[String]): Unit = {
    val resultCompiler = DefaultCompiler.apply(test.n1)

    resultCompiler.context.pastRuns.foreach{ case (runID, RunInfo(input, phaseResults)) => {
      println("Input: \n" + input)

      phaseResults.foreach{ case (phaseKey, result) => {
        println("Phase " + phaseKey + ": ")

        result match {
          case Left(error) => println(error)
          case Right(success) => println(success)
        }

        println("")
      }}
    }}
  }
}
