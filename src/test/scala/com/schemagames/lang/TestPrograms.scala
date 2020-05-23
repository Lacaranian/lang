package com.schemagames.lang

object TestPrograms {
  val generalTestProgram =
    """def testProgram = {
      |  def floop = 123
      |  def bloop = floop; def ploop = snoop;
      |  def noop = "nope"
      |
      |  noop
      |}
      |
      |def main = testProgram
      |""".stripMargin

  val simpleWhitespace =
    """def program = {
      |  def thing = 1
      |
      |  thing
      |}""".stripMargin

  val longerWhitespace =
    """def program = {
      |  def thing = 1
      |
      |  def thing2 = 2
      |
      |
      |
      |  thing2
      |}""".stripMargin

  val invalidIndentation = "def testProgram = {\n  \t  def something = 123\n\t  something\n}"

  val explicitDelimiterTest =
    """def test = {
      |  def thing = 1; def thing2 = 2; def thing3 = 3
      |
      |  def thing4 = 4;; def thing5 = thing2;
      |
      |  thing5
      |}
      |
      |""".stripMargin

  val applicationAssociativityTest = "def test = a b c d"

  val abstractionAndApplicationTest =
    """def S = \x -> (\y -> (\z -> (x y) (y z)))
      |def K = \x -> (\y -> x)
      |def I = \x -> x
      |
      |def KI = K I
      |
      |def test = KI "true" "false"
      |
      |def main = test
      |""".stripMargin
}
