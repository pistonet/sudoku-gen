import org.scalatest._
import flatspec._
import matchers._
import logic.Coords
import logic.BoardState
import java.io._

class BoardStateTest extends AnyFlatSpec with should.Matchers {

  // use to simulating reading from .csv files
  def stringToReader(s: String): BufferedReader = new BufferedReader(new StringReader(s))




  "equals" should "return only true if other is a BoardState of same size and contents" in {

    val bs1 = BoardState.createEmpty(3).withChanges(Map(Coords(2,3) -> Some(3)))
    val bs2 = BoardState.createEmpty(3).withChanges(Map(Coords(2,3) -> Some(3)))
    val bs3 = BoardState.createEmpty(2).withChanges(Map(Coords(2,3) -> Some(3)))
    val bs4 = BoardState.createEmpty(3).withChanges(Map(Coords(2,3) -> Some(1)))

    assert(bs1 == bs1)
    assert(bs1 == bs2)
    assert(bs1 != bs3)
    assert(bs1 != bs4)

  }





  "BoardState apply" should "create BoardStates"  in {

    // note coordinate order
    val contents: Vector[Vector[Option[Int]]] = Vector.tabulate(9,9)((y,x) => (x,y) match {
      case (2,3) => Some(1)
      case (8,8) => Some(2)
      case _ => None
    })

    val bs = BoardState(3,contents)

    assert(bs.sideLength == 9)
    assert(bs(Coords(2,3)) == Some(1))
    assert(bs(Coords(3,2)).isEmpty)
    assert(bs(Coords(8,8)) == Some(2))

  }

  it should "throw IllegalArgumentException when called with non-positive bsl or contents of wrong size" in {

    // blockSideLength = sqrt(9) = 3
    val contents: Vector[Vector[Option[Int]]] = Vector.fill(9,9)(None)

    a [IllegalArgumentException] should be thrownBy(BoardState(-1, contents))
    a [IllegalArgumentException] should be thrownBy(BoardState(0, contents))
    a [IllegalArgumentException] should be thrownBy(BoardState(2, contents))
    a [IllegalArgumentException] should be thrownBy(BoardState(4, contents))

  }





  "parseFromCsv" should "parse BoardStates correctly" in {

    // 2 same boards:
    val s1 ="""3,
               |0,3,0,5,0,0,0,1,9,
               |0,0,7,1,0,0,0,0,6,
               |0,0,8,0,0,9,3,0,0,
               |0,0,0,6,0,0,0,0,1,
               |0,6,0,2,0,1,0,7,0,
               |7,0,0,0,0,8,0,0,0,
               |0,0,5,8,0,0,1,0,0,
               |9,0,0,0,0,4,2,0,0,
               |6,1,0,0,0,5,0,3,0,
               |""".stripMargin
    val s2 = """3
               |,3,,5,,,,1,9
               |,,7,1,,,,,6
               |,,8,,,9,3
               |,,,6,,,,,1
               |,6,,2,,1,,7
               |7,,,,,8
               |,,5,8,,,1
               |9,,,,,4,2
               |6,1,,,,5,,3
               |""".stripMargin

    val bs1Option: Option[BoardState] = BoardState.parseFromCsv(stringToReader(s1))
    val bs2Option: Option[BoardState] = BoardState.parseFromCsv(stringToReader(s2))

    assert(bs1Option.isDefined)
    assert(bs2Option.isDefined)

    val bs1 = bs1Option.get
    val bs2 = bs2Option.get

    assert(bs1 == bs2)
    assert(bs1.sideLength == 9)
    assert(bs1(Coords(0,0)).isEmpty)
    assert(bs1(Coords(2,1)) == Some(7))
    assert(bs1(Coords(7,8)) == Some(3))

  }

  it should "handle cases where empty lines are omitted properly" in {

    // note coordinate order
    val contents1: Vector[Vector[Option[Int]]] = Vector.tabulate(9,9)((y,x) => (x,y) match {
      case (2,3) => Some(1)
      case (8,8) => Some(2)
      case _ => None
    })

    val board1 =
     """3
        |
        |
        |
        |,,1
        |
        |
        |
        |
        |,,,,,,,,2""".stripMargin

    assert(BoardState.parseFromCsv(stringToReader(board1)) == Some(BoardState(3,contents1)))


    val emptyBoard = "3"

    assert(BoardState.parseFromCsv(stringToReader(emptyBoard)) == Some(BoardState.createEmpty(3)))

  }

  it should "handle cases where cell is not an integer or outside the range properly" in {

    // note coordinate order
    val contents1: Vector[Vector[Option[Int]]] = Vector.tabulate(9,9)((y,x) => (x,y) match {
      case (2,3) => Some(1)
      case (8,8) => Some(2)
      case _ => None
    })

    val board1 =
     """3
        |
        |
        |
        |0   ,   10  ,  1
        |
        |
        |
        |
        |,__,hello, world,     ,,banana,,2""".stripMargin

    assert(BoardState.parseFromCsv(stringToReader(board1)) == Some(BoardState(3,contents1)))

  }

  it should "return None in cases where the data is not valid" in {

    val nothing: String = ""
    val negative =
      """-1
        |
        |
        |,,,1
        |
        |
        |
        |
        |
        |,,,,,,,,2""".stripMargin
    val zero =
      """0
        |
        |
        |,,,1
        |
        |
        |
        |
        |
        |,,,,,,,,2""".stripMargin
    val notInteger =
      """banana
        |
        |
        |,,,1
        |
        |
        |
        |
        |
        |,,,,,,,,2""".stripMargin

    assert(BoardState.parseFromCsv(stringToReader(nothing)).isEmpty)
    assert(BoardState.parseFromCsv(stringToReader(negative)).isEmpty)
    assert(BoardState.parseFromCsv(stringToReader(zero)).isEmpty)
    assert(BoardState.parseFromCsv(stringToReader(notInteger)).isEmpty)

  }




  "possibleNumbers" should "return the correct numbers" in {

    val s ="""3,
               |0,3,0,5,0,0,0,1,9,
               |0,0,7,1,0,0,0,0,6,
               |0,0,8,0,0,9,3,0,0,
               |0,0,0,6,0,0,0,0,1,
               |0,6,0,2,0,1,0,7,0,
               |7,0,0,0,0,8,0,0,0,
               |0,0,5,8,0,0,1,0,0,
               |9,0,0,0,0,4,2,0,0,
               |6,1,0,0,0,5,0,3,0,
               |""".stripMargin
    val bs: BoardState = BoardState.parseFromCsv(stringToReader(s)).get

    // scala> println(bs.printableString)
    //   3  |5    |  1 9
    //     7|1    |    6
    //     8|    9|3
    // ------------------
    //      |6    |    1
    //   6  |2   1|  7
    // 7    |    8|
    // ------------------
    //     5|8    |1
    // 9    |    4|2
    // 6 1  |    5|  3

    assert(bs.possibleNumbers(Coords(0,0)).sorted == Vector(2,4))
    assert(bs.possibleNumbers(Coords(1,0)).sorted == Vector(2,3,4))
    assert(bs.possibleNumbers(Coords(4,4)).sorted == Vector(3,4,5,9))

  }

  "markup" should "return the same numbers as possibleNumbers" in {
    val s = """4,
              |_ ,5 ,_ ,13,7 ,14,_ ,_ ,10,_ ,9 ,11,4 ,_ ,_ ,_
              |_ ,11,_ ,2 ,12,_ ,9 ,_ ,13,_ ,_ ,_ ,_ ,10,_ ,3
              |3 ,_ ,_ ,16,5 ,_ ,_ ,4 ,6 ,_ ,14,_ ,11,_ ,_ ,_
              |9 ,_ ,_ ,_ ,_ ,_ ,10,_ ,_ ,_ ,_ ,_ ,5 ,_ ,_ ,_
              |1 ,_ ,2 ,_ ,_ ,4 ,_ ,12,_ ,_ ,_ ,8 ,6 ,11,_ ,10
              |_ ,7 ,_ ,9 ,10,_ ,_ ,_ ,12,6 ,_ ,_ ,_ ,13,15,1
              |12,13,14,_ ,8 ,_ ,3 ,6 ,_ ,10,_ ,_ ,7 ,5 ,16,4
              |_ ,6 ,_ ,_ ,1 ,15,_ ,_ ,2 ,_ ,_ ,14,_ ,8 ,12,_
              |_ ,15,16,_ ,14,_ ,_ ,9 ,_ ,_ ,6 ,5 ,_ ,_ ,1 ,_
              |11,12,7 ,1 ,_ ,_ ,5 ,_ ,14,15,_ ,10,_ ,4 ,2 ,13
              |2 ,3 ,9 ,_ ,_ ,_ ,7 ,15,_ ,_ ,_ ,13,16,_ ,5 ,_
              |6 ,_ ,5 ,4 ,13,_ ,_ ,_ ,11,_ ,8 ,_ ,_ ,12,_ ,15
              |_ ,_ ,_ ,7 ,_ ,_ ,_ ,_ ,_ ,9 ,_ ,_ ,_ ,_ ,_ ,6
              |_ ,_ ,_ ,12,_ ,5 ,_ ,2 ,3 ,_ ,_ ,15,13,_ ,_ ,8
              |14,_ ,3 ,_ ,_ ,_ ,_ ,13,_ ,11,_ ,6 ,15,_ ,10,_
              |_ ,_ ,_ ,6 ,3 ,9 ,_ ,14,_ ,_ ,10,12,1 ,_ ,4 ,_""".stripMargin
    val bs: BoardState = BoardState.parseFromCsv(stringToReader(s)).get

    val markup: Map[Coords,Vector[Int]] = bs.markup

    bs.emptyLocations.forall(location => {
      bs.possibleNumbers(location).sorted == markup(location).sorted
    })

  }

}
