package org.ababup1192

import org.scalatest._

class NodePositionSpec extends FlatSpec with Matchers {

  "The root position" should "have empty path" in {

    NodePosition.ROOT.path shouldBe Seq()
    NodePosition().path shouldBe Seq()
  }

  "A parent of the root" should "be the root position " in {
    NodePosition.ROOT.parent shouldBe NodePosition.ROOT
  }

  "A parent of a position with a single element" should "be the root position " in {
    NodePosition(1).parent shouldBe NodePosition.ROOT
    NodePosition(Vector(1)).parent shouldBe NodePosition.ROOT
  }

  "Positions with a same path" should "be same" in {
    val position1 = NodePosition(1, 2, 3)
    val position2 = NodePosition(Vector(1, 2, 3))

    position1.path shouldBe position2.path
    position1 shouldBe position2
  }

  "A parent of siblings" should "be same" in {
    val parent = NodePosition(0)
    val childA = parent.child(1)
    val childB = parent.child(2)

    childA.parent shouldBe childB.parent
  }

  "Levels of positions with a same path length" should "be same" in {
    val position1 = NodePosition(1, 2, 3)
    val position2 = NodePosition(Vector(1, 2, 3))

    position1.path.length shouldBe position2.path.length
    NodePosition(1, 2, 3).level shouldBe NodePosition(Vector(1, 2, 3)).level
  }

}
