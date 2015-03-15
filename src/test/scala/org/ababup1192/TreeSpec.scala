package org.ababup1192

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  "Int node(1)" should "be able to trace position" in {
    val intTree = Tree[Int]
    val node1 = intTree.add(1)
    /*
      Root => Node(1): Position(0)
    */
    intTree.getPosition(node1) shouldBe Some(NodePosition(0))
  }

  "Int node(1) as children of rootNode" should "be able to trace position" in {
    val intTree = Tree[Int]
    val node1 = intTree.add(1, intTree.rootNode)
    /*
      Root => Node(1): Position(0)
    */
    intTree.getPosition(node1) shouldBe Some(NodePosition(0))
  }

  "Int node(1) which is made by assign a position of a parent node" should "be able to trace position" in {
    val intTree = Tree[Int]
    val node1 = intTree.add(1, NodePosition.ROOT)
    /*
      Root => Node(1): Position(0)
    */
    intTree.getPosition(node1) shouldBe Some(NodePosition(0))
  }

  "A Parent of Int node(1)" should "be the root node" in {
    val intTree = Tree[Int]
    val node1 = intTree.add(1)
    /*
      Root => Node(1): Position(0)
    */
    intTree.getParent(node1) shouldBe Some(intTree.rootNode)
    intTree.getParent(intTree.getPosition(node1).get) shouldBe Some(intTree.rootNode)
  }

  "Max level of int tree with int node(1) and node(2)" should "be 1" in {
    val intTree = Tree[Int]
    val node1 = intTree.add(1)
    val node2 = intTree.add(2)
    /*
      Root => Node(1): Position(0), Node(2): Position(1)
    */
    intTree.maxLevel shouldBe 1
    intTree.getChildren(NodePosition.ROOT) shouldBe List(node1, node2)
  }

  "Max level of int tree with some nodes" should "be 4" in {
    val intTree = Tree[Int]
    val node1 = intTree.add(1)
    val node2 = intTree.add(2, node1)
    val node3 = intTree.add(3, node2)
    val node4 = intTree.add(4, node3)
    val node5 = intTree.add(5, node1)
    val node6 = intTree.add(6, node1)

    /*
      Root => Node(1): Position(0) => Node(2): Position(00), Node(5): Position(01), Node(6): Position(02)
       => Node(3): Position(000) => Node(4): Position(0000)
    */
    intTree.maxLevel shouldBe 4
    intTree.getLevel(node4) shouldBe 4
    intTree.getLevel(intTree.getPosition(node5).get) shouldBe 2
    intTree.getSibling(node2) shouldBe List(node2, node5, node6)
    intTree.getNodeListByLevel(3) shouldBe List(node3)
    intTree.getSibling(intTree.getPosition(node4).get) shouldBe List(node4)
    intTree.getPosition(node6) shouldBe Some(NodePosition(0, 2))
  }

  "An int tree" should "remove node" in {
    val intTree = Tree[Int]
    val node1 = intTree.add(1)
    val node2 = intTree.add(2, node1)
    val node3 = intTree.add(3, node1)
    val node4 = intTree.add(4, node1)
    val node3Position = intTree.getPosition(node3).get
    intTree.removeNode(node3)

    /*
      [Before]
        Root => Node(1): Position(0) => Node(2): Position(00), Node(3): Position(01), Node(4): Position(02)
      [After]
        Root => Node(1): Position(0) => Node(2): Position(00), Node(4): Position(02)
    */
    intTree.getNode(node3Position) shouldBe None
    intTree.getChildren(node1) shouldBe List(node2, node4)
    intTree.getNodeListByLevel(2) shouldBe List(node2, node4)
  }

}
