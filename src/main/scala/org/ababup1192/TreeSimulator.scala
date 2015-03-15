package org.ababup1192

object TreeSimulator {
  def main(args: Array[String]): Unit = {
    val intTree = Tree[Int]
    val node1 = intTree.add(1)
    intTree.add(2, node1)
    intTree.add(3, node1)
    intTree.add(4, node1)
    intTree.removeNode(node1)

    intTree.display()
  }
}

