package org.ababup1192

object TreeSimulator {
  def main(args: Array[String]): Unit = {
    val tree = Tree[Int]
    val oneNode = tree.add(1)
    val twoNode = tree.add(2, oneNode)
    val threeNode = tree.add(3, twoNode)
    tree.add(4, twoNode)
    tree.add(5, tree.getPosition(threeNode).get)
    tree.add(6, tree.getPosition(threeNode).get)
    tree.add(7, tree.getPosition(threeNode).get)

    tree.display()
  }
}

