package org.ababup1192

object TreeSimulator {
  def main(args: Array[String]): Unit = {
    val intTree = Tree[Int]
    val node1 = intTree.add(1)
    intTree.add(2, node1)
    val node3 = intTree.add(3, node1)
    intTree.add(4, node1)
    intTree.removeNode(node3)

    visit(intTree, intTree.rootNode)
    intTree.display()
  }

  def visit(intTree: Tree[Int], visitedNode: TreeNode, level: Int = 1): Unit = {
    intTree.tree.get(visitedNode) match {
      case Some(list) =>
        list.toList.foreach { node =>
          println(s"level:$level, node:$node, parent:$visitedNode")
          visit(intTree, node, level + 1)
        }
      case _ =>
    }
  }
}

