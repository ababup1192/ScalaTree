package org.ababup1192

import scala.collection.mutable.{ArrayBuffer, OpenHashMap => HashMap}

sealed trait Node

case object RootNode extends Node

case class ElementNode[T](value: T) extends Node

/**
 * Tree data structure
 * @tparam T Node type
 */
class Tree[T] {

  val rootNode = RootNode
  val tree = HashMap[Node, ArrayBuffer[Node]](rootNode -> ArrayBuffer())

  private[this] val positionByNode = HashMap[Node, NodePosition](rootNode -> NodePosition.ROOT)
  private[this] val nodeByPosition = positionByNode.map(_ swap)
  private[this] val childrenByPosition = HashMap[NodePosition, ArrayBuffer[Node]]()
  private[this] val nodeListByLevel = HashMap[Int, ArrayBuffer[Node]](0 -> ArrayBuffer(rootNode))

  /**
   * Make node and add a new node to a tree as a root node children
   * @param value node content
   * @return new node
   */
  def add(value: T): Node = {
    add(value, rootNode)
  }

  /**
   * Make node and add a new node to a tree as children of an assigned node
   * @param value node content
   * @param parentNode a parent node of a new node
   * @return new node
   */
  def add(value: T, parentNode: Node): Node = {
    val node = ElementNode(value)
    addNode(node, parentNode)
    node
  }

  /**
   * Make node and add a new node to a tree as children of an assigned position
   * @param value node content
   * @param parentPosition a parent position of a new node
   * @return new node
   */
  def add(value: T, parentPosition: NodePosition): Node = {
    val node = ElementNode(value)
    addNode(node, parentPosition)
    node
  }

  /**
   * Add a new node to a tree as children of an assigned node
   * @param node new node
   * @param parentNode a parent node of a new node
   */
  protected def addNode(node: Node, parentNode: Node): Unit = {
    getPosition(parentNode).foreach { parentPosition =>
      val newLabel = tree.get(parentNode).map { nodeList =>
        nodeList += node
        nodeList.size - 1
      }.getOrElse {
        tree.put(parentNode, ArrayBuffer(node))
        0
      }
      addPosition(node, parentPosition.child(newLabel))
      addChildren(node, parentPosition)
      addNodeListByLevel(node)
    }
  }

  def getNode(position: NodePosition): Option[Node] = {
    nodeByPosition.get(position)
  }

  /**
   * Add a new node to a tree as children of an assigned node
   * @param node new node
   * @param parentPosition a parent position of a new node
   */
  protected def addNode(node: Node, parentPosition: NodePosition): Unit = {
    getNode(parentPosition).foreach { parentNode =>
      addNode(node, parentNode)
    }
  }

  /**
   * Find a position of node
   * @param node Node
   * @return an optional value of a node position
   */
  def getPosition(node: Node): Option[NodePosition] = {
    positionByNode.get(node)
  }


  /**
   * Add a position to a tree
   * @param node Node
   * @param position NodePosition
   */
  protected def addPosition(node: Node, position: NodePosition): Unit = {
    positionByNode.put(node, position)
    nodeByPosition.put(position, node)
  }


  def getParent(position: NodePosition): Option[Node] = {
    val parentPosition = position.parent
    nodeByPosition.get(parentPosition)
  }

  def getParent(node: Node): Option[Node] = {
    getPosition(node).map { position =>
      val parentPosition = position.parent
      nodeByPosition.get(parentPosition)
    }.getOrElse {
      None
    }
  }

  def getChildren(position: NodePosition): ArrayBuffer[Node] = {
    childrenByPosition.get(position).map { children =>
      children
    }.getOrElse {
      ArrayBuffer.empty[Node]
    }
  }

  def getChildren(node: Node): ArrayBuffer[Node] = {
    getPosition(node).map { position =>
      childrenByPosition.get(position).map { children =>
        children
      }.getOrElse {
        ArrayBuffer.empty[Node]
      }
    }.getOrElse {
      ArrayBuffer.empty[Node]
    }
  }

  protected def addChildren(node: Node, position: NodePosition): Unit = {
    childrenByPosition.get(position).map { children =>
      children += node
    }.getOrElse {
      childrenByPosition.put(position, ArrayBuffer(node))
    }
  }

  def getSibling(node: Node): ArrayBuffer[Node] = {
    getParent(node).map { parentNode =>
      getChildren(parentNode)
    }.getOrElse {
      ArrayBuffer.empty[Node]
    }
  }

  def getSibling(position: NodePosition): ArrayBuffer[Node] = {
    val parentPosition = position.parent
    getChildren(parentPosition)
  }

  def maxLevel: Int = nodeListByLevel.keys.max

  def getLevel(node: Node): Int = {
    getPosition(node).map(_.level).getOrElse(0)
  }

  def getLevel(position: NodePosition): Int = {
    position.level
  }

  protected def addNodeListByLevel(node: Node): Unit = {
    positionByNode.get(node).foreach { position =>
      val level = position.level
      nodeListByLevel.get(level).map { nodeList =>
        nodeList += node
      }.getOrElse {
        nodeListByLevel.put(level, ArrayBuffer(node))
      }
    }
  }

  def printNodeWithPosition(node: Node): Unit = {
    positionByNode.get(node).foreach { position =>
      println(node, position)
    }
  }

  def display(): Unit = {
    (0 to maxLevel).foreach { level =>
      nodeListByLevel.get(level).foreach { nodeList =>
        println(s"level:$level [${nodeList.mkString(", ")}]")
      }
    }
  }
}

object Tree {
  def apply[T] = new Tree[T]
}

