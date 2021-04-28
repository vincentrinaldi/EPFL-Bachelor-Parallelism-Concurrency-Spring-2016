package lockfree

import scala.annotation.tailrec

class SortedList extends AbstractSortedList {

  // The sentinel node at the head.
  private val _head = createNode(0, None, isHead = true)

  // The first logical node is referenced by the head.
  def firstNode: Option[Node] = _head.next

  // Finds the first node whose value satisfies the predicate.
  // Returns the predecessor of the node and the node.
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node]) = {
    def helper(pred: Int => Boolean, predecessor: Node, current: Option[Node]): (Node, Option[Node]) = {
      if (current == None) (predecessor, None)
      else if (current.get.atomicState.get._2) {
        if (predecessor.atomicState.compareAndSet((current, false), (current.get.next, false))) {}
        helper(pred, _head, firstNode)
      } 
      else if (pred(current.get.value)) (predecessor, current)
      else helper(pred, current.get, current.get.next)
    }
    helper(pred, _head, firstNode)
  }

  // Insert an element in the list.
  def insert(e: Int): Unit = {
    val pairToAdd = findNodeWithPrev(x => e < x)
    val newNode = createNode(e, pairToAdd._2)
    if (pairToAdd._1.atomicState.compareAndSet((pairToAdd._2, false), (Some(newNode), false))) {}
    else insert(e)
  }

  // Checks if the list contains an element.
  def contains(e: Int): Boolean = {
    if (findNodeWithPrev(x => e == x)._2 == None) false else true
  }

  // Delete an element from the list.
  // Should only delete one element when multiple occurences are present.
  def delete(e: Int): Boolean = {
    val pairToDelete = findNodeWithPrev(x => e == x)
    if (pairToDelete._2 == None) false
    else if (pairToDelete._2.get.mark) true else delete(e)
  }
}