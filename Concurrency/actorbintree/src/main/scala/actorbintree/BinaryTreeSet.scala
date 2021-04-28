/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case operation: Operation => root ! operation
    case GC => {
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case operation: Operation => pendingQueue = pendingQueue.enqueue(operation)
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot
      pendingQueue.map(root ! _)
      pendingQueue = Queue.empty
      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Contains(requester, id, elemToFind) => {
      if (elemToFind == elem && removed) requester ! ContainsResult(id, false)
      else if (elemToFind != elem) {
        val nextPosition = if (elemToFind > elem) Right else Left
        if (subtrees.contains(nextPosition)) subtrees(nextPosition) ! Contains(requester, id, elemToFind)
        else requester ! ContainsResult(id, false)
      } else requester ! ContainsResult(id, true)
    }
    case Insert(requester, id, elemToInsert) => {
      if (elemToInsert == elem && removed) {
        removed = false
        requester ! OperationFinished(id)
      } else if (elemToInsert != elem) {
          val nextPosition = if (elemToInsert > elem) Right else Left
          if (subtrees.contains(nextPosition)) subtrees(nextPosition) ! Insert(requester, id, elemToInsert)
          else {
            subtrees += (nextPosition -> context.actorOf(BinaryTreeNode.props(elemToInsert, false)))
            requester ! OperationFinished(id)
          }
      } else requester ! OperationFinished(id)
    }
    case Remove(requester, id, elemToRemove) => {
      if (elemToRemove == elem && removed) requester ! OperationFinished(id)
      else if (elemToRemove != elem) {
          val nextPosition = if (elemToRemove > elem) Right else Left
          if (subtrees.contains(nextPosition)) subtrees(nextPosition) ! Remove(requester, id, elemToRemove)
          else requester ! OperationFinished(id)
      } else {
        removed = true
        requester ! OperationFinished(id)
      }
    }
    case CopyTo(newRoot) => {
      context.become(copying(subtrees.values.toSet, removed))
      if (!removed) newRoot ! Insert(self, 0, elem)
      subtrees.values foreach (_ ! CopyTo(newRoot))
      if (removed && subtrees.isEmpty) context.parent ! CopyFinished
    }
  }

  // optional
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(_) => {
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        context.become(normal)
      } else context.become(copying(expected, true))
    }
    case CopyFinished => {
      val newSet = expected - sender
      if (newSet.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        context.become(normal)
      } else context.become(copying(newSet, insertConfirmed))
    }
  }
}
