/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import akka.event.Logging
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

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
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
  val log = Logging(context.system, this)

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = { 
    case Insert(requester, id, elem) => {
      root ! Insert(requester, id, elem)
    } 
    case Contains(requester, id, elem) => {
      root ! Contains(requester, id, elem)
    }
    case Remove(requester, id, elem) => {
      root ! Remove(requester, id, elem)
    }
    case GC => {
      var newRoot = createRoot
      log.info("In Garbage Collection Mode")
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive =  {
    case GC => PartialFunction.empty
    case CopyFinished => {
      log.info("Received root level CopyFinished")
      root ! PoisonPill
      root = newRoot
      context.become(normal)
      log.info("queue size " + pendingQueue.size)
      pendingQueue foreach {
         op => {
          root ! op
        }
      }
      pendingQueue = Queue.empty[Operation]
    }
    case op:Operation  => {
      // log.info("Operation queued while in GC Mode")
      pendingQueue = pendingQueue enqueue op
    } 
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  val log = Logging(context.system, this)

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, elem) => {
      // log.info(s"Insert $id with elem $elem")
      if(this.elem == elem) {
        removed = false;
        requester ! OperationFinished(id)
      }
      else if(elem < this.elem)  
        if(subtrees.contains(Left)) 
          subtrees(Left) ! Insert(requester, id, elem)
        else {
          subtrees += (Left -> context.actorOf(props(elem, false)))
          requester ! OperationFinished(id)
        }
      else 
        if(subtrees.contains(Right)) subtrees(Right) ! Insert(requester, id, elem)
        else {
          subtrees += (Right -> context.actorOf(props(elem, false)))
          requester ! OperationFinished(id)
        }
    } 
    case Contains(requester, id, elem) => {
      // log.info(s"Contains $id with elem $elem")
      if(this.elem == elem && !removed) requester ! ContainsResult(id, true)
      // else if(this.elem == elem && removed) requester ! ContainsResult(id, false)
      else if(elem < this.elem) 
        if(subtrees.contains(Left)) subtrees(Left) ! Contains(requester, id, elem)
        else requester ! ContainsResult(id, false)
      else 
        if(subtrees.contains(Right)) subtrees(Right) ! Contains(requester, id, elem)
        else requester ! ContainsResult(id, false)
    }
    case Remove(requester, id, elem) => {
      // log.info(s"Remove $id with elem $elem")
      if(this.elem == elem) {
        this.removed = true;
        requester ! OperationFinished(id)
      }
      else if(elem < this.elem) 
        if(subtrees.contains(Left)) subtrees(Left) ! Remove(requester, id, elem)
        else requester ! OperationFinished(id)
      else 
        if(subtrees.contains(Right)) subtrees(Right) ! Remove(requester, id, elem)
        else requester ! OperationFinished(id)
    }
    case CopyTo(treeNode) => {
        var insertConfirmed = true
        var expected = Set[ActorRef]()

        if(!this.removed) {
          insertConfirmed = false
        } else {
          insertConfirmed = true
        }

        if(subtrees.contains(Left)) {
          subtrees(Left) ! CopyTo(treeNode)
          expected += subtrees(Left)          
        }

        if(subtrees.contains(Right)) {
          subtrees(Right) ! CopyTo(treeNode)
          expected += subtrees(Right)
        }

        context.become(copying(expected, insertConfirmed))

        if(!insertConfirmed) {
          log.info("Inserting " + this.elem.toString + " into the new root")
          treeNode ! Insert(self, 1, this.elem)
        }

        if(subtrees.isEmpty && insertConfirmed) {
          log.info("Reporting CopyFinished ")
          context.parent ! CopyFinished
        }
        
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {

    // if insertion is not yet confirmed, wait for OperationFinished message from the insert
      case OperationFinished(id) => {
        log.info("Insert to another root  finished at: " + this.elem.toString)

         if(expected.isEmpty) {
          log.info("About to send CopyFinished to parent at " + this.elem.toString)
          context.parent ! CopyFinished
        } else {
         context.become(copying(expected, true))
        }
      }
      case CopyFinished => {
        log.info("Child sent CopyFinished to" +  this.elem.toString + " with remaining children? = " + expected.isEmpty.toString)
        if(expected.isEmpty && insertConfirmed) {
         log.info("About to send CopyFinished to parent at" + this.elem.toString)
         context.parent ! CopyFinished 
        } else if(expected.isEmpty && !insertConfirmed){
          context.become(copying(expected, insertConfirmed))
        } else if((expected - expected.head).isEmpty && insertConfirmed) {
          context.parent ! CopyFinished
        } else {
          context.become(copying(expected - expected.head, insertConfirmed))
        }
      }
  }
}
