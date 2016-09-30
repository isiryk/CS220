package dis02ll

/**
 * This is a skeleton for a LinkedList class. A linked list is a data structure
 * that consists of a collection of Nodes point to each other in sequence.
 * In this class, there is a head (points to the first Node in the list) and a
 * tail (points to the last Node in the list). These are used to access the
 elements and to add elements to the end of the list.
 */
class LinkedList[T] {

  /**
   * Nodes are the building blocks of the LinkedList.
   * Each one contains some data and a pointer to the next Node in the list.
   * 'next' should only be null if this is the last node in the list.
   */
  class Node[T](val data: T) {
    var next: Node[T] = null
  }

  var head: Node[T] = null
  var tail: Node[T] = null
  var size: Int = 0

  /**
   * Returns the element at the given index. Indices start at 0.
   * @throws IndexOutOfBoundsException if index is out of bounds
   * @param index the index of the value to get
   * @return the element at the given index
   */

  def apply(index: Int): T = {
    if(index > size || index < 0){
      throw new IndexOutOfBoundsException
    }
    var temp = head
    for(i <- 0 until index){
      temp = temp.next
    }
    return temp.data

  }

  /**
   * Adds an element to the end of the list.
   * @param elem the element to add
   * @return the LinkedList object, to allow chained calls to add
   */
  def add(elem: T): LinkedList[T] = {
    if (size == 0) {
      head = new Node[T](elem)
      tail = this.head
    } else {
      tail.next = new Node[T](elem)
      tail = tail.next
    }
    size += 1
    this
  }

  /**
   * Returns the index of the first occurrence of the given element.
   * If the element is not in the list, return -1.
   * @param elem the element to find
   * @return the index of the element
   */
  def find(elem: T): Int = {
    if(elem == null){
      return -1
    }
    var tempNode = head
    for(i <- 0 until size){
      if(tempNode.data == elem){
        return i
      }
      tempNode = tempNode.next
    }
    return -1
  }

  /**
   * Removes the element at the given index from the list.
   * @param index the index of the element to remove
   * @return true if the index was in the list and the element was removed. False otherwise.
   */
  def remove(index: Int): Boolean = {
    if(index < 0 || index >= size){
      return false
    }
    if(index == 0 && size == 1){
      head = head.next
      tail = head
      size = size - 1
      return true
    }
    var tempHead = head
    for(i <- 1 until index){
      tempHead = tempHead.next
    }
    if(index == size - 1){
      tail = tempHead
      tempHead.next = tempHead.next.next
      size = size - 1
      return true
    }
    tempHead.next = tempHead.next.next
    size = size - 1
    return true
  }

  /**
   * Removes the first occurrence given element from the list.
   * Hint: Use the functions you already wrote!!
   * @param elem the element to remove
   * @return true if the element was in the list, false otherwise.
   */
  def removeElem(elem: T): Boolean = {
    if (head.data == elem && size == 1) {
      head = head.next
      tail = head
      size = size - 1
      return true
    }
    var tempHead = head
    while (tempHead.next != elem) {
      if (tempHead.next == elem) {
        tempHead = tempHead.next.next
        size = size - 1
        return true
      }
    tempHead = tempHead.next
    }
    return false
  }
}

object LinkedList {
  /**
   * Constructs an empty linked list.
   */
  def apply[T](): LinkedList[T] = {
    val list = new LinkedList[T]()
    list
  }

  /**
   * Builds a linked list with the given values. Order should be preserved.
   * @param args The values to insert into the list.
   * @return The populated linked list.
   */
  def apply[T](args: T*): LinkedList[T] = {
    val list = new LinkedList[T]()
    for(i <- 0 until args.size){
      list.add(args(i))
    }
    list
  }
}
