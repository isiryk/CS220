package dis02ll

import dis02ll.LinkedList._
import org.scalatest.FunSuite

class LinkedListSuite extends FunSuite {

  def testList(): LinkedList[Int] = LinkedList(0,1,2,3,4,5,6,7,8,9)

  test("[1] Empty linked list creation") {
    val l = new LinkedList() // Default constructor
    assert(l.head == null, "Empty list should have null head")
    assert(l.size == 0, "Empty list should have size == 0")

    val o = LinkedList() // Creation using companion object
    assert(l.head == null, "Empty list should have null head")
    assert(l.size == 0, "Empty list should have size == 0")
  }

  test("[2] Adding an element") {
    val l = LinkedList[Int]()
    l.add(1)
    assert(l.head != null, "Head should not be null after adding an element")
    assert(l.tail != null, "Tail shound not be null after adding an element")
    assert(l.head == l.tail, "Head and tail should point to the same Node")
    assert(l.head.data == 1, "Head should contain '1'")
    assert(l.size == 1, "The list should have size 1 after adding an element")
  }

  test("[3] Adding multiple elements") {
    val l = LinkedList[Int]()
    for(c <- 1 to 10)
      l.add(c)
    assert(l.size == 10)
    var p = l.head.data
    var curr = l.head.next
    while (curr != null) {
      assert(p + 1 == curr.data)
      p = curr.data
      curr = curr.next
    }
  }

  test("[4] Create list with multiple elements") {
    val l = LinkedList(2,4,6,8,10) // Using the advanced copy constructor
    assert(l.size == 5)
    assert(l.head != null && l.tail != null)
    var p = l.head.data
    var curr = l.head.next
    while(curr != null) {
      assert(p + 2 == curr.data)
      p = curr.data
      curr = curr.next
    }
  }

  test("[5] LinkedList apply() method") {
    val l = LinkedList(1,2,3,4,5,6,7)
    assert(l.size == 7)

    assert(l(0) == 1, "apply(0) should return first element")
    assert(l(l.size-1) == 7, "apply(size-1) should return last element")
  }

  test("[6] Find elements in list") {
    val l = testList()
    assert(l.find(1) == 1)
    assert(l.find(9) == l.size-1)
    assert(l.find(11) == -1)
    assert(l.find(-100000) == -1)
  }

  test("[7] Remove element from middle of list") {
    val l = testList()
    val size = l.size
    val elem2 = l(2)
    val elem3 = l(3)
    assert(l.remove(2))
    assert(l.size == size-1)
    assert(l(2) != elem2)
    assert(l(2) == elem3)
  }

  test("[8] Remove first element") {
    val l = LinkedList(1)
    assert(l.remove(0))
    assert(l.head == l.tail, "After removal, head and tail should be equal")
    assert(l.head == null, "After removal, head should be null")
    assert(l.size == 0, "After removal, size should be 0")
  }
}
