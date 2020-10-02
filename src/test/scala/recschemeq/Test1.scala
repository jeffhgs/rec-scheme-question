package recschemeq

import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class SetSuite extends AnyFunSuite with BeforeAndAfter {

  /*
  test("An empty Set should have size 0") {
    assert(Set.empty.size == 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }*/

  before {
    Init.init()
  }

  test("can lookup Orange") {
    val o1 = Orange("navel")
    DirtyMagic.instance.lookup[Orange, FoodImpl](o1)
  }

  test("can lookup Bananna") {
    val o1 = Bananna("chiquita")
    DirtyMagic.instance.lookup[Bananna, FoodImpl](o1)
  }

  test("can lookup Lunch") {
    val o1 = Lunch(Seq())
    DirtyMagic.instance.lookup[Lunch, LunchImpl](o1)
  }

  test("can eat Orange") {
    val o1 = Orange("navel")
    val o1Run = DirtyMagic.instance.lookup[Orange, FoodImpl](o1)
    o1Run.eat()
  }

  test("can eatAll Lunch") {
    val o1 = Lunch(Seq(Orange("navel"), Bananna("chiquita"), Orange("blood")))
    val o1Run = DirtyMagic.instance.lookup[Lunch, LunchImpl](o1)
    o1Run.eatAll()
  }

}
