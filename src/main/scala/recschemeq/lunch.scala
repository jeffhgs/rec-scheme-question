package recschemeq

trait Food {}

case class Orange(name:String) extends Food

case class Bananna(name:String) extends Food

case class Lunch(foods:Seq[Food])

trait FoodRun {
  def eat() : Unit
}

trait LunchRun {
  def eatAll() : Unit
}

class BanannaImpl(bananna: Bananna) extends FoodRun {
  override def eat() = {
    println(s"${bananna.name} bananna was yummy")
  }
}
object BanannaImpl extends ImplFor[Bananna, FoodRun] {
  override def build(u: Bananna): FoodRun = {
    new BanannaImpl(u)
  }
}

class OrangeImpl(orange: Orange) extends FoodRun {
  override def eat() = {
    println(s"${orange.name} orange was yummy")
  }
}
object OrangeImpl extends ImplFor[Orange, FoodRun] {
  override def build(u: Orange): FoodRun = {
    new OrangeImpl(u)
  }
}

// Q: can LunchImpl be implemented using a recursion scheme or similar?
//    If so, what would it look like?
class LunchImpl(magic:Magic, lunch:Lunch) extends LunchRun {
  val foods : Seq[FoodRun] =
    lunch.foods.map(f => magic.lookup[Food,FoodRun](f))
  // initialization throws if any children initialization throws

  override def eatAll(): Unit = {
    println(s"Lunch has ${foods.length} foods")
    for(food <- foods) {
      food.eat()
    }
    println(s"Finished with lunch")
  }
}
object LunchImpl extends ImplFor[Lunch, LunchRun] {
  override def build(u: Lunch): LunchRun = {
    new LunchImpl(DirtyMagic.instance, u)
  }
}
/*
// For some reason this won't work: (or won't work in scalatest?)
object BanannaImplReg {
  DirtyMagic.instance.register[Bananna, FoodImpl](BanannaImpl)
}
*/
object Init {
  // This is a hard-coded version of the flakyreflection.init method
  def init(): Unit = {
    DirtyMagic.instance.register[Bananna, FoodRun](BanannaImpl)
    DirtyMagic.instance.register[Orange, FoodRun](OrangeImpl)
    DirtyMagic.instance.register[Lunch, LunchRun](LunchImpl)
  }
}


