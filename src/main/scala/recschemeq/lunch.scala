package recschemeq

trait Food {}

case class Orange(name:String) extends Food

case class Bananna(name:String) extends Food

case class Lunch(foods:Seq[Food])

trait FoodImpl {
  def eat() : Unit
}

trait LunchImpl {
  def eatAll() : Unit
}

class BanannaImpl(bananna: Bananna) extends FoodImpl {
  override def eat() = {
    println(s"${bananna.name} bananna was yummy")
  }
}
object BanannaImpl extends ImplFor[Bananna, FoodImpl] {
  override def build(u: Bananna): FoodImpl = {
    new BanannaImpl(u)
  }
}

class OrangeImpl(orange: Orange) extends FoodImpl {
  override def eat() = {
    println(s"${orange.name} orange was yummy")
  }
}
object OrangeImpl extends ImplFor[Orange, FoodImpl] {
  override def build(u: Orange): FoodImpl = {
    new OrangeImpl(u)
  }
}

class MyLunchImpl(magic:Magic, lunch:Lunch) extends LunchImpl {
  val foods : Seq[FoodImpl] = lunch.foods.map(f => magic.lookup[Food,FoodImpl](f))
  // throws if any children throw

  override def eatAll(): Unit = {
    println(s"Lunch has ${foods.length} foods")
    for(food <- foods) {
      food.eat()
    }
    println(s"Finished with lunch")
  }
}
object MyLunchImpl extends ImplFor[Lunch, LunchImpl] {
  override def build(u: Lunch): LunchImpl = {
    new MyLunchImpl(DirtyMagic.instance, u)
  }
}
/*
// For some reason this won't work: (or won't work in scalatest?)
object BanannaImplReg {
  DirtyMagic.instance.register[Bananna, FoodImpl](BanannaImpl)
}
*/
object Init {
  def init(): Unit = {
    DirtyMagic.instance.register[Bananna, FoodImpl](BanannaImpl)
    DirtyMagic.instance.register[Orange, FoodImpl](OrangeImpl)
    DirtyMagic.instance.register[Lunch, LunchImpl](MyLunchImpl)
  }
}


