package recschemeq

import scala.collection.mutable
import scala.reflect.ClassTag

class DirtyMagic() extends Magic {
  val mp : mutable.Map[(Class[_],Class[_]),Object] = new mutable.HashMap[(Class[_],Class[_]),Object]()
  /**
    * Search for an appropriate implementation of configuration a with type t,
    * then build an instance from the configuration.
    * May throw exception when a is an invalid configuration.
    *
    * Possible improvements include:
    *   - Use reflection to call .register automatically
    *   - Check to make sure registrations aren't changing
    *   - Separate configuration checking, which is allowed to fail, from
    *     instantiation from a checked configuration, which should not fail.
    *
    * @param a a configuration
    * @param t a desired class for the return type
    * @return a runtime instantiation of the configuration with the desired class
    */
  override def lookup[A, T](a: A)(implicit t: ClassTag[T]): T = {
    val cl1 = a.getClass
    val cl2 = t.runtimeClass
    mp.get((cl1,cl2)) match {
      case Some(b) =>
        val f = b.asInstanceOf[ImplFor[A,T]]
        f.build(a)
      case None => throw new DirtyMagic.LookupError(s"Could not find ${cl2} implementation for ${cl1}")
    }
  }
  def register[U,V](obj:ImplFor[U,V])(implicit u:ClassTag[U], v:ClassTag[V]) = {
    val cl1 = u.runtimeClass
    val cl2 = v.runtimeClass
//    println(s"registering ${cl2} implementation ${cl1}")
    mp.update((cl1,cl2), obj)
  }
}
object DirtyMagic {
  case class LookupError(msg:String) extends Throwable

  private var _instance : Option[DirtyMagic] = None
  def instance : DirtyMagic = {
    _instance match {
      case None =>
        val i = new DirtyMagic()
        _instance = Some(i)
        i
      case Some(i) =>
        i
    }
  }
}
