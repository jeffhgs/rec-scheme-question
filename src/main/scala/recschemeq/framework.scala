package recschemeq

import scala.reflect.ClassTag

trait ImplFor[U,V] {
  //def apply(u:U) : V
  def build(u:U) : V
}

trait Magic {
  /**
    * Search for an appropriate implementation of configuration a with type t.
    * May throw exception when a is an invalid configuration.
    * @param a a configuration
    * @param t a desired class for the return type
    * @return a runtime instantiation of the configuration with the desired class
    */
  def lookup[A,T](a:A)(implicit t:ClassTag[T]) : T
}

