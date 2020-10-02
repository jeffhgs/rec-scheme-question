package recschemeq

import java.lang.reflect.Method

import org.reflections.Reflections
import org.reflections.scanners.SubTypesScanner
import org.reflections.util.{ClasspathHelper, ConfigurationBuilder, FilterBuilder}
import collection.JavaConverters._

object flakyreflection {

  /**
    * Automatically scan for ImplFor implementations and register them.
    *
    * @param prefixOfPackage Java package prefix to scan
    */
  def init(prefixOfPackage:String) = {
    //println("about to find ImplFor declarations")
    val reflections = new Reflections(new ConfigurationBuilder()
      .setUrls(ClasspathHelper.forPackage(prefixOfPackage))
      .setScanners(new SubTypesScanner())
      .filterInputsBy(new FilterBuilder().includePackage(prefixOfPackage))
    )
    for (
      clImplFor <- reflections.getSubTypesOf(classOf[recschemeq.ImplFor[_, _]]).asScala;
      obj <- findCompanion[recschemeq.ImplFor[_, _]](clImplFor);
      mBuild <- clImplFor.getMethods.toSeq.filter(isBuild);
      (clTo,clFrom) <- findParams(mBuild)
    ) {
      //println(s"found ${clImplFor.getName} obj=${obj} clTo=${clTo} clFrom=${clFrom}")
      DirtyMagic.instance.register(obj)(reflect.ClassTag(clFrom), reflect.ClassTag(clTo))
    }
  }

  private def findCompanion[T](cl:Class[_])(implicit man:Manifest[T]) : Seq[T] =
    try {
      Seq(cl.getField("MODULE$").get(man.erasure).asInstanceOf[T])
    } catch {
      case ex: Throwable =>
        Seq()
    }

  private def findParams(m: Method) : Seq[(Class[_],Class[_])] = {
    if (m.getParameterTypes.nonEmpty) {
      Seq((m.getReturnType, m.getParameterTypes()(0)))
    } else {
      Seq()
    }
  }

  private def isBuild(m:Method) = {
    m.getName() == "build" && m.getReturnType != classOf[java.lang.Object]
  }
}
