package info.kwarc.mmt.uom

import java.net._
import java.io._
import java.util.jar._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import scala.collection.mutable.HashMap

object UOMServer {

  val impls = new HashMap[GlobalName, Implementation]

  def register(jarFileName : String) {
    val urlArray = new Array[URL](1)
    val jarFile = new File(jarFileName)
    urlArray(0) = jarFile.toURI.toURL 
    val child = new URLClassLoader(urlArray, this.getClass.getClassLoader)

    val jarInput = new JarInputStream(new FileInputStream(jarFileName))
    while(true) {
      val jarEntry = jarInput.getNextJarEntry
      if (jarEntry == null) {
        return
      }
      if ( jarEntry.getName.endsWith(".class") ) {
        val className = jarEntry.getName()
          .replaceAll("/", "\\.")
          .replaceAll(".class","")
        
        val classToLoad = Class.forName(className, true, child)
        classToLoad.getDeclaredMethods.map(method => { 
          if (method.getReturnType.getName.equals(
              "info.kwarc.mmt.uom.Implementation")) {
            val instance = classToLoad.newInstance
            val invokeResult = method.invoke(instance)
            invokeResult match {
              case impl : Implementation => { 
                impls(impl.name) = impl
              }
              case _ => {
                System.err.println("Wrong return type of method")
                System.exit(1)
              } 
            }
          }
        })
      }
    }
  }

  def simplify(term : Term) : Term = {
    term match {
      case OMA( OMID( GlobalName(parent, LocalName(List(NamedStep(name))))), 
          args) => {
        val term_* = GlobalName(parent, LocalName(List(NamedStep(name+"_*"))))
        if (impls.contains(term_* ))
          return impls(term_*).apply(args.map(simplify) :_*)
        else
          return OMA(OMID(GlobalName(parent, LocalName(List(NamedStep(name))))
                         ), args.map(simplify))
      }
      case _ => return term
    }
  }

  def main (args : Array[String]) {
    register(args(0))

    /* Test with some example terms */
    val base = DPath(new utils.xml.URI("http://cds.omdoc.org/unsorted/uom.omdoc"))
    val gname1 = GlobalName(OMMOD(MPath(base, LocalPath(List(
          "lists")))), LocalName(List(NamedStep("append_*"))))
    val gname2 = GlobalName(OMMOD(MPath(base, LocalPath(List(
          "lists_ext")))), LocalName(List(NamedStep("append_many_*"))))

    val ex = new org.omdoc.cds.unsorted.uom.omdoc.lists
    val ext = new org.omdoc.cds.unsorted.uom.omdoc.lists_ext

    /* append( [el], [el])  */
    val c1 = OMA(ex.append, OMA(ex.cons, ex.elem::ex.nil::Nil) :: 
        OMA(ex.cons, ex.elem::ex.nil::Nil) ::Nil)
    val t1 = (simplify(c1))
    println(t1+"\n\n")

    /* append_many([el], [el], [el]) */
    val c2 =(OMA(ext.append_many, List(
      OMA(ex.cons, ex.elem::ex.nil::Nil), 
      OMA(ex.cons, ex.elem::ex.nil::Nil),
      OMA(ex.cons, ex.elem::ex.nil::Nil)
      )))
    val t2 = simplify(c2)
    println(t2+"\n\n")

    /* append_many(c2)  */
    val c2_equal = OMA(ext.append_many, c2::Nil)
    if (simplify(c2_equal).equals(t2)) 
      println("They are the same as expceted\n\n")

    /* append(c1, c2)  */
    val c3 = OMA(ex.append, c1::c2::Nil)
    println(simplify(c3)+"\n\n")

    /* very composite, result should be a list with 10 el */
    val c4 = OMA(ext.append_many, c3::c2_equal::c1::Nil)
    println(simplify(c4))
  }
}



