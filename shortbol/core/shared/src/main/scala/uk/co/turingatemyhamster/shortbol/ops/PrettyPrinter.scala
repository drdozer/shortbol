package uk.co.turingatemyhamster
package shortbol
package ops

import _root_.shapeless._
import shortbol.{longhandAst => lAst}
import shortbol.{shorthandAst => sAst}

object PrettyPrinter {
  def apply(out: Appendable): PrettyPrinter = new PrettyPrinter(out)
}

trait PrintApp[T] {
  def apply(t: T): Unit
}


object PrintApp extends TypeClassCompanion[PrintApp] {
  implicit class PrintAppOps[T](val _t: T) extends AnyVal {
    def append(implicit pa: PrintApp[T]): Unit = pa apply _t
  }

  def using[T](f: T => Unit): PrintApp[T] = new PrintApp[T] {
    override def apply(t: T) = f(t)
  }

  object typeClass extends TypeClass[PrintApp] {
    override def coproduct[L, R <: Coproduct](cl: => PrintApp[L],
                                              cr: => PrintApp[R]) = PrintApp.using[:+:[L, R]]({
      case Inl(l) => cl apply l
      case Inr(r) => cr apply r
    })

    override val emptyCoproduct = PrintApp.using[CNil](_ => {})

    override def product[H, T <: HList](ch: PrintApp[H],
                                        ct: PrintApp[T]) = PrintApp.using({
      case h :: t =>
        ch apply h
        ct apply t
    })

    override val emptyProduct = PrintApp.using[HNil](_ => {})

    override def project[F, G](instance: => PrintApp[G],
                               to: (F) => G,
                               from: (G) => F) = PrintApp.using(a => instance apply to(a))
  }
}

/**
 * Created by nmrp3 on 15/06/15.
 */
class PrettyPrinter(out: Appendable, indent: Int = 0, indentDepth: Int = 2) {
  import PrintApp._

  def apply(s: sAst.SBFile) = s.append
  def apply(s: lAst.SBFile) = s.append
  def apply(t: sAst.TpeConstructor1) = t.append
  def apply(c: sAst.ConstructorApp) = c.append
  def apply(c: sAst.TopLevel.ConstructorDef) = c.append

  implicit val literal = PrintApp[sAst.Literal]
  implicit val identifier = PrintApp[sAst.Identifier]
  implicit val tpeConstructor = PrintApp[sAst.TpeConstructor]
  implicit val valueExp = PrintApp[sAst.ValueExp]
  implicit val topLevel = PrintApp[sAst.TopLevel]

  implicit def seq[T](implicit pa: PrintApp[List[T]]): PrintApp[Seq[T]] =
    PrintApp.typeClass.project(pa, _.to[List], implicitly[List[T]<:<Seq[T]])

  def appendNested(bdy: Seq[sAst.BodyStmt]) = bdy.append

  implicit lazy val indentStr = "\n" + (" " * indent)

  implicit lazy val string: PrintApp[String] = PrintApp.using(out append _)

  implicit lazy val instanceExp: PrintApp[sAst.InstanceExp] = PrintApp.using { ie =>
    indentStr.append
    ie.identifier.append
    " : ".append
    ie.cstrApp.append
  }

  implicit lazy val constructorDef: PrintApp[sAst.ConstructorDef] = PrintApp.using { cd =>
      indentStr.append
      cd.id.append
      if(cd.args.nonEmpty) {
        "(".append
        cd.args.head.append
        for(a <- cd.args.tail) {
          ", ".append
          a.append
        }
        ")".append
      }
      " => ".append
      cd.cstrApp.append
  }

  implicit lazy val blankLine: PrintApp[sAst.BlankLine] = PrintApp.using ( _ => "\n".append)

  implicit lazy val comment: PrintApp[sAst.Comment] = PrintApp.using { c =>
    "#".append
    c.commentText.append
    // fixme: should we have a newline here?
  }

  implicit lazy val constructorApp: PrintApp[sAst.ConstructorApp] = PrintApp.using { app =>
    app.cstr.append
    if(app.body.nonEmpty) {
      val pp = new PrettyPrinter(out, indent = indent + indentDepth)
      pp.appendNested(app.body)
    }
  }

  implicit lazy val qname: PrintApp[sAst.QName] = PrintApp.using { q =>
    q.prefix.append
    ":".append
    q.localName.append
  }

  implicit lazy val url: PrintApp[sAst.Url] = PrintApp.using { u =>
    "<".append
    u.url.append
    ">".append
  }

  implicit lazy val nsPrefx: PrintApp[sAst.NSPrefix] = PrintApp.using { _.pfx.append }

  implicit lazy val localName: PrintApp[sAst.LocalName] = PrintApp.using { _.name.append }

  implicit lazy val tpeConstructor1: PrintApp[sAst.TpeConstructor1] = PrintApp.using { c =>
    c.id.append
    if(c.args.nonEmpty) {
      "(".append
      c.args.head.append
      for(a <- c.args.tail) {
        ", ".append
        a.append
      }
      ")".append
    }
  }

  implicit lazy val tpeConstrcutorStar: PrintApp[sAst.TpeConstructorStar] = PrintApp.using { _ =>
    "*".append
  }

  implicit lazy val stringLiteral: PrintApp[sAst.StringLiteral] = PrintApp.using { str =>
    str.style.append
    str.datatype.foreach(_.append)
    str.language.foreach(_.append)
  }

  implicit lazy val singleLine: PrintApp[sAst.StringLiteral.SingleLine] = PrintApp.using { str =>
    if(str.isEscaped) {
      "{".append
      str.asString.append
      "}".append
    } else {
      "\"".append
      str.asString.append
      "\"".append
    }
  }

  implicit lazy val mutliLine: PrintApp[sAst.StringLiteral.MultiLine] = PrintApp.using { str =>
    val indentS = " " * str.indent
    "{\n".append
    indentS.append
    for(l <- str.ss) {
      l.append
      indentS.append
    }
    "}".append
  }

  implicit lazy val datatype: PrintApp[sAst.Datatype] = PrintApp.using { dt =>
    "^^".append
    dt.tpe.append
  }

  implicit lazy val language: PrintApp[sAst.Language] = PrintApp.using { l =>
    "@".append
    l.tag.append
  }

  implicit lazy val integerLiteral: PrintApp[sAst.IntegerLiteral] = PrintApp.using {
    _.i.toString.append
  }

  implicit lazy val assignment: PrintApp[sAst.Assignment] = PrintApp.using { a =>
    a.property.append
    " = ".append
    a.value.append
  }

  implicit lazy val propertyExp: PrintApp[sAst.PropertyExp] = PrintApp.using { p =>
    p.value match {
      case sAst.PropertyValue.Literal(l) =>
        sAst.Assignment(p.property, sAst.ValueExp.Literal(l)).append
      case sAst.PropertyValue.Reference(r) =>
        sAst.Assignment(p.property, sAst.ValueExp.Identifier(r)).append
      case sAst.PropertyValue.Nested(n) =>
        sAst.InstanceExp(p.property, n).append
    }
  }

  implicit lazy val bodyStmt: PrintApp[sAst.BodyStmt] = {
    val bsG = Generic[sAst.BodyStmt]
    val delegate = deriveInstance[sAst.BodyStmt, bsG.Repr]

    new PrintApp[sAst.BodyStmt] {
      override def apply(t: sAst.BodyStmt) = {
        indentStr.append
        delegate(t)
      }
    }
  }
}