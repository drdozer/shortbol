package uk.co.turingatemyhamster.shortbol.ops

import shapeless._
import uk.co.turingatemyhamster.shortbol.ast._

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

  def apply(s: SBFile) = s.append
  def apply(s: SBEvaluatedFile) = s.append
  def apply(t: TpeConstructor1) = t.append
  def apply(c: ConstructorApp) = c.append
  def apply(c: TopLevel.ConstructorDef) = c.append

  implicit val literal = PrintApp[Literal]
  implicit val identifier = PrintApp[Identifier]
  implicit val tpeConstructor = PrintApp[TpeConstructor]
  implicit val valueExp = PrintApp[ValueExp]
  implicit val topLevel = PrintApp[TopLevel]

  implicit def seq[T](implicit pa: PrintApp[List[T]]): PrintApp[Seq[T]] =
    PrintApp.typeClass.project(pa, _.to[List], implicitly[List[T]<:<Seq[T]])

  def appendNested(bdy: Seq[BodyStmt]) = bdy.append

  implicit lazy val indentStr = "\n" + (" " * indent)

  implicit lazy val string: PrintApp[String] = PrintApp.using(out append _)

  implicit lazy val instanceExp: PrintApp[InstanceExp] = PrintApp.using { ie =>
    indentStr.append
    ie.id.append
    " : ".append
    ie.cstrApp.append
  }

  implicit lazy val constructorDef: PrintApp[ConstructorDef] = PrintApp.using { cd =>
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

  implicit lazy val blankLine: PrintApp[BlankLine.type] = PrintApp.using ( _ => "\n".append)

  implicit lazy val comment: PrintApp[Comment] = PrintApp.using { c =>
    "#".append
    c.commentText.append
    // fixme: should we have a newline here?
  }

  implicit lazy val constructorApp: PrintApp[ConstructorApp] = PrintApp.using { app =>
    app.cstr.append
    if(app.body.nonEmpty) {
      val pp = new PrettyPrinter(out, indent = indent + indentDepth)
      pp.appendNested(app.body)
    }
  }

  implicit lazy val qname: PrintApp[QName] = PrintApp.using { q =>
    q.prefix.append
    ":".append
    q.localName.append
  }

  implicit lazy val url: PrintApp[Url] = PrintApp.using { u =>
    "<".append
    u.url.append
    ">".append
  }

  implicit lazy val nsPrefx: PrintApp[NSPrefix] = PrintApp.using { _.pfx.append }

  implicit lazy val localName: PrintApp[LocalName] = PrintApp.using { _.name.append }

  implicit lazy val tpeConstructor1: PrintApp[TpeConstructor1] = PrintApp.using { c =>
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

  implicit lazy val tpeConstrcutorStar: PrintApp[TpeConstructorStar] = PrintApp.using { _ =>
    "*".append
  }

  implicit lazy val stringLiteral: PrintApp[StringLiteral] = PrintApp.using { str =>
    str.style.append
    str.datatype.foreach(_.append)
    str.language.foreach(_.append)
  }

  implicit lazy val singleLine: PrintApp[StringLiteral.SingleLine] = PrintApp.using { str =>
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

  implicit lazy val mutliLine: PrintApp[StringLiteral.MultiLine] = PrintApp.using { str =>
    val indentS = " " * str.indent
    "{\n".append
    indentS.append
    for(l <- str.ss) {
      l.append
      indentS.append
    }
    "}".append
  }

  implicit lazy val datatype: PrintApp[Datatype] = PrintApp.using { dt =>
    "^^".append
    dt.tpe.append
  }

  implicit lazy val language: PrintApp[Language] = PrintApp.using { l =>
    "@".append
    l.tag.append
  }

  implicit lazy val integerLiteral: PrintApp[IntegerLiteral] = PrintApp.using {
    _.i.toString.append
  }

  implicit lazy val assignment: PrintApp[Assignment] = PrintApp.using { a =>
    a.property.append
    " = ".append
    a.value.append
  }

  implicit lazy val bodyStmt: PrintApp[BodyStmt] = {
    val bsG = Generic[BodyStmt]
    val delegate = deriveInstance[BodyStmt, bsG.Repr]

    new PrintApp[BodyStmt] {
      override def apply(t: BodyStmt) = {
        indentStr.append
        delegate(t)
      }
    }
  }
}