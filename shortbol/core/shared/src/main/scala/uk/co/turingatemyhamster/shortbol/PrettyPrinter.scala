package uk.co.turingatemyhamster
package shortbol

import shapeless._

object PrettyPrinter {
  def apply(out: Appendable): PrettyPrinter = new PrettyPrinter(out)
}

case class PrintApp[T](apply: T => Unit)

object PrintApp extends TypeClassCompanion[PrintApp] {
  implicit class PrintAppOps[T](val _t: T) extends AnyVal {
    def append(implicit pa: PrintApp[T]): Unit = pa apply _t
  }

  object typeClass extends TypeClass[PrintApp] {
    override def coproduct[L, R <: Coproduct](cl: => PrintApp[L],
                                              cr: => PrintApp[R]) = new PrintApp[:+:[L, R]]({
      case Inl(l) => cl apply l
      case Inr(r) => cr apply r
    })

    override val emptyCoproduct = new PrintApp[CNil](_ => {})

    override def product[H, T <: HList](ch: PrintApp[H],
                                        ct: PrintApp[T]) = new PrintApp ({
      case h :: t =>
        ch apply h
        ct apply t
    })

    override val emptyProduct = new PrintApp[HNil](_ => {})

    override def project[F, G](instance: => PrintApp[G],
                               to: (F) => G,
                               from: (G) => F) = new PrintApp(a => instance apply to(a))
  }
}

/**
 * Created by nmrp3 on 15/06/15.
 */
class PrettyPrinter(out: Appendable, indent: Int = 0, indentDepth: Int = 2) {
  import PrintApp._

//  def append(bdy: Seq[BodyStmt]) = seq[BodyStmt].apply(bdy)

  implicit lazy val indentStr = "\n" + (" " * indent)

//  implicit def seq[T](implicit pa: PrintApp[T]): PrintApp[Seq[T]] = PrintApp { _ foreach pa.apply }

  implicit lazy val string: PrintApp[String] = PrintApp(out append _)

  implicit lazy val sbFile: PrintApp[SBFile] = PrintApp(_.tops.append)

  implicit lazy val instanceExp: PrintApp[InstanceExp] = PrintApp { ie =>
    indentStr.append
    ie.id.append
    ": ".append
    ie.cstrApp.append
  }

  implicit lazy val constructorDef: PrintApp[ConstructorDef] = PrintApp { cd =>
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

  implicit lazy val blankLine: PrintApp[BlankLine.type] = PrintApp ( _ => "\n".append)

  implicit lazy val comment: PrintApp[Comment] = PrintApp { c =>
    "#".append
    c.commentText.append
    // fixme: should we have a newline here?
  }

  implicit lazy val `import`: PrintApp[Import] = PrintApp { i =>
      "import ".append
      i.path.append
  }


  implicit lazy val constructor: PrintApp[ConstructorApp] = PrintApp { app =>
    app.cstr.append
    if(app.body.nonEmpty) {
      val pp = new PrettyPrinter(out, indent = indent + indentDepth)
      pp.append(app.body)
    }
  }

  implicit lazy val qname: PrintApp[QName] = PrintApp { q =>
    "<".append
    q.prefix.append
    ":".append
    q.localName.append
    ">".append
  }

  implicit lazy val url: PrintApp[Url] = PrintApp { u =>
    "<".append
    u.append
    ">".append
  }

  implicit lazy val nsPrefx: PrintApp[NSPrefix] = PrintApp { _.pfx.append }

  implicit lazy val localName: PrintApp[LocalName] = PrintApp { _.name.append }

  implicit lazy val tpeConstructor1: PrintApp[TpeConstructor1] = PrintApp { c =>
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

  implicit lazy val tpeConstrcutorStar: PrintApp[TpeConstructorStar.type] = PrintApp { _ =>
    "*".append
  }

  implicit lazy val stringLiteral: PrintApp[StringLiteral] = PrintApp { str =>
    if(str.isEscaped) {
      "{".append
      str.s.append
      "}".append
    } else {
      "\"".append
      str.s.append
      "\"".append
    }
  }

  implicit lazy val mutliLineLiteral: PrintApp[MultiLineLiteral] = PrintApp { str =>
    val indentStr = " " * str.indent
    "{".append
    indentStr.append
    for(l <- str.ss) {
      l.append
      indentStr.append
    }
    "}".append
  }

  implicit lazy val integerLiteral: PrintApp[IntegerLiteral] = PrintApp {
    _.i.toString.append
  }

  implicit lazy val assignment: PrintApp[Assignment] = PrintApp { a =>
    a.property.append
    " = ".append
    a.value.append
  }

//  implicit lazy val nestedInstance: PrintApp[NestedInstance] = PrintApp {
//    _.nested.append
//  }
//
//  implicit lazy val topLevel: PrintApp[TopLevel] = PrintApp {
//    case t : Assignment => t.append
//    case t : BlankLine.type => t.append
//    case t : Comment => t.append
//    case t : InstanceExp => t.append
//    case t : ConstructorDef => t.append
//  }
//
//  implicit lazy val bodyStmt: PrintApp[BodyStmt] = PrintApp {
//    case b : Assignment => b.append
//    case b : BlankLine.type => b.append
//    case b : Comment => b.append
//    case b : NestedInstance => b.append
//    case b : ConstructorApp => b.append
//  }
//
//  implicit lazy val tpeConstructor: PrintApp[TpeConstructor] = PrintApp {
//    case t : TpeConstructor1 => t.append
//    case t : TpeConstructorStar.type => t.append
//  }
//
//  implicit lazy val valueExp: PrintApp[ValueExp] = PrintApp {
//    case id : Identifier => id.append
//    case lit : Literal => lit.append
//  }
//
//  implicit lazy val identifier: PrintApp[Identifier] = PrintApp {
//    case ln : LocalName => ln.append
//    case qn : QName => qn.append
//    case u : Url => u.append
//  }
//
//  implicit lazy val literal: PrintApp[Literal] = PrintApp {
//    case sl : StringLiteral => sl.append
//    case ml : MultiLineLiteral => ml.append
//    case il : IntegerLiteral => il.append
//  }

  val ln = implicitly[PrintApp[LocalName]] // succeeds
  val qn = implicitly[PrintApp[QName]] // succeeds
  val un = implicitly[PrintApp[Url]] // succeeds
  val in = implicitly[PrintApp[LocalName :+: QName :+: Url :+: CNil]] // succeeds
  val gi = implicitly[Generic[Identifier]] // fails
  val gg = Generic.apply[Identifier]
  val gm = Generic.materialize[Identifier, LocalName :+: QName :+: Url :+: CNil] // succeeds

  lazy val identifierX: PrintApp[Identifier] = PrintApp.apply[Identifier] // fails
  lazy val identifier: PrintApp[Identifier] = PrintApp.deriveInstance[Identifier, LocalName :+: QName :+: Url :+: CNil] // fails
//  implicit lazy val literal: PrintApp[Literal] = PrintApp.apply[Literal](null.asInstanceOf[Literal])
}
