package uk.co.turingatemyhamster.shortbol

import java.io.PrintWriter

/**
 * Created by nmrp3 on 15/06/15.
 */
class PrettyPrint(out: Appendable, indent: Int = 0, indentDepth: Int = 2) {
  lazy val indentStr = "\n" + (" " * indent)

  private def append(s: String) = out.append(s)

  def append(tl: TopLevel): Unit = tl match {
    case InstanceExp(id, app) =>
      append(indentStr)
      append(id)
      append(": ")
      append(app)
    case ConstructorDef(id, args, app) =>
      append(indentStr)
      append(id)
      if(args.nonEmpty) {
        append("(")
        append(args.head)
        for(a <- args.tail) {
          append(", ")
          append(a)
        }
        append(")")
      }
      append(" => ")
      append(app)
    case Import(path) =>
      append("import ")
      append(path)
    case c : Comment =>
      append(c)
      append("\n")
    case BlankLine =>
      append(BlankLine)
    case a : Assignment =>
      append(a)
  }

  def append(b: BlankLine.type): Unit = {
    append("\n")
  }

  def append(c: Comment): Unit = {
    append("#")
    append(c.commentText)
  }

  def append(app: ConstructorApp): Unit = {
    append(app.cstr)
    if(app.body.nonEmpty) {
      val pp = new PrettyPrint(out, indent = indent + indentDepth)
      for(b <- app.body)
        pp.append(b)
    }
  }

  def append(id: Identifier): Unit = id match {
    case LocalName(name) =>
      append(name)
    case QName(prefix, localName) =>
      append("<")
      append(prefix)
      append(":")
      append(localName)
      append(">")
    case Url(url) =>
      append("<")
      append(url)
      append(">")
  }

  def append(p: NSPrefix): Unit =
    append(p.pfx)

  def append(c: TpeConstructor): Unit = c match {
    case c : TpeConstructor1 =>
      append(c)
    case TpeConstructorStar =>
      append(TpeConstructorStar)
  }

  def append(c: TpeConstructor1): Unit = {
    append(c.id)
    if(c.args.nonEmpty) {
      append("(")
      append(c.args.head)
      for(a <- c.args.tail) {
        append(", ")
        append(a)
      }
      append(")")
    }
  }

  def append(c: TpeConstructorStar.type): Unit = {
    append("*")
  }

  def append(ve: ValueExp): Unit = ve match {
    case i : Identifier =>
      append(i)
    case s : StringLiteral =>
      append(s)
    case i : IntegerLiteral =>
      append(i)
  }

  def append(str: StringLiteral): Unit = {
    if(str.isMultiLine) {
      append("{")
      append(indentStr)
      append(str.s)
      append(indentStr)
      append("}")
    } else {
      append("\"")
      append(str.s)
      append("\"")
    }
  }

  def append(i: IntegerLiteral): Unit = {
    append(i.i.toString)
  }

  def append(bs: BodyStmt): Unit = bs match {
    case a : Assignment =>
      append(indentStr)
      append(a)
    case app : ConstructorApp =>
      append(indentStr)
      append(app)
    case ni : NestedInstance =>
      append(ni)
    case c : Comment =>
      append(indentStr)
      append(c)
    case BlankLine =>
      append(indentStr)
      append(BlankLine)
  }

  def append(a: Assignment): Unit = {
    append(a.property)
    append(" = ")
    append(a.value)
  }

  def append(ni: NestedInstance): Unit = {
    append(ni.nested)
  }

  def append(tlss: Seq[TopLevel]): Unit = {
    for(tls <- tlss) {
      append(tls)
    }
  }

}
