package uk.co.turingatemyhamster.shortbol

import java.io.PrintWriter

/**
 * Created by nmrp3 on 15/06/15.
 */
class PrettyPrint(out: Appendable, indent: Int = 0, indentDepth: Int = 2) {
  lazy val indentStr = " " * indent

  private def append(s: String) = out.append(s)

  def append(tl: TopLevel): Unit = tl match {
    case InstanceExp(id, cstr, body) =>
      append(indentStr)
      append(id)
      append(": ")
      append(cstr)
      append("\n")
      val pp = new PrettyPrint(out, indent = indent + indentDepth)
      for(b <- body)
        pp.append(b)
      append("\n")
    case ConstructorDef(id, args, cstr, body) =>
      append(indentStr)
      append(id)
      append(" => ")
      append(cstr)
      append("\n")
      val pp = new PrettyPrint(out, indent = indent + indentDepth)
      for(b <- body)
        pp.append(b)
      append("\n")

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

  def append(c: TpeConstructor): Unit = {
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

  def append(ve: ValueExp): Unit = ve match {
    case i : Identifier =>
      append(i)
    case s : StringLiteral =>
      append(s)
    case i : IntegerLiteral =>
      append(i)
  }

  def append(s: StringLiteral): Unit = {
    append("\"")
    append(s.s)
    append("\"")
  }

  def append(i: IntegerLiteral): Unit = {
    append(i.i.toString)
  }

  def append(bs: BodyStmt): Unit = bs match {
    case a : Assignment =>
      append(a)
    case na : NestedAssignment =>
      append(na)
    case ni : NestedInstance =>
      append(ni)
  }

  def append(a: Assignment): Unit = {
    append(indentStr)
    append(a.property)
    append(" = ")
    append(a.value)
  }

  def append(na: NestedAssignment): Unit = {
    append(indentStr)
    append(na.property)
    val pp = new PrettyPrint(out, indent = indent + indentDepth)
    for(b <- na.body)
      pp.append(b)
    append("\n")
  }

  def append(ni: NestedInstance): Unit = {
    append(indentStr)
    append(ni.nested)
  }


}
