package uk.co.turingatemyhamster.shortbol

//import org.parboiled2._
//
//trait DashandsParser extends Parser {
//
//  def ws: Rule0 = rule {
//    """\s+""".r
//  }
//
//  def nonWS: Rule0 = rule {
//    """\S+""".r
//  }
//
//
//
//  def dashandsDocument: Rule1[DashandsDocument] = rule {
//    topLevelStatement.* ~> (tlss => DashandsDocument(tlss))
//  }
//
//  def topLevelStatement: Rule1[TopLevelStatement] = rule {
//    `import` |
//      entityDeclaration |
//      constructorDeclaration
//  }
//
//  def `import`: Rule1[Import] = rule {
//    "import" ~ ws ~ capture(nonWS) ~ eol ~>
//      (imported => Import(imported))
//  }
//
//  def entityDeclaration: Rule1[EntityDeclaration] = rule {
//    ident ~ ws.? ~ ':' ~ ws.? ~ typeCstr ~ nl ~ indent ~ body ~ outdent ~>
//      ((ident, typeCstr, body) => EntityDeclaration)
//  }
//
//  def constructorDeclaration: Rule1[ConstructorDeclaration] = {
//    ident ~ ws.? ~ '(' ~ (id * (ws.? ~ ',' ~ ws.?)) ~ ')' ~ ws.? ~ '=>' ~ ws.? ~ typeCstr ~ nl ~ indent ~ body ~ outdent ~>
//      ((ident, args, typeCstr, body) => ConstructorDeclaration(ident, args, typeCstr, body))
//  }
//
//  def id: Rule1[ID] = rule {
//    capture(nonWS) ~>
//      (id => ID(id))
//  }
//
//
//}