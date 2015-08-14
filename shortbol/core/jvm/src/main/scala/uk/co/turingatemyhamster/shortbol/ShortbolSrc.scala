package uk.co.turingatemyhamster.shortbol

import fastparse.core.Result.Success

/**
 * Created by nmrp3 on 12/06/15.
 */
object ShortbolSrc {

  def main(args: Array[String]): Unit = {

    val p = ShortbolParser.parser

    println(ShortbolParser.LocalName.parse("x"))
    println(ShortbolParser.LocalName.parse("foo"))
    println(ShortbolParser.LocalName.parse("_foo"))
    println(ShortbolParser.LocalName.parse("4"))
    println(ShortbolParser.LocalName.parse("x4"))

    println(ShortbolParser.QName.parse("foo:bar"))

    println(ShortbolParser.Url.parse("http://example.org"))

    println(ShortbolParser.Identifier.parse("foo"))
    println(ShortbolParser.Identifier.parse("<foo:bar>"))
    println(ShortbolParser.Identifier.parse("<http://bla>"))

    println(ShortbolParser.Assignment.parse("foo = bar"))
    println(ShortbolParser.Assignment.parse("<foo:Bar> = <http://example.com/something>"))
    println(ShortbolParser.Assignment.parse("""name = "matthew""""))
    println(ShortbolParser.Assignment.parse("""age = 39"""))

    println(p.InstanceExp.parse(
      """foo : Bar
      """.stripMargin))

    println(p.InstanceExp.parse(
      """foo : Bar
        |  name = "baz"
      """.stripMargin))

    println(p.InstanceExp.parse(
      """foo : Bar
        |  name = "baz"
        |  value = 42
      """.stripMargin))

    println(p.InstanceExp.parse(
      """foo : Bar
        |  floop
        |    name = "baz"
        |    value = 42
      """.stripMargin))


    println(p.InstanceExp.parse(
      """foo : Bar
        |  floop : Floop
        |    name = "baz"
        |    value = 42
      """.stripMargin))


    println(p.InstanceExp.parse(
      """foo : Bar
        |  name = "baz"
        |  x : X
        |    y : Y
        |      name = "baz"
        |      value = 42
        |    name = "baz"
        |    value = 42
        |  z
        |    alpha = 66
        |    beta = "hohoho"
        |    gamma = <foo:Bar>
        |  value = <http://example.com>
      """.stripMargin))

    println(p.ConstructorDef.parse(
      """DnaComponent => ComponentDefinition
        |  type = <SBOL:DNA>
      """.stripMargin))

    println(p.ConstructorDef.parse(
      """BakeACake(candles, icing) => Cake
        |  withCandles = candles
        |  withIcing = icing
      """.stripMargin
    ))

    println(p.ConstructorDef.parse(
      """PinkCake => BakeACake(pinkCandles, pinkIcing)
        |
      """.stripMargin
    ))

    println(p.ConstructorDef.parse(
      """PinkCake(candles) => BakeACake(candles, "Pink Icing")
        |
      """.stripMargin
    ))

    println(p.TopLevels.parse(
    """foo : Bar
      |
      |x : Y
      |
      |alph => omega()
      |
    """.stripMargin
    ))

    println()
    println("Raw input")
    val raw = p.TopLevels.parse(
    """T(a) => F
      |  name = a
      |
      |ta : T("bob")
      |""".stripMargin
    )

    val expansion = p.InstanceExp.parse(
    """ta : F
      |  name = "bob"
    """.stripMargin
    )

    println(raw)
    println(expansion)

    val Success(raws, _) = raw
    val cstrs = Ops.constructors(raws)
    val inds = Ops.individuals(raws)

    val ex = ExpansionContext(cstrs, Bindings(Map()))

    import Expander.ops._

    val expanded = raws flatMap (_ expandWith ex)

    println(cstrs)
    println(inds)
    println(expanded)

    val pp = new PrettyPrinter(System.out)
    for(x <- raws)
      pp.append(x)

    println()

    for(e <- expanded)
      pp.append(e)

  }
}
