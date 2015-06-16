package uk.co.turingatemyhamster.shortbol

/**
 * Created by nmrp3 on 12/06/15.
 */
object ShortBOLSrc {

  def main(args: Array[String]): Unit = {

    val p = new ShortbolParser()

    println(p.LocalName.parse("x"))
    println(p.LocalName.parse("foo"))
    println(p.LocalName.parse("_foo"))
    println(p.LocalName.parse("4"))
    println(p.LocalName.parse("x4"))

    println(p.QName.parse("foo:bar"))

    println(p.Url.parse("http://example.org"))

    println(p.Identifier.parse("foo"))
    println(p.Identifier.parse("<foo:bar>"))
    println(p.Identifier.parse("<http://bla>"))

    println(p.Assignment.parse("foo = bar"))
    println(p.Assignment.parse("<foo:Bar> = <http://example.com/something>"))
    println(p.Assignment.parse("""name = "matthew""""))
    println(p.Assignment.parse("""age = 39"""))

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

    val fastparse.Result.Success(raws, _) = raw
    val cstrs = Ops.constructors(raws)
    val inds = Ops.individuals(raws)

    val cfi = Ops.constructorsForIndividuals(cstrs, inds)

    val ex = ExpansionContext(cstrs, Bindings(Map()))

    import ex._
    val expanded = ex.expansion(raws)(ex.SeqExpander(ex.TopLevelExpander))

    println(cstrs)
    println(inds)
    println(cfi)
    println(expanded)

    val pp = new PrettyPrint(System.out)
    for(x <- raws)
      pp.append(x)

    println()

    for(e <- expanded)
      pp.append(e)

  }
}
