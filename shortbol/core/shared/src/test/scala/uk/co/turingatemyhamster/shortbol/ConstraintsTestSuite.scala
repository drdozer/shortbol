package uk.co.turingatemyhamster.shortbol

import javax.xml.bind.annotation.adapters.CollapsedStringAdapter

import uk.co.turingatemyhamster.shortbol.ast._
import sugar._
import uk.co.turingatemyhamster.shortbol.ops._
import utest._

/**
  *
  *
  * @author Matthew Pocock
  */
object ConstraintsTestSuite extends TestSuite {

  def success[A](cr: ConstraintRule[A], a: A): Unit = {
    cr(a).fold(
      unexpectedFailure => assert(cr == null, unexpectedFailure == null),
      va => assert(va == a)
    )
  }

  def failure[A](cr: ConstraintRule[A], a: A): Unit = {
    cr(a).fold(nel => (),
      unexpectedSuccess => assert(cr == null, unexpectedSuccess == null))
  }

  def failure[A](cr: ConstraintRule[A], a: A, cv: ConstraintViolation[A]): Unit = {
    cr(a).fold(
      nel => assert(nel.head == cv),
      unexpectedSuccess => assert(cr == null, unexpectedSuccess == null))
  }

  override def tests = TestSuite {

    'SucceedAlways - {
      success(
        AlwaysSucceed[Int](),
        5)
    }

    'FailAlways - {
      failure(
        AlwaysFail[Int](Some("Test failure")),
        5,
        ConstraintFailure(AlwaysFail[Int](Some("Test failure")), 5))
    }

    'If - {
      'onlyIf - {
        "success onlyIf success" - {
          success(
            ConstraintRule.success[Int] onlyIf ConstraintRule.success[Int],
            42)
        }

        "success onlyIf fail" - {
          success(
            ConstraintRule.success[Int] onlyIf ConstraintRule.fail[Int],
            42)
        }

        "fail onlyIf success" - {
          failure(
            ConstraintRule.fail[Int] onlyIf ConstraintRule.success[Int],
            42,
            ConstraintViolation.failure(ConstraintRule.fail[Int], 42))
        }

        "fail onlyIf fail" - {
          success(
            ConstraintRule.fail[Int] onlyIf ConstraintRule.fail[Int],
            42)
        }
      }

      'unless - {
        "success unless success" - {
          success(
            ConstraintRule.success[Int] unless ConstraintRule.success[Int],
            42)
        }

        "success unless fail" - {
          success(
            ConstraintRule.success[Int] unless ConstraintRule.fail[Int],
            42)
        }

        "fail unless success" - {
          success(
            ConstraintRule.fail[Int] unless ConstraintRule.success[Int],
            42)
        }

        "fail unless fail" - {
          failure(
            ConstraintRule.fail[Int] unless ConstraintRule.fail[Int],
            42,
            ConstraintViolation.failure(ConstraintRule.fail[Int], 42))
        }
      }
    }

    'NotLessThan - {
      'greater - {
        success(
          NotLessThan(42),
          100)
      }

      'lesser - {
        failure(
          NotLessThan(42),
          1,
          ConstraintViolation.failure(NotLessThan(42), 1)
        )
      }
    }

    'NotGreaterThan - {
      'greater - {
        failure(
          NotGreaterThan(42),
          100,
          ConstraintViolation.failure(NotGreaterThan(42), 100)
        )
      }

      'lesser - {
        success(
          NotGreaterThan(42),
          1)
      }
    }

    'sizeNotLessThan - {
      'greater - {
        success(
          At('size, NotLessThan(2))((_: List[Int]).size),
          List(1, 2, 3, 4)
        )
      }

      'lesser - {
        failure(
          At('size, NotLessThan(2))((_: List[Int]).size),
          List(1),
          ViolationAt(List(1), 'size, ConstraintViolation.failure(NotLessThan(2), 1)))
      }
    }

    'EqualTo - {
      'areEqual - {
        success(
          EqualTo("bob"),
          "bob"
        )
      }

      'areUnequal - {
        failure(
          EqualTo("bob"),
          "jane",
          ConstraintViolation.failure(EqualTo("bob"), "jane")
        )
      }
    }

    'minCardinality - {
      'isUndefinedForNotMin - {
        val cc = OwlConstraints.minCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_maxCardinality, 2)))
        assert(cc.isEmpty)
      }

      'isDefinedForMin - {
        val cc = OwlConstraints.minCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_minCardinality, 2)))
        assert(cc.isDefined)
      }

      'succeedsWithMore - {
        val cc = OwlConstraints.minCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_minCardinality, 2))).get

        success(
          cc,
          BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: Nil)
      }

      'succeedsWithExact - {
        val cc = OwlConstraints.minCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_minCardinality, 2))).get

        success(
          cc,
          BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: Nil)
      }

      'failsWithFewer - {
        val cc = OwlConstraints.minCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_minCardinality, 2))).get
        val bs = BodyStmt.BlankLine(BlankLine()) :: List.empty[BodyStmt]

        failure(
          cc,
          bs,
          ViolationAt(bs, 'size, ConstraintViolation.failure(NotLessThan(2), 1)))
      }

    }

    'maxCardinality - {
      'isUndefinedForNotMax - {
        val cc = OwlConstraints.maxCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_minCardinality, 2)))
        assert(cc.isEmpty)
      }

      'isDefinedForMax - {
        val cc = OwlConstraints.maxCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_maxCardinality, 2)))
        assert(cc.isDefined)
      }

      'failsWithMore - {
        val cc = OwlConstraints.maxCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_maxCardinality, 2))).get
        val bs = BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: List.empty[BodyStmt]

        failure(
          cc,
          bs,
          ViolationAt(bs, 'size, ConstraintViolation.failure(NotGreaterThan(2), 3)))
      }

      'succeedsWithExact - {
        val cc = OwlConstraints.maxCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_maxCardinality, 2))).get

        success(
          cc,
          BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: Nil)
      }

      'succeedsWithTooFew - {
        val cc = OwlConstraints.maxCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_maxCardinality, 2))).get
        val bs = BodyStmt.BlankLine(BlankLine()) :: List.empty[BodyStmt]

        success(
          cc,
          bs)
      }
    }

    'exactCardinality - {
      'isUndefinedForNotExact - {
        val cc = OwlConstraints.exactCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_minCardinality, 2)))
        assert(cc.isEmpty)
      }

      'isDefinedForExact - {
        val cc = OwlConstraints.exactCardinalityConstraint(BodyStmt.Assignment(Assignment(OwlConstraints.owl_exactCardinality, 2)))
        assert(cc.nonEmpty)
      }

      'failsWithMore - {
        val cc = AndOf(
          OwlConstraints.exactCardinalityConstraint(
            BodyStmt.Assignment(Assignment(OwlConstraints.owl_exactCardinality, 2))))
        val bs = BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: List.empty[BodyStmt]

        failure(
          cc,
          bs,
          ViolationAt(bs, 'size, ConstraintViolation.failure(NotGreaterThan(2), 3)))
      }

      'succeedsWithExact - {
        val cc = AndOf(
          OwlConstraints.exactCardinalityConstraint(
            BodyStmt.Assignment(Assignment(OwlConstraints.owl_exactCardinality, 2))))

        success(
          cc,
          BodyStmt.BlankLine(BlankLine()) :: BodyStmt.BlankLine(BlankLine()) :: Nil)
      }

      'failsWithFewer - {
        val cc = AndOf(
          OwlConstraints.exactCardinalityConstraint(
            BodyStmt.Assignment(Assignment(OwlConstraints.owl_exactCardinality, 2))))
        val bs = BodyStmt.BlankLine(BlankLine()) :: List.empty[BodyStmt]

        failure(
          cc,
          bs,
          ViolationAt(bs, 'size, ConstraintViolation.failure(NotLessThan(2), 1)))
      }
    }

    'allValuesFrom - {
      'isUndefinedForNotAllValuesFrom - {
        val tc = OwlConstraints.allValuesFromConstraint(
          BodyStmt.Assignment(Assignment(OwlConstraints.owl_minCardinality, "myClass")))
        assert(tc.isEmpty)
      }

      'isDefinedForAllValuesFrom - {
        val tc = OwlConstraints.allValuesFromConstraint(
          BodyStmt.Assignment(Assignment(OwlConstraints.owl_allValuesFrom, "myClass")))
        assert(tc.isDefined)
      }

      'succeedsWithMatchingType - {
        val tc = OwlConstraints.allValuesFromConstraint(
          BodyStmt.Assignment(Assignment(OwlConstraints.owl_allValuesFrom, "myClass"))).get
        val bs = BodyStmt.InstanceExp(InstanceExp(
          "rod",
          ConstructorApp(TpeConstructor1("myClass", Seq()), Seq()))) :: List.empty[BodyStmt]

        success(
          tc,
          bs)
      }

      'failsWithNonMatchingType - {
        val tc = OwlConstraints.allValuesFromConstraint(
          BodyStmt.Assignment(Assignment(OwlConstraints.owl_allValuesFrom, "myClass"))).get
        val bs = BodyStmt.InstanceExp(InstanceExp(
          "rod",
          ConstructorApp(TpeConstructor1("jane", Seq()), Seq()))) :: List.empty[BodyStmt]

        failure(
          tc,
          bs,
          ViolationAt(
            bs, 0, ViolationAt(
              bs(0), 'type, ConstraintViolation.failure(
                EqualTo(Some("myClass" : Identifier)), Some("jane" : Identifier)))))
      }

      'failsWithNonMatchingType1 - {
        val tc = OwlConstraints.allValuesFromConstraint(
          BodyStmt.Assignment(Assignment(OwlConstraints.owl_allValuesFrom, "myClass"))).get
        val bs =
          BodyStmt.InstanceExp(InstanceExp(
            "rod",
            ConstructorApp(TpeConstructor1("myClass", Seq()), Seq()))) ::
            BodyStmt.InstanceExp(InstanceExp(
              "jane",
              ConstructorApp(TpeConstructor1("freddy", Seq()), Seq()))) :: List.empty[BodyStmt]

        failure(
          tc,
          bs,
          ViolationAt(
            bs, 1, ViolationAt(
              bs(1), 'type, ConstraintViolation.failure(
                EqualTo(Some("myClass" : Identifier)), Some("freddy" : Identifier)))))
      }
    }
  }

}
