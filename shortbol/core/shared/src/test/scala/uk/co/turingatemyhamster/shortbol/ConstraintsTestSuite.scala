package uk.co.turingatemyhamster.shortbol

import shorthandAst._
import sugar._
import ops._
import utest._
import monocle._

/**
  *
  *
  * @author Matthew Pocock
  */
object ConstraintsTestSuite extends TestSuite {

  def success[A](cr: Constraint[A], a: A): Unit = {
    cr(a).fold(
      unexpectedFailure => assert(cr == null, unexpectedFailure == null),
      va => assert(va == a)
    )
  }

  def failure[A](cr: Constraint[A], a: A): Unit = {
    cr(a).fold(nel => (),
      unexpectedSuccess => assert(cr == null, unexpectedSuccess == null))
  }

  def failure[A](cr: Constraint[A], a: A, exp: ConstraintViolation[A]): Unit = {
    cr(a).fold(
      nel => {
        val obs = nel.head
        assert(obs == exp)
      },
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
            Constraint.success[Int] onlyIf Constraint.success[Int],
            42)
        }

        "success onlyIf fail" - {
          success(
            Constraint.success[Int] onlyIf Constraint.fail[Int],
            42)
        }

        "fail onlyIf success" - {
          failure(
            Constraint.fail[Int] onlyIf Constraint.success[Int],
            42,
            ConstraintViolation.failure(Constraint.fail[Int], 42))
        }

        "fail onlyIf fail" - {
          success(
            Constraint.fail[Int] onlyIf Constraint.fail[Int],
            42)
        }
      }

      'unless - {
        "success unless success" - {
          success(
            Constraint.success[Int] unless Constraint.success[Int],
            42)
        }

        "success unless fail" - {
          success(
            Constraint.success[Int] unless Constraint.fail[Int],
            42)
        }

        "fail unless success" - {
          success(
            Constraint.fail[Int] unless Constraint.success[Int],
            42)
        }

        "fail unless fail" - {
          failure(
            Constraint.fail[Int] unless Constraint.fail[Int],
            42,
            ConstraintViolation.failure(Constraint.fail[Int], 42))
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
          ('size, (_: List[Int]).size) @: NotLessThan(2),
          List(1, 2, 3, 4)
        )
      }

      'lesser - {
        failure(
          ('size, (_: List[Int]).size) @: NotLessThan(2),
          List(1),
          NestedViolation(List(1), 'size, ConstraintViolation.failure(NotLessThan(2), 1))(null))
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
  }
}
