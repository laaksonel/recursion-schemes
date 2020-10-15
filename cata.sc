import cats.Functor
import cats.implicits._
import scala.language.higherKinds

// DSL of our expresssion, the expressions can nest other  expressions, recursive data structure
sealed trait Expression
case class Value(v: Int) extends Expression
case class Add(e1: Expression, e2: Expression) extends Expression

// 1 + 2 = 3
val exampleExpr: Expression = Add(Value(1), Value(2))

// F[A] => A, the algebra of the data structure (expression) above
val evaluatorExpr: Expression => Int = (expr: Expression) => expr match {
  case Value(i) => i
  case Add(e1, e2) => evaluatorExpr(e1) + evaluatorExpr(e2)
}

// Works fine, Expression => 3
evaluatorExpr(exampleExpr)

// --- Catamorphism ---
// Catamorphism does not depend on any particular algebra or data structures
// Catamorphism is just a generalized version of creating recursive data structure and folding over it

// First step is to abstract away Expression data structure
// Introduce a type parameter to abstract recursion away
// Suffix F means Functor, explained later
sealed trait ExpressionF[A]

case class ValueF[A](v: Int) extends ExpressionF[A]
// As we can see, the nested expressions are abstracted away, no recursion anymore
case class AddF[A](e1: A, e2: A) extends ExpressionF[A]

// The expressions are semantically exactly the same
// Original expresssion folds all the layers into a single expression
// New expression preserves the info about nesting by encoding it into the final type
// but is not actually recursive by definition
val originalExpr: Expression = Add(Value(1), Value(2))
val newExprF: ExpressionF[ExpressionF[Int]] = AddF(ValueF(1), ValueF(2))

// Because ExpressionF is not recursive anymore, we can call the evaluator only once
def evaluatorExprF(e: ExpressionF[Int]): Int = e match {
  case ValueF(v) => v
  case AddF(e1, e2) => e1 + e2
}

// Works, only one level
val three = evaluatorExprF(AddF(1, 2))

val nested: ExpressionF[ExpressionF[Int]] = AddF(ValueF(1), ValueF(2))
// Won't compile because of nesting
// val three = evaluatorExprF(nested)

// We could create another evaluator for nested type

// We could introduce another evaluator for nested expressions
// which calls our previous evaluator
def evaluatorExprF2(e: ExpressionF[ExpressionF[Int]]): Int = e match {
  case ValueF(v) => v
  case AddF(e1, e2) => evaluatorExprF(e1) + evaluatorExprF(e2)
}

// But that does not scale at all, we would need to create new evaluator for each level

// As an alternative solution we can generalize the nested type
// ExpressionF is a type constructor * -> *, we can abstract it with F
// and wrap th type into a data structure (because type aliases cannot be recursive in Scala)
final case class Fix[ExpressionF[_]](unFix: ExpressionF[Fix[ExpressionF]])
// Unfix is the wrapped expression ExpressionF[Fix[ExpressionF]]
final case class Fix[F[_]](unFix: F[Fix[F]])

// Now we can collapse the nested type into a general one
val exprF: ExpressionF[ExpressionF[Int]] = AddF(ValueF(1), ValueF(2))
val x: Fix[ExpressionF] = Fix(ValueF(1))
val fixedExprF: Fix[ExpressionF] = Fix(AddF(Fix(ValueF(1)), Fix(ValueF(2))))

// Given a Fix ExpressionF the only thing we can do with it
// is calling unfix which produces ExpressionF (Fix ExpressionF)
// The returned ExpressionF can be one of our ValueF, AddF or MultF
// having a Fix ExpressionF as their type parameter

// By calling unFix, we can get either ValueF[Fix[ExpressionF]] or AddF[Fix[ExpressionF]]
def evaluatoreFixedExprF(e: Fix[ExpressionF]): Int = e.unFix match {
  case ValueF(v) => v
  case AddF(e1, e2) => evaluatoreFixedExprF(e1) + evaluatoreFixedExprF(e2)
}

// To fold the whole structure, we want to evaluate Fix-typed structure
def evaluateFix(evaluator: (ExpressionF[Int] => Int), x: Fix[ExpressionF]): Int = ???

// This won't work since the inner type is wrong (Fix) and evaluatore expects Int
//def evaluateFix(evaluator: (ExpressionF[Int] => Int), e: Fix[ExpressionF]): Int =
//// e.unFix is ExpressionF[Fix[ExpressionF]], evaluator expects ExpressionF[Int]
//  evaluator(e.unFix)

// There's a way to apply evaluator to the inner value: Functor
// We can evaluate the inner, unfixed value (type ExpressionF[Fix[ExpressionF]])
implicit object ExpressionFunctor extends Functor[ExpressionF] {
  // F[A] => F[B]
  // This way we can apply evaluator to the unfixed value ExpressionF[A]
  override def map[A, B](fa: ExpressionF[A])(f: A => B): ExpressionF[B] = fa match {
    case ValueF(v)    => ValueF(v)
      // If ExpressionF is AddF, we call evaluateFix again for the inner value
      // When we hit our termination point (ValueF), we apply the final evaluator ExpressionF[Int] => Int
    case AddF(e1, e2) => AddF(f(e1), f(e2))
  }
}

def evaluateFix(evaluator: ExpressionF[Int] => Int)(e: Fix[ExpressionF]): Int =
  evaluator(Functor[ExpressionF].map(e.unFix)(evaluateFix(evaluator)))

// One final generalization
// ExpressionF is abstracted  away to F
// Also we don't need to preserve info about Int, abstract it with A
def evaluateFix[F[_], A](alg: (F[A] => A))(e: Fix[F])(implicit F: Functor[F]): A =
  alg(F.map(e.unFix)(evaluateFix(alg)))

// This is our final "cata"
def cata[F[_], A](alg: (F[A] => A))(e: Fix[F])(implicit F: Functor[F]): A =
  alg(F.map(e.unFix)(cata(alg)))

val threeFixedExpr: Fix[ExpressionF] = Fix(AddF(Fix(ValueF(1)), Fix(ValueF(2))))
val five = cata(evaluatorExprF)(threeFixedExpr)
