import cats.Functor
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
// Original expresssion folds all the layerss into a single expression
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
