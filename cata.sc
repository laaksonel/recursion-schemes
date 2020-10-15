// DSL of our expresssion, the expressions can nest other  expressions, recursive data structure
sealed trait Expression
case class Value(v: Int) extends Expression
case class Add(e1: Expression, e2: Expression) extends Expression

val exampleExpr = Add(Value(1), Value(2))

// F[A] => A, the evaluator of the data structure (expression) above
val algebra: Expression => Int = (expr: Expression) => expr match {
  case Value(i) => i
  case Add(e1, e2) => algebra(e1) + algebra(e2)
}

// Works fine, Expression => 3
algebra(exampleExpr)

// --- Catamorphism ---
// Catamorphism does not depend on any particular algebra or data structures
// Catamorphism is just a generalized version of creating recursive data structure and folding over it
