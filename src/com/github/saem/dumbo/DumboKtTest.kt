package com.github.saem.dumbo

import com.github.saem.dumbo.Program.Ast
import com.github.saem.dumbo.Program.Ast.Meta.Constraint
import com.github.saem.dumbo.Program.Ast.Expression
import com.github.saem.dumbo.Program.Ast.Expression.Literal
import com.github.saem.dumbo.Program.Ast.Expression.Operation
import io.kotlintest.properties.Gen
import io.kotlintest.properties.forAll
import io.kotlintest.shouldBe
import io.kotlintest.specs.FreeSpec

val emptyProgram = Program.EmptyProgram

val literalProgramTrue = Literal.True
val literalProgramFalse = Literal.False

val expressionAndTTT = Operation.Binary.And(Literal.True, Literal.True)
val expressionAndTFF = Operation.Binary.And(Literal.True, Literal.False)
val expressionAndFFF = Operation.Binary.And(Literal.False, Literal.False)
val expressionAndFTF = Operation.Binary.And(Literal.False, Literal.True)

val expressionOrTTT = Operation.Binary.Or(Literal.True, Literal.True)
val expressionOrTFT = Operation.Binary.Or(Literal.True, Literal.False)
val expressionOrFFF = Operation.Binary.Or(Literal.False, Literal.False)
val expressionOrFTT = Operation.Binary.Or(Literal.False, Literal.True)

val expressionNotTrue = Operation.Unary.Not(Literal.True)
val expressionNotFalse = Operation.Unary.Not(Literal.False)

val StandardLiterals = listOf(
    literalProgramTrue,
    literalProgramFalse
)

val StandardAndExpressions = listOf(
    expressionAndTTT,
    expressionAndTFF,
    expressionAndFFF,
    expressionAndFTF
)

val StandardOrExpressions = listOf(
    expressionOrTTT,
    expressionOrTFT,
    expressionOrFFF,
    expressionOrFTT
)

val StandardNotExpressions = listOf(
    expressionNotTrue,
    expressionNotFalse
)

val StandardExpressions = StandardLiterals +
        StandardAndExpressions +
        StandardOrExpressions +
        StandardNotExpressions

val StandardPrograms = listOf(emptyProgram) + StandardExpressions

class ProgramGen : Gen<Program> {
    override fun constants() = StandardPrograms

    override fun random() = ExpressionGen().random()
}

class AstGen : Gen<Ast> {
    private val astWithoutCostConstraint = AstWithoutCostConstraintGen()
    private val astWithCostConstraint = AstWithCostConstraintGen()

    override fun constants() = astWithoutCostConstraint.constants() +
            astWithCostConstraint.constants()
    override fun random(): Sequence<Ast> =
        Gen.oneOf(astWithoutCostConstraint,
            astWithCostConstraint).random()
}

class AstValidGen : Gen<Ast> {
    private val astWithoutCostConstraint = AstWithoutCostConstraintGen()
    private val astPassCostGen = AstWithCostConstraintPassGen()

    override fun constants() = astWithoutCostConstraint.constants() +
            astPassCostGen.constants()
    override fun random(): Sequence<Ast> =
        Gen.oneOf(astWithoutCostConstraint,
            astPassCostGen).random()
}

/**
 * Convenience class, delegates entirely to expression generation.
 */
class AstWithoutCostConstraintGen : Gen<Ast> {
    private val expressionGen = ExpressionGen()
    override fun constants() = expressionGen.constants()
    override fun random() = expressionGen.random()
}

class AstWithCostConstraintGen : Gen<Ast> {
    private val astFailCostGen = AstWithCostConstraintFailGen()
    private val astPassCostGen = AstWithCostConstraintPassGen()
    override fun constants() = astFailCostGen.constants() +
            astPassCostGen.constants()
    override fun random() = Gen.oneOf(astFailCostGen, astPassCostGen).random()
}

class AstWithCostConstraintPassGen : Gen<Constraint.CostConstraint> {
    override fun constants(): Iterable<Constraint.CostConstraint> =
        // Always passes, cost is the same
        StandardExpressions.map {
            Constraint.CostConstraint(it, programCost(it))
        } +
                // Always passes, cost is always larger
                StandardExpressions.map {
                    Constraint.CostConstraint(it, programCost(it) + 1)
                }

    override fun random(): Sequence<Constraint.CostConstraint> =
        ExpressionGen().map { expression ->
            val expressionCost = programCost(expression)

            // Get a positive, random number, near the program cost
            Gen.choose(
                expressionCost,
                expressionCost + 10
            )
                .random().first()
                .let {

                    // Some will pass, some will fail
                    Constraint.CostConstraint(expression, it)
                }
        }.random()
}

class AstWithCostConstraintFailGen : Gen<Constraint.CostConstraint> {
    override fun constants(): Iterable<Constraint.CostConstraint> =
        // Always fails, cost is always smaller
        StandardExpressions.map {
            Constraint.CostConstraint(it, programCost(it) - 1)
        } +
                // Always fails, only non-[EmptyProgram] should be 0 cost
                StandardExpressions.map {
                    Constraint.CostConstraint(it, 0)
                }

    override fun random(): Sequence<Constraint.CostConstraint> =
        ExpressionGen().map { expression ->
            val expressionCost = programCost(expression)

            // Get a positive, random number, inside the program cost
            Gen.choose(
                maxOf(0, expressionCost - 10),
                expressionCost // (upper end is exclusive)
            )
                .random().first()
                .let {

                    // All will fail
                    Constraint.CostConstraint(expression, it)
                }
        }.random()
}

const val MIN_EXPRESSION_SIZE: Int = 1
const val SOFT_MAX_EXPRESSION_SIZE: Int = 1_000

class ExpressionGen : Gen<Expression> {
    override fun constants() = StandardExpressions

    override fun random(): Sequence<Expression> =
        ExpressionGenWithLimit(
            Gen.choose(MIN_EXPRESSION_SIZE, SOFT_MAX_EXPRESSION_SIZE)
                .random()
                .first()
        ).random()

    inner class ExpressionGenWithLimit(
        private val softLimit: Int
    ) : Gen<Expression> {
        private val literalGen = LiteralGen()
        private val unaryGen = OperationUnaryGen(this)
        private val binaryGen = OperationBinaryGen(this)

        private var currentSize = 0

        override fun constants() = emptyList<Expression>()

        override fun random(): Sequence<Expression> = when (currentSize < softLimit) {
            true -> currentSize++.let { Gen.oneOf(literalGen, unaryGen, binaryGen) }
            else -> literalGen
        }.random()
    }

    class LiteralGen : Gen<Literal> {
        override fun constants() = StandardLiterals
        override fun random() = Gen.bool().map { Literal.fromBool(it) }.random()
    }

    class OperationUnaryGen(
        private val expressionGen: Gen<Expression>
    ) : Gen<Operation.Unary> {
        override fun constants() = listOf(
            Operation.Unary.Not(Literal.True),
            Operation.Unary.Not(Literal.False)
        )

        override fun random(): Sequence<Operation.Unary> =
            expressionGen.map { Operation.Unary.Not(it) }.random()
    }

    class OperationBinaryGen(
        private val expressionGen: Gen<Expression>
    ) : Gen<Operation.Binary> {
        override fun constants() = emptyList<Operation.Binary>()

        override fun random(): Sequence<Operation.Binary> =
            Gen.oneOf(
                OperationBinaryAndGen(expressionGen),
                OperationBinaryOrGen(expressionGen)
            ).random()
    }

    class OperationBinaryAndGen(
        private val expressionGen: Gen<Expression>
    ) : Gen<Operation.Binary.And> {
        override fun constants() = StandardAndExpressions
        override fun random(): Sequence<Operation.Binary.And> =
            Gen.bind(expressionGen, expressionGen)
            { a, b -> Operation.Binary.And(a, b) }
                .random()
    }

    class OperationBinaryOrGen(
        private val expressionGen: Gen<Expression>
    ) : Gen<Operation.Binary.Or> {
        override fun constants() = StandardOrExpressions
        override fun random(): Sequence<Operation.Binary.Or> =
            Gen.bind(expressionGen, expressionGen)
            { a, b -> Operation.Binary.Or(a, b) }
                .random()
    }

}

internal class DumboKtTest : FreeSpec() {
    init {
        "Validate programs" {
            forAll(ProgramGen()) { ast: Program -> validate(ast) }
        }

        "# Empty Programs" - {
            "Are valid" {
                validate(Program.EmptyProgram) shouldBe true
            }

            "Should have no cost" {
                programCost(Program.EmptyProgram) shouldBe 0
            }
        }

        "# Any non-empty program" - {
            "Cost constraints are optional" {
                forAll(AstWithoutCostConstraintGen()) {
                    validate(it) && it !is Constraint.CostConstraint
                }
            }

            "Can be valid or invalid" {
                forAll(AstGen()) {
                    val valid = validate(it)
                    when {
                        it is Constraint.CostConstraint && valid -> programCost(it) <= it.maximumCost
                        it is Constraint.CostConstraint && !valid -> programCost(it) > it.maximumCost
                        it !is Constraint.CostConstraint && valid -> programCost(it) > 0

                        // only constraints should invalidate -- likely bug
                        else -> false
                    }
                }
            }

            "Without cost constraint is valid" {
                forAll(AstWithoutCostConstraintGen()) {
                    validate(it) && programCost(it) > 0
                }
            }

            "A program must be within or equal to it's cost constraint" {
                forAll(AstWithCostConstraintPassGen()) {
                    validate(it) &&
                            programCost(it) <= it.maximumCost
                }
            }
        }
    }
}