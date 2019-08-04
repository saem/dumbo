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

fun literalGen() = object : Gen<Literal> by (Gen.bool().map { Literal.fromBool(it) }) {
    override fun constants(): Iterable<Literal> = StandardLiterals
}

fun expressionGen() = object : Gen<Expression> {
    private val literal = literalGen().random().iterator()

    override fun constants(): Iterable<Expression> = StandardExpressions
    override fun random(): Sequence<Expression> =
        Gen.choose(
            1,
            SOFT_MAX_EXPRESSION_SIZE + 1 // upper end is exclusive
        )
            .map { sizedExpression(it) }.random()

    fun sizedExpression(size: Int): Expression = when (size) {
        1 -> literal.next()
        2 -> Operation.Unary.Not(sizedExpression(size - 1))
        else -> Gen.choose(
            1,
            size // upper end is exclusive, don't need to subtract 1
        )
            .map {
                val leftSize = it
                val rightSize = size - leftSize

                val leftSideExpression = sizedExpression(leftSize)
                val rightSideExpression = sizedExpression(rightSize)

                Gen.from(
                    listOf(
                        Operation.Binary.And(leftSideExpression, rightSideExpression),
                        Operation.Binary.Or(leftSideExpression, rightSideExpression)
                    )
                ).random().first()
            }.random().first()
    }
}

fun validProgramGen(): Gen<Program> = object : Gen<Program> by Gen.oneOf(astValidGen()) {
    override fun constants(): Iterable<Program> = StandardPrograms
}

fun astGen(): Gen<Ast> = Gen.oneOf(astValidGen(), astInvalidGen())

fun astValidGen(): Gen<Ast> = Gen.oneOf(
    astWithoutCostConstraintGen(),
    astWithCostConstraintValidGen()
)

fun astInvalidGen(): Gen<Ast> = Gen.oneOf(astWithCostConstraintInvalidGen())

fun astWithoutCostConstraintGen(): Gen<Expression> = expressionGen()

fun astWithCostConstraintValidGen() : Gen<Constraint.CostConstraint> = Gen.oneOf(
    astWithCostConstraintExactValidGen(),
    astBelowCostConstraintValidGen()
)

fun astWithCostConstraintExactValidGen() : Gen<Constraint.CostConstraint> =
        astWithoutCostConstraintGen().map { Constraint.CostConstraint(it, programCost(it)) }

fun astBelowCostConstraintValidGen() : Gen<Constraint.CostConstraint> =
        Gen.bind(
            Gen.choose(1, 10),
            astWithoutCostConstraintGen()
        ) { programCostSurplus, expression ->
            Constraint.CostConstraint(
                expression,
                programCost(expression) + programCostSurplus
            )
        }


fun astWithCostConstraintInvalidGen(): Gen<Constraint.CostConstraint> =
        Gen.oneOf(
            astOneBelowCostConstraintInvalidGen(),
            astBelowCostConstraintInvalidGen()
        )

fun astOneBelowCostConstraintInvalidGen() : Gen<Constraint.CostConstraint> =
        expressionGen().map { Constraint.CostConstraint(it, programCost(it) - 1) }

fun astBelowCostConstraintInvalidGen() : Gen<Constraint.CostConstraint> =
        expressionGen().map {
            val expressionCost = programCost(it)
            val minCostDeficit = minOf(2, expressionCost)
            val maxCostDeficit = maxOf(minCostDeficit, expressionCost) + 1 // [Gen.choose]'s second param is exclusive

            val deficit = Gen.choose(
                minCostDeficit,
                maxCostDeficit
            ).random().first()

            Constraint.CostConstraint(it, deficit)
        }

const val SOFT_MAX_EXPRESSION_SIZE: Int = 1_000

internal class DumboKtTest : FreeSpec() {
    init {
        "Validate programs" {
            forAll(validProgramGen()) { ast: Program -> validate(ast) }
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
                forAll(Gen.oneOf<Program>(astWithoutCostConstraintGen())) {
                    validate(it) && it !is Constraint.CostConstraint
                }
            }

            "Can be valid or invalid" {
                forAll(astGen()) {
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
                forAll(astWithoutCostConstraintGen()) {
                    validate(it) && programCost(it) > 0
                }
            }

            "A program must be within or equal to it's cost constraint" {
                forAll(astWithCostConstraintValidGen()) {
                    validate(it) &&
                            programCost(it) <= it.maximumCost
                }
            }
        }
    }
}