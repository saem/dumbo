package com.github.saem.dumbo

import com.github.saem.dumbo.AstNode.Expression
import com.github.saem.dumbo.AstNode.Expression.Literal
import com.github.saem.dumbo.AstNode.Expression.Operation
import io.kotlintest.properties.Gen
import io.kotlintest.properties.forAll
import io.kotlintest.shouldBe
import io.kotlintest.specs.FreeSpec
import io.kotlintest.specs.StringSpec

val emptyProgram = AstNode.EmptyProgram

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

class AstGen : Gen<AstNode> {
    override fun constants() = StandardPrograms

    override fun random() = ExpressionGen().random()
}

class ExpressionGen : Gen<Expression> {
    override fun constants() = StandardExpressions

    override fun random(): Sequence<Expression> =
        ExpressionGenWithLimit(Gen.choose(1, 1000).random().first()).random()

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
            forAll(AstGen()) { ast: AstNode -> validate(ast) }
        }

        "# Empty Programs" - {
            "Are valid" {
                validate(AstNode.EmptyProgram) shouldBe true
            }

            "Should have no cost" {
                programCost(AstNode.EmptyProgram) shouldBe 0
            }
        }

        "# Expressions - Any non-empty program" - {
            "Are valid as long as the cost is non-negative" {
                forAll(ExpressionGen()) {
                    programCost(it) >= 0
                }
            }
        }
    }
}