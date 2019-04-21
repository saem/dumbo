package com.github.saem.dumbo

/**
 * # Program - AST - Abstract Syntax Tree
 *
 * These are the data structures for the AST, the assumption is everything
 * is parsed, precedence has been all worked out. For instance, the unary
 * operator [Program.Ast.Expression.Operation.Unary.Not] simply takes an
 * expression of arbitrary complexity so the parser would have to create an
 * expression tree and then handle parentheses and any other precedence bits
 * before hitting the AST.
 *
 * NB. Nested sealed classes are used to give a sense of context, it can make
 *       referencing things by fully qualified names awkward but between import
 *       aliases and the extra context provided is helpful.
 *
 * ## Language Description
 *
 * The language is currently one where boolean literals and operators can be
 * arranged in arbitrary expressions.
 *
 * ## Current Topic of Exploration
 *
 * There is a "cost" associated to each operation and the compiler can book keep
 * that for the programmer.
 *
 * ## Future Exploration:
 *
 * ### Functions
 * - Calls
 * - Enforce maximum cost
 *
 * ### Arrays or Binary Sequences
 * - Dependent type exploration
 * - Termination analysis
 *
 * ### An Additional Data Type
 * - Sum & Product types
 *
 * ### Type Level Abstractions
 * - Generics / Parameterized Types
 *
 * ### Pluggable Costing Models
 * - Not all machines are the same, and so costs need to be rethought
 */
sealed class Program {
    /**
     * All [Program] currently support a fixed cost for themselves, and can be
     * set on a per node basis. No support for dynamic costs yet.
     */
    open val cost: Cost = 0

    // Because that's a thing, not married to this, it can go
    object EmptyProgram : Program()

    /**
     * All programs are [Ast] nodes, that must either be an [Expression] or one
     * wrapped in a [Meta.Constraint.CostConstraint].
     */
    sealed class Ast : Program() {
        sealed class Meta : Ast() {
            sealed class Constraint : Meta() {
                data class CostConstraint(
                    val expression: Expression,
                    val maximumCost : Cost
                ) : Meta() {
                    init {
                        if (maximumCost < 0) {
                            throw RuntimeException(
                                "CostConstraint require a non-negative value")
                        }
                    }
                }
            }
        }

        /**
         * Everything is an [Expression], as it should be.
         */
        sealed class Expression : Ast() {

            /**
             * Literal values, also set a cost (1), no good reason, but it's a
             * static allocation at the least or loading of instructions.
             */
            sealed class Literal(override val cost: Cost = 1) : Expression() {
                object True : Literal()
                object False : Literal()

                companion object {
                    fun fromBool(boolean: Boolean) = when (boolean) {
                        true -> True
                        false -> False
                    }
                }
            }

            /**
             * Various operations supported within the language. Initially the
             * cost here was 1, and [Literal]s were 0, but seeing as an empty
             * program truly has no cost and literals at least have to load a
             * bit into instruction cache, [Operation] were bumped by an order
             * of magnitude (still too low), and [Literal] set to 0.
             */
            sealed class Operation(override val cost: Cost = 10) : Expression() {
                sealed class Binary : Operation() {

                    data class And(
                        val left: Expression,
                        val right: Expression
                    ) : Binary()

                    data class Or(
                        val left: Expression,
                        val right: Expression
                    ) : Binary()
                }

                /**
                 * Negation is very fast (just how transistors work) compared to
                 * other logical operations.
                 */
                sealed class Unary(override val cost: Cost = 5) : Operation() {
                    data class Not(val right: Expression) : Unary()
                }
            }
        }
    }
}

/**
 * Type alias for [Cost] for easy refactoring in the future.
 */
typealias Cost = Long

/**
 * Traverses a given [Program] and calculates the total cost in a breadth first
 * fashion.
 */
fun programCost(ast: Program): Long = when (ast) {
    Program.EmptyProgram -> ast.cost

    is Program.Ast.Meta.Constraint.CostConstraint -> ast.cost +
            programCost(ast.expression)

    Program.Ast.Expression.Literal.True -> ast.cost
    Program.Ast.Expression.Literal.False -> ast.cost
    is Program.Ast.Expression.Operation.Binary.And ->
        ast.cost + programCost(ast.left) + programCost(ast.right)
    is Program.Ast.Expression.Operation.Binary.Or ->
        ast.cost + programCost(ast.left) + programCost(ast.right)
    is Program.Ast.Expression.Operation.Unary.Not ->
        ast.cost + programCost(ast.right)
}

/**
 * All programs without [Program.Ast.Meta.Constraint.CostConstraint] are valid
 * by construction (otherwise Kotlin won't compile).
 *
 * The cost should be 0 for an EmptyProgram, while always greater than one
 * otherwise -- minimally a single literal. Program cost must be within the
 * [Program.Ast.Meta.Constraint.CostConstraint] if present.
 */
fun validate(ast: Program): Boolean = when (ast) {
    is Program.EmptyProgram -> ast.cost == 0L
    is Program.Ast.Meta.Constraint.CostConstraint ->
        programCost(ast) in 1..ast.maximumCost
    else -> programCost(ast) >= 0
}