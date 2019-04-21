package com.github.saem.dumbo

/**
 * # AstNode - AST - Abstract Syntax Tree
 *
 * These are the data structures for the AST, the assumption is everything
 * is parsed, precedence has been all worked out. For instance, the unary
 * operator [AstNode.Expression.Operation.Unary.Not] simply takes an expression
 * of arbitrary complexity so the parser would have to create an expression tree
 * and then handle parentheses and any other precedence bits before hitting the
 * AST.
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
 */
sealed class AstNode(
    /**
     * All [AstNode] currently support a fixed cost for themselves, and can be
     * set on a per node basis. No support for dynamic costs yet.
     */
    open val cost: Int = 0
) {

    // Because that's a thing, not married to this, it can go
    object EmptyProgram : AstNode()

    /**
     * All programs in dumbo are [Expression]s.
     */
    sealed class Expression : AstNode() {

        /**
         * Literal values, also set a cost (1), no good reason, but it's a static
         * allocation at the least or loading of instructions.
         */
        sealed class Literal(override val cost: Int = 1) : Expression() {
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
         * Various operations supported within the language. Since I applied a
         * cost to literals, I figured at least one order of magnitude more made
         * sense for actual operations
         */
        sealed class Operation(override val cost: Int = 10) : Expression() {
            sealed class Binary : Operation() {
                data class And(val left: Expression, val right: Expression) : Binary()
                data class Or(val left: Expression, val right: Expression) : Binary()
            }

            sealed class Unary : Operation() {
                data class Not(val right: Expression) : Unary()
            }
        }
    }
}

/**
 * Traverses a given [AstNode] and calculates the total cost in a breadth first
 * fashion.
 */
fun programCost(ast: AstNode): Long = when (ast) {
    AstNode.EmptyProgram -> ast.cost.toLong()
    AstNode.Expression.Literal.True -> ast.cost.toLong()
    AstNode.Expression.Literal.False -> ast.cost.toLong()
    is AstNode.Expression.Operation.Binary.And ->
        ast.cost + programCost(ast.left) + programCost(ast.right)
    is AstNode.Expression.Operation.Binary.Or ->
        ast.cost + programCost(ast.left) + programCost(ast.right)
    is AstNode.Expression.Operation.Unary.Not ->
        ast.cost + programCost(ast.right)
}

/**
 * All programs will be valid by construction (otherwise Kotlin won't compile).
 *
 * The cost should be 0 for an EmptyProgram, while always greater than one
 * otherwise -- minimally a single literal.
 */
fun validate(ast: AstNode): Boolean = when (ast) {
    is AstNode.EmptyProgram -> ast.cost == 0
    else -> programCost(ast) >= 0
}