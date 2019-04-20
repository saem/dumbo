package com.github.saem.dumbo

typealias FnName = String
typealias ParamName = String

sealed class AstNode (open val cost: Int = 0) {
    object EmptyProgram : AstNode()

    sealed class Expression: AstNode() {
        sealed class Literal(override val cost: Int = 1) : Expression() {
            object True : Literal()
            object False : Literal()

            companion object {
                fun fromBool(boolean: Boolean) = when (boolean) {
                    true -> Literal.True
                    false -> Literal.False
                }
            }
        }

        sealed class Operation(override val cost: Int = 10) : Expression() {
            sealed class Binary() : Operation() {
                data class And(val left: Expression, val right: Expression) : Binary()
                data class Or(val left: Expression, val right: Expression) : Binary()
            }
            sealed class Unary() : Operation() {
                data class Not(val right: Expression) : Unary()
            }
        }
    }
}

fun programCost(ast: AstNode) : Long = when(ast) {
    AstNode.EmptyProgram -> ast.cost.toLong()
    AstNode.Expression.Literal.True -> ast.cost.toLong()
    AstNode.Expression.Literal.False -> ast.cost.toLong()
    is AstNode.Expression.Operation.Binary.And -> ast.cost + programCost(ast.left) + programCost(ast.right)
    is AstNode.Expression.Operation.Binary.Or -> ast.cost + programCost(ast.left) + programCost(ast.right)
    is AstNode.Expression.Operation.Unary.Not -> ast.cost + programCost(ast.right)
}

fun validate(ast: AstNode) : Boolean = programCost(ast) >= 0