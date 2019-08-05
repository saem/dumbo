package com.github.saem.dumbo.record.lang

/**
 * A sample of the DSL used to see what I think about the syntax
 */
val dslSample = description("User") {
    record("UnverifiedUser") {
        field("email", "Email")
        field("password", "Password")
    }
}

fun main() {
    println(dslSample)
}

/**
 * Everything below this is the language and DSL definition
 *
 * @sample dslSample
 */

fun description(
    name: DescriptionName,
    init: Description.Builder.() -> Unit
): Description {
    return Description.Builder(name).also(init).build()
}

typealias IdentifierName = String

typealias DescriptionName = IdentifierName

sealed class Syntax

data class Description(
    val name: DescriptionName,
    val records: Set<Record> = emptySet()
) : Syntax() {
    class Builder(private val name: DescriptionName) :
        Scoped {
        private val records: MutableSet<Record> = mutableSetOf()

        fun record(name: RecordName, init: Record.Builder.() -> Unit) {
            records.add(Record.Builder(name).also(init).build())
        }

        internal fun build() = Description(name, records)
    }
}

typealias RecordName = IdentifierName

data class Record(
    val name: RecordName,
    val fields: Set<Field> = emptySet()
) : Syntax() {
    class Builder(private val name: RecordName) :
        Scoped {
        private val fields: MutableSet<Field> = mutableSetOf()

        fun field(
            name: FieldName,
            type: Type,
            init: Field.Builder.() -> Unit = {}
        ) {
            fields.add(Field.Builder(name, type).also(init).build())
        }

        internal fun build() = Record(name, fields)
    }
}

typealias FieldName = IdentifierName
typealias Type = String

data class Field(
    val name: FieldName,
    val type: Type
) : Syntax() {
    class Builder(
        private val name: FieldName,
        private val type: Type
    ) : Scoped {
        internal fun build() = Field(name, type)
    }
}

/**
 * Kotlin Infrastructure bits for DSLs
 */

/**
 * Implement this interface on a builder to restrict the implicit receiver scope
 * to only resolve to the most immediate one, so as to no allow weird language
 * quirks in the DSL.
 *
 * For example, at the time of writing, no nested [Record]s are valid so the
 * call to [Description.Builder.record] should be limited to only that builder
 * and not further nested within say a [Record.Builder.field] block.
 *
 * See the [kotlin manual on DSL Scoping for more details](https://kotlinlang.org/docs/reference/type-safe-builders.html#scope-control-dslmarker-since-11)
 */
@Scope
interface Scoped

@DslMarker
annotation class Scope