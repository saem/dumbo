package com.github.saem.dumbo.record.compiler

import arrow.core.Try
import arrow.core.extensions.`try`.monad.binding
import arrow.core.failure
import arrow.core.getOrElse
import arrow.core.recoverWith
import com.github.saem.dumbo.record.lang.dslSample
import com.github.saem.dumbo.record.lang.Description as SyntaxDescription
import com.github.saem.dumbo.record.lang.Field as SyntaxField
import com.github.saem.dumbo.record.lang.Record as SyntaxRecord
import com.github.saem.dumbo.record.lang.Type as SyntaxFieldType

fun main() {
    Compiler(dslSample).compile()
}

data class Compiler(val description: SyntaxDescription) {
    fun compile() : Description {
        println(description)

        val records = description.records.map {
            compileRecord(it).getOrElse { t -> throw t }
        }.toSet()

        return Description(
            description.name,
            records
        )
    }

    private fun compileRecord(r: SyntaxRecord) = binding {
        val fields = r.fields.map {
            compileField(it)
                .recoverWith { e ->
                    when (e) {
                        is FieldFailedCompilation -> RecordFailedCompilation(e, r)
                        else -> e
                    }.failure()
                }
                .getOrElse { t -> throw t }
        }.toSet()

        Record(
            r.name,
            fields
        )
    }

    private fun compileField(f: SyntaxField): Try<RecordField> = binding {
        val (type) = compileFieldType(f.type)

        RecordField(
            f.name,
            type
        )
    }.recoverWith {
        when (it) {
            is FieldTypeError -> FieldFailedCompilation(it, f)
            else -> it
        }.failure()
    }

    private fun compileFieldType(ft: SyntaxFieldType) = Try {
        FieldType.toType(ft) ?: throw UnknownFieldType(ft)
    }
}

typealias DescriptionName = String
typealias RecordName = String
typealias FieldName = String

sealed class Type

data class Description(
    val name: DescriptionName,
    val records: Set<Record>
)

data class Record(
    val name: RecordName,
    val recordFields: Set<RecordField>
) : Type()

data class RecordField(
    val name: FieldName,
    val type: FieldType
) : Type()

sealed class FieldType : Type() {
    object Email : FieldType()
    object Password : FieldType()

    companion object {
        fun toType(str: String) = when (str) {
            "Email" -> Email
            "Password" -> Password
            else -> null
        }
    }
}

sealed class CompilerError : RuntimeException() {
    override val message: String?
        get() = this.toString()
}

sealed class SyntaxError : CompilerError()
sealed class DescriptionError : SyntaxError()
sealed class RecordError : DescriptionError()
sealed class FieldError : RecordError()
sealed class FieldTypeError : FieldError()

data class UnknownFieldType(val fieldType: SyntaxFieldType) : FieldTypeError()

data class FieldFailedCompilation(
    val error: FieldError,
    val field: SyntaxField
) : RecordError()

data class RecordFailedCompilation(
    val error: RecordError,
    val record: SyntaxRecord
) : DescriptionError()