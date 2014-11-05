/*
* Copyright 2013 Toshiyuki Takahashi
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package com.github.tototoshi.csv

object CSVParser {

  abstract sealed trait State
  case object Start extends State
  case object Field extends State
  case object Delimiter extends State
  case object QuoteStart extends State
  case object QuoteEnd extends State
  case object QuotedField extends State

  private[this] sealed abstract class Result
  private[this] final case class AtEnd(fields: Vector[String]) extends Result
  private[this] final case class NoEnd(fields: Vector[String], state: State, field: StringBuilder) extends Result

  /**
   * {{{
   * scala> com.github.tototoshi.csv.CSVParser.parse("a,b,c", '\\', ',', '"')
   * res0: Option[List[String]] = Some(List(a, b, c))
   *
   * scala> com.github.tototoshi.csv.CSVParser.parse("\"a\",\"b\",\"c\"", '\\', ',', '"')
   * res1: Option[List[String]] = Some(List(a, b, c))
   * }}}
   */
  def parse(input: String, escapeChar: Char, delimiter: Char, quoteChar: Char): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    val buflen = buf.length

    @annotation.tailrec
    def loop(state: State, pos: Int, fields: Vector[String], field: StringBuilder): Result = {
      if (pos < buflen) {
        val c = buf(pos)
        state match {
          case Start => {
            c match {
              case `quoteChar` => {
                loop(QuoteStart, pos + 1, fields, field)
              }
              case `delimiter` => {
                loop(Delimiter, pos + 1, fields :+ field.toString, new StringBuilder)
              }
              case '\n' | '\u2028' | '\u2029' | '\u0085' | '\r' => {
                AtEnd(fields :+ field.toString)
              }
              case x => {
                loop(Field, pos + 1, fields, field += x)
              }
            }
          }
          case Delimiter => {
            c match {
              case `quoteChar` => {
                loop(QuoteStart, pos + 1, fields, field)
              }
              case `delimiter` => {
                loop(Delimiter, pos + 1, fields :+ field.toString, new StringBuilder)
              }
              case '\n' | '\u2028' | '\u2029' | '\u0085' | '\r' => {
                AtEnd(fields :+ field.toString)
              }
              case x => {
                loop(Field, pos + 1, fields, field += x)
              }
            }
          }
          case Field => {
            c match {
              case `escapeChar` => {
                if (pos + 1 < buflen) {
                  if (buf(pos + 1) == escapeChar
                    || buf(pos + 1) == delimiter) {
                    loop(Field, pos + 2, fields, field += buf(pos + 1))
                  } else {
                    throw new MalformedCSVException(input)
                  }
                } else {
                  loop(QuoteEnd, pos + 1, fields, field)
                }
              }
              case `delimiter` => {
                loop(Delimiter, pos + 1, fields :+ field.toString, new StringBuilder)
              }
              case '\n' | '\u2028' | '\u2029' | '\u0085' | '\r' => {
                AtEnd(fields :+ field.toString)
              }
              case x => {
                loop(Field, pos + 1, fields, field += x)
              }
            }
          }
          case QuoteStart => {
            c match {
              case `quoteChar` => {
                if (pos + 1 < buflen && buf(pos + 1) == quoteChar) {
                  loop(QuotedField, pos + 2, fields, field += quoteChar)
                } else {
                  loop(QuoteEnd, pos + 1, fields :+ field.toString, new StringBuilder)
                }
              }
              case x => {
                loop(QuotedField, pos + 1, fields, field + x)
              }
            }
          }
          case QuoteEnd => {
            c match {
              case `delimiter` => {
                loop(Delimiter, pos + 1, fields :+ field.toString, new StringBuilder)
              }
              case '\n' | '\u2028' | '\u2029' | '\u0085' | '\r' => {
                AtEnd(fields :+ field.toString)
              }
              case _ => {
                throw new MalformedCSVException(input)
              }
            }
          }
          case QuotedField => {
            c match {
              case `quoteChar` => {
                if (pos + 1 < buflen && buf(pos + 1) == quoteChar) {
                  loop(QuotedField, pos + 2, fields, field += quoteChar)
                } else {
                  loop(QuoteEnd, pos + 1, fields, field)
                }
              }
              case x => {
                loop(QuotedField, pos + 1, fields, field + x)
              }
            }
          }
        }
      } else {
        NoEnd(fields, state, field)
      }
    }

    loop(Start, 0, Vector.empty[String], new StringBuilder) match {
      case NoEnd(fields, Delimiter, _) => {
        Some((fields :+ "").toList)
      }
      case NoEnd(_, QuotedField, _) => {
        None
      }
      case NoEnd(fields, state, field) => {
        if (field.nonEmpty) {
          // When no crlf at end of file
          state match {
            case Field | QuoteEnd => {
              Some((fields :+ field.toString).toList)
            }
            case _ => {
              Some(fields.toList)
            }
          }
        } else {
          Some(fields.toList)
        }
      }
      case AtEnd(fields) =>
        Some(fields.toList)
    }
  }
}

class CSVParser(format: CSVFormat) {

  def parseLine(input: String): Option[List[String]] = {
    val parsedResult = CSVParser.parse(input, format.escapeChar, format.delimiter, format.quoteChar)
    if (parsedResult == Some(List("")) && format.treatEmptyLineAsNil) Some(Nil)
    else parsedResult
  }

}
