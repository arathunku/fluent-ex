defmodule Fluent.Parser do
  import NimbleParsec

  @newline 0x000A
  @carriage_return 0x000D

  # An FTL file defines a Resource consisting of Entries.

  any_char = utf8_char([])

  special_text_char =
    choice([
      utf8_char([?{]),
      utf8_char([?}])
    ])

  line_end =
    choice([
      ascii_char([@newline]),
      ascii_char([@carriage_return])
      |> optional(ascii_char([@newline]))
      |> replace(@newline)
    ])

  negative_sign = ascii_char([?-])
  sign = ascii_char([?+, ?-])
  digit = ascii_char([?0..?9])
  non_zero_digit = ascii_char([?1..?9])

  fractional_part =
    ascii_char([?.])
    |> times(digit, min: 1)

  exponent_indicator = ascii_char([?e, ?E])

  exponent_part =
    exponent_indicator
    |> optional(sign)
    |> times(digit, min: 1)

  integer_part =
    optional(negative_sign)
    |> choice([
      ascii_char([?0]),
      non_zero_digit |> repeat(digit)
    ])

  float_value =
    choice([
      integer_part |> concat(fractional_part) |> concat(exponent_part),
      integer_part |> traverse({:fill_mantissa, []}) |> concat(exponent_part),
      integer_part |> concat(fractional_part)
    ])
    |> reduce({List, :to_string, []})
    |> map({String, :to_float, []})
    |> post_traverse({:labeled_token, [:float_value]})

  int_value =
    empty()
    |> concat(integer_part)
    |> reduce({List, :to_string, []})
    |> map({String, :to_integer, []})
    |> post_traverse({:labeled_token, [:int_value]})

  sign = ascii_char([?+, ?-])

  # FractionalPart :: . Digit+
  fractional_part =
    ascii_char([?.])
    |> times(digit, min: 1)

  blank_inline = times(utf8_char([0x0020]), min: 1)

  comment_line =
    empty()
    |> ignore(optional(blank_inline))
    |> times(ignore(string("#")), min: 1)
    |> optional(repeat_while(any_char, {:not_line_terminator, []}))
    |> concat(optional(line_end))

  comment =
    times(comment_line, min: 1)
    |> reduce({List, :to_string, []})
    |> map({String, :trim, []})
    |> tag(:comment)

  text_char =
    empty()
    |> lookahead_not(line_end)
    |> lookahead_not(special_text_char)
    |> concat(any_char)
    |> reduce({List, :to_string, []})

  unicode_escape =
    choice([
      string("\\u") |> times(ascii_char([?0..?9, ?A..?F, ?a..?f]), 4),
      string("\\U") |> times(ascii_char([?0..?9, ?A..?F, ?a..?f]), 6)
    ])

  special_quoted_char =
    choice([
      string("\""),
      string("\\")
    ])

  special_escape =
    empty()
    |> string("\\")
    |> concat(special_quoted_char)

  quoted_char =
    choice([
      empty()
      |> lookahead_not(line_end)
      |> lookahead_not(special_quoted_char)
      |> concat(any_char),
      special_escape,
      unicode_escape
    ])

  string_literal =
    ignore(string("\""))
    |> repeat(quoted_char)
    |> ignore(string("\""))
    |> reduce({List, :to_string, []})
    |> unwrap_and_tag(:string_literal)

  number_literal =
    empty()
    |> choice([
      float_value,
      int_value
    ])

  identifier =
    ascii_char([?A..?Z, ?a..?z])
    |> repeat(ascii_char([?A..?Z, ?a..?z, ?_, ?0..?9, ?-]))
    |> reduce({List, :to_string, []})
    |> unwrap_and_tag(:id)

  inline_text =
    empty()
    # |> traverse({:mark_string_start, []})
    |> times(text_char, min: 1)
    # |> traverse({:string_value_token, []})
    |> reduce({List, :to_string, []})
    |> tag(:text_element)

  indented_char =
    empty()
    |> lookahead_not(string("."))
    |> lookahead_not(string("*"))
    |> lookahead_not(string("["))
    |> concat(text_char)

  blank =
    times(
      choice([
        blank_inline,
        line_end
      ]),
      min: 1
    )

  blank_block =
    times(
      empty()
      |> ignore(optional(blank_inline))
      |> concat(line_end),
      min: 1
    )
    |> reduce({List, :to_string, []})

  block_text =
    empty()
    |> concat(blank_block)
    |> ignore(blank_inline)
    |> concat(indented_char)
    |> optional(inline_text)

  function_reference =
    empty()
    |> concat(identifier)
    |> parsec(:call_arguments)
    |> tag(:function_reference)

  message_reference =
    empty()
    |> concat(identifier)
    |> parsec(:attribute_accessor)
    |> tag(:messegage_reference)

  defcombinatorp(
    :attribute_accessor,
    empty()
    |> ignore(string("."))
    |> concat(identifier)
  )

  named_argument =
    empty()
    |> concat(identifier)
    |> optional(blank)
    |> string(":")
    |> optional(blank)
    |> choice([
      string_literal,
      number_literal
    ])

  argument = choice([named_argument, parsec(:inline_expression)])

  argument_list =
    empty()
    |> repeat(argument |> optional(blank) |> string(",") |> optional(blank))
    |> optional(argument)

  defcombinatorp(
    :call_arguments,
    empty()
    |> ignore(optional(blank))
    |> ignore(string("("))
    |> ignore(optional(blank))
    |> concat(argument_list)
    |> ignore(optional(blank))
    |> ignore(string(")"))
    |> tag(:call_arguments)
  )

  variable_reference =
    empty()
    |> ignore(string("$"))
    |> concat(identifier)
    |> unwrap_and_tag(:variable_reference)

  term_reference =
    empty()
    |> ignore(string("-"))
    |> concat(identifier)
    |> optional(parsec(:attribute_accessor))
    |> optional(parsec(:call_arguments))
    |> tag(:term_reference)

  defcombinatorp(
    :inline_placeable,
    empty()
    |> ignore(string("{"))
    |> ignore(optional(blank))
    |> choice([
      # Order matters!
      parsec(:select_expression),
      parsec(:inline_expression)
    ])
    |> ignore(optional(blank))
    |> ignore(string("}"))
    |> unwrap_and_tag(:placeable)
  )

  defcombinatorp(
    :block_placeable,
    empty()
    |> ignore(blank_block)
    |> ignore(optional(repeat(blank_inline)))
    |> parsec(:inline_placeable)
  )

  pattern_element =
    empty()
    |> choice([
      inline_text,
      block_text,
      parsec(:inline_placeable),
      parsec(:block_placeable)
    ])

  defcombinatorp(:attribute,
    empty()
    |> ignore(line_end)
    |> ignore(optional(blank))
    |> ignore(string("."))
    |> concat(identifier)
    |> ignore(optional(blank_inline))
    |> ignore(string("="))
    |> ignore(optional(blank_inline))
    |> concat(parsec(:pattern))
    |> tag(:attribute)
  )

  # Patterns are values of Messages, Terms, Attributes and Variants.
  defcombinatorp(:pattern,
    times(pattern_element, min: 1)
    |> debug()
    |> reduce({__MODULE__, :reduce_pattern, []})
    |> unwrap_and_tag(:pattern)
  )

  message =
    empty()
    |> ignore(optional(blank_inline))
    |> concat(identifier)
    |> ignore(optional(blank_inline))
    |> ignore(string("="))
    |> ignore(optional(blank_inline))
    |> debug()
    |> concat(choice([
      parsec(:pattern) |> repeat(parsec(:attribute)),
      times(parsec(:attribute), min: 1)
    ]))
    |> reduce({__MODULE__, :flatten, []})

  term =
    empty()
    |> ignore(string("-"))
    |> concat(identifier)
    |> ignore(optional(blank_inline))
    |> ignore(string("="))
    |> ignore(optional(blank_inline))
    |> concat(parsec(:pattern))

  variant_key =
    empty()
    |> ignore(string("["))
    |> ignore(optional(blank))
    |> choice([
      number_literal,
      identifier
    ])
    |> ignore(optional(blank))
    |> ignore(string("]"))

  default_variant =
    empty()
    |> ignore(line_end)
    |> ignore(optional(blank))
    |> ignore(string("*"))
    |> concat(variant_key)
    |> ignore(optional(blank_inline))
    |> parsec(:pattern)
    |> traverse({:labeled_token, [:default_value]})

  variant =
    empty()
    |> concat(line_end)
    |> optional(blank)
    |> concat(variant_key)
    |> optional(blank_inline)
    |> parsec(:pattern)
    |> tag(:variant)

  variant_list =
    empty()
    |> repeat(variant)
    |> concat(default_variant)
    |> repeat(variant)
    |> concat(line_end)

  junk_line =
    empty()
    |> times(
      empty()
      |> lookahead_not(string("\n"))
      |> concat(any_char),
      min: 1
    )
    |> choice([
      line_end,
      eos()
    ])

  defcombinatorp(
    :junk,
    empty()
    |> concat(junk_line)
    |> repeat(
      empty()
      |> lookahead_not(ascii_char([?a..?z, ?A..?Z]))
      |> lookahead_not(ascii_char([?a..?z, ?A..?Z]))
      |> string("-")
      |> string("#")
      |> concat(junk_line)
    )
    |> reduce({List, :to_string, []})
    |> tag(:junk)
  )

  defcombinatorp(
    :entry,
    choice([
      choice([
        comment |> concat(message) |> ignore(optional(line_end)),
        empty() |> concat(message) |> ignore(optional(line_end))
      ])
      |> tag(:message),
      choice([
        comment |> concat(term) |> ignore(optional(line_end)),
        empty() |> concat(term) |> ignore(optional(line_end))
      ])
      |> tag(:term),
      comment,
      ignore(line_end)
    ])
  )

  defcombinatorp(
    :inline_expression,
    empty()
    |> choice([
      string_literal,
      number_literal,
      function_reference,
      message_reference,
      term_reference,
      variable_reference,
      parsec(:inline_placeable)
    ])
  )

  defcombinatorp(
    :select_expression,
    empty()
    |> parsec(:inline_expression)
    |> optional(blank)
    |> string("->")
    |> optional(blank_inline)
    |> concat(variant_list)
    |> tag(:select_expression)
  )

  defparsec(
    :parse,
    empty()
    |> debug()
    |> repeat(
      choice([
        parsec(:entry),
        blank_block,
        parsec(:junk)
      ])
    )
    |> tag(:resource),
    debug: false
  )

  def line_and_column({line, line_offset}, byte_offset, column_correction) do
    column = byte_offset - line_offset - column_correction + 1
    {line, column}
  end

  defp not_line_terminator(<<?\n, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(<<?\r, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(_, context, _, _), do: {:cont, context}

  defp mark_string_start(_rest, chars, context, loc, byte_offset) do
    {[chars], Map.put(context, :token_location, line_and_column(loc, byte_offset, 1))}
  end

  defp string_value_token(_rest, chars, context, _loc, _byte_offset) do
    value = '"' ++ tl(chars |> Enum.reverse()) ++ '"'
    value = "#{value}"
    {[{:string_value, context.token_location, value}], Map.delete(context, :token_location)}
  end

  def flatten(list) do
    list
    |> List.flatten()
  end

  def reduce_pattern(list) do
    list
    |> List.flatten()
    |> Enum.reduce([], fn
      {_, _} = v, [] ->
        [v]

      v, [] ->
        [{:text_element, [v]}]

      v, [head | tail] = acc ->
        if is_bitstring(v) do
          {:text_element, [v]}
        else
          v
        end
        |> case do
          {:placeable, _} ->
            [v | acc]

          {:text_element, collected_text} ->
            case head do
              {:text_element, prev_text} ->
                [
                  {
                    :text_element,
                    prev_text ++ collected_text
                  }
                  | tail
                ]

              _ ->
                # Reversing an order here so that at the end
                # We can reverse + join + prune
                # and we can also push more letter in next iter
                [{:text_element, Enum.reverse(collected_text)} | acc]
            end
        end
    end)
    |> Enum.reverse()
    |> Enum.map(fn
      {:text_element, letters} ->
        {:text_element, Enum.join(letters)}

      v ->
        v
    end)
  end

  defp labeled_token(_rest, chars, context, loc, byte_offset, token_name) do
    value = chars |> Enum.reverse()
    {[{token_name, line_and_column(loc, byte_offset, length(value)), value}], context}
  end

  defp fill_mantissa(_rest, raw, context, _, _), do: {'0.' ++ raw, context}
end
