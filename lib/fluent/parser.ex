defmodule Fluent.Parser do
  import NimbleParsec
  alias Fluent.Parser.Validators
  require Logger
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
    |> label("line end")

  negative_sign = ascii_char([?-])
  sign = ascii_char([?+, ?-])
  digit = ascii_char([?0..?9])
  non_zero_digit = ascii_char([?1..?9])

  # FractionalPart :: . Digit+
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

  blank_inline_single = utf8_char([0x0020])
  blank_inline = times(blank_inline_single, min: 1)

  comment_line =
    empty()
    |> post_traverse({:mark_start, []})
    |> ignore(times(string("#"), min: 1))
    |> ignore(optional(blank_inline_single))
    |> optional(repeat_while(any_char, {:not_line_terminator, []}))
    |> reduce({List, :to_string, []})
    |> post_traverse({:mark_end, []})
    |> ignore(optional(line_end))
    |> label("comment line")
    |> tag(:comment_line)

  comment =
    empty()
    |> times(comment_line, min: 1)
    |> reduce({:post_process_comments, []})
    |> unwrap_and_tag(:comment)
    |> label("comment")

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
    |> times(text_char, min: 1)
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
    |> post_traverse({Validators, :validate_function_name_all_caps, []})
    |> post_traverse({Validators, :validate_call_argument, []})

  message_reference =
    empty()
    |> concat(identifier)
    |> optional(parsec(:attribute_accessor))
    |> tag(:message_reference)
    |> label("message reference")

  defcombinatorp(
    :attribute_accessor,
    empty()
    |> ignore(string("."))
    |> concat(identifier)
    |> tag(:attribute_accessor)
    |> label("attribute accessor")
  )

  named_argument =
    empty()
    |> concat(identifier)
    |> ignore(optional(blank))
    |> ignore(string(":"))
    |> ignore(optional(blank))
    |> choice([
      string_literal,
      number_literal
    ])
    |> tag(:named_argument)

  argument = choice([named_argument, parsec(:inline_expression)])

  argument_list =
    empty()
    |> repeat(
      argument
      |> ignore(optional(blank))
      |> ignore(string(","))
      |> ignore(optional(blank))
    )
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
    |> label("inline placeable")
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
    |> label("pattern element")

  pattern =
    empty
    |> times(pattern_element, min: 1)
    |> reduce({__MODULE__, :reduce_pattern, []})
    |> unwrap_and_tag(:pattern)
    |> label("pattern")

  defcombinatorp(
    :attribute,
    empty()
    |> ignore(line_end)
    |> ignore(optional(blank))
    |> ignore(string("."))
    |> concat(identifier)
    |> ignore(optional(blank_inline))
    |> ignore(string("="))
    |> ignore(optional(blank_inline))
    |> concat(pattern)
    |> tag(:attribute)
    |> label("attribute")
  )

  message =
    empty()
    |> ignore(optional(blank_inline))
    |> concat(identifier)
    |> ignore(optional(blank_inline))
    |> ignore(string("="))
    |> ignore(optional(blank_inline))
    |> choice([
      pattern
      |> repeat(parsec(:attribute)),
      times(parsec(:attribute), min: 1)
    ])
    |> reduce({__MODULE__, :flatten, []})
    |> label("message")

  term =
    empty()
    |> ignore(string("-"))
    |> concat(identifier)
    |> ignore(optional(blank_inline))
    |> ignore(string("="))
    |> ignore(optional(blank_inline))
    |> concat(pattern)
    |> label("term")

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
    |> label("variant key")

  default_variant =
    empty()
    |> ignore(line_end)
    |> ignore(optional(blank))
    |> ignore(string("*"))
    |> concat(variant_key)
    |> ignore(optional(blank_inline))
    |> concat(pattern)
    |> traverse({:labeled_token, [:default_value]})

  variant =
    empty()
    |> ignore(line_end)
    |> ignore(optional(blank))
    |> concat(variant_key)
    |> ignore(optional(blank_inline))
    |> concat(pattern)
    |> tag(:variant)
    |> label("variant")

  variant_list =
    empty()
    |> repeat(variant)
    |> concat(default_variant)
    |> repeat(variant)
    |> ignore(line_end)
    |> tag(:variant_list)
    |> label("variant list")

  defparsec(:junk_line,
    empty()
    |> times(
      empty()
      |> lookahead_not(line_end)
      |> concat(any_char),
      min: 1
    )
    |> optional(line_end)
    |> reduce({List, :to_string, []})
    |> unwrap_and_tag(:content)
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
    |> ignore(optional(blank))
    |> ignore(string("->"))
    |> ignore(optional(blank_inline))
    |> concat(variant_list)
    |> tag(:select_expression)
    |> post_traverse({Validators, :validate_select_expression, []})
  )

  defparsec(
    :junk,
    empty()
    |> repeat(
      empty()
      |> lookahead_not(ascii_char([?a..?z, ?A..?Z]))
      |> lookahead_not(ascii_char([?a..?z, ?A..?Z]))
      |> string("-")
      |> string("#")
      |> parsec(:junk_line)
    )
  )

  defparsec(
    :entry,
    choice([
      ignore(line_end),
      empty
      |> ignore(optional(line_end))
      |> choice([
        message |> unwrap_and_tag(:message),
        term |> tag(:term),
        comment
      ])
      |> ignore(repeat(line_end))
    ])
    |> label("entry"),
    debug: false
  )

  def run(content) do
    {:ok, resource: run_process_chunk(content, [])}
  end

  def run_process_chunk("", acc) do
    Enum.reverse(acc)
  end

  def run_process_chunk(chunk, acc) do
    try do
      entry(chunk)
    rescue
      e in [Fluent.Parser.ValidationError] ->
        {:ok, [content: content], left, _, _, _} = junk_line(chunk)

        {:continue, left, {:junk,
           [
             annotation: create_annotation(e),
             content: content
           ]} }
    end
    |> case do
      {:continue, left, entry} ->
        run_process_chunk(left, [entry | acc])
      {:ok, [], left, _, _, _} ->
        run_process_chunk(left, acc)
      {:ok, [entry], left, _, _, _} ->
        run_process_chunk(left, [entry | acc])
      {:error, msg, content, _, _, _} = error ->
        Logger.debug("#{inspect(error)}")

        case junk_line(chunk) do
          {:ok, [content: ""], left, _, _, _} ->
            raise ArgumentError, "Junk not found?"
          {:ok, [content: content], left, _, _, _} ->
            run_process_chunk(left, [
              {:junk,
               [
                 annotation: create_annotation(%{
                   message: "Parsing error - #{inspect(msg)}"
                 }),
                 content: content
               ]}
              | acc
            ])
        end
    end
  end

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

  defp mark_start(_rest, content, context, loc, byte_offset) do
    {[content], Map.put(context, :span_start, byte_offset)}
  end

  defp mark_end(_rest, content, context, loc, byte_offset) do
    {[content, {context.span_start, byte_offset}], Map.delete(context, :span_start)}
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


  def create_annotation(%{message: message}) do
    %{
      message: message
    }
  end

  def post_process_comments(comments) do
    values = Keyword.get_values(comments, :comment_line)
    content =
      values
      |> Enum.map(fn [_, content] -> content end)
      |> List.flatten()
      |> Enum.join("\n")
    [[{first, _}, _] | _] = values
    [{_, last}, _] = List.last(values)

    [content: content, span: {first, last}]
  end

  def trim_leading(str) do
    str
  end

  def raise_error(_, _, _, _, _) do
    raise ArgumentError, "check"
  end
end
