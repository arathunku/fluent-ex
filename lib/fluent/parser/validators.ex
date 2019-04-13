defmodule Fluent.Parser.Validators do
  alias Fluent.Parser.ValidationError


  def validate_call_argument(rest, parsed, ctx, _, _) do
    if args = get_in(parsed, [:function_reference, :call_arguments]) do
      case validate_call_arguments_unique_names(args) do
        :ok -> nil
        :error ->
          raise ValidationError,
            message: "Named arguments must be unique."
      end

      case validate_call_arguments_position_named(args) do
        :ok -> nil
        :error ->
          raise ValidationError,
            message: "Positional arguments must not follow named arguments."
      end

      {parsed, ctx}
    else
      {parsed, ctx}
    end
  end

  def validate_function_name_all_caps(rest, parsed, ctx, _, _) do
    if id = get_in(parsed, [:function_reference, :id]) do
      if String.match?(id, ~r/^[A-Z][A-Z0-9_-]*$/) do
        {parsed, ctx}
      else
        raise ValidationError,
          message: """
          Invalid function name: #{id}.
          Function names must be all upper-case ASCII letters.
          """
      end
    else
      {parsed, ctx}
    end
  end

  defp validate_call_arguments_unique_names(args) do
    names =
      args
      |> Enum.filter(fn
        {k, _} -> k == :named_argument
        _ -> false
      end)
      |> Keyword.values()
      |> Enum.map(fn v -> Keyword.get(v, :id) end)
      |> Enum.filter(& &1)

    if Enum.count(names) != Enum.count(Enum.uniq(names)) do
      :error
    else
      :ok
    end
  end

  def validate_select_expression(_, parsed, ctx, _, _) do
    case Keyword.get(parsed, :select_expression) do
      [{:term_reference, term} | _] ->
        if Keyword.get(term, :attribute_accessor) do
          {parsed, ctx}
        else
          raise ValidationError,
            message: """
            Terms cannot be used as selectors - #{inspect(term)}
            """
        end
      _ ->
        {parsed, ctx}
    end
  end

  defp validate_call_arguments_position_named(args), do: validate_call_arguments_position_named(args, false)
  defp validate_call_arguments_position_named([], _), do: :ok
  defp validate_call_arguments_position_named([arg | args], found_named) do
    if elem(arg, 0) == :named_argument do
      validate_call_arguments_position_named(args, true)
    else
      if found_named do
        :error
      else
        validate_call_arguments_position_named(args)
      end
    end
  end
end
