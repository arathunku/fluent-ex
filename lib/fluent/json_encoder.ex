defimpl Jason.Encoder, for: [MapSet, Range, Stream, Tuple, Keyword] do
  def encode(struct, opts) do
    Jason.Encode.list(Enum.to_list(struct), opts)
  end
end

defimpl Jason.Encoder, for: [MapSet, Range, Stream, Tuple, Keyword] do
  def encode(struct, opts) do
    Jason.Encode.list(Enum.to_list(struct), opts)
  end
end

defmodule Fluent.JsonEncoder do
  def encode!(content) do
    Jason.encode!(transform(content))
  end

  def transform(content) when is_list(content) do
    Enum.map(content, fn
      {k, v} ->
        %{key: k, value: transform(v)}
      v ->
        %{value: transform(v)}
    end)
  end
  def transform(content) when is_tuple(content), do:
    Tuple.to_list(content) |> transform
  def transform(content), do: content
end
