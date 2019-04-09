defmodule Fluent.ParserTest do
  use ExUnit.Case
  @whitelist ~w(any_char)

# ## Literal text
# text-backslash-one = Value with \ a backslash
# text-backslash-two = Value with \\ two backslashes
# text-backslash-brace = Value with \{placeable}
# text-backslash-quote = Value with \""xx"
# text-backslash-u = \u0041
# text-backslash-backslash-u = \\u0041

# ## String literals
# quote-in-string = {"\""}
# backslash-in-string = {"\\"}
  test "basic parser" do
    result = Fluent.Parser.parse(File.read!("./test/fluent/test.flt"))
    IO.inspect(result, charlists: false)
    # assert {:ok, _} = result
  end

  # Path.wildcard("./test/fixtures/*.ftl")
  # |> Enum.filter(fn path ->
  #   Enum.any?(@whitelist, &String.contains?(path, &1))
  # end)
  # |> Enum.map(fn path ->
  #   @path path
  #   test "Testing parsing - #{path}" do
  #     content = File.read!(@path)

  #     assert {:ok, _} = Fluent.Parser.parse(content)
  #   end
  # end)
end
