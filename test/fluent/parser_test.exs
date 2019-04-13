
defmodule Fluent.ParserTest do
  use ExUnit.Case
  @snapshot_suffix ".snapshot.json"
  @whitelist ~w(any_char)

  # test "basic parser" do
  #   result = Fluent.Parser.run(File.read!("./test/fluent/test.flt"))
  #   IO.inspect(result, charlists: false)
  #   # assert {:ok, _} = result
  # end

  Path.wildcard("./test/fixtures/*.ftl")
  # |> Enum.filter(fn path ->
  #   Enum.any?(@whitelist, &String.contains?(path, &1))
  # end)
  |> Enum.map(fn path ->
    @path String.replace(path, ".ftl", "")
    @snapshot_path @path <> @snapshot_suffix

    test "Testing parsing - #{path}" do
      content = File.read!(@path <> ".ftl")
      {:ok, parsed} = Fluent.Parser.run(content)
      new_snapshot = Fluent.JsonEncoder.encode!(parsed)

      compare_snapshots(@snapshot_path, new_snapshot)
    end
  end)

  defp read_snapshot(path) do
    case File.read(path) do
      {:ok, content} -> content
      _ -> nil
    end
  end

  def compare_snapshots(path, new_snapshot) do
    previous_snapshot = read_snapshot(path)

    if System.get_env("UPDATE") || !previous_snapshot do
      File.write!(path, format_json(new_snapshot))
    else
      assert Jason.decode(new_snapshot) == Jason.decode(previous_snapshot)
    end
  end

  def format_json(json) do
    Jason.Formatter.pretty_print(json)
  end
end
