# Fluent


`fluent-ex` is inspired by [Project Fluent](https://projectfluent.org). Grammar based on [Fluent v0.9.0](https://github.com/projectfluent/fluent).

## Installation

```elixir
def deps do
  [
    {:fluent, "~> 0.1.0"}
  ]
end
```

# AST not stable, possible breaking changes


# Tests

`ftl` files are taken from official Fluent spec.
Elixir NimbleParser AST is snapshotted into separate files.
There's basic JsonEncoder but preferably, AST output would match official
json structure after transformation.

```bash
$ mix test

# if you'd like to update snapshots
$ UPDATE=true mix test
```

# TODO:

  - [ ] better errors - is there any way to avoid exceptions?
  - [ ] running expressions on fetch
  - [ ] Fluent.Bundle (adding languages, fetching translations, etc.)
  - [ ] Correct spans and locations in AST
  - [ ] Multiple comment types based on `###`
  - [ ] output json for AST matching tests in [fluent](https://github.com/projectfluent/fluent)
