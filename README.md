erl_nbt
=====

An Erlang/OTP library which can manipulate NBT (Named Binary Tag) data.

Please note that the API may change before the first major version is released.

Building & using the library
-----

| Operation | Command |
| --------- | ------- |
| Compile   | `rebar3 compile` |
| Test      | `rebar3 eunit` |

The easiest way to use the library is to declare it as a dependency in your build config.
Your build tool will automatically download the latest version from [Hex](https://hex.pm/packages/erl_nbt).
Depending on your build tool, add the following line to the "dependencies" section:

| Build Tool            | Dependency line |
| --------------------- | ------- |
| rebar3 (Erlang/OTP)   | `{erl_nbt, "0.1.0"}` |
| mix (Elixir)          | `{:erl_nbt "~> 0.1.0"}` |
| erlang.mk (Erlang)    | `dep_erl_nbt = hex 0.1.0` |

Legal notice
-----

erl_nbt Copyright (C) 2021 hypothermic.nl