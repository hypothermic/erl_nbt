erl_nbt
=====

[![Hex.pm Badge](https://img.shields.io/hexpm/v/erl_nbt.svg?style=flat)](https://hex.pm/packages/erl_nbt)

An Erlang/OTP library which can manipulate NBT (Named Binary Tag) data.

Please note that the API may change before the first major version is released.

Quick navigation:
[Build](#building-the-library)
[Usage](#using-the-library)
[Documentation](#documentation)
[Legal](#legal-notice)

-----

Building the library
-----

| Operation     | Command           |
| ------------- | ----------------- |
| Compile       | `rebar3 compile`  |
| Test          | `rebar3 eunit`    |
| Verify        | `rebar3 dialyzer` |
| Documentation | `rebar3 edoc`     |

Using the library
-----

The easiest way to use the library is to declare it as a dependency in your build config.
Your build tool will automatically download the latest version from [Hex](https://hex.pm/packages/erl_nbt).
Depending on your build tool, add the following line to the "dependencies" section:

| Build Tool            | Dependency line |
| --------------------- | ------- |
| rebar3 (Erlang/OTP)   | `{erl_nbt, "0.1.0"}` |
| mix (Elixir)          | `{:erl_nbt "~> 0.1.0"}` |
| erlang.mk (Erlang)    | `dep_erl_nbt = hex 0.1.0` |

Documentation
-----

The documentation can be generated locally by running `rebar3 edoc`.
A pre-generated version can be viewed online on [Hex docs](https://hexdocs.pm/erl_nbt/).

Legal notice
-----

erl_nbt Copyright (C) 2021 hypothermic.nl