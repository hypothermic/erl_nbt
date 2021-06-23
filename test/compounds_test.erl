%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%
%%% @end
%%% Created : 22. Jun 2021 11:56 PM
%%%-------------------------------------------------------------------
-module(compounds_test).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE,      "test/compounds.nbt").
-define(EXPECTED_NBT,
    #{
        "base compound" => #{
            "nested compound" => #{
                "int child of nested compound" => 420,
                "double child of nested compound" => 6.9,
                "string child of nested compound" => "Hello!",
                "very deeply nested compound!" => #{
                    "string example" => "Nobody will see this probably cuz tests r boring"
                }
            },
            "another nested compound" => #{
                "string_child_of_nested_compound" => "I'm a string!",
                "UTF-8 named int child!!" => 69
            }
        }
    }
).

compounds_test_() -> [
    {"file compounds.nbt",
        fun () ->
            {ok, InputData}     = file:read_file(?TEST_FILE),
            {ok, DecodedNbt}    = erl_nbt:decode(InputData),

            ?assertEqual(?EXPECTED_NBT, DecodedNbt)
        end
    }
].