%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%     Tests the erl_nbt functionality on the "compounds.nbt" example
%%% @end
%%% Created : 22. Jun 2021 11:56 PM
%%%-------------------------------------------------------------------
-module(compounds_test).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include_lib("erl_nbt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE,      "test/compounds.nbt").
-define(EXPECTED_NBT,
	#{
		"base compound" => #{
			"nested compound" => #{
				"int child of nested compound" => {?TAG_INT_TYPE, 420},
				"double child of nested compound" => {?TAG_DOUBLE_TYPE, 6.9},
				"string child of nested compound" => {?TAG_STRING_TYPE, "Hello!"},
				"very deeply nested compound!" => #{
					"string example" => {?TAG_STRING_TYPE, "Nobody will see this probably cuz tests r boring"}
				}
			},
			"z another nested compound" => #{
				"string_child_of_nested_compound" => {?TAG_STRING_TYPE, "I'm a string!"},
				"UTF-8 named int child!!" => {?TAG_INT_TYPE, 69}
			}
		}
	}
).

compounds_test_() -> [
	{"decode file compounds.nbt",
		fun () ->
			{ok, InputData}     = file:read_file(?TEST_FILE),
			{ok, DecodedNbt}    = erl_nbt:decode(InputData),

			?assertEqual(?EXPECTED_NBT, DecodedNbt)
		end
	},
	{"encode map and compare with file compounds.nbt",
		fun () ->
			{ok, EncodedNbt}    = erl_nbt:encode(?EXPECTED_NBT),
			{ok, CorrectData}   = file:read_file(?TEST_FILE),

			% Because child order of compound tags is not guaranteed
			% to be preserved with NBT tags, we can't test for equality.
			% So we only test if the length of these binaries are equal.
			?assertEqual(byte_size(CorrectData), byte_size(EncodedNbt))
		end
	}
].