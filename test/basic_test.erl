%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%     Tests the erl_nbt functionality on the "basic.nbt" example
%%% @end
%%% Created : 22. Jun 2021 10:38 PM
%%%-------------------------------------------------------------------
-module(basic_test).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include_lib("erl_nbt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE,      "test/basic.nbt").
-define(EXPECTED_NBT,
	#{
		"hello world" => #{
			"name" => {?TAG_STRING_TYPE, "Bananrama"}
		}
	}).

basic_test_() -> [
	{"decode file basic.nbt",
		fun () ->
			{ok, InputData}		= file:read_file(?TEST_FILE),
			{ok, DecodedNbt}	= erl_nbt:decode(InputData),

			?assertEqual(?EXPECTED_NBT, DecodedNbt)
		end
	},
	{"encode map and compare with file basic.nbt",
		fun () ->
			{ok, EncodedNbt}	= erl_nbt:encode(?EXPECTED_NBT),
			{ok, CorrectData}	= file:read_file(?TEST_FILE),

			?assertEqual(CorrectData, EncodedNbt)
		end
	},
	{"encode then decode",
		fun () ->
			{ok, EncodedNbt}	= erl_nbt:encode(?EXPECTED_NBT),
			{ok, DecodedNbt}	= erl_nbt:decode(EncodedNbt),
			?assertEqual(DecodedNbt, ?EXPECTED_NBT)
		end
	}
].