%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%     Tests the erl_nbt functionality on the "bigtest.nbt" example
%%%     provided by Notch
%%% @end
%%% Created : 14. Jul 2021 2:17 PM
%%%-------------------------------------------------------------------
-module(bigtest_test).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("bigtest_test.hrl").

-define(TEST_FILE,	"test/bigtest.nbt").
-define(OPTIONS,  	[{?OPTION_COMPRESSION, ?COMPRESSION_GZIP}]).

basic_test_() -> [
	{"decode file bigtest.nbt",
		fun () ->
			{ok, InputData}		= file:read_file(?TEST_FILE),
			{ok, DecodedNbt}	= erl_nbt:decode(InputData, ?OPTIONS),

			?assertEqual(?EXPECTED_NBT, DecodedNbt)
		end
	},
	{"encode map and compare with file bigtest.nbt",
		fun () ->
			{ok, EncodedNbt}	= erl_nbt:encode(?EXPECTED_NBT, ?OPTIONS),
			{ok, CorrectData}	= file:read_file(?TEST_FILE),

			% Because child order of compound tags is not guaranteed
			% to be preserved with NBT tags, we can't test for equality.
			% So we only test if the length of these binaries are equal.
			% - Also, depending on which compression implementation was
			% used on bigtest.nbt (yes, it's from 2011), it can add extra
			% bytes, so don't test length with gzip.
			?assertEqual(
				byte_size(zlib:gunzip(CorrectData)),
				byte_size(zlib:gunzip(EncodedNbt))
			)
		end
	},
	{"encode then decode",
		fun () ->
			{ok, EncodedNbt}	= erl_nbt:encode(?EXPECTED_NBT, ?OPTIONS),
			{ok, DecodedNbt}	= erl_nbt:decode(EncodedNbt, ?OPTIONS),

			?assertEqual(?EXPECTED_NBT, DecodedNbt)
		end
	}
].
