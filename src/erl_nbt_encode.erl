%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Provides encoding functions for NBT data
%%% @end
%%% Created : 23. Jun 2021 2:41 PM
%%%-------------------------------------------------------------------
-module(erl_nbt_encode).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include("erl_nbt.hrl").

%%%-------------------------------------------------------------------
%%% Private API
%%%-------------------------------------------------------------------

-export([
	encode/2
]).

-spec encode(
	Nbt :: erl_nbt:nbt(),
	Options :: list()
) ->
	{ok, Binary :: binary()} |
	{error, {invalid_nbt, Details :: string()}}.
encode(Nbt, Options) ->
	% Every NBT map is, in itself, a compound
	Encoded = encode_compound(Nbt),

	% Check if it needs to be compressed
	case lists:keyfind(compression, 1, Options) of
		% GZIP compression
		{?OPTION_COMPRESSION, ?COMPRESSION_GZIP} ->
			{ok, zlib:gzip(Encoded)};
		% Zlib compression
		{?OPTION_COMPRESSION, ?COMPRESSION_ZLIB} ->
			Z = zlib:open(),
			zlib:deflateInit(Z),
			Deflated = zlib:deflate(Z, Encoded, finish),
			zlib:deflateEnd(Z),
			{ok, list_to_binary(Deflated)};
		% Uncompressed
		_ ->
			{ok, Encoded}
	end.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

encode_tag(Key, {?TAG_BYTE_TYPE, Value}) ->
	<<
		?TAG_BYTE_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:8/big-signed-integer
	>>;

encode_tag(Key, {?TAG_SHORT_TYPE, Value}) ->
	<<
		?TAG_SHORT_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:16/big-signed-integer
	>>;

encode_tag(Key, {?TAG_INT_TYPE, Value}) ->
	<<
		?TAG_INT_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:32/big-signed-integer
	>>;

encode_tag(Key, {?TAG_LONG_TYPE, Value}) ->
	<<
		?TAG_LONG_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:64/big-signed-integer
	>>;

encode_tag(Key, {?TAG_FLOAT_TYPE, Value}) ->
	<<
		?TAG_FLOAT_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:32/big-signed-float
	>>;

encode_tag(Key, {?TAG_DOUBLE_TYPE, Value}) ->
	<<
		?TAG_DOUBLE_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:64/big-signed-float
	>>;

encode_tag(Key, {?TAG_BYTE_ARRAY_TYPE, Value}) ->
	<<
		?TAG_BYTE_ARRAY_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(length(Value)):32/big-signed-integer,
		(lists:foldl(fun (E, Acc) -> <<Acc/binary, E:8/signed-integer>> end, <<>>, Value))/binary
	>>;

encode_tag(Key, {?TAG_INT_ARRAY_TYPE, Value}) ->
	<<
		?TAG_INT_ARRAY_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(length(Value)):32/big-signed-integer,
		(lists:foldl(fun (E, Acc) -> <<Acc/binary, E:32/signed-integer>> end, <<>>, Value))/binary
	>>;

encode_tag(Key, {?TAG_LONG_ARRAY_TYPE, Value}) ->
	<<
		?TAG_LONG_ARRAY_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(length(Value)):32/big-signed-integer,
		(lists:foldl(fun (E, Acc) -> <<Acc/binary, E:64/signed-integer>> end, <<>>, Value))/binary
	>>;

encode_tag(Key, {?TAG_STRING_TYPE, Value}) ->
	<<
		?TAG_STRING_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(encode_string(Value))/binary
	>>;

encode_tag(Key, {?TAG_LIST_TYPE, ChildType, Children}) ->
	<<
		?TAG_LIST_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(tag_type_to_id(ChildType)):8/signed-integer,
		(length(Children)):32/big-signed-integer,
		(lists:foldl(fun (E, Acc) ->
			<<
				Acc/binary,
				(encode_value(ChildType, E))/binary
			>>
		end, <<>>, Children))/binary
	>>;

encode_tag(Key, Value) when is_map(Value) ->
	encode_tag(Key, {compound, Value});

encode_tag(Key, {?TAG_COMPOUND_TYPE, Value}) ->
	<<
		?TAG_COMPOUND_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(encode_compound(Value))/binary,
		?TAG_END_ID:8/unsigned-integer
	>>.

encode_string(String) ->
	<<
		(length(String)):16/unsigned-integer,
		(list_to_binary(String))/binary
	>>.

encode_value(?TAG_BYTE_TYPE, Value) ->
	<<Value:8/big-signed-integer>>;

encode_value(?TAG_SHORT_TYPE, Value) ->
	<<Value:16/big-signed-integer>>;

encode_value(?TAG_INT_TYPE, Value) ->
	<<Value:32/big-signed-integer>>;

encode_value(?TAG_LONG_TYPE, Value) ->
	<<Value:64/big-signed-integer>>;

encode_value(?TAG_FLOAT_TYPE, Value) ->
	<<Value:32/big-signed-float>>;

encode_value(?TAG_DOUBLE_TYPE, Value) ->
	<<Value:64/big-signed-float>>;

encode_value(?TAG_COMPOUND_TYPE, Value) ->
	<<
		(encode_compound(Value))/binary,
		?TAG_END_ID:8/unsigned-integer
	>>.

encode_compound(Nbt) ->
	maps:fold(fun (Key, Value, Acc) ->
		<<
			Acc/binary,
			(encode_tag(Key, Value))/binary
		>>
		end, <<>>, Nbt).

tag_type_to_id(Type) ->
	case Type of
		?TAG_BYTE_TYPE			-> ?TAG_BYTE_ID;
		?TAG_SHORT_TYPE			-> ?TAG_SHORT_ID;
		?TAG_INT_TYPE			-> ?TAG_INT_ID;
		?TAG_LONG_TYPE			-> ?TAG_LONG_ID;
		?TAG_FLOAT_TYPE			-> ?TAG_FLOAT_ID;
		?TAG_DOUBLE_TYPE		-> ?TAG_DOUBLE_ID;
		?TAG_BYTE_ARRAY_TYPE	-> ?TAG_BYTE_ARRAY_ID;
		?TAG_INT_ARRAY_TYPE		-> ?TAG_INT_ARRAY_ID;
		?TAG_LONG_ARRAY_TYPE	-> ?TAG_LONG_ARRAY_ID;
		?TAG_STRING_TYPE		-> ?TAG_STRING_ID;
		?TAG_LIST_TYPE			-> ?TAG_LIST_ID;
		?TAG_COMPOUND_TYPE		-> ?TAG_COMPOUND_ID
	end.