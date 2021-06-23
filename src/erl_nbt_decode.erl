%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Provides decoding/parsing functions for NBT data
%%% @end
%%% Created : 22. Jun 2021 11:07 PM
%%%-------------------------------------------------------------------
-module(erl_nbt_decode).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include("erl_nbt.hrl").

%%%-------------------------------------------------------------------
%%% Private API
%%%-------------------------------------------------------------------

-export([
	decode/1
]).

%%%-------------------------------------------------------------------
%%% @private Use the public equivalent erl_nbt:decode_with_rest/1
%%% @since 0.1.0
%%%
%%% @doc Decodes the first (nested) NBT tag in the specified binary.
%%%
%%% 	This function reads the first NBT tag from a binary.
%%% 	If it is a compound tag, it will read all children (including
%%% 	nested compound tags.)
%%%
%%% 	If the given binary has remaining data after the NBT tag,
%%% 	this remaining data will be returned as Rest.
%%%
%%% @end
%%%
%%% @param Binary The binary to read the NBT tag from
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt, Remainder}
%%%			where NBT is a map (same as type erl_nbt:nbt())
%%%			and Remainder is an (empty) binary
%%%		or an error tuple containing {error, {Reason, Details}}
%%% @end
%%%-------------------------------------------------------------------

-spec decode(Binary :: binary()) ->
	{ok, Nbt :: erl_nbt:nbt(), Rest :: binary()} |
	{error, {max_depth_reached, Depth :: integer()}} |
	{error, {max_count_reached, Count :: integer()}} |
	{error, {unknown_tag_id, EncounteredTagId :: integer()}}.
decode(Binary) ->
	% All NBT files/binaries are inherently compound tags, so read compound.
	decode_tag(Binary, 0, 0, #{}).

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

-spec decode_tag(Binary :: binary(), Count :: integer(), Depth :: integer(), Output :: map()) ->
	{ok, Nbt :: erl_nbt:nbt(), Rest :: binary()} |
	{error, {max_depth_reached, Depth :: integer()}} |
	{error, {max_count_reached, Count :: integer()}} |
	{error, {unknown_tag_id, EncounteredTagId :: integer()}}.

% Maximum tag depth has been reached, stop and return error.
decode_tag(_Binary, _Count, Depth, _Output) when Depth >= ?MAX_DEPTH ->
	{error, {max_depth_reached, Depth}};

% Maximum child count has been reached, stop and return error.
decode_tag(_Binary, Count, _Depth, _Output) when Count >= ?MAX_COUNT ->
	{error, {max_count_reached, Count}};

% Reached end of binary, this occurs because the root compound tag doesn't end with 0x00
decode_tag(<<>>, _Count, _Depth, Output) ->
	{ok, Output, <<>>};

% TAG_End reached, the current compound tag has been fully read. Return all children.
decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, _Count, _Depth, Output) when Id =:= ?TAG_END_ID ->
	{ok, Output, Rest};

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_BYTE_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Byte, Rest3} = decode_byte(Rest2),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => Byte});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_SHORT_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Short, Rest3} = decode_short(Rest2),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => Short});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_INT_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Int, Rest3} = decode_int(Rest2),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => Int});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_LONG_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Long, Rest3} = decode_long(Rest2),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => Long});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_FLOAT_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Float, Rest3} = decode_float(Rest2),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => Float});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_DOUBLE_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Double, Rest3} = decode_double(Rest2),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => Double});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_STRING_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, String, Rest3} = decode_string(Rest2),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => String});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, Depth, Output) when Id =:= ?TAG_COMPOUND_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, NestedCompound, Rest3} = decode_tag(Rest2, 0, Depth + 1, #{}),

	decode_tag(Rest3, Count + 1, Depth, Output#{Name => NestedCompound});

% Encountered an unknown tag ID, probably a malformed NBT file.
decode_tag(<<Id:8/unsigned-integer, _Rest/binary>>, _Count, _Depth, _Output) ->
	{error, {unknown_tag_id, Id}}.

decode_byte(<<Byte:8/signed-integer, Rest/binary>>) ->
	{ok, Byte, Rest}.

decode_short(<<Short:16/big-signed-integer, Rest/binary>>) ->
	{ok, Short, Rest}.

decode_int(<<Int:32/big-signed-integer, Rest/binary>>) ->
	{ok, Int, Rest}.

decode_long(<<Long:64/big-signed-integer, Rest/binary>>) ->
	{ok, Long, Rest}.

decode_float(<<Float:32/big-signed-float, Rest/binary>>) ->
	{ok, Float, Rest}.

decode_double(<<Double:64/big-signed-float, Rest/binary>>) ->
	{ok, Double, Rest}.

decode_string(<<Length:16/unsigned-integer, String:Length/binary, Rest/binary>>) ->
	{ok, binary_to_list(String), Rest}.