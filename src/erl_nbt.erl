%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Provides an interface for dealing with NBT data in Erlang
%%% @end
%%% Created : 22. Jun 2021 10:31 PM
%%%-------------------------------------------------------------------
-module(erl_nbt).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-export_type([
	% Structures
	nbt/0, nbt_key/0, nbt_value/0,

	% Integer and floating points
	nbt_byte/0, nbt_short/0, nbt_int/0, nbt_long/0, nbt_float/0, nbt_double/0,

	% Arrays
	nbt_byte_array/0, nbt_int_array/0, nbt_long_array/0,

	% Structure components
	nbt_string/0, nbt_compound/0
]).

-type nbt()				:: #{nbt_key() => nbt_value()}.
-type nbt_key()			:: string().
-type nbt_value()		:: nbt_byte() | nbt_short() | nbt_int() | nbt_int() | nbt_long() | nbt_float() | nbt_double() |
						   nbt_byte_array() | nbt_int_array() | nbt_long_array() | nbt_string() | nbt_compound().

-type nbt_byte()		:: byte().
-type nbt_short()		:: integer().
-type nbt_int()			:: integer().
-type nbt_long()		:: integer().
-type nbt_float()		:: float().
-type nbt_double()		:: float().

-type nbt_byte_array()	:: list().
-type nbt_int_array()	:: list().
-type nbt_long_array()	:: list().

-type nbt_string()		:: string().

-type nbt_compound()	:: map().

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-export([
	decode/1,
	encode/1
]).

%%%-------------------------------------------------------------------
%%% @since 0.1.0
%%%
%%% @doc Decodes the first (nested) NBT tag in the specified binary.
%%%
%%% 	This function reads the first NBT tag from a binary.
%%% 	If it is a compound tag, it will read all children (including
%%% 	nested compound tags.)
%%%
%%%		This function is for basic usage.
%%% 	I recommend using decode_with_rest in production because it
%%%		has better error handling.
%%%
%%% @end
%%%
%%% @param Binary The binary to read the NBT tag from
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt} where Nbt is a map
%%% @end
%%%-------------------------------------------------------------------

-spec decode(Binary :: binary()) -> {ok, nbt()}.
decode(Binary) ->
	{ok, Nbt, _} = decode_with_rest(Binary),

	{ok, Nbt}.

%%%-------------------------------------------------------------------
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
%%%			where Nbt is a map (same as type erl_nbt:nbt())
%%%			and Remainder is an (empty) binary
%%%		or an error tuple containing {error, {Reason, Details}}
%%% @end
%%%-------------------------------------------------------------------

-spec decode_with_rest(Binary :: binary()) -> {ok, nbt(), Rest :: binary()}.
decode_with_rest(Binary) ->
	erl_nbt_decode:decode(Binary).

%%%-------------------------------------------------------------------
%%% @since 0.1.0
%%%
%%% @doc Encodes an NBT tag in Erlang Map form to binary.
%%%
%%% 	This function accepts an NBT tag representation in map-form
%%%		(aka type erl_nbt:nbt()) and encodes it into a binary.
%%%
%%% @end
%%%
%%% @param Nbt The NBT tag to encode
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt} where Nbt is a map
%%% @end
%%%-------------------------------------------------------------------

-spec encode(Nbt :: nbt()) -> {ok, binary()}.
encode(Nbt) ->
	erl_nbt_encode:encode(Nbt).