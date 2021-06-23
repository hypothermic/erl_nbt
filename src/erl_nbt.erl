%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%
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
	nbt/0, nbt_key/0, nbt_value/0,
	nbt_byte/0, nbt_short/0, nbt_int/0, nbt_long/0, nbt_float/0, nbt_double/0,
	nbt_byte_array/0, nbt_int_array/0, nbt_long_array/0,
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

-spec decode(Binary :: binary()) -> {ok, nbt()}.
decode(Binary) ->
	{ok, Nbt, _} = decode_with_rest(Binary),

	{ok, Nbt}.

-spec decode_with_rest(Binary :: binary()) -> {ok, nbt(), Rest :: binary()}.
decode_with_rest(Binary) ->
	erl_nbt_decode:decode(Binary).

-spec encode(Nbt :: nbt()) -> {ok, binary()}.
encode(_Nbt) ->
	erlang:nif_error("Not implemented (yes, wrong error)").