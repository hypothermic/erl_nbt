%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Constants and limits for encoding/decoding NBT data
%%% @end
%%% Created : 23. Jun 2021 12:45 PM
%%%-------------------------------------------------------------------
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

%%%-------------------------------------------------------------------
%%% Constants
%%%-------------------------------------------------------------------

-define(TAG_END_ID,			0).

-define(TAG_BYTE_ID,		1).
-define(TAG_SHORT_ID,		2).
-define(TAG_INT_ID,			3).
-define(TAG_LONG_ID,		4).
-define(TAG_FLOAT_ID,		5).
-define(TAG_DOUBLE_ID,		6).

-define(TAG_BYTE_ARRAY_ID,	7).
-define(TAG_INT_ARRAY_ID,	11).
-define(TAG_LONG_ARRAY_ID,	12).

-define(TAG_STRING_ID,		8).
-define(TAG_LIST_ID,		9).

-define(TAG_COMPOUND_ID,	10).

%%%-------------------------------------------------------------------
%%% Limits according to NBT spec
%%%-------------------------------------------------------------------

-define(MAX_DEPTH,			256).
-define(MAX_COUNT,			1024).