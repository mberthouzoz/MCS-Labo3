%%%-------------------------------------------------------------------
%%% @author Michaël
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. juin 2016 11:15
%%%-------------------------------------------------------------------
-author("Michaël").

-define(PORT, 12345).
-define(MAX_CLIENT, 10000).
-define(MAX_TOPIC, 10000).

-define(LOG(Fmt), io:format(Fmt)).
-define(LOG(Fmt, Args), io:format(Fmt, Args)).