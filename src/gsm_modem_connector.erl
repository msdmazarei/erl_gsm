%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2018 2:05 PM
%%%-------------------------------------------------------------------
-module(gsm_modem_connector).
-author("msd").

-callback send_to_modem(term(),binary())-> ok.
-callback receive_from_modem(term())-> binary().

%% API
-export([]).
