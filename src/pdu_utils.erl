%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2018 4:04 PM
%%%-------------------------------------------------------------------
-module(pdu_utils).
-author("msd").

%% API
-export([is_partial_sms/1, tp_scts_to_epoch/1, get_headers_with_identifier/2]).
-include_lib("gsm_pdu.hrl").

has_concate_headers([]) -> false;
has_concate_headers([H]) ->
  case H of
    #udh{identifier = 0} -> true;
    _ -> false
  end;
has_concate_headers([H | T]) ->
  case H of
    #udh{identifier = 0} -> true;
    _ -> has_concate_headers(T)
  end.

is_partial_sms(#pdu{tpdu = #tpdu_deliver{dh = #dh{headers = Headers}}}) ->
  has_concate_headers(Headers).

tp_scts_to_epoch(#tp_scts{year = Y, month = M, day = D, hour = H, minute = Minu, second = S}) ->
  calendar:datetime_to_gregorian_seconds({{2000 + Y, M, D}, {H, Minu, S}}).

get_headers_with_identifier(#pdu{tpdu = #tpdu_deliver{dh =#dh{headers = Headers} }}, HeaderIdentifier) ->
  L = lists:filter(
    fun(H) ->
      case H of
        #udh{identifier = HeaderIdentifier} -> true;
        _ -> false
      end
    end, Headers),
  L.


