%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2018 11:15 AM
%%%-------------------------------------------------------------------
-module(gsm_pdu_serializers).
-author("msd").

%% API
-export([]).
-include_lib("gsm_pdu.hrl").
%% SERIALIZATION

-spec first_octet(first_octet()) -> binary().
first_octet(#first_octet{
  tp_mti = MTI,
  tp_rd = RD,
  tp_vpf = VPF,
  tp_ssr = SSR,
  tp_udhi = UDHI,
  tp_rp = RP

}) ->
  <<
    MTI:2,
    (boolean_to_bit(RD)):1,
    VPF:2,
    (boolean_to_bit(SSR)):1,
    (boolean_to_bit(UDHI)):1,
    (boolean_to_bit(RP)):1

  >>.

-spec type_of_address(type_of_address()) -> binary().
type_of_address(#type_of_address{ton = TON, npi = NPI}) ->
  <<1:1, TON:3, NPI:4>>.


-spec address_field(address_field()) -> binary().
address_field(
    #address_field{
      address_length = L,
      type_of_address = TOA,
      address_digits = Digits
    }) ->
  LNumber = encode_number_to_binlist(Digits),
  BNumber = list_to_binary(LNumber),
  BTOA = type_of_address_to_binary(TOA),
  <<L, BTOA/binary, BNumber/binary>>.

-spec tp_pid(tp_pid()) -> binary().
tp_pid(#tp_pid{
  two_high_bits = THB,
  telemetric_interwoking = TI,
  telemetric_devices_type = TD
}) ->
  <<
    THB:2,
    (boolean_to_bit(TI)):1,
    TD:5
  >>.

-spec tp_dsc(tp_dcs()) -> binary().
tp_dsc(#tp_dcs{
  compress = C,
  is_bit_0_1_class_meaning = I,
  alphabet_indication = A,
  message_class = M}) ->
  <<
    0:2, %% two high bits should be zero accordings to Standard GSM 3.4
    (boolean_to_bit(C)):1,
    (boolean_to_bit(I)):1,
    A:2,
    M:2>>.

-spec tp_scts(tp_scts())->binary().
tp_scts(#tp_scts{
  year = Y,
  month = M,
  day = D,
  hour = H,
  minute = Minu,
  second = S,
  timezone = T
})->
  <<Y,M,D,H,Minu,S,T>>.

-spec udh(udh())->binary().
udh(#udh{
  identifier = I,
  length = L,
  data=D
})->
  <<I,L,D/binary>>.

dh(#dh{length_of_user_data_header = L,
  headers = HL})->
  

%% UTILS


encode_number_to_binlist([A, B | T]) ->
  encode_number_to_binlist([], A, B, T);
encode_number_to_binlist([A]) ->
  encode_number_to_binlist([], A, 16#F, []);
encode_number_to_binlist([]) ->
  [].

encode_number_to_binlist(Result, A, B, [C]) ->
  <<R>> = <<B:4, A:4>>,
  NextResult = lists:append(Result, [R]),
  encode_number_to_binlist(NextResult, C, 16#F, []);
encode_number_to_binlist(Result, A, B, [C, D | T]) ->
  <<R>> = <<B:4, A:4>>,
  NextResult = lists:append(Result, [R]),
  encode_number_to_binlist(NextResult, C, D, T);
encode_number_to_binlist(Result, A, B, []) when is_number(A), is_number(B) ->
  <<R>> = <<B:4, A:4>>,
  lists:append(Result, [R]).


boolean_to_bit(false) -> 0;
boolean_to_bit(true) -> 1.

