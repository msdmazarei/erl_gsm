%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2018 3:02 PM
%%%-------------------------------------------------------------------
-module(gsm_pdu_deserializers).
-author("msd").
-include_lib("gsm_pdu.hrl").
%% API
-export([first_octet/1, type_of_address/1, address_field/1]).
-export([address_field_decimal_len/1, tp_pid/1, tp_dcs/1]).
-export([tp_scts/1, udh/1,dh/1,tpdu/1]).

-spec first_octet(binary()) -> { first_octet(),binary() }.
first_octet(<<RP:1, UDHI:1, SSR:1, VPF:2, RD:1, MTI:2 , Remain/binary>>) ->
  {
    #first_octet{
    tp_mti = MTI,
    tp_rd = bit_to_boolean(RD),
    tp_vpf = VPF,
    tp_ssr = bit_to_boolean(SSR),
    tp_udhi = bit_to_boolean(UDHI),
    tp_rp = bit_to_boolean(RP)
  } , Remain}.

-spec type_of_address(binary()) -> { type_of_address(), binary() }.
type_of_address(<<1:1, TON:3, NPI:4, Remain/binary>>) ->
  { #type_of_address{ton = TON, npi = NPI} , Remain }.

-spec address_field(binary()) -> { address_field(),binary() }.
address_field(<<0, BTOA:1/binary, Remain/binary>>) ->
  TOA = case BTOA of
          <<>> -> undefined;
          _ -> type_of_address(BTOA)
        end,
  { #address_field{
    address_length = 0,
    type_of_address = TOA
  }, Remain } ;
address_field(<<L, BTOA:1/binary, BNumber:L/binary, Remain/binary>>) when L > 0 ->
  Digits = decode_binlist_to_number(BNumber),
  TOA = type_of_address(BTOA),
  {
  #address_field{
    address_length = L,
    type_of_address = TOA,
    address_digits = Digits
  }, Remain } .


-spec address_field_decimal_len(binary()) -> {address_field(),binary()}.
address_field_decimal_len(<<0, Remain/binary>>) ->
  {#address_field{address_length = 0},Remain};
address_field_decimal_len(
    <<L1:1/binary, BTOA:1/binary, Remain/binary>>
) ->
  L = octet_to_deciaml(L1),
  <<BNumber:L,Remain1/binary>> = Remain,
  TOA = type_of_address(BTOA),
  Digits = decode_binlist_to_number(BNumber),
  {#address_field{
    address_length = L,
    type_of_address = TOA,
    address_digits = Digits
  },Remain1}.


-spec tp_pid(binary()) -> {tp_pid(),binary()}.
tp_pid(<<THB:2, TI:1, TD:5,Remain/binary>>) ->
  {
    #tp_pid{
    two_high_bits = THB,
    telemetric_interwoking = bit_to_boolean(TI),
    telemetric_devices_type = TD
  },Remain
  }.


-spec tp_dcs(binary()) -> {tp_dcs(),binary()}.
tp_dcs(<<
  _:2, %% two high bits should be zero accordings to Standard GSM 3.4
  C:1, I:1, A:2, M:2,Remain/binary>>)
  ->
  {
  #tp_dcs{
    compress = bit_to_boolean(C),
    is_bit_0_1_class_meaning = bit_to_boolean(I),
    alphabet_indication = A,
    message_class = M
  },Remain}.

-spec tp_scts(binary()) -> {tp_scts(),binary()}.
tp_scts(<<Y, M, D, H, Minu, S, T,Remain/binary>>) ->
  {
  #tp_scts{
    year = Y,
    month = M,
    day = D,
    hour = H,
    minute = Minu,
    second = S,
    timezone = T
  },Remain}.

-spec udh(binary()) -> {udh(),binary()}.
udh(<<I, L, D:L/binary,Remain/binary>>) ->
  {#udh{
    identifier = I,
    length = L,
    data = D
  },Remain}.

-spec dh_helper(integer(),binary(),list())->{list(),binary()}.
dh_helper(0,R,Headers)->
  {Headers,R};
dh_helper(Count,HeadersInBinary,Headers)->
  {H,Remain}=udh(HeadersInBinary),
  dh_helper(Count-1,Remain,[H|Headers]).


-spec dh(binary())->{dh(),binary()}.
dh(<<L,Headers/binary>>)
  ->
  {Hs,R}=dh_helper(L,Headers,[]),
  {#dh{
    length_of_user_data_header = L,
    headers = Hs
  },R}.

-spec tpdu(binary())->tpdu().
tpdu(B)->
  {FirstOctet,R1} = first_octet(B),
  << MR, R2/binary>> = R1,
  {DA,R3} = address_field(R2),
  {PID,R4} = tp_pid(R3),
  {DCS,R5} = tp_dcs(R4),
  {VPV,R6} = case FirstOctet#first_octet.tp_vpf of
               0 -> {undefined,R5};
               1 -> {undefined,R5};
               2 -> <<K,R_>> = R5, {K,R_};
               3 -> tp_scts(R5)
             end,
  <<UDL,R7>> = R6,
  <<Headers,R8>> = dh(R7),
  #tpdu{
    first_octet = FirstOctet,
    tp_mr = MR,
    tp_da = DA,
    tp_pid = PID,
    tp_dcs = DCS,
    tp_vp = VPV,
    tp_udl = UDL,
    dh = Headers,
    tp_ud = R8
  }.

bit_to_boolean(0) -> false;
bit_to_boolean(1) -> true.

-spec octet_to_deciaml(binary()) -> byte().
octet_to_deciaml(<<C:4, R:4>>) ->
  C * 10 + R.

-spec decode_binlist_to_number(binary()) -> list().
decode_binlist_to_number(Binary) -> decode_binlist_to_number(Binary, "").

-spec decode_binlist_to_number(binary(), list()) -> list().
decode_binlist_to_number(<<>>, Acc) -> Acc;
decode_binlist_to_number(<<A:4, B:4, Rest/binary>>, Acc) ->
  BNUMB = $0 + B,
  ACC1 = Acc ++ [BNUMB],
  ACC2 = case A of
           16#F -> ACC1;
           _ -> ACC1 ++ [$0 + A]
         end,
  decode_binlist_to_number(Rest, ACC2).