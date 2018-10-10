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
-export([decode_binlist_to_number/1]).

-spec first_octet(binary()) -> first_octet().
first_octet(<<RP:1, UDHI:1, SSR:1, VPF:2, RD:1, MTI:2>>) ->
  #first_octet{
    tp_mti = MTI,
    tp_rd = bit_to_boolean(RD),
    tp_vpf = VPF,
    tp_ssr = bit_to_boolean(SSR),
    tp_udhi = bit_to_boolean(UDHI),
    tp_rp = bit_to_boolean(RP)
  }.

-spec type_of_address(binary()) -> type_of_address().
type_of_address(<<1:1, TON:3, NPI:4>>) ->
  #type_of_address{ton = TON, npi = NPI}.

-spec address_field(binary()) -> address_field().
address_field(<<0, BTOA:1/binary>>) ->
  TOA = case BTOA of
          <<>> -> undefined;
          _ -> type_of_address(BTOA)
        end,
  #address_field{
    address_length = 0,
    type_of_address = TOA
  };
address_field(<<L, BTOA:1/binary, BNumber/binary>>) when L > 0 ->
  Digits = decode_binlist_to_number(BNumber),
  TOA = type_of_address(BTOA),

  #address_field{
    address_length = L,
    type_of_address = TOA,
    address_digits = Digits
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
  ACC1 = Acc ++[ BNUMB],
  ACC2 = case A of
            16#F -> ACC1;
            _ -> ACC1++ [$0 + A]
          end,
  decode_binlist_to_number(Rest, ACC2).