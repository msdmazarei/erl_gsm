%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2018 6:21 PM
%%%-------------------------------------------------------------------
-module(test_address_encode).
-author("msd").

-export([international_no/0,no_international_no/0,start_simple_gsm_state_machine/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gsm_pdu.hrl").


start_simple_gsm_state_machine(IpAddress,Port)->
  {ok,_} = tcp_gsm_modem_connector:start_link(IpAddress,Port,cellno_09360076133),
  gsm_state_machine:start_link(gsm_state_09360076133,tcp_gsm_modem_connector,cellno_09360076133).

simple_test() ->
  ?assert(true).


international_no() ->
  Phone = <<"15551234567">>,
  AddressField = #address_field {
    address_length = byte_size(Phone),
    type_of_address = #type_of_address {
       ton = 1,
      npi = 1
    },
    address_digits = binary:bin_to_list(Phone)
  },
  R = gsm_pdu:address_field_to_binary(AddressField),
  io:fwrite(bin_to_hex:bin_to_hex(R)),
  ?assert(R=="0B915155214365F7").

no_international_no() ->
  Phone = <<"989360076133">>,
  AddressField = #address_field {
    address_length = byte_size(Phone),
    type_of_address = #type_of_address {
      ton = 1,
      npi = 1
    },
    address_digits = binary:bin_to_list(Phone)
  },
  R = gsm_pdu:address_field_to_binary(AddressField),
  io:fwrite(bin_to_hex:bin_to_hex(R)),
  ?assert(R=="0B915155214365F7").
