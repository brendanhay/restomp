%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(restomp_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EQC(P),  ?assert(proper:quickcheck(P))).
-define(_EQC(P), ?_assert(proper:quickcheck(P))).

-compile(export_all).

%%
%% Units
%%

newline_after_null_test() ->
    Encoded = <<"\n"
                "SUBSCRIBE\n"
                "destination:/topic/queue\n"
                "\n\x00\n"
                "overflow..">>,
    Decoded = {frame, "SUBSCRIBE", [{"destination", "/topic/queue"}], []},
    ?assertEqual({ok, Decoded, <<"\noverflow..">>}, restomp:decode(Encoded)).

decode_message_test() ->
    Encoded = <<"MESSAGE\n"
                "content-type:text/plain\n"
                "destination:/topic/queue\n"
                "message-id:1\n"
                "content-length:6\n"
                "\n"
                "hello\n\0">>,
    Headers = [{"content-length", "6"},
               {"message-id", "1"},
               {"destination", "/topic/queue"},
               {"content-type", "text/plain"}],
    Decoded = {frame, "MESSAGE", Headers, [<<"hello\n">>]},
    ?assertEqual({ok, Decoded, <<>>}, restomp:decode(Encoded)).

encode_message_test() ->
    Encoded = <<"MESSAGE\n"
                "content-type:text/plain\n"
                "destination:/topic/queue\n"
                "message-id:1\n"
                "content-length:6\n"
                "\n"
                "hello\n\0">>,
    Headers = [{"content-length", "6"},
               {"message-id", "1"},
               {"destination", "/topic/queue"},
               {"content-type", "text/plain"}],
    Decoded = {frame, "MESSAGE", Headers, [<<"hello\n">>]},
    {Encoded, restomp:encode(Decoded)}.

without_content_type_binary_test() ->
    Header = <<"\n"
                "SEND\n"
                "destination:/topic/queue\n"
                "content-length:">>,
    Msg = <<"\u0ca0\ufffd\x00\n\x01hello\x00"/utf8>>,
    Size = list_to_binary(integer_to_list(size(Msg)) ++ "\n\n"),
    Encoded = <<Header/binary, Size/binary, Msg/binary>>,
    Decoded = {frame, "SEND", [{"destination", "/topic/queue"},
                               {"content-length", integer_to_list(size(Msg))}],
              [<<"hello">>]},
    ?assertEqual({ok, Decoded, <<>>}, restomp:decode(Encoded)).

newline_after_nul_and_leading_nul_test() ->
    Encoded = <<"\n"
                "\x00SUBSCRIBE\n"
                "destination:/topic/queue\n"
                "\n\x00\n"
                "\x00SEND\n"
                "destination:/topic/queue\n"
                "content-type:text/plain\n"
                "\nhello\n\x00\n">>,
    Decoded = {frame, "SUBSCRIBE", [{"destination", "/topic/queue"}], []},
    ?assertMatch({ok, Decoded, _}, restomp:decode(Encoded)).

%%
%% Helpers
%%

%%
%% Properties
%%

%% header_encode_test() ->
%%     ?EQC(?FORALL(F, headers(),
%%                  begin
%%                      {ok, Decoded, _} = restomp:parse(F),
%%                      {frame,}
%%                  end)).

%%
%% Generators
%%

headers() -> ?LET(L, list({word(), word()}), L).

word() -> non_empty(list(oneof(lists:seq(33, 126)))).
