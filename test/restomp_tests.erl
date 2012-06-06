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
    ?assertEqual(Encoded, restomp:encode(Decoded)).

without_content_type_binary_test() ->
    Header = <<"\n"
                "SEND\n"
                "destination:/topic/queue\n"
                "content-length:18\n\n"/utf8>>,
    Msg = <<"\u0ca0\ufffd\x00\n\x01hello"/utf8>>,
    Encoded = <<Header/binary, Msg/binary, "\x00"/utf8>>,
    Decoded = {frame, "SEND", [{"content-length", "18"},
                               {"destination", "/topic/queue"}],
               [Msg]},
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
