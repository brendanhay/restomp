%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(restomp).

-include("include/restomp.hrl").

%% API
-export([new/0,
         push/2]).


%%
%% Types
%%

-record(p, {bytes = <<>> :: binary()}).

-opaque stomp_parser() :: #p{}.

-type stomp_message()  :: {msg, _}.
-type stomp_messages() :: [stomp_message()].
-type stomp_result()   :: {ok | {error, atom()}, stomp_messages(), stomp_parser()}.

-exported_types([stomp_message/0,
                 stomp_messages/0,
                 stomp_result/0]).

%%
%% API
%%

-spec new() -> stomp_parser().

-spec push(stomp_parser(), binary()) -> stomp_result().

%%
%% Private
%%

-spec parse(binary(), stomp_messages()) -> stomp_result().
