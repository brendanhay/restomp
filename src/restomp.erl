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

-opaque parser() :: #p{}.

-type message()  :: {msg, _}.
-type messages() :: [message()].
-type result()   :: {ok | {error, atom()}, messages(), parser()}.

-exported_types([message/0,
                 messages/0,
                 result/0]).

%%
%% API
%%

-spec new() -> parser().

-spec push(binary(), parser()) -> result().

%%
%% Private
%%

-spec parse(binary(), messages()) -> result().
