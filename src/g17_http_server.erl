-module(g17_http_server).

-export([start/1]).

-define(MAX_CONNS, 3000).

start(Port) ->
    app_misc:start(cowboy),
    Dispatch   = cowboy_router:compile([
                                        {'_', [
                                               {'_', g17_http_handler, []}
                                              ]}
                                       ]),
    cowboy:start_http(http_g17, 10, [{port, Port}, {max_connections, ?MAX_CONNS}],
                      [{env, [{dispatch, Dispatch}]}]).
