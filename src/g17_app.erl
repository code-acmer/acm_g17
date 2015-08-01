-module(g17_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->    
    application:load(g17),    
    {ok, Config} = application:get_env(g17, config),
    HttpdPort = proplists:get_value(httpd_port, Config),
    g17_http_server:start(HttpdPort),
    g17_sup:start_link(Config).

stop(_State) ->
    ok.
