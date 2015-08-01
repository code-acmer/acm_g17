-module(g17_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("g17.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(OUT(Format, Args),
        io:format("~p:~p"++ Format, [?MODULE, ?LINE]++ Args)).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Config) ->

    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Config]) ->
     % io:format("~p~n",[Config]),
    {ok, #state{
         address = proplists:get_value(address, Config),
         game_id = proplists:get_value(game_id, Config),
         server_id = proplists:get_value(server_id, Config),
         money_key = proplists:get_value(money_key, Config),
         simple_key = proplists:get_value(simple_key, Config),
         app_key = proplists:get_value(app_key, Config),
         app_secret = proplists:get_value(app_secret, Config)
    }}.

handle_call({query_guild_info, _GuildIds, _UserIds} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_info:url/2, fun g17_guild_info:decode/1),
    {reply, Reply, State};

handle_call({query_guild_ageinfo, _GuildId, _UserId} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_ageinfo:url/2, fun g17_guild_ageinfo:decode/1),
    {reply, Reply, State};

handle_call({query_user_info, _UserId} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_user_info:url/2, fun g17_user_info:decode/1),
    {reply, Reply, State};

handle_call({guild_create, _UserId, _GuildName} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_create:url/2, fun g17_guild_create:decode/1),
    {reply, Reply, State};

handle_call({guild_quit, _UserId, _GuildId} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_quit:url/2, fun g17_guild_quit:decode/1),
    {reply, Reply, State};

handle_call({guild_join, _UserId, _GuildId} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_join:url/2, fun g17_guild_join:decode/1),
    {reply, Reply, State};
handle_call({guild_applyjoin, _UserId, _GroupId, _GroupName, _MemberCnt, _GuildId} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_applyjoin:url/2, fun g17_guild_applyjoin:decode/1),
    {reply, Reply, State};

handle_call({query_role_info, UserId} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_role_info:url/2, fun g17_role_info:decode/1),
    {reply, Reply, State};

handle_call({order_submit, OrderId, OrderType, UserId, Coin, Extra, Money, RoleId} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_order_submit:url/2, fun g17_order_submit:decode/1),
    {reply, Reply, State};

handle_call({guild_apply_reject_notify, RefuseMsgList} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_apply_reject_notify:url/2, fun g17_guild_apply_reject_notify:decode/1),
    {reply, Reply, State};

handle_call({guild_newbie_notify, JoinMsgList} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_guild_newbie_notify:url/2, fun g17_guild_newbie_notify:decode/1),
    {reply, Reply, State};

handle_call({quit_guild_notify, QuitMsgList} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_quit_guild_notify:url/2, fun g17_quit_guild_notify:decode/1),
    {reply, Reply, State};

handle_call({user_info_notify, UserIds} = Data, _From, State) ->
    Reply = request(State, Data, fun g17_user_info_notify:url/2, fun g17_user_info_notify:decode/1),
    {reply, Reply, State};

handle_call({passport_checkin, DeviceId, Token, UserId} = Data, _From, State) ->
    %% ?OUT("passport_checkin : ~p~n", [Data]),
    Reply = request(State, Data, fun g17_passport_checkin:url/2, fun g17_passport_checkin:decode/1),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    %% ?OUT("request ignored : ~p~n ", [_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

request(State, Data, UrlFun, DecodeFun) ->
    RequestUrl = UrlFun(State, Data),
   case httpc:request(RequestUrl) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, DecodeFun(Body)};
        {ok, {{_Version, _Other, ReasonPhrase}, _Headers, _Body}} ->
            {error, ReasonPhrase};
        {error, Reason} -> {error, Reason}
    end.



% request_post(State, Data, UrlFun, DecodeFun) ->
%     {RequestUrl, Body}= UrlFun(State, Data),
%     case httpc:request(post, {RequestUrl, [], "application/json;charset=utf-8", Body}, [], []) of
%         {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
%             {ok, DecodeFun(Body)};
%         {ok, {{_Version, _Other, ReasonPhrase}, _Headers, _Body} = Ret} ->
%             {error, Ret};
%         {error, Reason} -> {error, Reason}
%     end.

