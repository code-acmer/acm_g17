-module(g17_guild_info).
-export([url/2, decode/1]).

-include("g17.hrl").



url(#state{
        address = Address,
        game_id = GameId,
        simple_key = Key
    }, {_, GuildIds, UserIds}) ->

   GameIdParam = g17_util:game_id_param(GameId),
   StrGuildIds = string:join(GuildIds, "_"),
   {GuildIdsParam, EncodeGuildIdsParam} =  {"guild_ids=" ++ StrGuildIds, "guild_ids=" ++ g17_util:urlencode(StrGuildIds)},
   StrUserIds = string:join(UserIds, "_"),
   {UserIdsParam,  EncodeUserIdsParam} =  {"user_ids=" ++ StrUserIds, "user_ids=" ++ g17_util:urlencode(StrUserIds)},
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   SignParam = g17_util:sign_param([GameIdParam, EncodeGuildIdsParam, EncodeUserIdsParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([GameIdParam, GuildIdsParam, UserIdsParam, TimeParam, SignParam]),
   lists:concat([Address, "api/guild/info?", QueryString]).




% {ok,{obj,[{"code",1},
%       {"data",
%        {obj,[{"12345",
%               {obj,[{"guild_id",12345},
%                     {"guild_name",<<"u516cu4f1au540du79f01">>},
%                     {"guild_logo",<<"http://static.17g.com/images/guild/l"...>>},
%                     {"owner_user_id",1111}]}},
%              {"12345",
%               {obj,[{"guild_id",12345},
%                     {"guild_name",<<"u516cu4f1au540du79f01">>},
%                     {"guild_logo",<<"http://static.17g.com/images/gui"...>>},
%                     {"owner_user_id",1111}]}}]}},
%       {"msg",<<>>}]},
% []}

decode(Body) ->
    % {ok,{obj,[{"code",Code},
    %       {"data",
    %        {obj, GuildObjs}
    %       },
    %       {"msg", Msg}]},
    % []} = rfc4627:decode(Body),

    {ok, {obj, ObjAttr}, []} = rfc4627:decode(Body),
    Code = proplists:get_value("code", ObjAttr),
    Msg = proplists:get_value("msg", ObjAttr),
    Result = #query_guild_info_ret{
                code = Code,
                msg = Msg
            },
    case Code of
      ?G17_SIMPLE_RET_SUCCESS ->
          {obj, GuildObjs} = proplists:get_value("data", ObjAttr),
          GuildListRet =
              lists:foldl(fun({QueryId,
                                  {obj, Attr}
                              }, Acc) ->
                              [#guild_info_ret{
                                  query_id = QueryId,
                                  guild_id = g17_util:fix_guild_id(g17_util:to_string(proplists:get_value("guild_id", Attr))),
                                  guild_name = proplists:get_value("guild_name", Attr),
                                  guild_logo = proplists:get_value("guild_logo", Attr),
                                  owner_user_id = g17_util:to_string(proplists:get_value("owner_user_id", Attr))
                              } | Acc]
                          end, [], GuildObjs),
          Result#query_guild_info_ret{
             data = GuildListRet
          };
      _ -> Result
    end.




integer_list_to_string(Ids) ->
   string:join([integer_to_list(Id) || Id <-Ids], "_").
