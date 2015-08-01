-module(g17_guild_ageinfo).
-export([url/2, decode/1]).

-include("g17.hrl").


% 查询平台上指定公会编号内的公会成员的活跃信息。
% 用于游戏内需要判断用户活跃信息的功能（如：游戏内的借将功能）。用户公龄值就是用户的活跃天数

% http://www.17g.com/api/guild/ageInfo?
% game_id=$game_id&guild_id=$guild_id&user_id=$user_id&time=$time&sign=$sign

% game_id     int     是   游戏编号,由联运平台提供,每款游戏编号固定不变
% guild_id    string  是   平台公会编号
% user_id     string  是   平台用户编号
% time    int     是   平台服务器的时间戳(为长整数,单位秒). 此时间点5分钟后链接失效.
% sign    string(32)  是   参数签名, 计算方法见表后”Sign签名算法”说明
% Sign签名算法

%             md5(game_id=$game_id&guild_id=$guild_id&user_id=$user_id&time=$time&key=$key)


url(#state{
        address = Address,
        game_id = GameId,
        simple_key = Key
    }, {_, GuildId, UserId}) ->

   GameIdParam = g17_util:game_id_param(GameId),
   GuildIdParam = g17_util:guild_id_param(GuildId),
   UserIdParam = g17_util:user_id_param(UserId),
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   SignParam = g17_util:sign_param([GameIdParam, GuildIdParam, UserIdParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([GameIdParam, GuildIdParam, UserIdParam, TimeParam, SignParam]),
   lists:concat([Address, "api/guild/ageInfo?", QueryString]).



% {"code":1,
% "msg":"SUCCESS",
% "data":{
%     "guild_id":123,
%     "user_id":123456,
%     "series_age_cnt":0,
%     "last_add_dt":"2015-03-29"
%     }
% }



% 接口返回代码
%     1 公会公龄信息查询成功
%     -1 参数错误
%     -2 查询时间过期
%     -3 签名错误
%     -4 用户不存在
%     -5 公会编号错误
%     -6 公会成员错误

% data    公会公龄信息数据
% guild_id    公会编号
% user_id     用户编号
% series_age_cnt  连续公龄(活跃天)值
% (连续多少天获得公龄,只要有一天没有获得公龄就从0开始重新累计.
% 最长只记录7天的连续公龄,即此值最大为7)
% last_add_dt     最后一次获得公龄(活跃天)的时间
% msg     提示信息

decode(Body) ->
    {ok,{obj, ObjAtrrs}, []} = rfc4627:decode(Body),
    Code = proplists:get_value("code", ObjAtrrs),
    Msg = proplists:get_value("msg", ObjAtrrs),
    Result = #query_user_ageinfo_ret{
                code = Code,
                msg = Msg
            },
    case Code of
        1 ->
            {obj, Attr} = proplists:get_value("data", ObjAtrrs),
            UserAgeInfoRet = #user_ageinfo_ret{
                               guild_id = g17_util:fix_guild_id(g17_util:to_string(proplists:get_value("guild_id", Attr))),
                               user_id = g17_util:to_string(proplists:get_value("user_id", Attr)),
                               series_age_cnt = g17_util:to_integer(proplists:get_value("series_age_cnt", Attr)),
                               last_add_dt = g17_util:to_string(proplists:get_value("last_add_dt", Attr))
                            },
            Result#query_user_ageinfo_ret{
                data = UserAgeInfoRet
            };
        _ ->
            Result
    end.

