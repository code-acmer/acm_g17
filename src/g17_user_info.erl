-module(g17_user_info).
-export([url/2, decode/1]).
-include("g17.hrl").

% 接口描述

% 查询平台上用户的相关信息。
% 接口提供方

% 联运平台提供接口, 游戏厂商调用
% 接口地址示例

%                         http://www.17g.com/api/user/info?game_id=$game_id&user_id=$user_id&time=$time&sign=$sign


% 接口参数说明
% 参数  类型  必传  说明
% http://www.17g.com/api/user/info            用户信息查询接口的地址
% game_id     bigint  是   游戏编号,由联运平台提供,每款游戏编号固定不变
% user_id     bigint  是   平台用户编号
% time    int     是   时间戳单位为秒
% sign    string(32)  是   参数签名, 计算方法见表后”Sign签名算法”说明
% Sign签名算法

%             md5(game_id=$game_id&user_id=$user_id&time=$time&key=$key)




url(#state{
        address = Address,
        game_id = GameId,
        simple_key = Key
    }, {_, UserId}) ->

   GameIdParam = g17_util:game_id_param(GameId),
   UserIdParam = g17_util:user_id_param(UserId),
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   SignParam = g17_util:sign_param([GameIdParam, UserIdParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([GameIdParam, UserIdParam, TimeParam, SignParam]),
   lists:concat([Address, "api/user/info?", QueryString]).






% {"code":1,"msg":"SUCCESS","data":{"user_id":12345,"guild_id":1,"mb_title_id":1,"mb_number_id":1}}


% 属性  说明
% code

% 接口返回代码
%     1 用户存在
%     -1 用户不存在
%     -2 参数错误
%     -3 签名错误
%     -4 查询时间过期

% data    角色信息数据, 格式为数组, 当有多个角色信息时可一同返回
% user_id     用户编号
% guild_id    所属公会编号
% mb_title_id     公会内职位编号（如果没有加入平台上公会或没有设置此值时,值为 0）
% mb_number_id    公会内第几号人物编号（如果没有加入平台上公会或没有设置此值时,值为 0）
% msg     提示信息

decode(Body) ->
    {ok,{obj, ObjAtrrs}, []} = rfc4627:decode(Body),
    Code = proplists:get_value("code", ObjAtrrs),
    Msg = proplists:get_value("msg", ObjAtrrs),
    Result = #query_user_info_ret{
                code = Code,
                msg = Msg
            },
    case Code of
        1 ->
            UserInfoObj = proplists:get_value("data", ObjAtrrs),
            add_user_info(Result, UserInfoObj);
        _ ->
            Result
    end.


add_user_info(Result, Obj) ->
    {obj, Attr} = Obj,
    UserInfoRet = #user_info_ret{
                     user_id = g17_util:to_string(proplists:get_value("user_id", Attr)),
                     guild_id = g17_util:fix_guild_id(g17_util:to_string(proplists:get_value("guild_id", Attr))),
                     mb_title_id = g17_util:to_integer(proplists:get_value("mb_title_id", Attr)),
                     mb_number_id = g17_util:to_integer(proplists:get_value("mb_number_id", Attr))
                  },
    Result#query_user_info_ret{
         data = UserInfoRet
    }.



