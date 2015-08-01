-module(g17_role_info).
-export([url/2, decode/1]).
-include("g17.hrl").


% user_id=$user_id&server_id=$server_id&time=$time&sign=$sign
% md5(user_id=$user_id&server_id=$server_id&time=$time&key=$key)
url(#state{
        address = Address,
        server_id = ServerId,
        simple_key = Key
    }, {_, UserId}) ->

   UserIdParam = g17_util:user_id_param(UserId),
   ServerIdParam = g17_util:server_id_param(ServerId),
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   SignParam = g17_util:sign_param([UserIdParam, ServerIdParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([UserIdParam, ServerIdParam, TimeParam, SignParam]),
   lists:concat([Address, "role_info.php?", QueryString]).



% {"code":1,"data":[{"rold_id":"12345","role_name":"\u5f20\u4e09","role_level":"1"}],"msg":"\u67e5\u8be2\u6210\u529f"}
decode(Body) ->
   {ok, {obj, ObjAttr}, []} = rfc4627:decode(Body),
   Code = proplists:get_value("code", ObjAttr),
   Msg = proplists:get_value("msg", ObjAttr),
   Result = #query_role_info_ret{
        code = Code,
        msg = Msg
   },
   case Code of
     1 ->
        [{obj, RoleInfoAttr}] = proplists:get_value("data", ObjAttr),
        RoleInfoRet = #role_info_ret{
            role_id = list_to_integer(proplists:get_value("role_id", RoleInfoAttr)),
            role_name = proplists:get_value("role_name", RoleInfoAttr),
            role_level = list_to_integer(proplists:get_value("role_level", RoleInfoAttr))
        },
        Result#query_role_info_ret{
            data = RoleInfoRet
        };
    _ -> Result
    end.

