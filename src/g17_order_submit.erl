-module(g17_order_submit).
-export([url/2, decode/1]).
-include("g17.hrl").



% http://s1.zwj.17g.com/order_submit.php?
% order_id=$order_id&order_type=$order_type&user_id=$user_id
% &server_id=$server_id&coin=$coin&extra=$extra
% &money=$money&time=$time&sign=$sign&role_id=$role_id

% order_id    string(32)  是   订单号长度小于40位， 并且支持 字母、数字、下划线
% 订单号,全平台唯一,不重复
% order_type  int     是   订单类型, 1RMB直充订单, 2平台币订单, 3元宝基金订单
% 当订单类型为3元宝基金订单时, 只能给玩家加值绑定元宝
% user_id     bigint  是   玩家在平台的用户编号
% server_id   int     是   所属服务器,一区为1,二区为2,以此类推
% coin    int     是   充值游戏货币数量
% extra   int     是   充值加送的绑定货币数量
% money   float   是   充值金额单位元(精确到小数点后2位,例如: 1.23)
% time    int     是   平台服务器的时间戳(为长整数,单位秒)
% sign    string(32)  是   参数签名, 计算方法见表后”Sign签名算法”说明
% role_id     bigint  是   角色ID, 从验证用户接口返回值中获得

% md5(order_id=$order_id&order_type=$order_type&user_id=$user_id
% &server_id=$server_id&coin=$coin&extra=$extra&money=$money&time=$time&key=$key)


% {"code":1,"msg":"\u5151\u6362\u6210\u529f"}
% code

% 接口返回代码
%     1 成功
%     -1 失败
%     -2 角色不存在
%     -3 IP限制
%     -4 md5校验错误
%     -5 订单号已存在
%     -6 time时间已过期(时间差在前后3分钟内)
%     -7 游戏服务器繁忙
%     -8 参数错误

% msg     提示信息

url(#state{
        address = Address,
        server_id = ServerId,
        money_key = Key
    }, {order_submit, OrderId, OrderType, UserId, Coin, Extra, Money, RoleId}) ->
   OrderIdParam = "order_id=" ++ OrderId,
   EncodeOrderIdParam = "order_id=" ++ g17_util:urlencode(OrderId),
   OrderTypeParam = "order_type=" ++ integer_to_list(OrderType),
   UserIdParam = g17_util:user_id_param(UserId),
   ServerIdParam = g17_util:server_id_param(ServerId),
   CoinParam = "coin=" ++ integer_to_list(Coin),
   ExtraParam = "extra=" ++ integer_to_list(Extra),
   MoneyParam = "money=" ++ float_to_list(Money, [{decimals, 2}]),
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   RoleIdParam = "role_id=" ++ integer_to_list(RoleId),
   SignParam = g17_util:sign_param([EncodeOrderIdParam, OrderTypeParam, UserIdParam, ServerIdParam,
                                    CoinParam, ExtraParam, MoneyParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([OrderIdParam, OrderTypeParam, UserIdParam, ServerIdParam,
                                                        CoinParam, ExtraParam, MoneyParam,
                                                         TimeParam, SignParam, RoleIdParam]),
   lists:concat([Address, "order_submit.php?", QueryString]).


decode(Body) ->
    g17_util:simple_ret(Body).
