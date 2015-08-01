-module(g17_user_info_notify).
-export([url/2, decode/1]).
-include("g17.hrl").


% http://api.zwj.17g.com/user_info_notify.php?data=$data&time=$time&sign=$sign

url(#state{
       address = Address,
       simple_key = Key
    }, {user_info_notify, UserIds}) ->
    UserIdsData = user_ids_data(UserIds),
    DataParam = "data=" ++ UserIdsData,
    EncodeDataParam = "data=" ++ g17_util:urlencode(UserIdsData),
    TimeParam = g17_util:time_param(),
    EncodeKeyParam = g17_util:key_param(Key),
    SignParam = g17_util:sign([EncodeDataParam, TimeParam, EncodeKeyParam]),
    QueryString = g17_util:query_string([DataParam, TimeParam, SignParam]),
    lists:concat([Address, "user_info_notify.php?", QueryString]).


user_ids_data(UserIds) ->
    string:join([integer_to_list(Id) || Id <- UserIds], "_").



decode(Body) ->
    g17_util:simple_ret(Body).


