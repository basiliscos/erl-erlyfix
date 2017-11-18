-module(erlyfix_utils).

-export([checksum/1]).

checksum(Acc, []) -> Acc rem 256;
checksum(Acc, [H | T]) when is_list(H) ->
    checksum(checksum(Acc, H), T);
checksum(Acc, [H | T]) when is_integer(H) ->
    checksum(Acc + H, T).

checksum(IOList) when is_list(IOList) ->
    checksum(0, IOList);
checksum(Data) when is_binary(Data) ->
    lists:sum([Char || <<Char>> <= Data]) rem 256.
