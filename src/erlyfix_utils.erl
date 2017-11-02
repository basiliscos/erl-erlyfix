-module(erlyfix_utils).

-export([checksum/1]).

checksum(Acc, []) -> Acc rem 256;
checksum(Acc, [H | T]) when is_list(H) ->
    checksum(checksum(Acc, H), T);
checksum(Acc, [H | T]) when is_integer(H) ->
    checksum(Acc + H, T).

checksum(IOList) -> checksum(0, IOList).
