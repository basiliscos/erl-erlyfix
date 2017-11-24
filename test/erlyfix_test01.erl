-module(erlyfix_test01).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix.hrl").

load_test() ->
    {error, Err} = erlyfix_protocol:load("..."),
    ?assertEqual(enoent, Err).

load() ->
    {ok, P} = erlyfix_protocol:load("test/FIX44.xml"),
    P.


protocol_load_test() ->
    P = load(),
    ?assertEqual(#protocol_version{ major = 4, minor = 4, servicepack = 0 },
        P#protocol.protocol_version),

    % simple field
    {ok, F_account_1} = erlyfix_utils:lookup(P, {field, by_name, 'Account'}),
    ?assertEqual('Account', F_account_1#field.name),
    ?assertEqual(1, F_account_1#field.number),
    ?assertEqual(#{}, F_account_1#field.value4key),
    ?assertEqual(#{}, F_account_1#field.value4description),
    {ok, F_account_2} = erlyfix_utils:lookup(P, {field, by_number, 1 }),
    not_found = erlyfix_utils:lookup(P, {field, by_number, 9999999 }),
    ?assertEqual(F_account_1, F_account_2),

    % field with values
    {ok, F_AdvSide} = erlyfix_utils:lookup(P, {field, by_name, 'AdvSide' }),
    V_BUY_1 = maps:get(<<"BUY">>, F_AdvSide#field.value4description),
    ?assertEqual('B', V_BUY_1#value_def.key),
    ?assertEqual(<<"BUY">>, V_BUY_1#value_def.description),
    V_BUY_2 = maps:get('B', F_AdvSide#field.value4key),
    ?assertEqual(V_BUY_1, V_BUY_2),
    %?debugFmt("value4description == ~p", [F_AdvSide#field.value4description]),
    {ok, _V_SELL} = maps:find(<<"SELL">>, F_AdvSide#field.value4description),
    {ok, _V_CROSS} = maps:find(<<"CROSS">>, F_AdvSide#field.value4description),
    {ok, _V_TRADE} = maps:find(<<"CROSS">>, F_AdvSide#field.value4description),

    % simple component
    {ok, C_CommissionData} = erlyfix_utils:lookup(P, {component, 'CommissionData' }),
    ?assertEqual('CommissionData', C_CommissionData#component.name),
    {ok, _F_Commission} = maps:find('Commission', C_CommissionData#component.composite4name),
    {ok, _F_CommType} = maps:find('CommType', C_CommissionData#component.composite4name),
    {ok, _F_CommCurrency} = maps:find('CommCurrency', C_CommissionData#component.composite4name),
    {ok, F_FundRenewWaiv} = maps:find('FundRenewWaiv', C_CommissionData#component.composite4name),
    {ok, F_FundRenewWaiv} = maps:find(F_FundRenewWaiv, C_CommissionData#component.composite4field),

    % component with group
    {ok, C_Stipulations} = erlyfix_utils:lookup(P, {component, 'Stipulations' }),
    {ok, G_NoStipulations_Id} = maps:find('NoStipulations', C_Stipulations#component.composite4name),
    {ok, _F_NoStipulations} = erlyfix_utils:lookup(P, {field, by_name, 'NoStipulations'}),
    ?assertEqual(error, maps:find('NoStipulations', C_Stipulations#component.mandatoryComposites)),
    G_NoStipulations = array:get(G_NoStipulations_Id, P#protocol.container),
    ?assertEqual('NoStipulations', G_NoStipulations#group.name),
    {ok, F_StipulationType} = maps:find('StipulationType', G_NoStipulations#group.composite4name),
    {ok, F_StipulationValue} = maps:find('StipulationValue', G_NoStipulations#group.composite4name),
    {ok, G_NoStipulations_Id} = maps:find(F_StipulationType, C_Stipulations#component.composite4field),
    {ok, G_NoStipulations_Id} = maps:find(F_StipulationValue, C_Stipulations#component.composite4field),

    % component with group with component
    {ok, C_NestedParties} = erlyfix_utils:lookup(P, {component, 'NestedParties' }),
    {ok, G_NoNestedPartyIDs_Id} = maps:find('NoNestedPartyIDs', C_NestedParties#component.composite4name),
    G_NoNestedPartyIDs = array:get(G_NoNestedPartyIDs_Id, P#protocol.container),
    {ok, C_NstdPtysSubGrp_id} = maps:find('NstdPtysSubGrp', G_NoNestedPartyIDs#group.composite4name),
    C_NstdPtysSubGrp = array:get(C_NstdPtysSubGrp_id, P#protocol.container),
    {ok, G_NoNestedPartySubIDs_Id} = maps:find('NoNestedPartySubIDs', C_NstdPtysSubGrp#component.composite4name),
    G_NoNestedPartySubIDs = array:get(G_NoNestedPartySubIDs_Id, P#protocol.container),
    {ok, F_NestedPartySubID} = maps:find('NestedPartySubID', G_NoNestedPartySubIDs#group.composite4name),
    {ok, F_NestedPartySubIDType} = maps:find('NestedPartySubIDType', G_NoNestedPartySubIDs#group.composite4name),
    {ok, G_NoNestedPartyIDs_Id} = maps:find(F_NestedPartySubID, C_NestedParties#component.composite4field),
    {ok, G_NoNestedPartyIDs_Id} = maps:find(F_NestedPartySubIDType, C_NestedParties#component.composite4field),

    % component with mandatory group
    {ok, C_CpctyConfGrp} = erlyfix_utils:lookup(P, {component, 'CpctyConfGrp' }),
    {ok, G_NoCapacities} = maps:find('NoCapacities', C_CpctyConfGrp#component.composite4name),
    ?assertEqual(G_NoCapacities, maps:get('NoCapacities', C_CpctyConfGrp#component.mandatoryComposites)),

    % header
    Header = P#protocol.header,
    {ok, F_BeginString} = maps:find('BeginString', Header#header.composite4name),
    ?assertEqual(F_BeginString, maps:get('BeginString', Header#header.mandatoryComposites)),
    {ok, _C_Hop} = maps:find('Hop', Header#header.composite4name),
    ?assertEqual(error, maps:find('Hop', Header#header.mandatoryComposites)),

    % trailer
    Trailer = P#protocol.trailer,
    {ok, _F_SignatureLength} = maps:find('SignatureLength', Trailer#trailer.composite4name),
    ?assertEqual(error, maps:find('SignatureLength', Trailer#trailer.mandatoryComposites)),
    {ok, F_CheckSum} = maps:find('CheckSum', Trailer#trailer.composite4name),
    ?assertEqual(F_CheckSum, maps:get('CheckSum', Trailer#trailer.mandatoryComposites)),

    % message
    {ok, M_Logon} = erlyfix_utils:lookup(P, {message, by_name, 'Logon' }),
    ?assertEqual('Logon', M_Logon#message.name),
    ?assertEqual('A', M_Logon#message.type),
    ?assertEqual('admin', M_Logon#message.category),
    {ok, F_EncryptMethod} = maps:find('EncryptMethod', M_Logon#message.composite4name),
    {ok, F_EncryptMethod} = maps:find('EncryptMethod', M_Logon#message.mandatoryComposites),
    {ok, _G_NoMsgTypes} = maps:find('NoMsgTypes', M_Logon#message.composite4name),
    ?assertEqual(error, maps:find('NoMsgTypes', M_Logon#message.mandatoryComposites)),
    {ok, M_Logon} = erlyfix_utils:lookup(P, {message, by_type, 'A' }).
