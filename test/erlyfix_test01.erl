-module(erlyfix_test01).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).


protocol_load_test() ->
    P = load(),
    ?assertEqual(#protocol_version{ major = 4, minor = 4, servicepack = 0 },
        erlyfix_protocol:version(P)),

    % simple field
    {ok, F_account_1} = erlyfix_protocol:lookup(P, {field, by_name, "Account" }),
    ?assertEqual("Account", F_account_1#field.name),
    ?assertEqual("1", F_account_1#field.number),
    ?assertEqual(#{}, F_account_1#field.value4key),
    ?assertEqual(#{}, F_account_1#field.value4description),
    {ok, F_account_2} = erlyfix_protocol:lookup(P, {field, by_number, "1" }),
    ?assertEqual(F_account_1, F_account_2),

    % field with values
    {ok, F_AdvSide} = erlyfix_protocol:lookup(P, {field, by_name, "AdvSide" }),
    V_BUY_1 = maps:get("BUY", F_AdvSide#field.value4description),
    ?assertEqual("B", V_BUY_1#value_def.key),
    ?assertEqual("BUY", V_BUY_1#value_def.description),
    V_BUY_2 = maps:get("B", F_AdvSide#field.value4key),
    ?assertEqual(V_BUY_1, V_BUY_2),
    %?debugFmt("value4description == ~p", [F_AdvSide#field.value4description]),
    {ok, _V_SELL} = maps:find("SELL", F_AdvSide#field.value4description),
    {ok, _V_CROSS} = maps:find("CROSS", F_AdvSide#field.value4description),
    {ok, _V_TRADE} = maps:find("TRADE", F_AdvSide#field.value4description),

    % simple component
    {ok, C_CommissionData} = erlyfix_protocol:lookup(P, {component, "CommissionData" }),
    ?assertEqual("CommissionData", C_CommissionData#component.name),
    ?assertEqual("CommissionData", C_CommissionData#component.name),
    {ok, _F_Commission} = maps:find("Commission", C_CommissionData#component.composite4name),
    {ok, _F_CommType} = maps:find("CommType", C_CommissionData#component.composite4name),
    {ok, _F_CommCurrency} = maps:find("CommCurrency", C_CommissionData#component.composite4name),
    {ok, _F_FundRenewWaiv} = maps:find("FundRenewWaiv", C_CommissionData#component.composite4name),

    % component with group
    {ok, C_Stipulations} = erlyfix_protocol:lookup(P, {component, "Stipulations" }),
    {ok, G_NoStipulations} = maps:find("NoStipulations", C_Stipulations#component.composite4name),
    ?assertEqual(error, maps:find("NoStipulations", C_Stipulations#component.mandatoryComposites)),
    ?assertEqual("NoStipulations", G_NoStipulations#group.name),
    {ok, _F_StipulationType} = maps:find("StipulationType", G_NoStipulations#group.composite4name),
    {ok, _F_StipulationValue} = maps:find("StipulationValue", G_NoStipulations#group.composite4name),

    % component with group with component
    {ok, C_NestedParties} = erlyfix_protocol:lookup(P, {component, "NestedParties" }),
    {ok, G_NoNestedPartyIDs} = maps:find("NoNestedPartyIDs", C_NestedParties#component.composite4name),
    {ok, C_NstdPtysSubGrp} = maps:find("NstdPtysSubGrp", G_NoNestedPartyIDs#group.composite4name),
    {ok, G_NoNestedPartySubIDs} = maps:find("NoNestedPartySubIDs", C_NstdPtysSubGrp#component.composite4name),
    {ok, _F_NestedPartySubID} = maps:find("NestedPartySubID", G_NoNestedPartySubIDs#group.composite4name),
    {ok, _F_NestedPartySubIDType} = maps:find("NestedPartySubIDType", G_NoNestedPartySubIDs#group.composite4name),

    % component with mandatory group
    {ok, C_CpctyConfGrp} = erlyfix_protocol:lookup(P, {component, "CpctyConfGrp" }),
    {ok, G_NoCapacities} = maps:find("NoCapacities", C_CpctyConfGrp#component.composite4name),
    ?assertEqual(G_NoCapacities, maps:get("NoCapacities", C_CpctyConfGrp#component.mandatoryComposites)),

    % header
    Header = P#protocol.header,
    {ok, F_BeginString} = maps:find("BeginString", Header#header.composite4name),
    ?assertEqual(F_BeginString, maps:get("BeginString", Header#header.mandatoryComposites)),
    {ok, _C_Hop} = maps:find("Hop", Header#header.composite4name),
    ?assertEqual(error, maps:find("Hop", Header#header.mandatoryComposites)),

    % trailer
    Trailer = P#protocol.trailer,
    {ok, _F_SignatureLength} = maps:find("SignatureLength", Trailer#trailer.composite4name),
    ?assertEqual(error, maps:find("SignatureLength", Trailer#trailer.mandatoryComposites)),
    {ok, F_CheckSum} = maps:find("CheckSum", Trailer#trailer.composite4name),
    ?assertEqual(F_CheckSum, maps:get("CheckSum", Trailer#trailer.mandatoryComposites)).
