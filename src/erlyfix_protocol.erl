-module(erlyfix_protocol).
-include("include/erlyfix.hrl").
-include("debug.hrl").

-export([load/1, load/2, version/1, lookup/2, serialize/3]).

-record(context, {
    name        :: string(),
    protocol    :: protocol(),
    c4n         :: map()
}).

%% XML-parsing

find_attr(Name, Attributes, Fn) ->
    F = fun(I) ->
        case I of
             {attribute,Name, _, _, _} -> true;
             _ -> false
         end
    end,
    case lists:filter(F, Attributes) of
        [{attribute,Name, _, _, Value}] -> {ok, Fn(Value)};
        _ -> {not_found}
    end.

find_required(Attributes) ->
    case find_attr("required", Attributes, fun(X) -> X end) of
        {ok, Value} ->
            case Value of
                "Y" -> {ok, true};
                "N" -> {ok, false}
            end;
        {not_found} -> {not_found}
    end.

callback(Event, Acc) ->
    %io:format("~p~n", [Event]),
    case Event of
        {startElement, [], "fix", [], Attributes} ->
            %io:format("~p~n", [Acc]),
            {ok, Minor} = find_attr("minor", Attributes, fun erlang:list_to_integer/1),
            {ok, Major} = find_attr("major", Attributes, fun erlang:list_to_integer/1),
            {ok, Servicepack} = find_attr("servicepack", Attributes, fun erlang:list_to_integer/1),
            {ok, 'FIX'} = find_attr("type", Attributes, fun erlang:list_to_atom/1),
            Version = #protocol_version {
                major = Major,
                minor = Minor,
                servicepack = Servicepack},
            Acc#{ version => Version };
        {startElement,[],"header",[],[]} -> [ [] | Acc ];
        {endElement,[],"header",[]} -> [H | T] = Acc, T#{ header => H };
        {startElement,[],"trailer",[],[]} -> [ [] | Acc ];
        {endElement,[],"trailer",[]} -> [H | T] = Acc, T#{ trailer => H };
        {startElement,[],"messages",[],[]} -> [ [] | Acc ];
        {endElement,[],"messages",[]} -> [H | T] = Acc, T#{ messages => H };
        {startElement,[],"components",[],[]} -> [ [] | Acc ];
        {endElement,[],"components",[]} -> [H | T] = Acc, T#{ components => H };
        {startElement,[],"fields",[],[]} -> [ [] | Acc ];
        {endElement,[],"fields",[]} -> [H | T] = Acc, T#{ fields => H };
        {startElement, [], "field", [], Attributes} ->
            [H | T] = Acc,
            {ok, Name} = find_attr("name", Attributes, fun erlang:list_to_atom/1),
            case find_required(Attributes) of
                {ok, Required} ->                   % reference
                    FieldRef = #field_ref{ name = Name, required = Required },
                    [ {field_ref, FieldRef}, H | T ];
                {not_found} ->                      % definition
                    {ok, Number} = find_attr("number", Attributes, fun erlang:list_to_integer/1),
                    {ok, Type} = find_attr("type", Attributes, fun erlang:list_to_atom/1),
                    [ [], {field_def, Name, Number, Type, H} | T ]
                    %Acc
            end;
        {endElement,[],"field",[]} ->
            [F, H | T] = Acc,
            case is_list(F) of
                false ->                            % reference
                    {field_ref, FieldRef} = F,
                    [ [FieldRef | H] | T ];
                true ->                             % definition
                    {field_def, Name, Number, Type, H2} = H,
                    FieldDef = #field_def {
                        name = Name,
                        number = Number,
                        type = Type,
                        values = F},
                    [ [FieldDef | H2] | T ]
            end;
        {startElement, [], "message", [], Attributes} ->
            %io:format("message :: ~p~n", [Attributes]),
            {ok, Name} = find_attr("name", Attributes, fun erlang:list_to_atom/1),
            {ok, Type} = find_attr("msgtype", Attributes, fun erlang:list_to_atom/1),
            {ok, Category} = find_attr("msgcat", Attributes, fun erlang:list_to_atom/1),
            [ [], Name, Type, Category | Acc ];
        {endElement,[],"message",[]} ->
            [Composites, Name, Type, Category | Acc1] = Acc,
            MessageRef = #message_ref {
                name = Name,
                type = Type,
                category = Category,
                composites = Composites},
            [H | T1] = Acc1,
            [ [MessageRef | H] | T1 ];
        {startElement, [], "component", [], Attributes} ->
            [H | T] = Acc,
            {ok, Name} = find_attr("name", Attributes, fun erlang:list_to_atom/1),
            case find_required(Attributes) of
                {ok, Required} ->                   % reference
                    ComponentRef = #component_ref{ name = Name, required = Required },
                    [ {component_ref, ComponentRef}, H | T ];
                {not_found} ->                      % definition
                    [ [], {component_def, Name, H} | T ]
            end;
        {endElement,[],"component",[]} ->
            [F, H | T] = Acc,
            case is_list(F) of
                false ->                            % reference
                    {component_ref, ComponentRef} = F,
                    [ [ComponentRef | H] | T ];
                true ->                             % definition
                    {component_def, Name, H2} = H,
                    ComponentDef = #component_def {
                        name = Name,
                        composites = F},
                    [ [ComponentDef | H2] | T ]
            end;
        {startElement, [], "group", [], Attributes} ->
            {ok, Name} = find_attr("name", Attributes, fun erlang:list_to_atom/1),
            {ok, Reqired} = find_required(Attributes),
            [ [], {group_def, Name, Reqired} | Acc ];
        {endElement,[],"group",[]} ->
            [Composites, {group_def, Name, Required} | Acc1] = Acc,
            GroupDef = #group_def { name = Name, required = Required, composites = Composites},
            [H | T1] = Acc1,
            [ [GroupDef | H] | T1 ];
        {startElement, [], "value", [], Attributes} ->
            {ok, Key} = find_attr("enum", Attributes, fun erlang:list_to_atom/1),
            {ok, Description} = find_attr("description", Attributes, fun erlang:list_to_binary/1),
            ValueDef = #value_def{ key = Key, description = Description},
            [H | T] = Acc,
            [ [ValueDef | H] | T ];
        _ -> Acc
    end.

% merge maps
merge_map_element(Acc, _Extension, []) -> {ok, Acc};
merge_map_element(Acc0, Extension, [H | T]) ->
    E_m = maps:get(H, Acc0),
    Acc1 = case maps:find(H, Extension) of
        {ok, E_e} -> Acc0#{ H => E_m ++ E_e };
        error -> Acc0
    end,
    merge_map_element(Acc1, Extension, T).

merge_maps(Main, Extension) ->
    % check versions match
    V_m = maps:get(version, Main),
    V_e = maps:get(version, Extension),
    V_m = V_e,
    Plan = [header, trailer, messages, components, fields],
    merge_map_element(Main, Extension, Plan).

%% Constuction

map_values(Acc, []) -> Acc;
map_values({Value4Key, Value4Description}, [Value | Rest]) ->
    K = Value#value_def.key,
    D = Value#value_def.description,
    Acc = {
        Value4Key#{K => Value},
        Value4Description#{D => Value}
    },
    map_values(Acc, Rest).


construct_fields(Acc, []) -> Acc;
construct_fields({Container, F4Name, F4Number}, [FieldDefinition | Rest]) ->
    ValuesList = FieldDefinition#field_def.values,
    {Value4Key, Value4Description} = map_values({ #{}, #{} }, ValuesList),
    Name = FieldDefinition#field_def.name,
    Number = FieldDefinition#field_def.number,
    Id = array:size(Container),
    Field = #field {
        id     = Id,
        name   = Name,
        number = Number,
        type   = FieldDefinition#field_def.type,
        value4key = Value4Key,
        value4description = Value4Description,
        composite4field = #{}},
    Acc = {
        array:set(Id, Field, Container),
        F4Name#{ Name => Id },
        F4Number#{ Number => Id}
    },
    construct_fields(Acc, Rest).

construct_composite_mapping({Co4Name, MC}, []) -> {Co4Name, MC};
construct_composite_mapping({Co4Name, MC}, [{Ref, Def} | T ]) ->
    {Name, Required} = case element(1, Ref) of
        field_ref -> { Ref#field_ref.name, Ref#field_ref.required };
        component_ref -> { Ref#component_ref.name, Ref#component_ref.required };
        group_def -> { Ref#group_def.name, Ref#group_def.required }
    end,
    Co4NameNew = Co4Name#{ Name => Def },
    MCNew = case Required of
        true -> MC#{ Name => Def};
        false -> MC
    end,
    construct_composite_mapping({Co4NameNew, MCNew}, T).

zip(Refs, ReverseDefNames) ->
    F = fun({_Name, Composite}) -> Composite end,
    Defs = lists:reverse( lists:map(F, ReverseDefNames)),
    RefsDefs = lists:zip(Refs, Defs),
    {Co4Name, MC} = construct_composite_mapping({ #{}, #{} }, RefsDefs),
    {Co4Name, MC}.


uplift_non_field(Composite, C4F, UpperC4F, _Container) ->
    L0 = maps:keys(C4F),
    L1 = lists:map(fun(F) -> {F, Composite} end, L0),
    CC4Field = maps:from_list(L1),
    maps:merge(UpperC4F, CC4Field).

uplift_list([], _F4Name, C4Field, _Container) -> C4Field;
uplift_list([H | T], F4Name, C4Field, Container) ->
    % ?DEBUG(H),
    % ?DEBUG(C4Field),
    Composite = array:get(H, Container),
    % ?DEBUG(element(1, Composite)),
    case element(1, Composite) of
        field ->
            uplift_list(T, F4Name, C4Field#{H => H}, Container);
        group ->
            C4F1 = uplift_non_field(H, Composite#group.composite4field, C4Field, Container),
            % "main" field, pointing to group count and group type
            {ok, FieldID} = maps:find(Composite#group.name, F4Name),
            C4F2 = C4F1#{FieldID => H},
            uplift_list(T, F4Name, C4F2, Container);
        component ->
            C4F1 = uplift_non_field(H, Composite#component.composite4field,  C4Field, Container),
            uplift_list(T, F4Name, C4F1, Container)
    end.
uplift(F4Name, C4Name, Container) ->
    % ?DEBUG(C4Name),
    uplift_list(maps:values(C4Name), F4Name, #{}, Container).

get_composites(Acc, _C4Name, _F4Name, []) -> {ok, Acc};
get_composites({Container0, L}, C4Name, F4Name, [H | T]) ->
    case element(1, H) of
        component_ref ->
            Name = H#component_ref.name,
            case maps:find(Name, C4Name) of
                {ok, Composite} ->
                    Acc = {Container0, [{Name, Composite} | L] },
                    get_composites(Acc, C4Name, F4Name, T);
                error -> not_found
            end;
        field_ref ->
            Name = H#field_ref.name,
            case maps:find(Name, F4Name) of
                {ok, Composite} ->
                    Acc = {Container0, [{Name, Composite} | L] },
                    get_composites(Acc, C4Name, F4Name, T);
                error -> not_found
            end;
        group_def ->
            Name = H#group_def.name,
            CompositeRefs = H#group_def.composites,
            case get_composites({Container0, []}, C4Name, F4Name, CompositeRefs) of
                {ok, {Container1, SubCompositesNames}} ->
                    {Co4Name, MC} = zip(CompositeRefs, SubCompositesNames),
                    Id = array:size(Container1),
                    Composite = #group{
                        id = Id,
                        name = Name,
                        composite4name = Co4Name,
                        mandatoryComposites = MC,
                        composite4field = uplift(F4Name, Co4Name, Container1)
                    },
                    Acc = {
                        array:set(Id, Composite, Container1),
                        [{Name, Id} | L]
                    },
                    get_composites(Acc, C4Name, F4Name, T);
                not_found -> not_found
            end
    end.

construct_components({Container0, C4Name} = Acc0, F4Name, Queue) ->
    case queue:is_empty(Queue) of
        true -> Acc0;
        false ->
            { {value, ComponentRef}, QLeft} = queue:out(Queue),
            %io:format("trying to construct composite ~p~n", [ComponentRef]),
            #component_def{ name = Name, composites = SubCompositeRefs } = ComponentRef,
            case get_composites({Container0, []}, C4Name, F4Name, SubCompositeRefs) of
                {ok, {Container1, SubCompositesNames}} ->
                    % io:format("found subcomposites ~p :: ~p~n", [ComponentRef, SubCompositesNames]),
                    {Co4Name, MC} = zip(SubCompositeRefs, SubCompositesNames),
                    Id = array:size(Container1),
                    % ?DEBUG(Co4Name),
                    Composite = #component{
                        id  = Id,
                        name = Name,
                        composite4name = Co4Name,
                        mandatoryComposites = MC,
                        composite4field = uplift(F4Name, Co4Name, Container1)
                    },
                    Acc1 = {
                        array:set(Id, Composite, Container1),
                        C4Name#{ Name => Id }
                    },
                    construct_components(Acc1, F4Name, QLeft);
                not_found ->
                    Q2 = queue:in(ComponentRef, QLeft),
                    construct_components(Acc0, F4Name, Q2)
            end
    end.



construct_messages(Acc, _C4Name, _F4Name, []) -> Acc;
construct_messages({M4Name, M4Type, Container0}, C4Name, F4Name, [H | T]) ->
    #message_ref { name = Name, type = Type, category = Category, composites = CompositeRefs} = H,
    {ok, {Container1, SubCompositesNames}} = get_composites({Container0, []}, C4Name, F4Name, CompositeRefs),
    {Co4Name, MC} = zip(CompositeRefs, SubCompositesNames),
    Message = #message {
        name = Name,
        type = Type,
        category = Category,
        composite4name = Co4Name,
        mandatoryComposites = MC,
        composite4field = uplift(F4Name, Co4Name, Container1)
    },
    M4NameNew = M4Name#{ Name => Message},
    M4TypeNew = M4Type#{ Type => Message},
    construct_messages({M4NameNew, M4TypeNew, Container1}, C4Name, F4Name, T).

construct(Map) ->
    % fields
    FieldDefinitions = maps:get(fields, Map),
    ComponentDefinitions = maps:get(components, Map),
    {Container0, F4Name, F4Number} = construct_fields({array:new(), #{}, #{}}, FieldDefinitions),

    % components
    ComponentsQueue = queue:from_list(ComponentDefinitions),
    {Container1, C4Name} = construct_components({Container0, #{}}, F4Name, ComponentsQueue),

    % header
    HeaderRefs = maps:get(header, Map),
    {ok, {Container1, HeaderCompositeNames}} = get_composites({Container1, []}, C4Name, F4Name, HeaderRefs),
    {H_C4Name, H_MC} = zip(HeaderRefs, HeaderCompositeNames),
    Header = #header{
        composite4name = H_C4Name,
        mandatoryComposites = H_MC,
        composite4field = uplift(F4Name, H_C4Name, Container1)
    },

    % trailer
    TrailerRefs = maps:get(trailer, Map),
    {ok, {Container1, TrailerCompositeNames}} = get_composites({Container1, []}, C4Name, F4Name, TrailerRefs),
    {T_C4Name, T_MC} = zip(TrailerRefs, TrailerCompositeNames),
    Trailer = #trailer{
        composite4name = T_C4Name,
        mandatoryComposites = T_MC,
        composite4field = uplift(F4Name, T_C4Name, Container1)
    },

    % messages
    MessagesRefs = maps:get(messages, Map),
    {M4Name, M4Type, Container2} = construct_messages({#{}, #{}, Container1}, C4Name, F4Name, MessagesRefs),

    #protocol{
        protocol_version = maps:get(version, Map),
        header           = Header,
        trailer          = Trailer,
        field4name       = F4Name,
        field4number     = F4Number,
        component4name   = C4Name,
        message4name     = M4Name,
        message4type     = M4Type,
        container        = Container2
    }.

%% Interface method

construct_protocol(Map) ->
    Protocol0 = construct(Map),
    Helpers = erlyfix_parser:compile(Protocol0),
    Protocol0#protocol{ parser_helpers = Helpers}.


load(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            {ok, Map, _} = erlsom:parse_sax(Bin, #{}, fun callback/2),
            Protocol = construct_protocol(Map),
            {ok, Protocol};
        {error, Reason}  -> {error, Reason}
    end.

load(MainPath, ExtensionPath) ->
    case file:read_file(MainPath) of
        {ok, MainBin} ->
            case file:read_file(ExtensionPath) of
                {ok, ExtensionBin} ->
                    {ok, MainMap, _} = erlsom:parse_sax(MainBin, #{}, fun callback/2),
                    {ok, ExtensionMap, _} = erlsom:parse_sax(ExtensionBin, #{}, fun callback/2),
                    {ok, Map} = merge_maps(MainMap, ExtensionMap),
                    Protocol = construct_protocol(Map),
                    {ok, Protocol};
                {error, Reason}  -> {error, Reason}
            end;
        {error, Reason}  -> {error, Reason}
    end.


version(Protocol)-> Protocol#protocol.protocol_version.

% lookup
lookup(Protocol, Criterium) ->
    {K, L, JustId} = case Criterium of
        {field, by_name, X} -> {X, Protocol#protocol.field4name, true};
        {field, by_number, X} -> {X, Protocol#protocol.field4number, true};
        {component, X} -> {X, Protocol#protocol.component4name, true};
        {message, by_name, X} -> {X, Protocol#protocol.message4name, false};
        {message, by_type, X} -> {X, Protocol#protocol.message4type, false}
    end,
    case maps:find(K, L) of
        {ok, CompositeId} ->
            %?DEBUG(CompositeId),
            case JustId of
                true ->
                    Composite = array:get(CompositeId, Protocol#protocol.container),
                    {ok, Composite};
                false ->
                    {ok, CompositeId}
            end;
        error -> not_found
    end.

% serialize group item
serialize_group_item(AccContainer, CTX, []) ->
    serialize_composite(AccContainer, CTX, []);
serialize_group_item(AccContainer, CTX, [H | T]) ->
    case serialize_composite(AccContainer, CTX, H) of
        {ok, NewAccContainer} -> serialize_group_item(NewAccContainer, CTX, T);
        {error, Reason} -> {error, Reason}
    end.

% serialize_field
serialize_field({Size0, Acc0, MC}, F, Type, Value) ->
    case erlyfix_fields:serialize_field({Size0, Acc0}, F, Type, Value) of
        {ok, {Size1, Acc1}} -> {ok, {Size1, Acc1, maps:remove(F#field.name, MC)}};
        {error, Reason} -> {error, Reason}
    end.

% serialize group
serialize_group(AccContainer, CTX, Items) ->
    L = length(Items),
    case L > 0 of
        true ->
            FieldId = maps:get(CTX#context.name, CTX#context.protocol#protocol.field4name),
            F = array:get(FieldId, CTX#context.protocol#protocol.container),
            case serialize_field(AccContainer, F, unchecked, L) of
                {ok, NewAccContainer} -> serialize_group_item(NewAccContainer, CTX, Items);
                {error, Reason} -> {error, Reason}
            end;
        false ->
            % silently omit empty group
            {ok, AccContainer}
    end.

% serialize composite
serialize_composite({_Size, _Acc, MC} = AccContainer, CTX, []) ->
    case maps:size(MC) of
        0 -> {ok, AccContainer};
        _S ->
            [Name | _T ] = maps:keys(MC),
            Err = io_lib:format("Missing mandatory '~s' for '~s'", [Name, CTX#context.name]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason}
    end;
serialize_composite({Size, L, _MC} = AccContainer, CTX, [H | T]) ->
    Name = erlang:element(1, H),
    % ?DEBUG(Name),
    case maps:find(Name, CTX#context.c4n) of
        {ok, CompositeId} ->
            % serialize head (subcomposite)
            Payload = erlang:element(2, H),
            Composite = array:get(CompositeId, CTX#context.protocol#protocol.container),
            R = case erlang:element(1, Composite) of
                component ->
                    NewCTX = #context {
                        name = Composite#component.name,
                        protocol = CTX#context.protocol,
                        c4n = Composite#component.composite4name
                    },
                    NewMC = Composite#component.mandatoryComposites,
                    serialize_composite({Size, L, NewMC}, NewCTX, Payload);
                group ->
                    NewCTX = #context{
                        name = Composite#group.name,
                        protocol = CTX#context.protocol,
                        c4n = Composite#group.composite4name
                    },
                    NewMC = Composite#group.mandatoryComposites,
                    serialize_group({Size, L, NewMC}, NewCTX, Payload);
                field ->
                    serialize_field(AccContainer, Composite, checked, Payload)
            end,
            case R of
                {ok, NewAccContainer} ->
                    serialize_composite(NewAccContainer, CTX, T);
                {error, Reason} -> {error, Reason}
            end;
        error ->
            Err = io_lib:format("'~s' is not available for '~s'", [Name, CTX#context.name]),
            {error, erlang:iolist_to_binary(Err) }
    end.


process_pipeline(Acc, []) -> {ok, Acc};
process_pipeline(Acc0, [H | T]) ->
    R = H(Acc0),
    case R of
        {ok, Acc1} -> process_pipeline(Acc1, T);
        {error, Details} -> {error, Details}
    end.

serialize_message(Protocol, Message, MessageFields) ->
    H = Protocol#protocol.header,
    T = Protocol#protocol.trailer,
    HT_C4N = maps:merge(H#header.composite4name, T#trailer.composite4name),
    HT_MC = maps:merge(H#header.mandatoryComposites, T#trailer.mandatoryComposites),
    C4N_i = maps:merge(Message#message.composite4name, HT_C4N),
    MC_i = maps:merge(Message#message.mandatoryComposites, HT_MC),

    ManagedFields = ['BeginString', 'BodyLength', 'MsgType', 'CheckSum'],
    C4N = maps:without(ManagedFields, C4N_i),
    MC_managed = maps:without(ManagedFields, MC_i),

    % managed fields
    {ok, F_Type} = lookup(Protocol, {field, by_name, 'MsgType' }),
    {ok, F_BodyLength} = lookup(Protocol, {field, by_name, 'BodyLength' }),
    {ok, F_BeginString} = lookup(Protocol, {field, by_name, 'BeginString' }),
    {ok, F_CheckSum} = lookup(Protocol, {field, by_name, 'CheckSum' }),

    Fn_add_MsgType = fun(Acc0) ->
        serialize_field(Acc0, F_Type, unchecked, Message#message.type)
    end,
    CTX = #context{ name = Message#message.name, protocol = Protocol, c4n = C4N },
    Fn_serialize_body = fun(Acc) ->
        serialize_composite(Acc, CTX, MessageFields)
    end,
    Fn_reverse_body = fun({Size0, List0, MC0}) -> {ok, {Size0, lists:reverse(List0), MC0}} end,

    Fn_wrap_body = fun({SizeB,  AccB, MCB}) ->
        Fn_headers = [
            fun(_Acc) -> serialize_field({0, [], MCB}, F_BodyLength, unchecked, SizeB) end,
            fun(Acc) ->
                Version = Protocol#protocol.protocol_version,
                ProtocolID = io_lib:format("FIX.~B.~B", [Version#protocol_version.major, Version#protocol_version.minor]),
                serialize_field(Acc, F_BeginString, unchecked, ProtocolID)
            end,
            fun({_SizeH, AccH, _MC} = AccContainer) ->
                CheckSum = erlyfix_utils:checksum([AccH | AccB]),
                PaddedCS = io_lib:format("~3..0B", [CheckSum]),
                serialize_field(AccContainer, F_CheckSum, unchecked, PaddedCS)
            end,
            fun({SizeTH, [AccT | AccH], MC1}) -> {ok, {SizeTH + SizeB, [ AccH, AccB, AccT], MC1} } end
        ],
        process_pipeline({SizeB,  AccB, MCB}, Fn_headers)
    end,

    R = process_pipeline({0, [], MC_managed}, [
        Fn_add_MsgType,
        Fn_serialize_body,
        Fn_reverse_body,
        Fn_wrap_body
    ]),

    % ?DEBUG(R),
    case R of
        {ok, {_Size, Acc, _MC2}} -> {ok, Acc};
        {error, Reason} -> {error, Reason}
    end.

serialize(Protocol, MessageName, MessageFields) ->
    case maps:find(MessageName, Protocol#protocol.message4name) of
        {ok, Message} -> serialize_message(Protocol, Message, MessageFields);
        error ->
            Err = io_lib:format("Message '~s' not found", [MessageName]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason}
    end.
