-module(erlyfix_protocol).
-include("erlyfix_records.hrl").

-export([load/1, version/1, lookup/2, serialize/3]).
-define(SEPARATOR, <<1:8>>).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

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
construct_fields({Field4Name, Field4Number}, [FieldDefinition | Rest]) ->
    ValuesList = FieldDefinition#field_def.values,
    {Value4Key, Value4Description} = map_values({ #{}, #{} }, ValuesList),
    Name = FieldDefinition#field_def.name,
    Number = FieldDefinition#field_def.number,
    Field = #field{
        name   = Name,
        number = Number,
        type   = FieldDefinition#field_def.type,
        value4key = Value4Key,
        value4description = Value4Description},
    Acc = {
        Field4Name#{ Name => Field },
        Field4Number#{ Number => Field}
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

get_composites(Acc, _C4Name, _F4Name, []) -> {ok, Acc};
get_composites(Acc, C4Name, F4Name, [H | T]) ->
    case element(1, H) of
        component_ref ->
            Name = H#component_ref.name,
            case maps:find(Name, C4Name) of
                {ok, Composite} -> get_composites([{Name, Composite} | Acc], C4Name, F4Name, T);
                error -> not_found
            end;
        field_ref ->
            Name = H#field_ref.name,
            case maps:find(Name, F4Name) of
                {ok, Composite} -> get_composites([{Name, Composite} | Acc], C4Name, F4Name, T);
                error -> not_found
            end;
        group_def ->
            Name = H#group_def.name,
            CompositeRefs = H#group_def.composites,
            case get_composites([], C4Name, F4Name, CompositeRefs) of
                {ok, SubCompositesNames} ->
                    {Co4Name, MC} = zip(CompositeRefs, SubCompositesNames),
                    Composite = #group{ name = Name, composite4name = Co4Name, mandatoryComposites = MC},
                    get_composites([{Name, Composite} | Acc], C4Name, F4Name, T);
                not_found -> not_found
            end
    end.

construct_components(C4Name, F4Name, Queue) ->
    case queue:is_empty(Queue) of
        true -> C4Name;
        false ->
            { {value, ComponentRef}, QLeft} = queue:out(Queue),
            %io:format("trying to construct composite ~p~n", [ComponentRef]),
            #component_def{ name = Name, composites = SubCompositeRefs } = ComponentRef,
            case get_composites([], C4Name, F4Name, SubCompositeRefs) of
                {ok, SubCompositesNames} ->
                    %io:format("found subcomposites ~p :: ~p~n", [ComponentRef, SubCompositesNames]),
                    {Co4Name, MC} = zip(SubCompositeRefs, SubCompositesNames),
                    Composite = #component{ name = Name, composite4name = Co4Name, mandatoryComposites = MC},
                    C4NameNew = C4Name#{ Name => Composite },
                    construct_components(C4NameNew, F4Name, QLeft);
                not_found ->
                    Q2 = queue:in(ComponentRef, QLeft),
                    construct_components(C4Name, F4Name, Q2)
            end
    end.

construct_messages(Acc, _C4Name, _F4Name, []) -> Acc;
construct_messages({M4Name, M4Type}, C4Name, F4Name, [H | T]) ->
    #message_ref { name = Name, type = Type, category = Category, composites = CompositeRefs} = H,
    {ok, SubCompositesNames} = get_composites([], C4Name, F4Name, CompositeRefs),
    {Co4Name, MC} = zip(CompositeRefs, SubCompositesNames),
    Message = #message {
        name = Name,
        type = Type,
        category = Category,
        composite4name = Co4Name,
        mandatoryComposites = MC
    },
    M4NameNew = M4Name#{ Name => Message},
    M4TypeNew = M4Type#{ Type => Message},
    construct_messages({M4NameNew, M4TypeNew}, C4Name, F4Name, T).

construct(Map) ->
    % fields
    FieldDefinitions = maps:get(fields, Map),
    ComponentDefinitions = maps:get(components, Map),
    {Field4Name, Field4Number} = construct_fields({ #{}, #{}}, FieldDefinitions),

    % components
    ComponentsQueue = queue:from_list(ComponentDefinitions),
    C4Name = construct_components(#{}, Field4Name, ComponentsQueue),

    % header
    HeaderRefs = maps:get(header, Map),
    {ok, HeaderCompositeNames} = get_composites([], C4Name, Field4Name, HeaderRefs),
    {H_C4Name, H_MC} = zip(HeaderRefs, HeaderCompositeNames),
    Header = #header{ composite4name = H_C4Name, mandatoryComposites = H_MC },

    % trailer
    TrailerRefs = maps:get(trailer, Map),
    {ok, TrailerCompositeNames} = get_composites([], C4Name, Field4Name, TrailerRefs),
    {T_C4Name, T_MC} = zip(TrailerRefs, TrailerCompositeNames),
    Trailer = #trailer{ composite4name = T_C4Name, mandatoryComposites = T_MC },

    % messages
    MessagesRefs = maps:get(messages, Map),
    {M4Name, M4Type} = construct_messages({#{}, #{}}, C4Name, Field4Name, MessagesRefs),

    #protocol{
        protocol_version = maps:get(version, Map),
        header           = Header,
        trailer          = Trailer,
        field4name       = Field4Name,
        field4number     = Field4Number,
        component4name   = C4Name,
        message4name     = M4Name,
        message4type     = M4Type}.

%% Interface method

load(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
          {ok, Map, _} = erlsom:parse_sax(Bin, #{}, fun callback/2),
          Protocol = construct(Map),
          Protocol;
          %Map;
        Error -> Error
    end.

version(Protocol)-> Protocol#protocol.protocol_version.

% lookup
lookup(Protocol, Criterium) ->
    {K, L} = case Criterium of
        {field, by_name, X} -> {X, Protocol#protocol.field4name};
        {field, by_number, X} -> {X, Protocol#protocol.field4number};
        {component, X} -> {X, Protocol#protocol.component4name};
        {message, by_name, X} -> {X, Protocol#protocol.message4name};
        {message, by_type, X} -> {X, Protocol#protocol.message4type}
    end,
    case maps:find(K, L) of
        {ok, Field} -> {ok, Field};
        error -> not_found
    end.

% decompose composite

decompose(C) when is_record(C, field) -> {field, C};
decompose(C) when is_record(C, component) -> {non_field, C#component.name, C#component.composite4name, C#component.mandatoryComposites };
decompose(C) when is_record(C, group) -> {non_field, C#group.name, C#group.composite4name, C#group.mandatoryComposites };
decompose(C) when is_record(C, header) -> {non_field, header, C#header.composite4name, C#header.mandatoryComposites };
decompose(C) when is_record(C, trailer) -> {non_field, trailer, C#trailer.composite4name, C#trailer.mandatoryComposites }.


% serialize composite

serialize_composite(AccContainer, {_P, _N, _C4N, #{}},  []) -> {ok, AccContainer};
serialize_composite(_AccContainer, {_P, N, _C4N, MC},  []) ->
    [Name | _T ] = maps:keys(MC),
    Err = iolib:format("Missing mandatory '~w' for '~w'", [Name, N]),
    Reason = erlang:iolist_to_binary(Err),
    {error, Reason};
serialize_composite({Size, Acc}, {P, N, C4N, MC}, [H | T]) ->
    Name = erlang:element(1, H),
    case maps:find(Name, C4N) of
        {ok, Composite} ->
            % serialize head (subcomposite)
            R = case decompose(Composite) of
                {non_field, S_N, S_C4N, S_MC} -> serialize_composite(Acc, {P, S_N, S_C4N, S_MC}, H);
                {field, F} -> erlyfix_fields:serialize_field({Size, Acc}, F, H)
            end,
            % serialize tail
            % ?DEBUG(R),
            case R of
                {ok, {NewSize, NewAcc}} ->
                    NewMandatoryComposites = maps:remove(Name, MC),
                    serialize_composite({NewSize, NewAcc}, {P, N, C4N, NewMandatoryComposites}, T);
                {error, Reason} -> {error, Reason}
            end;
        {error} ->
            Err = iolib:format("'~w' is not available for '~w'", [Name, N]),
            {error, erlang:iolist_to_binary(Err) }
    end.

checksum(Acc, []) -> Acc rem 256;
checksum(Acc, [H | T]) when is_list(H) ->
    H_Acc = checksum(Acc, H),
    checksum(H_Acc, T);
checksum(Acc, [H | T]) when is_binary(H) ->
    F = fun(X, Acc0) -> X + Acc0 end,
    lists:foldl(F, Acc, binary_to_list(H)).


serialize_message(Protocol, Message, MessageFields) ->
    H = Protocol#protocol.header,
    T = Protocol#protocol.trailer,
    HT_C4N = maps:merge(H#header.composite4name, T#trailer.composite4name),
    HT_MC = maps:merge(H#header.mandatoryComposites, T#trailer.mandatoryComposites),
    N = Message#message.name,
    C4N_i = maps:merge(Message#message.composite4name, HT_C4N),
    MC_i = maps:merge(Message#message.mandatoryComposites, HT_MC),

    C4N = maps:without(['BeginString', 'BodyLength', 'MsgType'], C4N_i),
    MC = maps:without(['CheckSum'], MC_i),

    F_Type = maps:get('MsgType', Protocol#protocol.field4name),
    {ok, {Size0, Acc0}} = erlyfix_fields:serialize_field({0, []}, F_Type, {'MsgType', Message#message.type}),
    {ok, {SizeB, Acc1}} = serialize_composite({Size0, Acc0}, {Protocol, N, C4N, MC}, MessageFields),
    AccB = lists:reverse(Acc1),

    % add length header
    F_BodyLength = maps:get('BodyLength', Protocol#protocol.field4name),
    {ok, {SizeL, AccL}} = erlyfix_fields:serialize_field({0, []}, F_BodyLength, {'BodyLength', SizeB}),

    % add begin strign header
    Version = Protocol#protocol.protocol_version,
    ProtocolID = io_lib:format("FIX.~B.~B", [Version#protocol_version.major, Version#protocol_version.minor]),
    F_BeginString = maps:get('BeginString', Protocol#protocol.field4name),
    {ok, {SizeH, AccH}} = erlyfix_fields:serialize_field({SizeL, AccL}, F_BeginString, {'BeginString', ProtocolID}),
    ?DEBUG(AccH),

    % checksum trailer,
    F_CheckSum = maps:get('CheckSum', Protocol#protocol.field4name),
    CheckSum = checksum(0, [AccH | AccB]),
    {ok, {_SizeT, AccT}} = erlyfix_fields:serialize_field({0, []}, F_CheckSum, {'CheckSum', CheckSum}),

    AccR = [AccH,  AccB, AccT],
    ?DEBUG(AccR),
    {ok, AccR}.
    %lists:reverse(AccL).

serialize(Protocol, MessageName, MessageFields) ->
    case maps:find(MessageName, Protocol#protocol.message4name) of
        {ok, Message} -> serialize_message(Protocol, Message, MessageFields);
        error ->
            Err = iolib:format("Message 'MessageName' not found", [MessageName]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason}
    end.
