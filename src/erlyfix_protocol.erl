-module(erlyfix_protocol).
-include("erlyfix_records.hrl").

-export([load/1]).

find_attr(Name, Attributes) ->
    F = fun(I) ->
        case I of
             {attribute,Name, _, _, _} -> true;
             _ -> false
         end
    end,
    case lists:filter(F, Attributes) of
        [{attribute,Name, _, _, Value}] -> {ok, Value};
        _ -> {not_found}
    end.

callback(Event, Acc) ->
    %io:format("~p~n", [Event]),
    case Event of
        {startElement, [], "fix", [], Attributes} ->
            %io:format("~p~n", [Acc]),
            {ok, Minor} = find_attr("minor", Attributes),
            {ok, Major} = find_attr("major", Attributes),
            {ok, Servicepack} = find_attr("servicepack", Attributes),
            {ok,"FIX"} = find_attr("type", Attributes),
            Version = #protocol_version {
                major = list_to_integer(Major),
                minor = list_to_integer(Minor),
                servicepack = list_to_integer(Servicepack)},
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
            {ok, Name} = find_attr("name", Attributes),
            case find_attr("required", Attributes) of
                {ok, Value} ->                      % field reference
                    Required = case Value of
                        "Y" -> true;
                        "N" -> false
                    end,
                    FieldRef = #field_ref{ name = Name, required = Required },
                    [ {field_ref, FieldRef}, H | T ];
                {not_found} ->                      % field definition
                    {ok, Number} = find_attr("number", Attributes),
                    {ok, Type} = find_attr("type", Attributes),
                    [ [], {field_def, Name, Number, Type, H} | T ]
                    %Acc
            end;
        {endElement,[],"field",[]} ->
            [F, H | T] = Acc,
            case is_list(F) of
                false ->                            % field reference
                    {field_ref, FieldRef} = F,
                    [ [FieldRef | H] | T ];
                true ->                             % field definition
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
            {ok, Name} = find_attr("name", Attributes),
            {ok, Type} = find_attr("msgtype", Attributes),
            {ok, Category} = find_attr("msgcat", Attributes),
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
            {ok, Name} = find_attr("name", Attributes),
            [ [], Name | Acc ];
        {endElement,[],"component",[]} ->
            [Composites, Name | Acc1] = Acc,
            ComponentRef = #component_ref { name = Name, composites = Composites},
            [H | T1] = Acc1,
            [ [ComponentRef | H] | T1 ];
        {startElement, [], "group", [], Attributes} ->
            {ok, Name} = find_attr("name", Attributes),
            [ [], Name | Acc ];
        {endElement,[],"group",[]} ->
            [Composites, Name | Acc1] = Acc,
            GroupRef = #group_ref { name = Name, composites = Composites},
            [H | T1] = Acc1,
            [ [GroupRef | H] | T1 ];
        {startElement, [], "value", [], Attributes} ->
            {ok, Key} = find_attr("enum", Attributes),
            {ok, Description} = find_attr("description", Attributes),
            ValueDef = #value_def{ key = Key, description = Description},
            [H | T] = Acc,
            [ [ValueDef | H] | T ];
        _ -> Acc
    end.

load(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
          {ok, _, _} = erlsom:parse_sax(Bin, #{}, fun callback/2);
        Error ->
          Error
    end.
