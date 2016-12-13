module Siren.Value exposing (Value(..), toString)


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue


toString : Value -> String
toString value =
    case value of
        StringValue string ->
            string

        IntValue int ->
            Basics.toString int

        FloatValue float ->
            Basics.toString float

        BoolValue bool ->
            Basics.toString bool

        NullValue ->
            "Null"
