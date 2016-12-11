module Siren.Entity exposing (..)

import Dict
import Siren exposing (..)


rels : Entity -> Rels
rels e =
    case e of
        Entity rels _ _ _ _ _ ->
            rels

        EntityLink rels _ _ _ _ ->
            rels


classes : Entity -> Classes
classes e =
    case e of
        Entity _ classes _ _ _ _ ->
            classes

        EntityLink classes _ _ _ _ ->
            classes


properties : Entity -> Properties
properties e =
    case e of
        Entity _ _ properties _ _ _ ->
            properties

        EntityLink _ _ _ _ _ ->
            Dict.empty


links : Entity -> Links
links e =
    case e of
        Entity _ _ _ links _ _ ->
            links

        EntityLink _ _ _ _ _ ->
            []


entities : Entity -> Entities
entities e =
    case e of
        Entity _ _ _ _ entities _ ->
            entities

        EntityLink _ _ _ _ _ ->
            []


actions : Entity -> Actions
actions e =
    case e of
        Entity _ _ _ _ _ actions ->
            actions

        EntityLink _ _ _ _ _ ->
            []
