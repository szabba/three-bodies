module Layout.Footer
  ( view ) where

import Html exposing (..)
import Html.Attributes as Attributes


view : Html
view =
  div
    [ Attributes.id "footer" ]
    [ avatar ]


avatar : Html
avatar =
  img
    [ Attributes.id "avatar"
    , Attributes.src gravatarURL
    , Attributes.alt "avatar"
    , Attributes.width avatarSize
    , Attributes.height avatarSize
    ]
    []


avatarSize : Int
avatarSize = 80


{- FIXME: Get an MD5 implementation in Elm, so we can just hash an e-mail. -}
gravatarURL : String
gravatarURL =
  "http://www.gravatar.com/avatar/c883fb6c6f1304c1b4b6eb1b0147b792?s=80&d=mm"