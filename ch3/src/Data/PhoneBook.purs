module Data.PhoneBook where

import Data.Array
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }
type PhoneBook = Array Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ": " ++
                  entry.phone

emptyBook :: PhoneBook
emptyBook = empty
