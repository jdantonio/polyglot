module Data.PhoneBook where

import Data.Maybe
import Data.List

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }
type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ": " ++
                  entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry entry book = Cons entry book

findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
