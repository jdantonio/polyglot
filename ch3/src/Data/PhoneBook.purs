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

find :: forall a. (a -> Boolean) -> Data.List.List a -> Maybe a
find fn book = head $ filter fn book

filterByPhone :: String -> Entry -> Boolean
filterByPhone phone entry = entry.phone == phone

findByPhone :: String -> PhoneBook -> Maybe Entry
findByPhone phone book = find (filterByPhone phone) book

filterByName :: String -> String -> Entry -> Boolean
filterByName firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

findByName :: String -> String -> PhoneBook -> Maybe Entry
findByName firstName lastName book = find (filterByName firstName lastName) book

-- > :i Data.PhoneBook
-- > let example = { firstName: "John", lastName: "Smith", phone: "555-555-5555" }
-- > let printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book
-- > let john = { firstName: "John", lastName: "Smith", phone: "555-555-5555" }
-- > let book1 = insertEntry john emptyBook
-- > let printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book
-- > printEntry "John" "Smith" book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> find (\entry -> entry.phone == "555-555-5555") book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> find (\entry -> entry.firstName == "John" && entry.lastName == "Smith") book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> find (\entry -> entry.firstName == "John" && entry.lastName == "Foo") book1
-- Nothing
-- > showEntry <$> findByPhone "555-555-5555" book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> findByPhone "bogus" book1
-- Nothing
-- > showEntry <$> findByName "John" "Smith" book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> findByName "John" "Bogus" book1
-- Nothing
