module Data.PhoneBook (
  Entry(..),
  PhoneBook(..),
  emptyBook,
  showEntry,
  insertEntry,
  findEntry,
  containsEntry,
  findByPhone,
  findByName,
  containsPhone,
  containsName
  ) where

import Data.Maybe
import Data.List

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }
type PhoneBook = List Entry

emptyBook :: PhoneBook
emptyBook = empty

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ": " ++
                  entry.phone

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry entry book = Cons entry book

filterByPhone :: String -> Entry -> Boolean
filterByPhone phone entry = entry.phone == phone

filterByName :: String -> String -> Entry -> Boolean
filterByName firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

findEntry :: forall a. (a -> Boolean) -> Data.List.List a -> Maybe a
findEntry fn book = head $ filter fn book

containsEntry :: forall a. (a -> Boolean) -> Data.List.List a -> Boolean
containsEntry fn book = not $ null $ filter fn book

findByPhone :: String -> PhoneBook -> Maybe Entry
findByPhone phone book = findEntry (filterByPhone phone) book

findByName :: String -> String -> PhoneBook -> Maybe Entry
findByName firstName lastName book = findEntry (filterByName firstName lastName) book

containsPhone :: String -> PhoneBook -> Boolean
containsPhone phone book = containsEntry (filterByPhone phone) book

containsName :: String -> String -> PhoneBook -> Boolean
containsName firstName lastName book = containsEntry (filterByName firstName lastName) book

-- > :i Data.PhoneBook
-- > let example = { firstName: "John", lastName: "Smith", phone: "555-555-5555" }
-- > let john = { firstName: "John", lastName: "Smith", phone: "555-555-5555" }
-- > let book1 = insertEntry john emptyBook
-- > let printEntry firstName lastName book = showEntry <$> findByName firstName lastName book
-- > printEntry "John" "Smith" book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> findEntry (\entry -> entry.phone == "555-555-5555") book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> findEntry (\entry -> entry.firstName == "John" && entry.lastName == "Smith") book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> findEntry (\entry -> entry.firstName == "John" && entry.lastName == "Foo") book1
-- Nothing
-- > showEntry <$> findByPhone "555-555-5555" book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> findByPhone "bogus" book1
-- Nothing
-- > showEntry <$> findByName "John" "Smith" book1
-- Just ("Smith, John: 555-555-5555")
-- > showEntry <$> findByName "John" "Bogus" book1
-- Nothing
-- > containsName "John" "Smith" book1
-- true
-- > containsName "John" "Bogus" book1
-- false
-- > containsPhone "555-555-5555" book1
-- true
-- > containsPhone "Bogus" book1
-- false
