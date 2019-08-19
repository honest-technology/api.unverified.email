module Test.Hspec.Wai.ValueMatchers where

import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Hspec.Wai
import Data.String.Interpolate (i)
import Text.StringConvert (s)

introspectBody :: (Text -> [Maybe String]) -> MatchBody
introspectBody matchers = MatchBody introspectionMatcher
  where
  introspectionMatcher _ body = introspectWith (s body)
  introspectWith tbody = if (introspect tbody == []) then Nothing else Just (unlines (introspect tbody))
  introspect tbody = catMaybes (matchers tbody)

equalsTo :: (Show a, Eq a) => a -> a -> Maybe String
equalsTo x y = if x == y then Nothing else Just [i|#{show x} did not equal #{show y}|]

maybeEqualsTo ::  (Show a, Eq a) => Maybe a -> a -> Maybe String
maybeEqualsTo (Just x) y = if x == y then Nothing else Just [i|#{show x} did not equal #{show y}|]
maybeEqualsTo Nothing y = Just [i|expected #{show y} not found|]

contains :: Maybe Text -> Text -> Maybe String
contains (Just x) y = if (y `T.isInfixOf` x) then Nothing else Just [i|could not find #{y} in #{x}|]
contains Nothing y = Just [i|expected #{y} not found|]

hasLength :: Maybe Text -> Int -> Maybe String
hasLength (Just t) n =
  if (T.length t == n)
  then Nothing
  else Just [i|length of #{show t} (#{T.length t}) is different than #{n} |]
hasLength Nothing n = Just [i|could not find anything with length #{n}|]

listHasLength :: (Foldable t, Show (t a)) => Maybe (t a) -> Int -> Maybe String
listHasLength (Just t) n =
  if (length t == n)
  then Nothing
  else Just [i|length of #{show t} (#{length t}) is different than #{n} |]
listHasLength Nothing n = Just [i|could not find anything with length #{n}|]

isUUID :: Maybe Text -> Maybe String
isUUID (Just t) = if (isJust (UUID.fromText t)) then Nothing else Just [i|#{t} is not a valid uuid|]
isUUID Nothing = Just "could not find the expected UUID"

