module MR.Assert (assert) where

import MR.Dates
import MR.Types

import Data.List (filter)
import Control.Applicative ((<$>))

multAnd :: [Bool] -> Bool -> [Bool]
multAnd bs = \b -> map ((&&) b) bs

andPair :: (Bool, Bool) -> Bool
andPair tuple = (fst tuple) && (snd tuple)

assert :: [Assertion] -> [Event] -> [Bool]
assert [] _ = [True]
assert al@((Assertion i p (DateSpan Nothing Nothing)):as) events = map andPair $ zip (assertChunkWith p <$> splitIntoChunks al events) (assert as events)
assert al@(a:as) events = (assertChunkWith (predicate a) <$> splitIntoChunks al events) >>= multAnd (assert as events)

splitIntoChunks :: [Assertion] -> [Event] -> [[EventInfo]]
splitIntoChunks [] _ = []
splitIntoChunks (a:b:as) events | (interval a ) == AllSpan =
    let spans = splitSpan (interval b) (dateSpan b)
    in [map info $ filter (\e -> spanContainsDate d $ day e) events | d<-(map (spanLeftShift AllSpan) spans)]
splitIntoChunks (a:b:as) events | (dateSpan a) == (DateSpan Nothing Nothing) = 
    let a' = Assertion (interval a) (predicate a) (spanLeftShift (interval a) (dateSpan b))
        in splitIntoChunks (a':b:as) events
splitIntoChunks (a:as) events = [map info $ filter (\e -> spanContainsDate d $ day e) events | d<-(splitSpan (interval a) (dateSpan a))]

assertChunkWith :: Predicate -> [EventInfo] -> Bool
assertChunkWith p es = fst $ assert p es where
    assert Nil es = (True,es)
    assert p [] = (False,[])
    assert (Single a) (e:es) | a<=e = (True, es)
    assert p@(Single a) (e:es) = assert p es
    assert (And p q) es = let (b, r) = assert p es
                              (b', r') = assert q es
                              in (b && b', smallest r r')
    assert (Or p q) es = let (b, r) = assert p es
                             (b', r') = assert q es
                             in (b || b', biggest r r')
    assert (Seq p q) es= let (b, r) = assert p es
                             in let (b', r') = assert q r
                                    in (b && b', r')
    smallest l l' | length l <= length l' = l
    smallest _ l' = l'
    biggest l l' | length l >= length l' = l
    biggest _ l' = l'
