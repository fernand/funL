module MR.QueryParser (parseQuery, parseForDateSpan, parseForEvents) where

import MR.Types
import MR.Dates

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), pure)
import Data.List (nub)
import qualified Data.Text as T
import Data.Aeson (Value(..))

lexer = P.makeTokenParser haskellDef

integer = P.integer lexer
stringLiteral = P.stringLiteral lexer
comma = P.comma lexer
commaSep = P.commaSep lexer
semiSep = P.semiSep lexer
parens = P.parens lexer
brackets = P.brackets lexer
identifier = P.identifier lexer
symbol = P.symbol lexer

parseQuery :: String -> Either ParseError [Assertion]
parseQuery s = parse p_assertions "" s

p_assertions = semiSep p_assertion
p_assertion = Assertion <$> p_interval <*> (toPredicate <$> p_predicate) <*> p_dateSpan

p_interval = choice [p_interval_allspan, p_interval_rest]
	<?> "number and interval type"
p_interval_allspan = AllSpan <$ symbol "all"
p_interval_rest = fromInteger <$> integer >>= itvl where
	itvl i = ((Days i) <$ symbol "days")
		<|> ((Weeks i) <$ symbol "weeks")
		<|> ((Months i) <$ symbol "months")
		<|> ((Quarters i) <$ symbol "quarters")
		<|> ((Years i) <$ symbol "years")

-- new Predicate type for syntax sugar
data SPredicate = AndArray [SPredicate] | OrArray [SPredicate]
    | SeqArray [SPredicate] | Atom EventInfo deriving (Show)

p_predicate = (option "and" identifier >>= (p_pvalue) <?> "predicate") where 
        p_pvalue "and" = AndArray <$> p_predArray
        p_pvalue "or" = OrArray <$> p_predArray
        p_pvalue "seq" = SeqArray <$> p_predArray
        p_pvalue eName = Atom <$> (p_eventInfo eName) <?> "event name"

p_predArray = brackets (commaSep p_predicate)

p_eventInfo eName = (EventInfo eName) <$> parens (commaSep p_eventProperty)
    <?> "event filter"
p_eventProperty = (,) <$> (T.pack <$> identifier <* char '=') <*> p_jvalue
    <?> "event properties"

p_jvalue = choice [
    string "false" *> pure (Bool False)
    , string "true" *> pure (Bool True)
    , string "null" *> pure Null
    , String . T.pack <$> stringLiteral
    , Number . fromInteger <$> integer]
    <?> "json value, probably unsupported"

p_dateSpan = parens (mkdatespan <$> option "\"\"" (stringLiteral <* comma)
    <*> option "\"\"" (stringLiteral))
    <?> "date range"

toPredicate :: SPredicate -> Predicate
toPredicate (Atom e) = Single e
toPredicate (AndArray []) = Nil
toPredicate (OrArray []) = Nil
toPredicate (SeqArray []) = Nil
toPredicate (AndArray (p:[])) = toPredicate p
toPredicate (OrArray (p:[])) = toPredicate p
toPredicate (SeqArray (p:[])) = toPredicate p
toPredicate (AndArray (p1:p2:[])) = And (toPredicate p1) (toPredicate p2)
toPredicate (OrArray (p1:p2:[])) = Or (toPredicate p1) (toPredicate p2)
toPredicate (SeqArray (p1:p2:[])) = Seq (toPredicate p1) (toPredicate p2)
toPredicate (AndArray (p:ps)) = And (toPredicate p) (toPredicate $ AndArray ps)
toPredicate (OrArray (p:ps)) = Or (toPredicate p) (toPredicate $ OrArray ps)
toPredicate (SeqArray (p:ps)) = Seq (toPredicate p) (toPredicate $ SeqArray ps)

processWith :: ([Assertion] -> [a]) -> String -> Either String [a]
processWith walk s = case parseQuery s of
    Left err -> Left $ "parse failed: " ++ show err
    Right as -> Right $ walk as

parseForDateSpan = processWith dwalk
dwalk :: [Assertion] -> [[DateSpan]]
dwalk [] = [[]]
dwalk (a:as) | dateSpan a == DateSpan Nothing Nothing = dwalk as
dwalk (a:as) = splitSpan (interval a) (dateSpan a) >>= \dSpan -> map ((++) [dSpan]) (dwalk as)

parseForEvents = processWith ewalk
ewalk :: [Assertion] -> [String]
ewalk l = nub $ foldPreds name (map predicate l)
    