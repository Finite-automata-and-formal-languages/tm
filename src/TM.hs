module TM where

import Control.Exception
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Char
import Data.Foldable
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Turing
import Data.Void
import System.Environment
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Debug.Trace

newtype GenericError = GenericError String

instance Show GenericError where
  show (GenericError msg) = msg

instance Exception GenericError

duplicates :: Ord a => [a] -> [a]
duplicates = go S.empty where
  go seen (x:xs) | x `S.member` seen = x:xs'
                 | otherwise = xs'
                 where xs' = go (S.insert x seen) xs
  go _ [] = []

type State_ = Text
type Symbol = Char

data TM = TM
  { states :: [State_]
  , input :: [Symbol]
  , tape :: [Symbol]
  , start :: State_
  , blank :: Symbol
  , accepting :: [State_]
  , transitions :: Map (State_, Symbol) (State_, Symbol, Movement)
  } deriving (Show)

type Transition = ((State_, Symbol), (State_, Symbol, Movement))

type Parser = Parsec Void Text

whitespace, hwhitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "#") empty
hwhitespace = L.space hspace1 (L.skipLineComment "#") empty

lexeme = L.lexeme whitespace
hlexeme = L.lexeme hwhitespace

symbol :: Parser Char
symbol = satisfy (\c -> not (isSpace c) && c /= '#')

state :: Parser Text
state = takeWhile1P Nothing (\c -> not (isSpace c) && c /= '#')

movement :: Parser Movement
movement = MoveLeft <$ "L" <|> MoveRight <$ "R"

arrow :: Parser ()
arrow = void $ "->" <|> "→"

transition :: Parser Transition
transition = do
  whitespace
  from <- hlexeme state
  read <- hlexeme symbol
  hlexeme arrow
  to <- hlexeme state
  write <- hlexeme symbol
  move <- hlexeme movement
  eol
  pure ((from, read), (to, write, move))

parser :: Parser TM
parser = do
  whitespace
  hlexeme "States:"
  states <- state `sepEndBy` hwhitespace
  eol
  case duplicates states of
    [] -> pure ()
    d -> fail ("duplicate states: " <> show d)
  hlexeme "Input:"
  input <- symbol `sepEndBy` hwhitespace
  eol
  case duplicates input of
    [] -> pure ()
    d -> fail ("duplicate input symbols: " <> show d)
  hlexeme "Tape:"
  tape <- symbol `sepEndBy` hwhitespace
  eol
  case duplicates tape of
    [] -> pure ()
    d -> fail ("duplicate tape symbols: " <> show d)
  case intersect input tape of
    [] -> pure ()
    d -> fail ("symbols belong to both input and tape alphabets: " <> show d)
  lexeme "Start:"
  start <- state
  eol
  unless (start `elem` states) do
    fail ("start state " <> show start <> " does not belong to the list of states")
  lexeme "Blank:"
  blank <- symbol
  eol
  unless (blank `notElem` (input ++ tape)) do
    fail "blank symbol should not belong to either alphabet"
  lexeme "Accepting:"
  accepting <- state `sepEndBy` hwhitespace
  case duplicates accepting of
    [] -> pure ()
    d -> fail ("duplicate accepting states: " <> show d)
  case accepting \\ states of
    [] -> pure ()
    d -> fail ("invalid accepting states: " <> show d)
  eol
  transitions <- M.fromListWithKey (\ k _ _ -> error ("duplicate transition for " <> show k))
    <$> transition `sepEndBy` whitespace
  eof
  pure TM{..}

expected :: String -> MachineResult
expected w
  | (zeros, w1) <- span (== '0') w
  , (ones, w2) <- span (== '1') w1
  , (twos, "") <- span (== '2') w2
  , length zeros == length ones && length ones == length twos
  = Accept
  | otherwise = Reject

passive :: MachineResult -> String
passive Accept = "accepted"
passive Reject = "rejected"

checkTm :: Bool -> String -> Text -> IO ()
checkTm parseOnly file src = do
  TM{..} <- case parse parser file src of
    Left bundle -> throwIO (GenericError (errorBundlePretty bundle))
    Right tm -> pure tm
  unless parseOnly do
    let
      δ (from, read') = do
        -- traceM (T.unpack from <> " " <> show read')
        let read = fromMaybe blank read'
        (to, write, move) <- M.lookup (from, read) transitions
        let write' = if write == blank then Nothing else Just write
        pure (to, write', move)
      tm = mkTuringMachine δ start accepting
    msgs <- execWriterT $ for_ [0..10] \ length -> do
      for_ (replicateM length "012" :: [String]) \ word -> do
        let result = runFor 10000 tm word
            expectation = expected word
        case result of
          Nothing -> lift $ throwIO (GenericError ("The machine runs for too many steps on input " <> word))
          Just result -> do
            when (result /= expectation) do
              tell [word <> " is " <> passive result <> " but should be " <> passive expectation]
    unless (null msgs) do
      throwIO (GenericError (unlines msgs))
