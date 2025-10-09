{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

maybeRead :: String -> Maybe Int
maybeRead s = 
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing 

parseToUnknown :: [String] -> Either LogMessage String
parseToUnknown = Right . unwords

parseWordsToEither :: [String] -> Either LogMessage String
parseWordsToEither ws@("I" : timestamp : messages) =
  case maybeRead timestamp of
    Just ts -> Left $ LogMessage Info ts (unwords messages)
    Nothing ->  parseToUnknown ws
parseWordsToEither ws@("W" : timestamp : messages) =
  case maybeRead timestamp of
    Just ts -> Left $ LogMessage Warning ts (unwords messages)
    Nothing -> parseToUnknown ws
parseWordsToEither ws@("E" : errorLevel : timestamp : messages) =
  case maybeRead timestamp of
    Just ts ->  
      case maybeRead errorLevel of
        Just el -> Left $ LogMessage (Error el) ts (unwords messages)
        Nothing -> parseToUnknown ws
    Nothing -> parseToUnknown ws
parseWordsToEither ws = parseToUnknown ws

parseEitherToMessage :: Either LogMessage String -> LogMessage
parseEitherToMessage (Left l) = l
parseEitherToMessage (Right xs) = Unknown xs

parseMessage :: String -> LogMessage
parseMessage = parseEitherToMessage . parseWordsToEither . words

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l Leaf = Node Leaf l Leaf
insert l1@(LogMessage _ timestamp1 _) (Node leftSubTree l2@(LogMessage _ timestamp2 _) rightSubTree)
  | timestamp1 < timestamp2 = Node (insert l1 leftSubTree) l2 rightSubTree
  | otherwise = Node leftSubTree l2 (insert l1 rightSubTree)
insert _ t@(Node _ (Unknown _) _) = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftSubTree l rightSubTree) = inOrder leftSubTree ++ [l] ++ inOrder rightSubTree

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error x) _ _) = x >= 50
isSevereError _ = False

extractMessage :: LogMessage -> String
extractMessage (Unknown s) = s
extractMessage (LogMessage _ _ s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = fmap extractMessage . filter isSevereError . inOrder . build

