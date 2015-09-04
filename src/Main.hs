{-# LANGUAGE OverloadedStrings #-}

import Data.Map (fromList, findWithDefault, Map, insert)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
-- import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Prelude hiding (lookup, take)
import Data.ByteString.Char8 (ByteString, pack)
import System.IO (Handle, hSetBinaryMode, hSetBuffering, BufferMode(..))
import Data.Attoparsec.ByteString.Char8 hiding (takeTill)
import qualified Data.ByteString as S
import Control.Applicative

type Key   = ByteString
type Value = ByteString
type DB    = Map Key Value

data Command = Get Key
             | Set Key Value
             | Unknown
             deriving (Eq, Show)

-- Redis data format
-- *3   -- 3 args
-- $3   -- 3 character arg
-- set
-- $4   -- 4 character arg
-- name
-- $4   -- 4 character arg
-- Nico

data Reply = Bulk (Maybe ByteString)   -- Just a string, like "set"
           | MultiBulk (Maybe [Reply]) -- The whole message
           deriving (Eq, Show)

parseReply :: Reply -> Maybe Command
parseReply (MultiBulk (Just xs)) =
    case xs of
        [Bulk (Just "get"), Bulk (Just a)]                  -> Just $ Get a
        [Bulk (Just "set"), Bulk (Just a), Bulk (Just b)]   -> Just $ Set a b
        _                                                   -> Just Unknown
parseReply _ = Nothing

bulk :: Parser Reply
bulk = Bulk <$> do -- I still don't understand attoparsec's use of <$>
    len <- char '$' *> signed decimal <* endOfLine
    if len < 0
        then return Nothing
        else Just <$> take len <* endOfLine

multiBulk :: Parser Reply
multiBulk = MultiBulk <$> do
    len <- char '*' *> signed decimal <* endOfLine
    if len < 0
        then return Nothing
        else Just <$> count len replyParser

replyParser :: Parser Reply
replyParser = choice [bulk, multiBulk]


atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

updateValue :: (DB -> DB) -> TVar DB -> IO ()
updateValue fn x = atomically $ modifyTVar x fn

main :: IO ()
main = do
    putStrLn "Hi Redis!"
    --database <- atomically $ newTVar $ fromList [("__version__", "0.1.0")]
