{-# LANGUAGE
    OverloadedStrings
    #-}

module Main where

import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import qualified Data.ByteString.Char8 as BC
import Data.JsonStream.Parser
import qualified Data.Text as T

import Debug.Trace

main :: IO ()
main = putStrLn "hello world"



jsonParse :: Conduit BC.ByteString (ResourceT IO) (T.Text, Value)
jsonParse = doParse parseOutput
  where
    parseOutput :: ParseOutput (T.Text, Value)
    parseOutput = runParser (objectItems value)

    doParse :: ParseOutput (T.Text, Value) -> Conduit BC.ByteString (ResourceT IO) (T.Text, Value)
    doParse out = case out of
        ParseYield value newOutput  -> do
            yield $ value
            doParse newOutput
        ParseNeedData cont ->
            awaitForever $ \i -> do
                doParse (cont i)
        ParseDone remaining -> return ()
        ParseFailed err -> error err


jsonDump :: Sink (T.Text, Value) (ResourceT IO) [(T.Text, Value)]
jsonDump = go []
  where
    go acc = do
        val <- await
        case val of
            Just v -> trace (show v) $ go (v:acc)
            Nothing -> return acc


{-
jsonAnalysis :: (a -> b) -> Conduit a (ResourceT IO) b
jsonAnalysis
-}
