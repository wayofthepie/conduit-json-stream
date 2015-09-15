{-# LANGUAGE
    OverloadedStrings
    #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.JsonStream.Parser
import qualified Data.Text as T
import Control.Monad.Trans.Resource

import Debug.Trace

main :: IO ()
main = putStrLn "hello world"

jsonParse :: Show a => Parser a -> Conduit BC.ByteString (ResourceT IO) a
jsonParse p = doParse $ parseOutput p
  where
    parseOutput :: Show a => Parser a -> ParseOutput a
    parseOutput p = runParser p

    doParse :: Show a => ParseOutput a -> Conduit BC.ByteString (ResourceT IO) a
    doParse out = case out of
        ParseYield value newOutput  -> do
            yield $ value
            doParse newOutput
        ParseNeedData cont ->
            awaitForever $ \i -> do
                doParse (cont i)
        ParseDone remaining -> return ()
        ParseFailed err -> error err


-- | Function to test the above.
jsonC :: String -> IO [(T.Text, Value)]
jsonC fp = runResourceT $
    let p = objectItems value :: Parser (T.Text, Value)
    in CB.sourceFile fp
        $= CB.lines
        $$ jsonParse p
        $= CL.consume
