{-# LANGUAGE
    OverloadedStrings
    #-}

module Main where

import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.JsonStream.Parser
import Control.Monad.Trans.Resource


main :: IO ()
main = putStrLn "hello world"

jsonParse :: Parser a -> Conduit BC.ByteString (ResourceT IO) a
jsonParse p = doParse $ parseOutput p
  where
    parseOutput :: Parser a -> ParseOutput a
    parseOutput p = runParser p

    doParse :: ParseOutput a -> Conduit BC.ByteString (ResourceT IO) a
    doParse out = case out of
        ParseYield value newOutput  -> do
            yield $ value
            doParse newOutput
        ParseNeedData cont ->
            awaitForever $ \i -> do
                doParse (cont i)
        ParseDone remaining -> return ()
        ParseFailed err -> error err


-- | Function to test the jsonParse conduit.
-- E.g
-- @
--  jsonC "example/test2.json" $
--      (objectItems value :: Parser (T.Text, Value))
-- @
jsonC :: String -> Parser a -> IO [a]
jsonC f p = runResourceT $
    CB.sourceFile f
        $= CB.lines
        $$ jsonParse p
        $= CL.consume
