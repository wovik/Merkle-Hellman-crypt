module Main where

import           MerkleHellman
import           MerkleHellmanIO
import           System.Random
import           System.Environment

encodeMsg :: String -> Message
encodeMsg ('0' : r) = 0 : encodeMsg r
encodeMsg ('1' : r) = 1 : encodeMsg r
encodeMsg _         = []

decodeMsg :: Message -> String
decodeMsg (0 : r) = '0' : decodeMsg r
decodeMsg (1 : r) = '1' : decodeMsg r
decodeMsg _       = []


takeAction :: [String] -> IO ()
takeAction ("gen" : n : file : []) = do
    gen <- newStdGen
    let aLength = read n :: Integer
    let keyPair = generate gen aLength
    saveKeys keyPair file


takeAction ("enc" : file : message : []) = do
    pub <- loadPublicKey file
    let msg = encodeMsg message
    if (length msg /= length pub)
        then putStrLn "Message should be same length as key"
        else do
            let crypt = show $ encrypt pub msg
            putStrLn $ show crypt


takeAction ("dec" : file : crypt : []) = do
    priv <- loadPrivateKey file
    let c   = read crypt :: Integer
    let msg = decodeMsg $ decrypt priv c
    putStrLn $ show msg

takeAction _ = do
    putStrLn "Supported actions are: gen, enc, dec"

main :: IO ()
main = do
    args <- getArgs
    takeAction args

