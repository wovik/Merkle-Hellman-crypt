module MerkleHellmanIO
    ( savePrivateKey
    , loadPrivateKey
    , savePublicKey
    , loadPublicKey
    , saveKeys
    )
where

import           MerkleHellman
import           System.IO

savePrivateKey :: PrivKey -> String -> IO ()
savePrivateKey key path = writeFile path $ show key

loadPrivateKey :: String -> IO PrivKey
loadPrivateKey path = do
    contents <- readFile path
    let key = read contents
    return key

savePublicKey :: PubKey -> String -> IO ()
savePublicKey key path = writeFile path $ show key

loadPublicKey :: String -> IO PubKey
loadPublicKey path = do
    contents <- readFile path
    let key = read contents
    return key

saveKeys :: KeyPair -> String -> IO ()
saveKeys (priv, pub) path = do
    savePrivateKey priv (path ++ ".priv")
    savePublicKey pub (path ++ ".pub")
