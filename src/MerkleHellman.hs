module MerkleHellman
  ( generate
  , encrypt
  , decrypt
  , KeyPair
  , PubKey
  , PrivKey
  , Message
  , Cryptogram
  )
where

import           System.Random
import           GHC.Exception                  ( throw )

type KeyPair = (PrivKey, PubKey)
type PubKey = [Integer]
type PrivKey = (Integer, Integer, [Integer])
type Message = [Integer]
type Cryptogram = Integer

generatePrivateKey :: StdGen -> Integer -> PrivKey
generatePrivateKey g n = (m, w, rods)
 where
  (m , newGen )    = randomR (2 ^ (2 * n + 1) + 1, 2 ^ (2 * (n + 1)) - 1) g
  (w', newGen')    = randomR (2, m - 2) g
  w                = w' `div` gcd m w'
  (rods, newGen'') = generateRods newGen' n n


generateRods :: StdGen -> Integer -> Integer -> ([Integer], StdGen)
generateRods g 0 n = ([], g)
generateRods g i n = (restA ++ [a], gen)
 where
  (restA, gen   ) = generateRods g (i - 1) n
  (a    , newGen) = generateA gen i n

generateA :: StdGen -> Integer -> Integer -> (Integer, StdGen)
generateA g i n = randomR ((2 ^ (i - 1) - 1) * 2 ^ n, (2 ^ (i - 1)) * 2 ^ n) g

generatePublicKey :: PrivKey -> PubKey
generatePublicKey (_, _, []) = []
generatePublicKey (m, w, (a : as)) =
  (w * a `mod` m) : generatePublicKey (m, w, as)

generate :: StdGen -> Integer -> KeyPair
generate g n = (priv, pub)
 where
  priv = generatePrivateKey g n
  pub  = generatePublicKey priv

encrypt :: PubKey -> Message -> Cryptogram
encrypt []           []           = 0
encrypt (key : keys) (msg : msgs) = msg * key + (encrypt keys msgs)
encrypt _            _            = error "Message should be same length as key"

eGCD :: Integer -> Integer -> (Integer, Integer, Integer)
eGCD 0 b = (b, 0, 1)
eGCD a b = let (g, s, t) = eGCD (b `mod` a) a in (g, t - (b `div` a) * s, s)

decryptImpl :: Cryptogram -> [Integer] -> Message
decryptImpl s []       = []
decryptImpl s (a : as) = decryptImpl s1 as ++ [msg]
  where (msg, s1) = if s >= a then (1, s - a) else (0, s)

decrypt :: PrivKey -> Cryptogram -> Message
decrypt (m, w, as) crypt = decryptImpl s (reverse as)
 where
  (_, invW, _) = eGCD w m
  s            = crypt * invW `mod` m

