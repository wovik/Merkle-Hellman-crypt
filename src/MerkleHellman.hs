module MerkleHellman
    (   
        generate,
        encrypt,
        decrypt
    ) where

import System.Random

type KeyPair = (PrivKey, PubKey)
type PubKey = [Integer]
type PrivKey = (Integer, Integer, [Integer])
type Message = [Integer]
type Cryptogram = Integer

generatePrivateKey :: StdGen -> (PrivKey, StdGen)
generatePrivateKey g = ((m, w, rods), newGen'')
    where
        (m, newGen) = randomR (2^201 + 1, 2^202 - 1) g
        (w', newGen') = randomR (2, m - 2) g
        w = w' `div` gcd m w'
        (rods, newGen'') = generateRods newGen' 100


generateRods :: StdGen -> Integer -> ([Integer], StdGen)
generateRods g 0 = ([], g)
generateRods g i = (restA ++ [a], gen)
    where
        (restA, gen) = generateRods g (i-1)
        (a, newGen) = generateA gen i

generateA :: StdGen -> Integer -> (Integer, StdGen)
generateA g i = randomR ((2^(i-1)-1)*2^100, (2^(i-1))*2^100) g

generatePublicKey :: StdGen -> PrivKey ->(PubKey, StdGen)
generatePublicKey g = undefined

generate :: StdGen -> KeyPair
generate g = (priv, [])
    where
        (priv, newGen) = generatePrivateKey g

encrypt :: PubKey -> Message -> Cryptogram
encrypt [] [] = 0
encrypt (key:keys) (msg:msgs) = msg*key + (encrypt keys msgs)

eGCD :: Integer -> Integer -> (Integer,Integer,Integer)
eGCD 0 b = (b, 0, 1)
eGCD a b = let (g, s, t) = eGCD (b `mod` a) a
           in (g, t - (b `div` a) * s, s)

decryptImpl s [] = []
decryptImpl s (a:as) = decryptImpl s1 as ++ [msg]
    where
        (msg, s1) = if s >= a then (1,s-a) else (0,s)

decrypt :: PrivKey -> Cryptogram -> Message
decrypt (m, w, as) crypt = decryptImpl s (reverse as)
    where
        (_, invW, _) = eGCD w m
        s = crypt*invW `mod` m
        
