module MerkleHellmanSpec
    ( spec
    )
where

import           MerkleHellman
import           System.Random
import           Test.Hspec

pubKey = [5457, 1663, 216, 6013, 7439]
privKey = (8443, 2550, [171, 196, 457, 1191, 2410])
msg = [0, 1, 0, 1, 1]
crypt = 15115

increasingSum _ [] = True
increasingSum s (a:as)
    | s < a = increasingSum (s+a) as
    | s >= a = False

spec :: Spec
spec = do
    describe "encypting" $ do
        it "should encrypt" $ do
            encrypt pubKey msg `shouldBe` crypt
    describe "decrypting" $ do
        it "should decrypt" $ do
            decrypt privKey crypt `shouldBe` msg
    let g = mkStdGen 2 
        keyPair = generate g 5
        ((m, w, as'), as) =  keyPair in
        describe "generating small numbers" $ do
            it "should generate small keys" $ do
                keyPair `shouldBe` ((3612,1781,[21,53,117,245,501]),[1281,481,2493,2905,117])
            it "should retrun w and m with gcd == 1" $ do
                gcd m w `shouldBe` 1
            it "should keep increasing value of as'" $ do
                as' `shouldSatisfy` (increasingSum 0)
    let g = mkStdGen 3
        keyPair = generate g 100
        ((m, w, as'), as) =  keyPair in
        describe ("generating large numbers with seed " ++ show g) $ do
            it "should retrun w and m with gcd == 1" $ do
                gcd m w `shouldBe` 1
            it "should keep increasing value of as'" $ do
                as' `shouldSatisfy` (increasingSum 0)
            it "should make private key of length 100"$ do
                length as' `shouldBe` 100
            it "should make public key of length 100"$ do
                length as `shouldBe` 100
