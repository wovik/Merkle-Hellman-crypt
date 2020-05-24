module MerkleHellmanSpec (spec) where 

import Test.Hspec
import MerkleHellman

pubKey = [5457,1663,216,6013,7439]
privKey = (8443, 2550, [171,196,457,1191,2410])
msg = [0, 1, 0, 1, 1]
crypt = 15115

spec :: Spec
spec = do
    describe "encypting" $ do
        it "should encrypt" $ do
            encrypt pubKey msg `shouldBe` crypt
    describe "decrypting" $ do
        it "should decrypt" $ do
            decrypt privKey crypt `shouldBe` msg