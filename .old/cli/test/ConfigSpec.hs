module ConfigSpec (spec) where

import qualified Codchi.Config.V012 as V012
import Data.Aeson.Safe
import Test.Hspec

spec :: Spec
spec = do
    describe "basic parsing" $ do
        it "v0.1.2" $ do
            cfg <- decodeFileStrict @V012.Config "test/Config/v012.json"
            cfg `shouldNotBe` Nothing
