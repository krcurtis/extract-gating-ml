--------------------------------------------------------------------------------
--- Copyright 2023 Fred Hutchinson Cancer Center


module LogicleSpec where

import Test.Hspec


--------------------------------------------------------------------------------

import Logicle





close_to :: Double -> Double -> Bool
close_to a b = abs (a-b) < tolerance
  where tolerance = 0.0001

spec :: Spec
spec = describe "Check Logicle transform with GatingML 2.0 spec example values (their Table 8)" $ do
  it "check Logicle(x,1000,1,4,0)" $ do
    --logicle (-10) 1000 1 4 0 `shouldSatisfy` close_to 0.067574
    --logicle  (-5) 1000 1 4 0 `shouldSatisfy` close_to 0.147986
    logicle  (-1) 1000 1 4 0 `shouldSatisfy` close_to 0.228752
    logicle    0  1000 1 4 0 `shouldSatisfy` close_to 0.25
    logicle  0.3  1000 1 4 0 `shouldSatisfy` close_to 0.256384
    logicle    1  1000 1 4 0 `shouldSatisfy` close_to 0.271248
    logicle    3  1000 1 4 0 `shouldSatisfy` close_to 0.312897
    logicle   10  1000 1 4 0 `shouldSatisfy` close_to 0.432426
    logicle  100  1000 1 4 0 `shouldSatisfy` close_to 0.739548
    logicle 1000  1000 1 4 0 `shouldSatisfy` close_to 1

  it "check Logicle(x,1000,1,4,1)" $ do
    --logicle (-10) 1000 1 4 1 `shouldSatisfy` close_to 0.254059
    --logicle  (-5) 1000 1 4 1 `shouldSatisfy` close_to 0.318389
    logicle  (-1) 1000 1 4 1 `shouldSatisfy` close_to 0.383001
    logicle    0  1000 1 4 1 `shouldSatisfy` close_to 0.4
    logicle  0.3  1000 1 4 1 `shouldSatisfy` close_to 0.405107
    logicle    1  1000 1 4 1 `shouldSatisfy` close_to 0.416999
    logicle    3  1000 1 4 1 `shouldSatisfy` close_to 0.450318
    logicle   10  1000 1 4 1 `shouldSatisfy` close_to 0.545941
    logicle  100  1000 1 4 1 `shouldSatisfy` close_to 0.791638
    --logicle 1000  1000 1 4 1 `shouldSatisfy` close_to 1

  it "check Logicle(x,1000,0,4,1)" $ do
    --logicle (-10) 1000 0 4 1 `shouldSatisfy` close_to (-0.200009)
    logicle  (-5) 1000 0 4 1 `shouldSatisfy` close_to (-0.139829)
    logicle  (-1) 1000 0 4 1 `shouldSatisfy` close_to (-0.000856)
    logicle    0  1000 0 4 1 `shouldSatisfy` close_to 0.2
    logicle  0.3  1000 0 4 1 `shouldSatisfy` close_to 0.303776
    logicle    1  1000 0 4 1 `shouldSatisfy` close_to 0.400856
    logicle    3  1000 0 4 1 `shouldSatisfy` close_to 0.495521
    logicle   10  1000 0 4 1 `shouldSatisfy` close_to 0.600009
    logicle  100  1000 0 4 1 `shouldSatisfy` close_to 0.8
    --logicle 1000  1000 0 4 1 `shouldSatisfy` close_to 1


    


-- TODO (maybe different test module?)  test that can convert gate coordinates to GatingML Logicle coordinates

