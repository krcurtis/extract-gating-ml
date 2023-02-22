--------------------------------------------------------------------------------
--- Copyright 2023 Fred Hutchinson Cancer Center

{-# LANGUAGE OverloadedStrings #-}


module GateConversionSpec where

import Test.Hspec


--------------------------------------------------------------------------------

import ParseDiva
import IntermediateGate
import GatingML
import Logicle


import Diva2Intermediate


close_to :: Double -> Double -> Bool
close_to a b = abs (a-b) < tolerance
  where tolerance = 0.0001

spec :: Spec
spec = describe "check steps in gate conversion from Diva to GatingML" $ do
  let  x1 = 20.0
       x2 = 5000.0
       y1 = 15.0
       y2 = 12000.0
  
       x3 = -10.0
       x4 = 1000.0
       
       r1 = Region RectangleRegion "CH1" "CH2"  [(x1,y1), (x2, y1), (x2, y2), (x1, y2)]
       r2 = Region RectangleRegion "CH3" "CH4"  [(x1,y1), (x2, y1), (x2, y2), (x1, y2)]
       r3 = Region PolygonRegion "CH3" "CH4"  [(x1,y1), (x2, y1), (x2, y2), (x1, y2)]
       r4 = Region RectangleRegion "CH5" "CH6"  [(x3,y1), (x4, y1), (x4, y2), (x3, y2)]

       g1 = DivaGate "P1" True "All Events" r1 False False False False 0 0 "NA"
       g2 = DivaGate "P2" True "All Events\\P1" r2 False False True True 0 0 "NA"
       g3 = DivaGate "P3" True "All Events\\P1\\P2" r3 False False True True 0 0 "NA"
       g4 = DivaGate "P4" True "All Events\\P1\\P2\\P3" r4 True True True True 200 200 "NA"       
       
  it "check linear intermediate gate" $ do
    let i1 = convert_diva_gate g1 ["CH2"]

    brg_id i1 `shouldBe` "P1"
    brg_parent_id i1 `shouldBe` Nothing
    brg_x_compensated i1 `shouldBe` False
    brg_y_compensated i1 `shouldBe` True
    brg_x_transform i1 `shouldBe` Linear
    brg_y_transform i1 `shouldBe` Linear
    brg_x_channel i1 `shouldBe` "CH1"
    brg_y_channel i1 `shouldBe` "CH2"
    brg_x_range i1 `shouldBe` (x1, x2)
    brg_y_range i1 `shouldBe` (y1, y2)
    
    
