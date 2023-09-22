--------------------------------------------------------------------------------
--- Copyright 2023 Fred Hutchinson Cancer Center

{-# LANGUAGE OverloadedStrings #-}


module GateConversionSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

import ParseDiva
import IntermediateGate
import GatingML
import Logicle
import DivaTransform (log10)

import Diva2Intermediate
import Intermediate2GatingML

close_to :: Double -> Double -> Bool
close_to a b = abs (a-b) < tolerance
  where tolerance = 0.0001

spec :: Spec
spec = describe "check steps in gate conversion from Diva to GatingML" $ do
  let x1 = 20.0
      x2 = 5000.0
      y1 = 15.0
      y2 = 12000.0

      x3 = -10.0
      x4 = 1000.0

      lx1 = 1.3010299956639813 -- log10 x1
      lx2 = 3.6989700043360187 -- log10 x2
      ly1 = 1.1760912590556813 -- log10 y1
      ly2 = 4.079181246047625  -- log10 x2

      paramT = 2.0**18
      paramM = 4.5
      rx1 = 1.0 + (1.0/paramM) * log10 (x1/paramT)
      rx2 = 1.0 + (1.0/paramM) * log10 (x2/paramT)
      ry1 = 1.0 + (1.0/paramM) * log10 (y1/paramT)
      ry2 = 1.0 + (1.0/paramM) * log10 (y2/paramT)

      r1 = Region RectangleRegion "CH1" "CH2"  [(x1,y1), (x2, y1), (x2, y2), (x1, y2)]
      r2 = Region RectangleRegion "CH3" "CH4"  [(lx1,ly1), (lx2, ly1), (lx2, ly2), (lx1, ly2)]
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

  it "check linear gate to GatingML record" $ do
    let i1 = convert_diva_gate g1 ["CH2"]
    let m = Map.empty
    let f1 = convert_intermediate_gate i1 m "Comp_M"

    rg_id f1 `shouldBe` "P1"
    rg_parent_id f1 `shouldBe` Nothing
    rg_x_dim f1 `shouldBe` GatingDimension Nothing Nothing (Just x1) (Just x2) "CH1"
    rg_y_dim f1 `shouldBe` GatingDimension (Just "Comp_M") Nothing (Just y1) (Just y2) "CH2"


  it "check log intermediate gate" $ do
    let i2 = convert_diva_gate g2 ["CH4"]

    brg_id i2 `shouldBe` "P2"
    brg_parent_id i2 `shouldBe` (Just "P1")
    brg_x_compensated i2 `shouldBe` False
    brg_y_compensated i2 `shouldBe` True
    brg_x_transform i2 `shouldBe` (Log paramT paramM)
    brg_y_transform i2 `shouldBe` (Log paramT paramM)
    brg_x_channel i2 `shouldBe` "CH3"
    brg_y_channel i2 `shouldBe` "CH4"
    (fst . brg_x_range) i2 `shouldSatisfy` close_to rx1
    (snd . brg_x_range) i2 `shouldSatisfy` close_to rx2
    (fst . brg_y_range) i2 `shouldSatisfy` close_to ry1
    (snd . brg_y_range) i2 `shouldSatisfy` close_to ry2

  it "check log gate to GatingML record" $ do
    let i2 = convert_diva_gate g2 ["CH4"]
    let m = Map.fromList [(Log paramT paramM, "Log1")]
    let f2 = convert_intermediate_gate i2 m "Comp_M"

    rg_id f2 `shouldBe` "P2"
    rg_parent_id f2 `shouldBe` Just "P1"

    (gd_compensation_ref . rg_x_dim $ f2) `shouldBe` Nothing
    (gd_transformation_ref . rg_x_dim $ f2) `shouldBe` Just "Log1"
    (gd_minimum . rg_x_dim $ f2) `shouldBe` Just (fst . brg_x_range $ i2)
    (gd_maximum . rg_x_dim $ f2) `shouldBe` Just (snd . brg_x_range $ i2)
    (gd_name . rg_x_dim $ f2) `shouldBe` "CH3"

    (gd_compensation_ref . rg_y_dim $ f2) `shouldBe` Just "Comp_M"
    (gd_transformation_ref . rg_y_dim $ f2) `shouldBe` Just "Log1"
    (gd_minimum . rg_y_dim $ f2) `shouldBe` Just (fst . brg_y_range $ i2)
    (gd_maximum . rg_y_dim $ f2) `shouldBe` Just (snd . brg_y_range $ i2)
    (gd_name . rg_y_dim $ f2) `shouldBe` "CH4"

  it "check (limited) biexponential intermediate gate" $ do
    let i4 = convert_diva_gate g4 ["CH5"]
        paramA = 0.0

    brg_id i4 `shouldBe` "P4"
    brg_parent_id i4 `shouldBe` (Just "P3")
    brg_x_compensated i4 `shouldBe` True
    brg_y_compensated i4 `shouldBe` False
    (b_T . brg_x_transform) i4 `shouldBe` paramT
    (b_M . brg_x_transform) i4 `shouldBe` paramM
    (b_A . brg_x_transform) i4 `shouldBe` paramA    
    (b_T . brg_y_transform) i4 `shouldBe` paramT
    (b_M . brg_y_transform) i4 `shouldBe` paramM
    (b_A . brg_y_transform) i4 `shouldBe` paramA    
    brg_x_channel i4 `shouldBe` "CH5"
    brg_y_channel i4 `shouldBe` "CH6"
    --(fst . brg_x_range) i4 `shouldSatisfy` close_to rx1
    --(snd . brg_x_range) i4 `shouldSatisfy` close_to rx2
    --(fst . brg_y_range) i4 `shouldSatisfy` close_to ry1
    --(snd . brg_y_range) i4 `shouldSatisfy` close_to ry2

  it "check (limited) biexponential intermediate gate to GatingML record" $ do
    let i4 = convert_diva_gate g4 ["CH5"]
        paramW = (b_W . brg_x_transform) i4
        paramA = 0.0    
        m = Map.fromList [(Biexponential paramT paramW paramM paramA, "Logicle1")]
        f4 = convert_intermediate_gate i4 m "Comp_M"

    rg_id f4 `shouldBe` "P4"
    rg_parent_id f4 `shouldBe` Just "P3"

    (gd_compensation_ref . rg_x_dim $ f4) `shouldBe` Just "Comp_M"
    (gd_transformation_ref . rg_x_dim $ f4) `shouldBe` Just "Logicle1"
    --(gd_minimum . rg_x_dim $ f4) `shouldBe` Just (fst . brg_x_range $ i2)
    --(gd_maximum . rg_x_dim $ f4) `shouldBe` Just (snd . brg_x_range $ i2)
    (gd_name . rg_x_dim $ f4) `shouldBe` "CH5"

    (gd_compensation_ref . rg_y_dim $ f4) `shouldBe` Nothing
    (gd_transformation_ref . rg_y_dim $ f4) `shouldBe` Just "Logicle1"
    --(gd_minimum . rg_y_dim $ f4) `shouldBe` Just (fst . brg_y_range $ i2)
    --(gd_maximum . rg_y_dim $ f4) `shouldBe` Just (snd . brg_y_range $ i2)
    (gd_name . rg_y_dim $ f4) `shouldBe` "CH6"



  it "check proper handling of negative infinity in rectangle gate when converted to GatingML record" $ do

    let x2 = 5000.0
        y2 = 12000.0
        lx1 = log 0 
        lx2 = 3.6989700043360187 -- log10 x2
        ly1 = log 0 
        ly2 = 4.079181246047625  -- log10 x2
    
        paramT = 2.0**18
        paramM = 4.5
        
        rx2 = 1.0 + (1.0/paramM) * log10 (x2/paramT)
        ry2 = 1.0 + (1.0/paramM) * log10 (y2/paramT)

        r2 = Region RectangleRegion "CH3" "CH4"  [(lx1,ly1), (lx2, ly1), (lx2, ly2), (lx1, ly2)]

        i2 = BasicRectangleGate { brg_id = "P2"
                                , brg_parent_id = Just "P1"
                                , brg_x_compensated = False
                                , brg_y_compensated = True
                                , brg_x_transform = Log paramT paramM
                                , brg_y_transform = Log paramT paramM
                                , brg_x_channel = "CH3"
                                , brg_y_channel = "CH4"
                                , brg_x_range = (lx1, lx2)
                                , brg_y_range = (ly1, ly2) }

    let m = Map.fromList [(Log paramT paramM, "Log1")]
    let f2 = convert_intermediate_gate i2 m "Comp_M"

    rg_id f2 `shouldBe` "P2"
    rg_parent_id f2 `shouldBe` Just "P1"

    (gd_compensation_ref . rg_x_dim $ f2) `shouldBe` Nothing
    (gd_transformation_ref . rg_x_dim $ f2) `shouldBe` Just "Log1"
    (gd_minimum . rg_x_dim $ f2) `shouldBe` Nothing
    (gd_maximum . rg_x_dim $ f2) `shouldBe` Just (snd . brg_x_range $ i2)
    (gd_name . rg_x_dim $ f2) `shouldBe` "CH3"

    (gd_compensation_ref . rg_y_dim $ f2) `shouldBe` Just "Comp_M"
    (gd_transformation_ref . rg_y_dim $ f2) `shouldBe` Just "Log1"
    (gd_minimum . rg_y_dim $ f2) `shouldBe` Nothing
    (gd_maximum . rg_y_dim $ f2) `shouldBe` Just (snd . brg_y_range $ i2)
    (gd_name . rg_y_dim $ f2) `shouldBe` "CH4"
