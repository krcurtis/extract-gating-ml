--------------------------------------------------------------------------------
--- Intermediate Gate structures
--- Copyright 2023 Fred Hutchinson Cancer Center


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module IntermediateGate where


import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes)

import GHC.Generics
import Control.DeepSeq

--------------------------------------------------------------------------------


data IntermediateGate = BasicRectangleGate
                        { brg_id :: T.Text
                        , brg_parent_id :: Maybe T.Text
                        , brg_x_compensated :: Bool    -- assume only one compensation matrix is going to be used
                        , brg_y_compensated :: Bool 
                        , brg_x_transform :: Transform
                        , brg_y_transform :: Transform
                        , brg_x_channel :: T.Text
                        , brg_y_channel :: T.Text
                        , brg_x_range :: (Double, Double)
                        , brg_y_range :: (Double, Double)
                        }
                      | BasicPolygonGate
                        { bpg_id :: T.Text
                        , bpg_parent_id :: Maybe T.Text
                        , bpg_x_compensated :: Bool    -- assume only one compensation matrix is going to be used
                        , bpg_y_compensated :: Bool 
                        , bpg_x_transform :: Transform
                        , bpg_y_transform :: Transform
                        , bpg_x_channel :: T.Text
                        , bpg_y_channel :: T.Text
                        , bpg_vertices :: [(Double, Double)]
                        }                      
  deriving (Show, Eq, Generic, NFData)


data Transform = Linear
               | Log { l_T :: Double, l_M :: Double }
               | Biexponential { b_T :: Double
                               , b_W :: Double
                               , b_M :: Double
                               , b_A :: Double }
  deriving (Show, Eq, Ord, Generic, NFData)
                   


is_log_transform :: Transform -> Bool
is_log_transform Linear = False
is_log_transform (Log _ _) = True
is_log_transform (Biexponential _ _ _ _) = False

is_logicle_transform :: Transform -> Bool
is_logicle_transform Linear = False
is_logicle_transform (Log _ _) = False
is_logicle_transform (Biexponential _ _ _ _) = True

