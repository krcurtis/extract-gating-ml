--------------------------------------------------------------------------------
--- Basic GatingML structures
--- Copyright 2022 Keith Curtis
--- Copyright 2022 Fred Hutchinson Cancer Center


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GatingML where


import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes)

import GHC.Generics
import Control.DeepSeq

--------------------------------------------------------------------------------

data Gate = RectangleGate
            { rg_id :: T.Text
            , rg_parent_id :: Maybe T.Text
            , rg_x_dim :: GatingDimension
            , rg_y_dim :: GatingDimension
            }
            | PolygonGate
              { pg_id :: T.Text
              , pg_parent_id :: Maybe T.Text              
              , pg_x_dim :: GatingDimension
              , pg_y_dim :: GatingDimension
              , pg_points :: [(Double, Double)]
              }
  deriving (Show, Eq, Generic, NFData)



data GatingDimension = GatingDimension
                       { gd_compensation_ref :: Maybe T.Text    -- what is this supposed to mean
                       , gd_transformation_ref :: Maybe T.Text  -- refers to label of transformation to be applied to the data
                       , gd_minimum :: Maybe Double       -- used in rectangle but not polygon gates
                       , gd_maximum :: Maybe Double       -- used in rectangle but not polygon gates
                       , gd_name :: T.Text -- refers to FCS channel?
                       }
  deriving (Show, Eq, Generic, NFData)

-- to represent the already-inverted spillover or cross-talk matrix 
data Compensation = Compensation
                    { c_fluorochromes :: [T.Text]
                    , c_spectrum_rows :: [[Double]]
                    }
  deriving (Show, Eq, Generic, NFData)
                    
