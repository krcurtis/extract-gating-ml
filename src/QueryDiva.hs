--------------------------------------------------------------------------------
--- Copyright 2022 Fred Hutchinson Cancer Center
--- Pull out various bits of information from DivaInfo record

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}


module QueryDiva where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes, fromJust, isJust, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as L

import ParseDiva


--------------------------------------------------------------------------------

data GateNode = GateNode { gn_label :: T.Text
                         , gn_gate :: DivaGate
                         , gn_branches :: [ GateNode ]
                         }
               | RootNode { rn_label :: T.Text
                          , rn_branches :: [GateNode]
                          }
  deriving (Show, Eq)



--------------------------------------------------------------------------------

show_tube_gates :: DivaInfo -> IO ()
show_tube_gates DivaInfo{..} = do
  let records = [ (s, dt_tube_name t, length . dt_gates $ t ) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t, g) -> putStrLn $ "Specimen: " <> s <> "  Tube: " <> t <> "  n_gates: " <> (show g)) records

show_tube_compensation_summary :: DivaInfo -> IO ()
show_tube_compensation_summary DivaInfo{..} = do
  let records = [ (s, dt_tube_name t, length . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t, c) -> putStrLn $ "Specimen: " <> s <> "  Tube: " <> t <> "  Number parameters with compensation: " <> (show c)) records

show_tube_info :: DivaInfo -> IO ()
show_tube_info DivaInfo{..} = do
  let records = [ (s, dt_tube_name t, length . dt_gates $ t, length . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t,g,c) -> putStrLn $ "Specimen: " <> s <> "  Tube: " <> t <> "  n_gates: " <> (show g) <> "  compensation params: " <> (show c)) records

show_diva_info :: DivaInfo -> IO ()
show_diva_info DivaInfo{..} = do
  putStrLn $ "Global Worksheet n_gates: " <> (show . length $ di_global_worksheet_gates) <> "  compensation params: " <> (show . length $ di_global_worksheet_compensation_info )
  putStrLn $ L.intercalate "\t" ["Specimen", "Tube", "n_gates", "n_compensation_params"]
  let records = [ (s, dt_tube_name t, length . dt_gates $ t, length . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t,g,c) -> putStrLn $ L.intercalate "\t" [s, t, show g, show c]) records


find_specimen_tube_gates :: DivaInfo -> String -> String -> Maybe [DivaGate]
find_specimen_tube_gates DivaInfo{..} specimen tube_label = do
    tubes <- Map.lookup specimen di_specimen_tubes
    tube <- L.find (\t -> tube_label == dt_tube_name t) tubes
    return . dt_gates $ tube

              


find_gate_with_name :: [DivaGate] -> String -> Maybe DivaGate
find_gate_with_name gates gate_name = L.find (\g -> text_name == dg_name g) gates
  where
    text_name = T.pack gate_name


find_all_gates_with_same_name :: String -> DivaInfo -> [(String, DivaGate)]
find_all_gates_with_same_name gate_name DivaInfo{..} = global_gate ++ tube_gates
  where
    global_gate = case (find_gate_with_name di_global_worksheet_gates gate_name) of
                    Nothing -> []
                    Just g -> [("Global worksheet", g)]
                    
    find_tube_gates :: (String, DivaTube) -> Maybe (String, DivaGate)
    find_tube_gates (specimen,tube) = do
      gate <- find_gate_with_name (dt_gates tube) gate_name
      return (specimen <> "/" <> (dt_tube_name $ tube), gate)
    tube_gates = catMaybes . map find_tube_gates $ [ (s,t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes]
    

show_all_compensated_channels :: DivaInfo -> IO ()
show_all_compensated_channels DivaInfo{..} = do
  putStrLn $ "Global Worksheet compensated channels: " <> (L.intercalate ","  (map fst di_global_worksheet_compensation_info))
  putStrLn $ L.intercalate "\t" ["Specimen", "Tube", "compensation_channels"]
  let records = [ (s, dt_tube_name t, map fst . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t,cs) -> putStrLn $ L.intercalate "\t" [s, t, L.intercalate "," cs]) records



{-
show_gates_with_same_name :: DivaInfo -> String -> IO ()
-}


--------------------------------------------------------------------------------
-- Gate Hierarchy stuff
-- assume gates in order in list
-- assumes Diva Gates have parent IDs encoded as the full path like "All Events\P1\P2\P3\P4\P5\P6\P7\P8"

diva_default_root_node = RootNode "All Events" []

add_node_to_hierarchy :: GateNode -> [T.Text] -> DivaGate -> GateNode

add_node_to_hierarchy (RootNode root_label branches) (x:[]) g | root_label == x = root'
  where
    label = dg_name g    
    (node, other_branches) = L.partition (\a_node -> gn_label a_node == label) branches
    node' = case node of
              [] -> GateNode label g []
              _ -> error $ "ERROR multiple nodes found with label " <> (T.unpack label)
    branches' = node' : other_branches
    root' = RootNode root_label branches'

add_node_to_hierarchy (RootNode root_label branches) (x:y:ys) g | root_label == x = root'
  where
    (node_list, other_branches) = L.partition (\a_node -> gn_label a_node == y) branches
    node' = case node_list of
              [] -> error $ "ERROR Expected intermediate node " <> (T.unpack y) <> " but node was not found along path " <> (T.unpack . dg_parent $ g)
              [gate_node] -> add_node_to_hierarchy gate_node ys g
              _ -> error $ "ERROR multiple nodes found with label " <> (T.unpack y)
    branches' = node' : other_branches
    root' = RootNode root_label branches'


add_node_to_hierarchy (GateNode label gate branches) [] g = node'
  where
    (node, other_branches) = L.partition (\a_node -> gn_label a_node == dg_name g) branches
    leaf_node = case node of
                  [] -> GateNode (dg_name g) g []
                  _ -> error $ "ERROR multiple nodes found with label " <> (T.unpack . dg_name $ g)
    branches' = leaf_node : other_branches
    node' = GateNode label gate branches'
    
add_node_to_hierarchy (GateNode label gate branches) (y:ys) g = node'
  where
    (node, other_branches) = L.partition (\a_node -> gn_label a_node == y) branches
    intermediate_node = case node of
                          [] -> error $ "ERROR Expected intermediate node " <> (T.unpack y) <> " but node was not found along path " <> (T.unpack . dg_parent $ g)
                          [gate_node] -> add_node_to_hierarchy gate_node ys g
                          _ -> error $ "ERROR multiple nodes found with label " <> (T.unpack y)
    branches' = intermediate_node : other_branches
    node' = GateNode label gate branches'
    



build_hierarchy :: GateNode -> [DivaGate] -> GateNode
build_hierarchy root [] = root
build_hierarchy root (x:xs) = build_hierarchy root' xs
  where
    node_path = T.splitOn "\\" (dg_parent x)
    root' = add_node_to_hierarchy root node_path x


brief_gate_info :: DivaGate -> String
brief_gate_info gate | (r_type . dg_region $ gate) == RectangleRegion = (T.unpack . dg_name $ gate) <> " Rectangle [" <> (T.unpack . r_xparam . dg_region $ gate) <> ", " <> (T.unpack . r_yparam . dg_region $ gate) <> "]"
brief_gate_info gate | (r_type . dg_region $ gate) == PolygonRegion = (T.unpack . dg_name $ gate) <> " Polygon [" <> (T.unpack . r_xparam . dg_region $ gate) <> ", " <> (T.unpack . r_yparam . dg_region $ gate) <> "], n_points: " <> (show . length . r_points . dg_region $ gate)
brief_gate_info gate | (r_type . dg_region $ gate) == IntervalRegion = (T.unpack . dg_name $ gate) <> " Interval [" <> (T.unpack . r_xparam . dg_region $ gate) <> ", " <> (T.unpack . r_yparam . dg_region $ gate) <> "]"
brief_gate_info gate | (r_type . dg_region $ gate) == UnknownRegion = (T.unpack . dg_name $ gate) <> " Unknown [" <> (T.unpack . r_xparam . dg_region $ gate) <> ", " <> (T.unpack . r_yparam . dg_region $ gate) <> "]"


data BranchDisplay = NotVisible | Continues | NodeHere | LastNode

marking_string :: BranchDisplay -> String
marking_string NotVisible = "    "
marking_string Continues  = "|   "
marking_string NodeHere   = "|-- "
marking_string LastNode   = "`-- "


-- for xs, this is from right to left
hierarchy_indent_string :: [BranchDisplay] -> String
hierarchy_indent_string [] = []
hierarchy_indent_string (x:xs) = hierarchy_indent_string xs ++ marking_string x


build_text_hierarchy :: [BranchDisplay] -> [GateNode] -> [String]
build_text_hierarchy previous_levels [] = []
build_text_hierarchy [] ((RootNode label branches):[]) = (T.unpack label) : build_text_hierarchy  [] branches
build_text_hierarchy [] ((GateNode label g branches):[]) = current : build_text_hierarchy levels' branches
  where
    levels' = LastNode : []
    current = hierarchy_indent_string levels' <> (brief_gate_info g)

build_text_hierarchy [] ((GateNode label g branches):gs) = text ++ build_text_hierarchy [] gs
  where
    levels' = NodeHere : []
    current = hierarchy_indent_string levels' <> (brief_gate_info g)
    text = current : build_text_hierarchy levels' branches

build_text_hierarchy (LastNode:others) ((GateNode label g branches):[]) = text
  where
    levels' = LastNode : NotVisible : others
    current = hierarchy_indent_string levels' <> (brief_gate_info g)
    text = current : build_text_hierarchy levels' branches

build_text_hierarchy (LastNode:others) ((GateNode label g branches):gs) = text ++ build_text_hierarchy (LastNode:others) gs
  where
    levels' = NodeHere : NotVisible : others
    current = hierarchy_indent_string levels' <> (brief_gate_info g)
    text = current : build_text_hierarchy levels' branches

build_text_hierarchy (NodeHere:others) ((GateNode label g branches):[]) = text
  where
    levels' = LastNode : Continues : others
    current = hierarchy_indent_string levels' <> (brief_gate_info g)
    text = current : build_text_hierarchy levels' branches

build_text_hierarchy (NodeHere:others) ((GateNode label g branches):gs) = text ++ build_text_hierarchy (NodeHere:others) gs
  where
    levels' = NodeHere : Continues : others
    current = hierarchy_indent_string levels' <> (brief_gate_info g)
    text = current : build_text_hierarchy levels' branches




empty_region :: Region
empty_region = Region { r_type  = RectangleRegion
                      , r_xparam = ""
                      , r_yparam = ""
                      , r_points = [] }

empty_gate :: DivaGate
empty_gate = DivaGate { dg_name = ""
                      , dg_enabled = True
                      , dg_parent = "All Events"
                      , dg_region = empty_region
                      , dg_x_scaled = False
                      , dg_y_scaled = False
                      , dg_x_log = False
                      , dg_y_log = False
                      , dg_x_scale = 0.0
                      , dg_y_scale = 0.0
                      , dg_input = "" }




p1_gate = empty_gate { dg_name = "P1", dg_parent = "All Events"}
p2_gate = empty_gate { dg_name = "P2", dg_parent = "All Events\\P1"}
p3_gate = empty_gate { dg_name = "P3", dg_parent = "All Events\\P1"}
p4_gate = empty_gate { dg_name = "P4", dg_parent = "All Events\\P1\\P2"}
p5_gate = empty_gate { dg_name = "P5", dg_parent = "All Events"}

show_hierarchy :: [DivaGate] -> IO ()
show_hierarchy gates = do
  let root = build_hierarchy diva_default_root_node gates
      lines = build_text_hierarchy [] [root]
  mapM_ putStrLn lines


