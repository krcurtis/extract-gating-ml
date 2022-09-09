--------------------------------------------------------------------------------
--- Copyright 2022 Fred Hutchinson Cancer Center
--- Print nicely formatted text table

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}


module PrettyPrintTable where

import Prelude hiding ((<>))

import qualified Text.PrettyPrint.Boxes as Box
import Text.PrettyPrint.Boxes  ((<+>), (<>))

import qualified Data.List as L
import Data.Tuple.Utils (fst3, snd3, thd3)



--------------------------------------------------------------------------------

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

thd4 :: (a,b,c,d) -> c
thd4 (_,_,c,_) = c

fth4 :: (a,b,c,d) -> d
fth4 (_,_,_,d) = d



--------------------------------------------------------------------------------

display_column3_table :: (String, String, String) -> [(String, String, String)] -> IO ()
display_column3_table (label1, label2, label3) rows = do
    let vals1 = map fst3 rows
        vals2 = map snd3 rows
        vals3 = map thd3 rows
        n = length rows
        column1 = stylized_column Box.left label1 vals1
        column2 = stylized_column Box.left label2 vals2
        column3 = stylized_column Box.right label3 vals3
        sep = stylized_separator n
        table =  column1  <> sep <> column2 <> sep <> column3
        --table =  column1  <+> column2 <+> column3
    Box.printBox table


display_column4_table :: (String, String, String, String) -> [(String, String, String, String)] -> IO ()
display_column4_table (label1, label2, label3, label4) rows = do
    let vals1 = map fst4 rows
        vals2 = map snd4 rows
        vals3 = map thd4 rows
        vals4 = map fth4 rows
        n = length rows
        column1 = stylized_column Box.left label1 vals1
        column2 = stylized_column Box.left label2 vals2
        column3 = stylized_column Box.right label3 vals3
        column4 = stylized_column Box.right label4 vals4
        sep = stylized_separator n
        --table =  column1  <+> column2 <+> column3 <+> column4
        table =  column1  <> sep <> column2 <> sep <> column3 <> sep <> column4
    Box.printBox table


display_many_column_table :: [String] -> [[String]] -> IO ()
display_many_column_table header rows = do
    let n_rows = length rows
        column_vals = L.transpose rows
        n_cols = length columns
        alignments = Box.left : Box.left : repeat Box.right
        columns = map (\(x,y,z) -> stylized_column x y z) (zip3 alignments header column_vals)
        sep = stylized_separator n_rows
        table =  Box.hcat Box.top (L.intersperse sep columns)
    Box.printBox table





stylized_column :: Box.Alignment -> String -> [String] -> Box.Box
stylized_column align header rows = let header_box = Box.text header
                                        rest_box = Box.vcat align (map Box.text rows)
                                        header_len = Box.cols header_box
                                        rest_len   = Box.cols rest_box
                                        separator_len = max header_len rest_len
                                        separator_box = Box.text (replicate separator_len '-')
                                    in
                                        Box.vcat align [header_box, separator_box, rest_box]


stylized_separator :: Int -> Box.Box
stylized_separator n_basic_rows = let header_box = Box.text " | "
                                      join_box   = Box.text "-+-"
                                      rest_box   = Box.vcat Box.left . map Box.text . replicate n_basic_rows $ " | "
                                  in
                                      Box.vcat Box.left [header_box, join_box, rest_box]
