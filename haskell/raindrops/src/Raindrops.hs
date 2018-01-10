module Raindrops (convert) where

plingPlangPlong = [(3, "Pling"),
                   (5, "Plang"),
                   (7, "Plong")]

convert input =
  let raindropList = [element |
                      (value, element) <- plingPlangPlong,
                      (mod input value) == 0]
  in if length raindropList == 0
     then show input
     else concat raindropList
