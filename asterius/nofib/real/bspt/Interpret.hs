
module Interpret

where

import Params (Button(..),Command(..),delimiter,buttonIndent,buttonWidth,buttons,

import EuclidGMS (inScreen,Point(..),mkPoint)
import GeomNum
import Stdlib (between,splitAt_YORK)

type Operation = (Command,Points)

type Points = [Point]

type Operations = [Operation]

interpret :: [String] -> Operations
interpret (head:residue) = (operation:interpret residue' )

toOperation :: Command -> [String] -> (Operation,[String])
toOperation Polygon str = ((Polygon,mkPoints points),out)

toOperation Union str = ((Union,mkPoints points),out)

toOperation Intersect str = ((Intersect,mkPoints points),out)

toOperation Subtract str = ((Subtract,mkPoints points),out)

toOperation Classify str = ((Classify,mkPoints points),out)

toOperation Complement str = ((Complement,[]),str)
toOperation Partition str = ((Partition,[]),str)
toOperation Render str = ((Render,[]),str)
toOperation Area str = ((Area,[]),str)
toOperation Quit str = ((Quit,[]),str)
toOperation Null str = ((Null,[]),str)

mkPoints :: [String] -> Points
mkPoints = filter inScreen.map mkPoint

toCommand :: Point -> Command
toCommand (Pt x y) | x<fromIntegral buttonIndent ||

search :: Numb -> [Button] -> Button
search y [] = (Null,0,"")
search y ((command,d,str):butts) | between (fromIntegral d)
                              (fromIntegral buttonHeight) y = (command,d,str)
search y ((command,d,str):butts) = search y butts
