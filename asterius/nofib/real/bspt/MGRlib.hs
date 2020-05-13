
module MGRlib

		 line, setCursor, setEvent, setMode, shapeWindow,
		 stringTo, printOver, movePrintTo,writeVert)

where

command :: String -> [Int] -> String
command str ns = ('\ESC':foldr f "" ns)
		where f n "" = show n ++ str
		      f n s  = show n ++ "," ++ s

setTextRegion :: [Int] -> String
setTextRegion = command "t"

textReset :: String
textReset = ('\ESC':"t")

clear :: String
clear = "\^L"

clearEvent event = command "e" [mapEvent event]

mapEvent event = if ((event == 3) || (event == 4)) then (2-event) else event

func :: Int -> String
func mode = command "b" [mode]

line :: [Int] -> String
line = command "l"   -- x0 y0 x1 y1

setCursor n = command "h" [n]

setEvent event str = command ("e"++str) [mapEvent event, length str]

setMode mode = command "S" [mode]

shapeWindow :: [Int] -> String
shapeWindow = command "W" -- x y w h

stringTo :: Int -> Int -> Int -> String -> String
stringTo win x y str = command ("."++str) [win,x,y,length str]

printOver :: Int -> Int -> String -> String
printOver w h str = concat [func 0, stringTo 0 w h (take (length str) spaces),func 4,stringTo 0 w h str,func 15]

		spaces = ' ':spaces

movePrintTo :: Int -> Int -> String -> String
movePrintTo w h str = concat [func 4, stringTo 0 w h str, func 15]

writeVert :: (Int,Int) -> [String] -> String
writeVert (x,y) [] = []
writeVert (x,y) (a:l) = printOver x y a ++ writeVert (x,(y+11)) l
