
module Init

		labelButtons,clearRender,clearTree,clearText,
		toNoTextRegion,toTextRegion,
		toMouseRegion,

where
import Params (Command(..),Button(..),delimiter,renderHeight,windowWidth,

import Stdlib (map2,mapcat,seQuence)
import MGRlib (setEvent, setMode, setCursor, textReset, clear,

initialiseMouse :: String
initialiseMouse = seQuence [	setEvent 1 "%p\n",

initialiseScreen :: String
initialiseScreen =  	seQuence [	setMode 7,

					drawScreen,
					toNoTextRegion]

drawScreen :: String
drawScreen = 	seQuence [	line [0,renderHeight+1,windowWidth,renderHeight+1],

drawMouse :: String
drawMouse = seQuence [	movePrintTo mouseCaptionAcross mouseCaptionDown "Mouse",
		    		drawBox mouseBox,
				drawBox button1Box,
				drawBox button2Box,
				drawBox button3Box,
				writeVert button1TextOrigin (map2 spacer "RESERVED" "SYSTEM !"),
		    		labelButtons ("BUTTON   ","DISABLED ") ("SELECT   ","OPERATION")]

labelButtons :: (String,String) -> (String,String) -> String
labelButtons (label2_1,label2_2) (label3_1,label3_2)
			= seQuence [	toMouseRegion,
			  		writeVert button2TextOrigin (map2 spacer label2_1 label2_2),
					writeVert button3TextOrigin (map2 spacer label3_1 label3_2),
			  		toNoTextRegion]

spacer :: Char -> Char -> String
spacer x y = x:gap:[y]

clearRender :: String
clearRender = clearRegion renderRegion

clearTree :: String
clearTree = clearRegion treeRegion

clearButton :: Button -> String
clearButton (_,d,_) = clearRegion (button d)

clearRegion :: [Int] -> String
clearRegion region = seQuence [setTextRegion (inside region), clear, toNoTextRegion]

clearText :: String
clearText = clear

inside :: [Int] -> [Int]
inside [top,left,width,height] = [top+1,left+1,width-2,height-2]

toScreenRegion :: String
toScreenRegion = setTextRegion windowRegion

toNoTextRegion :: String
toNoTextRegion = setTextRegion noTextRegion

toTextRegion :: String
toTextRegion = setTextRegion textRegion

toMouseRegion :: String
toMouseRegion = setTextRegion mouseRegion

drawButtons :: [Button] -> String
drawButtons = mapcat drawButton

drawButton :: Button -> String
drawButton (_,d,name) = seQuence [ 	printOver textIn (d+textDown) name,
					drawBox [buttonIndent,d,buttonWidth,buttonHeight]  ]

mark :: Command -> String
mark cmd = seQuence [	toScreenRegion,

noMark :: Command -> String
noMark cmd = seQuence [	toScreenRegion,

		where 	
		title (_,d,str) = movePrintTo textIn (d+textDown) str

mapcatButton :: (Button->String) -> Command -> [Button] -> String
mapcatButton f cmd [] = []
mapcatButton f cmd (butt@(cmd',_,_):butts)

drawBox :: [Int] -> String
drawBox [x,y,width,height] = seQuence [	line [x,y,x,y+height],
				 		line [x,y+height,x+width,y+height],
						line [x+width,y+height,x+width,y],
						line [x+width,y,x,y]]

reset :: String
reset = seQuence [mark Quit, textReset, setCursor 0]

indicate :: Command -> [String] -> String
indicate Null _ = ""
indicate cmd str = mark cmd ++ seQuence str ++ noMark cmd

unlabelButtons = labelButtons ("BUTTON   ","DISABLED ")
                               ("SELECT   ","OPERATION")

labelDefinePoly = labelButtons ("COMPLETE ","POLYGON  ")
                                 ("DEFINE   ","VERTEX   ")

labelClassify = labelButtons ("FINISH   ","CLASSIFY ")
                              ("CLASSIFY ","POINT    ")
