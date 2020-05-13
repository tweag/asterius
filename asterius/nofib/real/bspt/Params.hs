
module Params where

data Command = Polygon | Union | Intersect | Subtract | Complement | Partition
                | Render | Classify | Area | Quit | Null deriving (Eq)

--partain: type String = [Char]

type Button = (Command,Int,String)

delimiter :: String
delimiter = "         "

blank :: String
blank = "              "

buttonHeight :: Int
buttonHeight = 40
buttonWidth :: Int
buttonWidth = 110
buttonIndent :: Int
buttonIndent = 5

textIn :: Int
textIn = buttonIndent+4
textDown :: Int
textDown =  30

renderRegion :: [Int]
renderRegion = [renderLeft,renderTop,windowWidth,renderHeight]

renderTop :: Int
renderTop = 0
renderLeft :: Int
renderLeft = 120
renderHeight :: Int
renderHeight = 510
renderWidth :: Int
renderWidth = 780

windowRegion :: [Int]
windowRegion = [0,0,windowWidth,windowHeight]

textRegion :: [Int]
textRegion = [0,(renderHeight+5),(renderLeft-5),50]

noTextRegion :: [Int]
noTextRegion = treeRegion

treeRegion :: [Int]
treeRegion = [renderLeft,(renderHeight+5),
                                (windowWidth-renderLeft),(windowHeight-renderHeight)]

mouseRegion :: [Int]
mouseRegion = [0,0,windowWidth,windowHeight]

windowLeft :: Int
windowLeft = 100
windowTop :: Int
windowTop = 50
windowWidth :: Int
windowWidth = 900
windowHeight :: Int
windowHeight = 700

mouseBox :: [Int]
mouseBox = [mouseLeft,mouseTop,mouseWidth,mouseHeight]
mouseLeft :: Int
mouseLeft = buttonIndent
mouseTop :: Int
mouseTop = renderHeight+55
mouseWidth :: Int
mouseWidth = (renderLeft-10)
mouseHeight :: Int
mouseHeight = mbuttonHeight+18

button1Box :: [Int]
button1Box = [(buttonIndent+5),(renderHeight+60),mbuttonWidth,mbuttonHeight]
button2Box :: [Int]
button2Box = [(buttonIndent+40),(renderHeight+60),mbuttonWidth,mbuttonHeight]
button3Box :: [Int]
button3Box = [(buttonIndent+75),(renderHeight+60),mbuttonWidth,mbuttonHeight]

button1TextOrigin,button2TextOrigin,button3TextOrigin :: (Int,Int)
button1TextOrigin = ((buttonIndent+8),(renderHeight+75))
button2TextOrigin = ((buttonIndent+43), (renderHeight+75))
button3TextOrigin = ((buttonIndent+78), (renderHeight+75))

mbuttonWidth :: Int
mbuttonWidth = 30
mbuttonHeight :: Int
mbuttonHeight = 104

mouseCaptionDown,mouseCaptionAcross :: Int
mouseCaptionDown = (renderHeight+74+mbuttonHeight)
mouseCaptionAcross = buttonIndent+38

buttons :: [Button]
buttons = [primitiveButton,unionButton,intersectButton,subtractButton,
		complementButton, partitionButton, renderButton,
				classifyButton, areaButton, quitButton]

primitiveButton :: Button
primitiveButton = (Polygon,10,	"   Polygon    ")
unionButton :: Button
unionButton = (Union,60,		"    Union     ")
intersectButton :: Button
intersectButton = (Intersect,110,	"  Intersect   ")
subtractButton :: Button
subtractButton = (Subtract,160,	"   Subtract   ")
complementButton :: Button
complementButton = (Complement,210,	"  Complement  ")
partitionButton :: Button
partitionButton = (Partition,260,	"Partitionings ")
renderButton :: Button
renderButton = (Render,310,		"    Render    ")
classifyButton :: Button
classifyButton = (Classify,360,	"Classify Point")
areaButton :: Button
areaButton = (Area,410,		"     Area     ")
quitButton :: Button
quitButton = (Quit,460,		"     Quit     ")

button :: Int -> [Int]
button d = [buttonIndent,d,buttonWidth,buttonHeight]	

mouseDispx, mouseDispy :: Int
mouseDispx = 5
mouseDispy = 4

gap = ' '
