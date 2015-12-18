{-# LANGUAGE OverloadedStrings #-}
module Final where

import System.Random
import Control.Concurrent
import Graphics.Blank
import Data.Fixed
import Data.List (nub)
import qualified Data.Text as Text
import           Data.Monoid((<>))

-- 2 Points Coordinates - Top Left corner and Bottom Right corner to draw a rectangle
data RectCoord = RectCoord { p1 :: (Double, Double), p2 :: (Double, Double) } deriving (Show)
data CarState  = Alive | Dead deriving(Show, Eq)
data CarPosit  = Top | Mid | Bot deriving (Eq, Show)
data MainCar   = MainCar {state :: CarState, position :: CarPosit, body::(Canvas ())}
data EnemyCar  = EnemyCar {frontTop :: (Double,Double), lane :: CarPosit}
data Game      = Game {player :: MainCar, score :: Int, enemies :: [EnemyCar]}


-- All things that are rectangle, but require different colors
streetDraw = fillStyle "#848484" >> strokeStyle "black" >> lineWidth 1 >> fill () >> stroke ()
coverDraw  = fillStyle "#FFFFFF" >> strokeStyle "#FFFFFF" >> lineWidth 1 >> fill () >> stroke ()
brickDraw  = fillStyle "#8A0808" >> strokeStyle "black" >> lineWidth 1 >> fill () >> stroke ()
carBodyDr  = fillStyle "#FA5858" >> strokeStyle "black" >> lineWidth 1 >> fill () >> stroke ()
scoreBDraw = fillStyle "#0A2A12" >> strokeStyle "#FFFFFF" >> lineWidth 2 >> fill () >> stroke ()

-- Draw a rectanlge between two points, top left and bottom right
rectDraw :: (RectCoord, (Canvas ())) -> Canvas ()
rectDraw (coord, funct) = do
                beginPath ()
                moveTo (x1, y1) >> lineTo (x1, y2) >> lineTo (x2, y2) >> lineTo (x2, y1) >>lineTo (x1, y1)
                closePath()
                funct
                where 
                    (x1, y1) = p1 coord
                    (x2, y2) = p2 coord

-- Creates tuples of points for drawing bricks in a rectangular area
brickTuples :: RectCoord -> [RectCoord]
brickTuples (RectCoord (p1,p2) (q1,q2)) = [ RectCoord (a,b)(a+40, b+20) | b<- rows, a<- columns]
                where 
                    columns = dropWhile  (\x->x < p1) (takeWhile (\x->x<q1)[40*m |m<- [0..q1]])
                    rows = dropWhile  (\x->x < p2) (takeWhile (\x->x<q2)[20*n| n<- [0..q2]])


filterPossibleCollisions :: [EnemyCar] -> [EnemyCar]
filterPossibleCollisions [] = []
filterPossibleCollisions (x:xs)= if a < 140 && a > -30 then x : filterPossibleCollisions xs
                                 else filterPossibleCollisions xs
                                    where 
                                        (a,b) = frontTop x

collides :: MainCar -> [EnemyCar] -> Bool
collides plr [] = False
collides plr (x:xs) = if position plr == lane x then True
                      else collides plr xs 

-- Takes coordinate tuples and draws a wall of bricks. Every second row starts drawing from a smaller starting point
-- so the brick edges don't match up in alternate lines.
listDraw :: [RectCoord] -> (Canvas ()) -> Canvas ()
listDraw [] f = stroke ()
listDraw ((RectCoord (p1, p2) (q1, q2)):xs) f = do
        if (mod' p2 40 == 0)
        then do     
                rectDraw((RectCoord (p1-20, p2) (q1-20, q2)),f)
                listDraw xs f
        else do
                rectDraw((RectCoord (p1, p2) (q1, q2)), f)
                listDraw xs f

-- Road line tupes
roadTuples :: Double -> Double -> [RectCoord]
roadTuples up lwr = [RectCoord (a,up)(b,lwr) | (a,b) <- stndpts] where stndpts = [(p*20, p*20+15) | p <-[0..40]]

altRoadTuples :: Double -> Double -> [RectCoord]
altRoadTuples up lwr = [RectCoord (a,up)(b,lwr) | (a,b) <- stndpts] where stndpts = [(p*20-10, p*20+5) | p <-[0..40]]

-- Draws a wall in a given rectangular area by creating brick tuples and then calling the listDraw method.
drawWall :: RectCoord -> Canvas ()
drawWall x = listDraw (brickTuples x) brickDraw

-- Draw road lines
drawRoad :: (Double -> Double -> [RectCoord]) -> Double -> Double -> Canvas ()
drawRoad tup x y = listDraw (tup x y) coverDraw


getCar :: CarPosit -> Int -> MainCar
getCar x key = MainCar {state= Alive, position=z, body= y } 
   where (z, y) = if x == Bot 
             then 
                if key == 38
                then (Mid, (carDraw ((RectCoord (50, 250) (150, 300))))) -- MID
                else (Bot, (carDraw ((RectCoord (50, 375) (150, 425))))) --BOT
             else 
                if x == Mid
                then
                    if key == 38 
                    then (Top, (carDraw ((RectCoord (50, 125) (150, 175))))) -- TOP
                    else
                        if key == 40
                        then (Bot, (carDraw ((RectCoord (50, 375) (150, 425))))) --BOT
                        else (Mid, (carDraw ((RectCoord (50, 250) (150, 300))))) -- MID
                else 
                    if key == 40
                    then (Mid, (carDraw ((RectCoord (50, 250) (150, 300))))) -- MID
                    else (Top, (carDraw ((RectCoord (50, 125) (150, 175))))) -- TOP

drawEnemies :: [EnemyCar] -> Canvas ()
drawEnemies [] = stroke()
drawEnemies (x:xs) = do enemyDraw x >> drawEnemies xs

-- Take a list of Ints (i.e. Random Lanes) and produce enemy cars.
makeEnemies :: [Int] -> [EnemyCar]
makeEnemies [] = []
makeEnemies (x:xs) = 
    (EnemyCar{
        lane= if x == 1 then Top else 
                    if x == 2 then Mid else Bot
        ,frontTop= (600,30)}) : makeEnemies xs


placeEnemies :: Double -> [EnemyCar] -> [EnemyCar]
placeEnemies m [] = []
placeEnemies m (x:xs) = 
    (EnemyCar {
        frontTop= (m, n),
        lane= (lane x)
    }) : placeEnemies (m+400) xs
    where
        n = if (lane x) == Top 
            then 125
            else 
                if (lane x) == Mid
                then 250
                else 375

moveEnemies :: Double -> [EnemyCar] -> [EnemyCar]
moveEnemies m [] = []
moveEnemies m ((EnemyCar (a,b) ln):xs) = (EnemyCar { frontTop= (a-m,b), lane= ln}) : (moveEnemies m xs)

--- Unwraps the (Maybe (Maybe Int)) received from mainControl so loop can interpret
--- which key on the keyboard is pressed.
getCont :: (Maybe (Maybe Int)) -> Int
getCont (Just (Just c)) = c
getCont _ = -1

--- The functions main, loop, mainControl and Draw are the ones that send 
--- stuff to be printed on the Canvas.
--  HERE IS THE MAIN FUNCTION
main :: IO ()
main = blankCanvas 3000 { events = ["keyup","keydown"] } $ \ context -> do
        g <- getStdGen
        let gameState = Game {player = (MainCar{ position= Mid, state= Alive, 
                                                 body= (rectDraw ((RectCoord (50, 175) (150, 225)), brickDraw))}), 
                                        score=0,
                                        enemies= placeEnemies 800 $ makeEnemies $ take 100 (randomRs (1::Int, 3::Int) g)}
        loop context 0 gameState


--- Communicates with mainControl to check for keyDown events
--- receives a new state from the mainControl and then draws it
--- on the canvas
loop :: DeviceContext -> Int -> Game -> IO a
loop context pressed gameState = do
        send context $ do
                let (w,h) = (width context, height context)
                clearRect (0,0,w,h) >> beginPath() >> save()
                draw pressed gameState
                restore()
        mainControl context gameState

--- The real hero! checks conditions, makes a new state and then sends it
--- back to loop so it can ask draw to draw it on the Canvas.
mainControl :: DeviceContext -> Game -> IO a
mainControl context gameState= do
        events <- flush context
        let eveTups = map (\(x,y) -> (eType x, eWhich y)) [(a,a) | a<- events]
        let getVal = getCont $ lookup "keydown" eveTups
        let imdState = if (state (player gameState)) == Alive 
            then Game { player= 
                (getCar (position (player gameState)) getVal), 
                enemies= moveEnemies 10 (enemies gameState), 
                score= 1+(score gameState)}
            else gameState

        let newState = if collides (player imdState) (filterPossibleCollisions (enemies imdState))
                       then Game { player= MainCar {state =Dead, position= position (player imdState),
                                   body= body (player imdState)
                                 }, enemies= enemies imdState, score = score imdState}
                       else imdState
        loop context getVal newState

--- Receives the state from loop and then draws the
--- gameState on canvas. Pressed is an integer that represents what keyboard key is pressed.
draw :: Int -> Game -> Canvas ()
draw pressed gameState= 
            do
                rectDraw ((RectCoord (0, 0) (800, 600)), streetDraw)
                drawWall (RectCoord (0,0) (840,100))
                drawWall (RectCoord (0,450) (840,600 ))

                if (state (player gameState)) == Alive then do
                    if (score gameState) `mod` 10 < 5
                        then do drawRoad roadTuples 210 214 >> drawRoad roadTuples 335 339
                        else do drawRoad altRoadTuples 210 214 >> drawRoad altRoadTuples 335 339
                    -- Player car 
                    body $ (player gameState)
                    -- Enemy cars 
                    drawEnemies (enemies gameState)
                else do
                    fillStyle "#FFFFFF" >> font "80pt Calibri" >> fillText("Game Over",150,212)

                --ScoreBoard
                rectDraw ((RectCoord (570, 10) (780, 90)), scoreBDraw)
                fillStyle "#FFFFFF" >> font "14pt Calibri"
                fillText("Score       :     " <> Text.pack (show (score gameState)),590,35)
                fillText("State       :     " <> Text.pack (show (state (player gameState))),590,54)
                fillText("Position  :     " <> Text.pack (show (position (player gameState))),590,73)

                -- White out the hidden area
                rectDraw ((RectCoord (801, 0) (10000,  10000)), coverDraw)
                rectDraw ((RectCoord (0, 601) (10000, 10000)), coverDraw)



carDraw :: RectCoord -> Canvas ()
carDraw coord = do
                -- Back top wheel
                beginPath ()
                moveTo (x1+12.5, y1-10)
                lineTo (x1+12.5, y1)
                lineTo (x1+37.5, y1)
                lineTo (x1+37.5, y1-10)
                lineTo (x1+37.5, y1-10)
                fillStyle "Black"
                fill ()
                closePath ()
                -- Front Top wheel
                beginPath ()
                moveTo (x2-37.5, y1-7)
                lineTo (x2-37.5, y1)
                lineTo (x2-12.5, y1)
                lineTo (x2-12.5, y1-7)
                lineTo (x2-37.5, y1-7)
                fillStyle "Black"
                fill ()
                -- Back Bottom wheel
                beginPath ()
                moveTo (x1+12.5, y2+10)
                lineTo (x1+12.5, y2)
                lineTo (x1+37.5, y2)
                lineTo (x1+37.5, y2+10)
                lineTo (x1+37.5, y2+10)
                fillStyle "Black"
                fill ()
                closePath ()
                -- Front Bottom wheel
                beginPath ()
                moveTo (x2-37.5, y2+7)
                lineTo (x2-37.5, y2)
                lineTo (x2-12.5, y2)
                lineTo (x2-12.5, y2+7)
                lineTo (x2-37.5, y2+7)
                fillStyle "Black"
                fill ()
                closePath()

                moveTo (x1, y1)
                beginPath ()
                lineTo (x1, y2)
                lineTo (x2, y2)
                lineTo (x2, y1)
                lineTo (x1, y1)
                strokeStyle "black"
                stroke ()
                fillStyle "#0B0B61"
                fill ()
                closePath()
                where 
                    (x1, y1) = p1 coord
                    (x2, y2) = p2 coord


enemyDraw :: EnemyCar -> Canvas ()
enemyDraw en = do
                -- Back top wheel
                beginPath ()
                moveTo (x1+12.5, y1-7)
                lineTo (x1+12.5, y1)
                lineTo (x1+37.5, y1)
                lineTo (x1+37.5, y1-7)
                lineTo (x1+37.5, y1-7)
                fillStyle "Black"
                fill ()
                closePath ()
                -- Front Top wheel
                beginPath ()
                moveTo (x2-37.5, y1-10)
                lineTo (x2-37.5, y1)
                lineTo (x2-12.5, y1)
                lineTo (x2-12.5, y1-10)
                lineTo (x2-37.5, y1-10)
                fillStyle "Black"
                fill ()
                -- Back Bottom wheel
                beginPath ()
                moveTo (x1+12.5, y2+7)
                lineTo (x1+12.5, y2)
                lineTo (x1+37.5, y2)
                lineTo (x1+37.5, y2+7)
                lineTo (x1+37.5, y2+7)
                fillStyle "Black"
                fill ()
                closePath ()
                -- Front Bottom wheel
                beginPath ()
                moveTo (x2-37.5, y2+10)
                lineTo (x2-37.5, y2)
                lineTo (x2-12.5, y2)
                lineTo (x2-12.5, y2+10)
                lineTo (x2-37.5, y2+10)
                fillStyle "Black"
                fill ()
                closePath()

                moveTo (x1, y1)
                beginPath ()
                lineTo (x1, y2)
                lineTo (x2, y2)
                lineTo (x2, y1)
                lineTo (x1, y1)
                strokeStyle "black"
                stroke ()
                fillStyle "red"
                fill ()
                closePath()
                where 
                    (x1,y1)= frontTop en
                    (x2,y2)= (x1+100, y1+50)