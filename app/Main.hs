{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), play)

class Widget a where
   handleInput :: Event -> a -> a
   step :: Float -> a -> a
   render :: a -> Picture


data Hoverable a = Hoverable {
   hoverableWidget :: a ,
   hoverableOnHover :: Event -> a -> a }

instance Widget a => Widget (Hoverable a) where
   handleInput event (Hoverable widget f) = Hoverable (((f event) . (handleInput event)) widget) f
   step dt (Hoverable widget f) = Hoverable (step dt widget) f
   render (Hoverable widget f) = render widget

data Button = Button {
   buttonColor :: Color ,
   buttonWidth :: Float ,
   buttonHeight :: Float ,
   buttonPosX :: Float ,
   buttonPosY :: Float }

onHover :: Event -> Float -> Float -> Float -> Float -> Color
onHover (EventMotion (x,y)) posX posY width height = 
   if (x>=posX - width / 2) && (x<=posX + width / 2 ) 
      then if (y >= posY - height / 2) && (y <= posY + height / 2) then red else blue
      else blue
onHover _ _ _ _ _= blue

instance Widget Button where
   handleInput event = id 
   step _ = id
   render Button{..} = 
      color buttonColor $ 
         translate buttonPosX buttonPosY $ 
            rectangleSolid buttonWidth buttonHeight 

--handleInput event button@Button{..} = button { 
--      buttonColor = onHover event buttonPosX buttonPosY buttonWidth buttonHeight }


data Container a = Container { containerWidgets :: [a] }

instance Widget a => Widget (Container a) where
   handleInput event (Container widgets) = Container $ fmap (handleInput event) widgets
   step _ = id
   render (Container widgets) = pictures $ fmap render widgets


-- Should start with how we want the API to look
-- The first thing i will like is to create API for displaying
-- EX:
-- column $ do
--    text "Hello world"
--    row $ do
--       text "A"
--       text "B"
--    text "Footer"

playWidget :: Widget a => a -> IO ()
playWidget button = play (InWindow "Nice Window" (800, 500) (400, 100)) white 30 button render handleInput step 

-- How to add a wrapper to widgets that will have custom 
-- functionality
-- First idea is to create a class that will add beheaviour to any widget
-- Second idea is to have a widget data type that will contain the methods
-- needed for step, handleInput and render and then will compose the functions


-- onEvent :: Widget a => (Event -> a -> a) -> a -> a
-- onEvent f widget = widget { handleInput = \event -> (f event).(widgetHandleInput event) }
   

main :: IO ()
main = do 
   let blueButton = Button blue 100.0 50.0 30.0 30.0
   let greenButton = Button green 100.0 50.0 30.0 90.0
   --let greenButton = Hoverable (Button green 100.0 50.0 30.0 90.0) $ \event button@Button{..} -> button { 
   --   buttonColor = onHover event buttonPosX buttonPosY buttonWidth buttonHeight }
   -- proble is here that the types aren't homogeneous
   -- one solution is phantom types ?
   -- or the solution with just a single data type widget

   -- let onEvent (\event button -> button { buttonColor = red }) $ blueButton
   playWidget $ Container [blueButton, greenButton]
