# EECS776-FinalProject
Implementation of a simple 2D game in Haskell

Haskar is a simple, browser-based game, which makes use of Haskell’s Blank Canvas binding to the HTML5 Canvas API. The objective of this game is to maneuver a vehicle across three lanes of oncoming traffic, without colliding into another vehicle. The player’s score increases as long as they have not crashed, and the game is over once this occurs.

After the main function has finished building an initial game state, it calls loop. The game runs through a chain of continuous calls among mainControl, loop and draw.

Important data structures include :

		MainCar

			The player’s car. It has properties like state, position and body. State is of type CarState which can be dead or alive. The game goes on until the state of the main car is Alive. Position is of type CarPosit which can be MThis game is achieved through the continuous calling of three important functions

            mainControl  	 :: DeviceContext -> Game -> IO a
            loop 		 :: DeviceContext -> Int -> Game -> IO a
            draw 		 :: Int -> Game -> Canvas ()
            
        After the main function has finished building an initial game state, it calls loop. The game runs through a chain of continuous calls among mainControl, loop and draw. id, Bot, Top corresponding to Middle, Bottom and Top lanes on the street respectively. Body is of type Canvas () which holds the drawing of the car.

		EnemyCar

		EnemyCar holds a tuple of doubles in its property frontTop and lane defines holds a value of type carPosit, to represent which lane the enemy car is traveling in. A list of EnemyCar objects is held by the an object of type GameState which is passed around in functions to represent current state of the game.


What the important functions do...

		main
		
			Sets up blank-canvas at port 3000 on locals host
			Builds a list of EnemyCar objects with random positions
			Builds the MainCar
			Sets up the initial game state with the aforementioned and calls loop
			
		loop
		
			Clears the canvas
			Uses the game state from the argument and calls draw
			Calls mainControl to analyze the game state
			
		mainControl
		
			Listens to the player’s “keydown” events (for switching lanes)
			Advances the coordinates of the other cars along the lane
			Checks whether the user has collided with another car
			Increments the score while the player is alive
			Builds a new GameState object with the new objects and calls loop.
			The draw function accepts an Int (corresponding to the current “keydown” event) and the current game state to update the canvas with the next
  
