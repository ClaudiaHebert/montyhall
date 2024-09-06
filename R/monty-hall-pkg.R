#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title 
#' Choose original contestant door 
#' @description
#' This function randomly decides the original contestant choice from the available doors 
#' @details
#' In this case the function is picking randomly from three doors. 
#' @param 
#' No arguments needed for this function
#' @return 
#' Will return an integer vector of the contestant's chosen door 
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host open goat door 
#' @description
#' This function opens a door that the contestant did NOT pick and that has a goat behind it  
#' @details
#' @param 
#' one character vector, one numeric vector  
#' args = current game, chosen door 
#' @return 
#' number of door that host opened
#' @examples
#' open_goat_door(game, a.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Contestant Keeps or Switches Door 
#' @description
#' In this function, the contestant decides whether they will keep the original door they chose 
#' or if they will switch to the remaining unopened door
#' @details
#' Determines final door based on decision to stay or switch
#' @param 
#' logical vector of stay
#' numeric vectors of previously opened host door and original contestant choice 
#' @return 
#' Returns final choice that will be opened to determine game outcome - numeric 
#' vector between 1 and 3
#' @examples
#' change_door(stay = F, opened.door, a.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Determine whether game is won or lost
#' @description
#' This function looks at the final door the contestant chooses to see if it is a goat or a car
#' @details
#' If the final door is a car, the game is won. If the final door is a goat, the game is lost.
#' @param 
#' Arguments are one numeric vector (final.pick) and one 3 character vector (game)
#' @return 
#' Returns game outcome - "Win" or "Lose"
#' @examples
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Create wrapper function
#' @description
#' This function wraps all the previously created variables up into one function 
#' @details
#' This function assigns previous functions to objects and also stores the results
#' of switching doors and the results of keeping the same door. It saves the game 
#' results as a data frame 
#' @param 
#' no arguments needed for this function 
#' @return 
#' Returns game results 
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Create a simulation
#' @description This function runs the previous wrapper function 100 times and 
#' stores the outcome each time.
#' @details
#' This simulation provides the outcome in a table comparing wins and losses for 
#' each of the two game strategies (switch and stay). 
#' @param 
#' n = number of times you'd like the simulation to run 
#' @return 
#' Returns data frame outputted as a 4x4 table (switch/stay by win/lost)
#' @examples
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
