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
#' @param
#' No arguments are used by the function.
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
#'    Select a door from the three available doors
#'
#' @description
#'   `select_door()` randomly selects a door number between 1 and 3.
#'
#' @details
#'   The contestant selects a door. To replicate this, doors are randomly selected.
#'   A goat door will be revealed next and the contestant has the option to stay
#'   with their current selection or switch.
#'
#' @param
#'   No arguments are used by the function.
#'
#' @return
#'   The function returns a number between 1 and 3.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens goat door
#'
#' @description
#'   `open_goat_door( game, a.pick )` randomly selects a door number between 1 and 3.
#'
#' @details
#'   The contestant picks a door, host reveals a door with the goat and the contestant
#'   has the option to stay with their selected door or switch to the other unopened door.
#'   This function determines which door(s) is a goat door and was not selected by the participant. If the
#'   participant selected a car door, there remain two unopened goat doors and one will be randomly
#'   revealed. If the participant selected a goat door, there is only one unopened goat
#'   door to choose from, and that one will be revealed.
#'
#' @param
#'   game: the vector from the create_game() function. No default.
#'   a.pick: the value from the select_door() function. No default.
#'
#' @return
#'   The function returns a number between 1 and 3, representing a door that has a goat and was
#'   not selected by the participant.
#'
#' @examples
#'   open_goat_door( c("goat","goat","car"), 3 )
#'   open_goat_door( c("goat","car","goat"), 1 )
#'
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
#' Change doors
#' @description
#' `change_door()` function returns a final pick.
#' @details
#' If the contestant decides to stay with the original pick, the function returns
#' the originally selected door. If the contestant decides to switch doors, then
#' the function reveals the number of the other unopened door.
#' @param
#' No arguments are used by the function
#' stay: TRUE or FALSE. TRUE returns the results from the select_door()
#'   function. FALSE returns the other unopened door. Defaults to TRUE.
#'   opened.door: the numerical value from the open_goat_door() function. No default.
#'   a.pick: the vector from the create_game() function. No default.
#'
#' @return
#'   The function returns a number between 1 and 3: either the value from the select_door() function
#'   (if stay = TRUE) or the number that is neither the opened_goat_door() value nor the
#'   select_door() value (if stay = FALSE)
#'
#' @examples
#'   change_door( T, 1, 3 )
#'   change_door( F, 1, 3 )
#'
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



#' @title
#' Determine if contestant has won
#' @details
#' The function reveals whether the final selection is a car or a goat door.
#' @param
#'   final.pick: the numerical value from the change_door() function. No default.
#'   game: the vector from the create_game() function. No default.
#'
#' @return
#'   The function returns the string "WIN" if the final pick was a "car" door or "LOSE" if the final
#'   pick was a "goat" door.
#'
#' @examples
#'   determine_winner( 3, c( "goat","goat","car" ))
#'   determine_winner( 1, c( "goat","car","goat" ))
#'
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





#' @title
#' Game wrapper function
#' @description
#' Wraps all previous functions into one function.
#' @details
#' The function performs each function necessary to return win or lose results. Each game is played
#' with both a stay and a switch result, so the user can determine which strategy has a higher
#' proportion of wins and losses.
#'
#' @param
#' No arguments are used by this function.
#'
#' @return
#' The function returns a data frame indicating whether each strategy wins
#' or loses.
#'
#' @examples
#' play_game()
#'
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






#' @title
#' Simulate the game
#' @description
#' `play_n_games` is the function that allows the contestant to simulate the game
#' as many times `n` as they want.
#' @details
#' This function is useful to determine which strategy is better to choose.
#' @param
#' n: the contestant can determine the number of times they would like to play the game.
#' Defaults to 100.
#' @return
#' The function returns a table with the wins and losses for each selection.
#' @examples
#' play_n_games(100)
#' play_n_games(10000)
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
