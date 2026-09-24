#' @title Create a New Monty Hall Game
#'
#' @description
#' Creates a new Monty Hall game by randomly assigning
#' two goats and one car behind three doors.
#'
#' @details
#' The position of the car is randomized each time the
#' function is called.
#'
#' @return
#' A character vector of length three containing two
#' goats and one car.
#'
#' @examples
#' create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample(
    x = c("goat", "goat", "car"),
    size = 3,
    replace = FALSE
  )

  return(a.game)
}



#' @title Select a Door
#'
#' @description
#' Simulates a contestant selecting one of three doors.
#'
#' @details
#' A random door numbered 1 through 3 is selected.
#'
#' @return
#' An integer representing the selected door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function()
{
  a.pick <- sample(
    1:3,
    size = 1,
    replace = FALSE
  )

  return(a.pick)
}



#' @title Open a Goat Door
#'
#' @description
#' Determines which goat door the host can open after
#' the contestant makes an initial selection.
#'
#' @details
#' The host cannot open the contestant's selected door.
#' If multiple goat doors are available, one is selected
#' at random.
#'
#' @param game A character vector created by create_game().
#'
#' @param a.pick An integer indicating the contestant's
#' initial door selection.
#'
#' @return
#' An integer indicating which door is opened.
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' open_goat_door(game, pick)
#'
#' @export
open_goat_door <- function(game, a.pick)
{
  goat_index <- which(game == "goat")

  host_doors <- goat_index[!(goat_index %in% a.pick)]

  if(length(host_doors) == 1)
  {
    return(host_doors)
  }

  sample(host_doors, size = 1)
}



#' @title Change or Keep Door
#'
#' @description
#' Determines the contestant's final door choice after
#' choosing whether to stay or switch.
#'
#' @details
#' If stay is TRUE the original door is kept. If stay is
#' FALSE the contestant switches to the remaining
#' unopened door.
#'
#' @param stay Logical value indicating whether the
#' contestant stays with the original choice.
#'
#' @param opened.door Integer identifying the host's
#' opened door.
#'
#' @param a.pick Integer identifying the contestant's
#' original door.
#'
#' @return
#' An integer representing the final door selection.
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' opened <- open_goat_door(game, pick)
#' change_door(FALSE, opened, pick)
#'
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick)
{
  if(stay)
  {
    return(a.pick)
  }

  doors <- c(1, 2, 3)

  used.doors <- c(opened.door, a.pick)

  remaining.door <- doors[!(doors %in% used.doors)]

  return(remaining.door)
}



#' @title Determine the Winner
#'
#' @description
#' Determines whether the contestant wins or loses.
#'
#' @details
#' The contestant wins if the final door selection
#' contains the car.
#'
#' @param final.pick Integer indicating the contestant's
#' final door choice.
#'
#' @param game Character vector created by create_game().
#'
#' @return
#' Returns either "WIN" or "LOSE".
#'
#' @examples
#' game <- create_game()
#' determine_winner(1, game)
#'
#' @export
determine_winner <- function(final.pick, game)
{
  car_index <- which(game == "car")

  if(final.pick == car_index)
  {
    return("WIN")
  }

  if(final.pick != car_index)
  {
    return("LOSE")
  }
}



#' @title Play a Complete Monty Hall Game
#'
#' @description
#' Simulates a complete Monty Hall game and reports
#' outcomes for both stay and switch strategies.
#'
#' @details
#' Creates a game, allows a contestant to select a
#' door, opens a goat door, and evaluates both
#' possible strategies.
#'
#' @return
#' A data frame containing strategies and outcomes.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function()
{
  new.game <- create_game()

  first.pick <- select_door()

  opened.door <- open_goat_door(
    new.game,
    first.pick
  )

  final.pick.stay <- change_door(
    stay = TRUE,
    opened.door,
    first.pick
  )

  final.pick.switch <- change_door(
    stay = FALSE,
    opened.door,
    first.pick
  )

  outcome.stay <- determine_winner(
    final.pick.stay,
    new.game
  )

  outcome.switch <- determine_winner(
    final.pick.switch,
    new.game
  )

  strategy <- c("stay", "switch")

  outcome <- c(
    outcome.stay,
    outcome.switch
  )

  game.results <- data.frame(
    strategy,
    outcome,
    stringsAsFactors = FALSE
  )

  return(game.results)
}
