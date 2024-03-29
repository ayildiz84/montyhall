% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.r
\name{open_goat_door}
\alias{open_goat_door}
\title{Host opens goat door}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{game:}{the vector from the create_game() function. No default.
a.pick: the value from the select_door() function. No default.}
}
\value{
The function returns a number between 1 and 3, representing a door that has a goat and was
not selected by the participant.
}
\description{
\code{open_goat_door( game, a.pick )} randomly selects a door number between 1 and 3.
}
\details{
The contestant picks a door, host reveals a door with the goat and the contestant
has the option to stay with their selected door or switch to the other unopened door.
This function determines which door(s) is a goat door and was not selected by the participant. If the
participant selected a car door, there remain two unopened goat doors and one will be randomly
revealed. If the participant selected a goat door, there is only one unopened goat
door to choose from, and that one will be revealed.
}
\examples{
  open_goat_door( c("goat","goat","car"), 3 )
  open_goat_door( c("goat","car","goat"), 1 )

}
