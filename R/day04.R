#' Day 04: Giant Squid
#'
#' [Giant Squid](https://adventofcode.com/2021/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' You\'re already almost 1.5km (almost a mile) below the surface of the
#' ocean, already so deep that you can\'t see any sunlight. What you *can*
#' see, however, is a giant squid that has attached itself to the outside
#' of your submarine.
#'
#' Maybe it wants to play
#' [bingo](https://en.wikipedia.org/wiki/Bingo_(American_version))?
#'
#' Bingo is played on a set of boards each consisting of a 5x5 grid of
#' numbers. Numbers are chosen at random, and the chosen number is *marked*
#' on all boards on which it appears. (Numbers may not appear on all
#' boards.) If all numbers in any row or any column of a board are marked,
#' that board *wins*. (Diagonals don\'t count.)
#'
#' The submarine has a *bingo subsystem* to help passengers (currently, you
#' and the giant squid) pass the time. It automatically generates a random
#' order in which to draw numbers and a random set of boards (your puzzle
#' input). For example:
#'
#'     7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
#'
#'     22 13 17 11  0
#'      8  2 23  4 24
#'     21  9 14 16  7
#'      6 10  3 18  5
#'      1 12 20 15 19
#'
#'      3 15  0  2 22
#'      9 18 13 17  5
#'     19  8  7 25 23
#'     20 11 10 24  4
#'     14 21 16 12  6
#'
#'     14 21 17 24  4
#'     10 16 15  9 19
#'     18  8 23 26 20
#'     22 11 13  6  5
#'      2  0 12  3  7
#'
#' After the first five numbers are drawn (`7`, `4`, `9`, `5`, and `11`),
#' there are no winners, but the boards are marked as follows (shown here
#' adjacent to each other to save space):
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' After the next six numbers are drawn (`17`, `23`, `2`, `0`, `14`, and
#' `21`), there are still no winners:
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' Finally, `24` is drawn:
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' At this point, the third board *wins* because it has at least one
#' complete row or column of marked numbers (in this case, the entire top
#' row is marked: `14 21 17 24  4`).
#'
#' The *score* of the winning board can now be calculated. Start by finding
#' the *sum of all unmarked numbers* on that board; in this case, the sum
#' is `188`. Then, multiply that sum by *the number that was just called*
#' when the board won, `24`, to get the final score, `188 * 24 = 4512`.
#'
#' To guarantee victory against the giant squid, figure out which board
#' will win first. *What will your final score be if you choose that
#' board?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b()
f04a <- function(x) {

}


#' @rdname day04
#' @export
f04b <- function(x) {

}


f04_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}

read_picks <- function(file) {
  read.csv(file, header = FALSE, nrows = 1)[1,] |>
    t() |>
    as.vector()
}

read_boards <- function(file) {
  skip <- 2
  num_boards <- (R.utils::countLines(file) - 1)/6
  boards <- replicate(num_boards, diag(5), simplify = FALSE)
  for (i in 1:num_boards) {
      board <- read.table(file, skip = skip, nrows = 5)
      skip <- skip + 6
      boards[[i]] <- as.matrix(board)
  }
  boards
}

# Run bingo. Iterate through picks, keeping track of board state.
# Let us know how many turns, winner, and winning score
bingo <- function(picks, boards) {
  # initialize dabs
  dabs <- replicate(length(boards), matrix(FALSE, 5, 5), simplify = FALSE)
  wins <- list("round" = integer(length = length(boards)),
               "winner" = integer(length = length(boards)),
               "score" = integer(length = length(boards)))
  names(wins) <- c("round", "winner", "score")

  for (j in 1:length(picks)) {
    for (i in 1:length(boards)) {
      # dab boards
      dabs[[i]][which(boards[[i]] == picks[j])] <- TRUE
      if (check_bingo(dabs[[i]])) {
        score <- score_bingo(boards[[i]], dabs[[i]], picks[j])
        if (!(i %in% wins[[1]])) {
          wins[[1]] <- c(wins[[1]], j)
          wins[[2]] <- c(wins[[2]], i)
          wins[[3]] <- c(wins[[3]], score)

          print(c("round" = j, "winner" = i, "score" = score))
        }
      }
    }
  }
  wins
}

# check for bingo win. Return TRUE for win.
check_bingo <- function(dab) {
  for (row in 1:5) {
    if (all(dab[row,])) return(TRUE)
  }
  for (col in 1:5) {
    if (all(dab[,col])==TRUE) return(TRUE)
  }
  FALSE
}

score_bingo <- function(board, dab, pick) {
  marked <- board[which(!dab)]
  sum(marked) * pick
}
