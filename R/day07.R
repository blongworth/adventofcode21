#' Day 07: The Treachery of Whales
#'
#' [The Treachery of Whales](https://adventofcode.com/2021/day/7)
#'
#' @name day07
#' @rdname day07
#' @details
#'
#' **Part One**
#'
#' A giant [whale](https://en.wikipedia.org/wiki/Sperm_whale) has decided
#' your submarine is its next meal, and it\'s much faster than you are.
#' There\'s nowhere to run!
#'
#' Suddenly, a swarm of crabs (each in its own tiny submarine - it\'s too
#' deep for them otherwise) zooms in to rescue you! They seem to be
#' preparing to blast a hole in the ocean floor; sensors indicate a
#' *massive underground cave system* just beyond where they\'re aiming!
#'
#' The crab submarines all need to be aligned before they\'ll have enough
#' power to blast a large enough hole for your submarine to get through.
#' However, it doesn\'t look like they\'ll be aligned before the whale
#' catches you! Maybe you can help?
#'
#' There\'s one major catch - crab submarines can only move horizontally.
#'
#' You quickly make a list of *the horizontal position of each crab* (your
#' puzzle input). Crab submarines have limited fuel, so you need to find a
#' way to make all of their horizontal positions match while requiring them
#' to spend as little fuel as possible.
#'
#' For example, consider the following horizontal positions:
#'
#'     16,1,2,0,4,2,7,1,2,14
#'
#' This means there\'s a crab with horizontal position `16`, a crab with
#' horizontal position `1`, and so on.
#'
#' Each change of 1 step in horizontal position of a single crab costs 1
#' fuel. You could choose any horizontal position to align them all on, but
#' the one that costs the least fuel is horizontal position `2`:
#'
#' -   Move from `16` to `2`: `14` fuel
#' -   Move from `1` to `2`: `1` fuel
#' -   Move from `2` to `2`: `0` fuel
#' -   Move from `0` to `2`: `2` fuel
#' -   Move from `4` to `2`: `2` fuel
#' -   Move from `2` to `2`: `0` fuel
#' -   Move from `7` to `2`: `5` fuel
#' -   Move from `1` to `2`: `1` fuel
#' -   Move from `2` to `2`: `0` fuel
#' -   Move from `14` to `2`: `12` fuel
#'
#' This costs a total of `37` fuel. This is the cheapest possible outcome;
#' more expensive outcomes include aligning at position `1` (`41` fuel),
#' position `3` (`39` fuel), or position `10` (`71` fuel).
#'
#' Determine the horizontal position that the crabs can align to using the
#' least fuel possible. *How much fuel must they spend to align to that
#' position?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f07a(x)` returns .... For Part Two,
#'   `f07b(x)` returns ....
#' @export
#' @examples
#' f07a(example_data_07())
#' f07b()
f07a <- function(x) {
  crabs <- as.integer(unlist(strsplit(x, ",")))
  map <- min(crabs):max(crabs)
  fuel <- integer(length(crabs))
  pos_tot <- integer(length(map))
  names(pos_tot) <- map
  for (pos in map) {
    # total fuel for location
    fuel <- abs(crabs - pos)
    pos_tot[as.character(pos)] <- sum(fuel)
  }
  min(pos_tot)
}


#' @rdname day07
#' @export
f07b <- function(x) {
  crabs <- as.integer(unlist(strsplit(x, ",")))
  map <- min(crabs):max(crabs)
  pos_tot <- double(length(map))
  names(pos_tot) <- map
  fuel <- double(length(crabs))
  for (pos in map) {
    # total fuel for location
    steps <- abs(crabs - pos)
    fuel <- steps * (steps + 1)/2
    pos_tot[as.character(pos)] <- sum(fuel)
  }
  min(pos_tot)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day07
#' @export
example_data_07 <- function() {
  "16,1,2,0,4,2,7,1,2,14"
}

# cool mapped sol'n from Emil Hvitfeldt
EHD7 <- function(x, part = "a") {
  crabs <- as.integer(unlist(strsplit(x, ",")))
  values <- seq(min(crabs), max(crabs))
  if (part)
    adjust <- function(n) n * (n + 1) / 2
  fuels <- purrr::map_dbl(values, ~sum(adjust(abs(crabs - .x))))
  min(fuels)
  dd
}
