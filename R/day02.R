#' Day 02: Dive!
#'
#' [Dive!](https://adventofcode.com/2021/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' Now, you need to figure out how to [pilot this
#' thing]{title="Tank, I need a pilot program for a B212 helicopter."}.
#'
#' It seems like the submarine can take a series of commands like
#' `forward 1`, `down 2`, or `up 3`:
#'
#' -   `forward X` increases the horizontal position by `X` units.
#' -   `down X` *increases* the depth by `X` units.
#' -   `up X` *decreases* the depth by `X` units.
#'
#' Note that since you\'re on a submarine, `down` and `up` affect your
#' *depth*, and so they have the opposite result of what you might expect.
#'
#' The submarine seems to already have a planned course (your puzzle
#' input). You should probably figure out where it\'s going. For example:
#'
#'     forward 5
#'     down 5
#'     forward 8
#'     up 3
#'     down 8
#'     forward 2
#'
#' Your horizontal position and depth both start at `0`. The steps above
#' would then modify them as follows:
#'
#' -   `forward 5` adds `5` to your horizontal position, a total of `5`.
#' -   `down 5` adds `5` to your depth, resulting in a value of `5`.
#' -   `forward 8` adds `8` to your horizontal position, a total of `13`.
#' -   `up 3` decreases your depth by `3`, resulting in a value of `2`.
#' -   `down 8` adds `8` to your depth, resulting in a value of `10`.
#' -   `forward 2` adds `2` to your horizontal position, a total of `15`.
#'
#' After following these instructions, you would have a horizontal position
#' of `15` and a depth of `10`. (Multiplying these together produces
#' `150`.)
#'
#' Calculate the horizontal position and depth you would have after
#' following the planned course. *What do you get if you multiply your
#' final horizontal position by your final depth?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f02a(x)` returns .... For Part Two,
#'   `f02b(x)` returns ....
#' @export
#' @examples
#' f02a(example_data_02())
#' f02b()
f02a <- function(x) {
  x %>%
    mutate(axis = ifelse(dir == "forward", "x", "y"),
           dist = ifelse(dir == "up", -value, value)) %>%
    group_by(axis) %>%
    summarize(dist = sum(dist))
}


#' @rdname day02
#' @export
f02b <- function(x) {
  x %>%
    mutate(axis = ifelse(dir == "forward", "x", "y"),
           value = ifelse(dir == "up", -value, value)) %>%
    select(axis, value) %>%
    pivot_wider(names_from = axis, values_from = value)
    (aim = 0,
           aim = ifelse(dir != "forward", lag(aim) + value, lag(aim))) %>%
    group_by(axis) %>%
    summarize(units = sum(units))

}

# iterative approach
sub_pos_iter <- function(x) {
  pos = c(x = 0, y = 0)
  aim = 0
  for (i in 1:nrow(x)) {
    if (x[i,1] == "forward") {
      pos[1] = pos[1] + x[i,2]
      pos[2] = pos[2] + aim * x[i,2]
    } else {
      if (x[i,1] == "up") {
        aim = aim - x[i,2]
      } else {
        aim = aim + x[i,2]
      }
    }
  }
  pos
}

f02_helper <- function(x) {
  readr::read_delim("inst/input02.txt", delim = " ", col_names = c("dir", "value"))
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
example_data_02 <- function(example = 1) {
    dplyr::tribble(~dir, ~value,
            "forward", 5,
          "down", 5,
          "forward", 8,
          "up", 3,
          "down", 8,
          "forward", 2
    )
}


x <- f02_helper()
f02a(x) %>%
  summarize(prod = prod(units))
