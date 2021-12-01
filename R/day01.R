#' Day 01: Sonar Sweep
#'
#' [Sonar Sweep](https://adventofcode.com/2021/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' You\'re minding your own business on a ship at sea when the overboard
#' alarm goes off! You rush to see if you can help. Apparently, one of the
#' Elves tripped and accidentally sent the sleigh keys flying into the
#' ocean!
#'
#' Before you know it, you\'re inside a submarine the Elves keep ready for
#' situations like this. It\'s covered in Christmas lights (because of
#' course it is), and it even has an experimental antenna that should be
#' able to track the keys if you can boost its signal strength high enough;
#' there\'s a little meter that indicates the antenna\'s signal strength by
#' displaying 0-50 *stars*.
#'
#' Your instincts tell you that in order to save Christmas, you\'ll need to
#' get all *fifty stars* by December 25th.
#'
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the Advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#'
#' As the submarine drops below the surface of the ocean, it automatically
#' performs a sonar sweep of the nearby sea floor. On a small screen, the
#' sonar sweep report (your puzzle input) appears: each line is a
#' measurement of the sea floor depth as the sweep looks further and
#' further away from the submarine.
#'
#' For example, suppose you had the following report:
#'
#'     199
#'     200
#'     208
#'     210
#'     200
#'     207
#'     240
#'     269
#'     260
#'     263
#'
#' This report indicates that, scanning outward from the submarine, the
#' sonar sweep found depths of `199`, `200`, `208`, `210`, and so on.
#'
#' The first order of business is to figure out how quickly the depth
#' increases, just so you know what you\'re dealing with - you never know
#' if the keys will get [carried into deeper
#' water]{title="Does this premise seem fishy to you?"} by an ocean current
#' or a fish or something.
#'
#' To do this, count *the number of times a depth measurement increases*
#' from the previous measurement. (There is no measurement before the first
#' measurement.) In the example above, the changes are as follows:
#'
#'     199 (N/A - no previous measurement)
#'     200 (increased)
#'     208 (increased)
#'     210 (increased)
#'     200 (decreased)
#'     207 (increased)
#'     240 (increased)
#'     269 (increased)
#'     260 (decreased)
#'     263 (increased)
#'
#' In this example, there are *`7`* measurements that are larger than the
#' previous measurement.
#'
#' *How many measurements are larger than the previous measurement?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f01a(x)` returns .... For Part Two,
#'   `f01b(x)` returns ....
#' @export
#' @examples
#' f01a(example_data_01())
#' f01b()
tally_deeper <- function(depths) {
  deeper_tally <- 0L
  for (i in 2:length(depths)) {
   if (depths[i] > depths[i-1]) {
     deeper_tally <- deeper_tally + 1
   }
  }
  deeper_tally
}


#' @rdname day01
#' @export
tally_window <- function(depths) {
  deeper_tally <- 0L
  for (i in 4:length(depths)) {
   prev_sum <- sum(depths[c(i-3, i-2, i-1)])
   sum <- sum(depths[c(i-2, i-1, i)])
   if (sum > prev_sum) {
     deeper_tally <- deeper_tally + 1
   }
  }
  deeper_tally

}

tally_deeper_tidy <- function(x) {
 sum(x > dplyr::lag(x), na.rm = TRUE)
}

tally_window_tidy <- function(x) {
 s <- na.omit(x + lead(x) + lead(x,2))
 sum(s > lag(s), na.rm = TRUE)
}

tally_deeper_simple <- function(d) sum(diff(d)>0)
tally_window_simple <- function(d) sum(diff(d, lag = 3)>0)


#' @rdname day01
#' @export
tally_window <- function(depths) {
  deeper_tally <- 0L
  for (i in 4:length(depths)) {
   prev_sum <- sum(depths[c(i-3, i-2, i-1)])
   sum <- sum(depths[c(i-2, i-1, i)])
   if (sum > prev_sum) {
     deeper_tally <- deeper_tally + 1
   }
  }
  deeper_tally

}

get_day1 <- function(file = here::here("inst/input01.txt")) {
  read.table(file)[[1]]
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export
example_data_01 <- function(example = 1) {
  l <- list(
    a = c(199,
          200,
          208,
          210,
          200,
          207,
          240,
          269,
          260,
          263
    )
  )
  l[[example]]
}
