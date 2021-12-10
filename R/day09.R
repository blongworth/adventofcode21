#' Day 09: Smoke Basin
#'
#' [Smoke Basin](https://adventofcode.com/2021/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' These caves seem to be [lava
#' tubes](https://en.wikipedia.org/wiki/Lava_tube). Parts are even still
#' volcanically active; small hydrothermal vents release smoke into the
#' caves that slowly [settles like
#' rain]{title="This was originally going to be a puzzle about watersheds, but we're already under water."}.
#'
#' If you can model how the smoke flows through the caves, you might be
#' able to avoid it and be that much safer. The submarine generates a
#' heightmap of the floor of the nearby caves for you (your puzzle input).
#'
#' Smoke flows to the lowest point of the area it\'s in. For example,
#' consider the following heightmap:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Each number corresponds to the height of a particular location, where
#' `9` is the highest and `0` is the lowest a location can be.
#'
#' Your first goal is to find the *low points* - the locations that are
#' lower than any of its adjacent locations. Most locations have four
#' adjacent locations (up, down, left, and right); locations on the edge or
#' corner of the map have three or two adjacent locations, respectively.
#' (Diagonal locations do not count as adjacent.)
#'
#' In the above example, there are *four* low points, all highlighted: two
#' are in the first row (a `1` and a `0`), one is in the third row (a `5`),
#' and one is in the bottom row (also a `5`). All other locations on the
#' heightmap have some lower adjacent location, and so are not low points.
#'
#' The *risk level* of a low point is *1 plus its height*. In the above
#' example, the risk levels of the low points are `2`, `1`, `6`, and `6`.
#' The sum of the risk levels of all low points in the heightmap is
#' therefore `15`.
#'
#' Find all of the low points on your heightmap. *What is the sum of the
#' risk levels of all low points on your heightmap?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' f09a(example_data_09())
#' f09b()
f09a <- function(data) {
  grid <- make_grid(data)
  minima <- find_minima(grid)
  sum(minima$z + 1)
}

# minimum = all adjacent values are higher
# find adjacent values
# use a matrix for data

make_grid <- function(x) {
  str_split(x, '') |>
    unlist() |>
    as.integer() |>
    matrix(nrow = 100)
}

find_minima <- function(grid) {
  minima <- tibble(x = integer(), y = integer(), z = integer())
  for (x in 1:nrow(grid)) {
    for (y in 1:ncol(grid)) {
      z <- grid[x,y]
      if (x == 1) {
        neighbors <- c(grid[x+1,y])
      } else if (x == nrow(grid)) {
        neighbors <- c(grid[x-1,y])
      } else {
        neighbors <- c(grid[x+1,y], grid[x-1,y])
      }
      if (y == 1) {
        neighbors <- c(neighbors, grid[x,y+1])
      } else if (y == ncol(grid)) {
        neighbors <- c(neighbors, grid[x,y-1])
      } else {
        neighbors <- c(neighbors, grid[x,y+1], grid[x,y-1])
      }
      if (all(z < neighbors)) {
        minima <- bind_rows(minima, tibble_row(x = x, y = y, z = z))
      }
    }
  }
  minima
}
#' @rdname day09
#' @export
f09b <- function(x, ) {

}
# for all minima, look at adjacent points
# add to area and todo if greater than minimum and less than 9
# store done points in array


f09_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
