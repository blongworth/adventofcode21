#' Day 11: Dumbo Octopus
#'
#' [Dumbo Octopus](https://adventofcode.com/2021/day/11)
#'
#' @name day11
#' @rdname day11
#' @details
#'
#' **Part One**
#'
#' You enter a large cavern full of rare bioluminescent [dumbo
#' octopuses](https://www.youtube.com/watch?v=eih-VSaS2g0)! They seem to
#' not like the Christmas lights on your submarine, so you turn them off
#' for now.
#'
#' There are 100
#' [octopuses]{title="I know it's weird; I grew up saying 'octopi' too."}
#' arranged neatly in a 10 by 10 grid. Each octopus slowly gains *energy*
#' over time and *flashes* brightly for a moment when its energy is full.
#' Although your lights are off, maybe you could navigate through the cave
#' without disturbing the octopuses if you could predict when the flashes
#' of light will happen.
#'
#' Each octopus has an *energy level* - your submarine can remotely measure
#' the energy level of each octopus (your puzzle input). For example:
#'
#'     5483143223
#'     2745854711
#'     5264556173
#'     6141336146
#'     6357385478
#'     4167524645
#'     2176841721
#'     6882881134
#'     4846848554
#'     5283751526
#'
#' The energy level of each octopus is a value between `0` and `9`. Here,
#' the top-left octopus has an energy level of `5`, the bottom-right one
#' has an energy level of `6`, and so on.
#'
#' You can model the energy levels and flashes of light in *steps*. During
#' a single step, the following occurs:
#'
#' -   First, the energy level of each octopus increases by `1`.
#' -   Then, any octopus with an energy level greater than `9` *flashes*.
#'     This increases the energy level of all adjacent octopuses by `1`,
#'     including octopuses that are diagonally adjacent. If this causes an
#'     octopus to have an energy level greater than `9`, it *also flashes*.
#'     This process continues as long as new octopuses keep having their
#'     energy level increased beyond `9`. (An octopus can only flash *at
#'     most once per step*.)
#' -   Finally, any octopus that flashed during this step has its energy
#'     level set to `0`, as it used all of its energy to flash.
#'
#' Adjacent flashes can cause an octopus to flash on a step even if it
#' begins that step with very little energy. Consider the middle octopus
#' with `1` energy in this situation:
#'
#'     Before any steps:
#'     11111
#'     19991
#'     19191
#'     19991
#'     11111
#'
#'     After step 1:
#'     34543
#'     40004
#'     50005
#'     40004
#'     34543
#'
#'     After step 2:
#'     45654
#'     51115
#'     61116
#'     51115
#'     45654
#'
#' An octopus is *highlighted* when it flashed during the given step.
#'
#' Here is how the larger example above progresses:
#'
#'     Before any steps:
#'     5483143223
#'     2745854711
#'     5264556173
#'     6141336146
#'     6357385478
#'     4167524645
#'     2176841721
#'     6882881134
#'     4846848554
#'     5283751526
#'
#'     After step 1:
#'     6594254334
#'     3856965822
#'     6375667284
#'     7252447257
#'     7468496589
#'     5278635756
#'     3287952832
#'     7993992245
#'     5957959665
#'     6394862637
#'
#'     After step 2:
#'     8807476555
#'     5089087054
#'     8597889608
#'     8485769600
#'     8700908800
#'     6600088989
#'     6800005943
#'     0000007456
#'     9000000876
#'     8700006848
#'
#'     After step 3:
#'     0050900866
#'     8500800575
#'     9900000039
#'     9700000041
#'     9935080063
#'     7712300000
#'     7911250009
#'     2211130000
#'     0421125000
#'     0021119000
#'
#'     After step 4:
#'     2263031977
#'     0923031697
#'     0032221150
#'     0041111163
#'     0076191174
#'     0053411122
#'     0042361120
#'     5532241122
#'     1532247211
#'     1132230211
#'
#'     After step 5:
#'     4484144000
#'     2044144000
#'     2253333493
#'     1152333274
#'     1187303285
#'     1164633233
#'     1153472231
#'     6643352233
#'     2643358322
#'     2243341322
#'
#'     After step 6:
#'     5595255111
#'     3155255222
#'     3364444605
#'     2263444496
#'     2298414396
#'     2275744344
#'     2264583342
#'     7754463344
#'     3754469433
#'     3354452433
#'
#'     After step 7:
#'     6707366222
#'     4377366333
#'     4475555827
#'     3496655709
#'     3500625609
#'     3509955566
#'     3486694453
#'     8865585555
#'     4865580644
#'     4465574644
#'
#'     After step 8:
#'     7818477333
#'     5488477444
#'     5697666949
#'     4608766830
#'     4734946730
#'     4740097688
#'     6900007564
#'     0000009666
#'     8000004755
#'     6800007755
#'
#'     After step 9:
#'     9060000644
#'     7800000976
#'     6900000080
#'     5840000082
#'     5858000093
#'     6962400000
#'     8021250009
#'     2221130009
#'     9111128097
#'     7911119976
#'
#'     After step 10:
#'     0481112976
#'     0031112009
#'     0041112504
#'     0081111406
#'     0099111306
#'     0093511233
#'     0442361130
#'     5532252350
#'     0532250600
#'     0032240000
#'
#' After step 10, there have been a total of `204` flashes. Fast
#' forwarding, here is the same configuration every 10 steps:
#'
#'     After step 20:
#'     3936556452
#'     5686556806
#'     4496555690
#'     4448655580
#'     4456865570
#'     5680086577
#'     7000009896
#'     0000000344
#'     6000000364
#'     4600009543
#'
#'     After step 30:
#'     0643334118
#'     4253334611
#'     3374333458
#'     2225333337
#'     2229333338
#'     2276733333
#'     2754574565
#'     5544458511
#'     9444447111
#'     7944446119
#'
#'     After step 40:
#'     6211111981
#'     0421111119
#'     0042111115
#'     0003111115
#'     0003111116
#'     0065611111
#'     0532351111
#'     3322234597
#'     2222222976
#'     2222222762
#'
#'     After step 50:
#'     9655556447
#'     4865556805
#'     4486555690
#'     4458655580
#'     4574865570
#'     5700086566
#'     6000009887
#'     8000000533
#'     6800000633
#'     5680000538
#'
#'     After step 60:
#'     2533334200
#'     2743334640
#'     2264333458
#'     2225333337
#'     2225333338
#'     2287833333
#'     3854573455
#'     1854458611
#'     1175447111
#'     1115446111
#'
#'     After step 70:
#'     8211111164
#'     0421111166
#'     0042111114
#'     0004211115
#'     0000211116
#'     0065611111
#'     0532351111
#'     7322235117
#'     5722223475
#'     4572222754
#'
#'     After step 80:
#'     1755555697
#'     5965555609
#'     4486555680
#'     4458655580
#'     4570865570
#'     5700086566
#'     7000008666
#'     0000000990
#'     0000000800
#'     0000000000
#'
#'     After step 90:
#'     7433333522
#'     2643333522
#'     2264333458
#'     2226433337
#'     2222433338
#'     2287833333
#'     2854573333
#'     4854458333
#'     3387779333
#'     3333333333
#'
#'     After step 100:
#'     0397666866
#'     0749766918
#'     0053976933
#'     0004297822
#'     0004229892
#'     0053222877
#'     0532222966
#'     9322228966
#'     7922286866
#'     6789998766
#'
#' After 100 steps, there have been a total of `1656` flashes.
#'
#' Given the starting energy levels of the dumbo octopuses in your cavern,
#' simulate 100 steps. *How many total flashes are there after 100 steps?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f11a(x)` returns .... For Part Two,
#'   `f11b(x)` returns ....
#' @export
#' @examples
#' f11a(example_data_11())
#' f11b()
f11a <- function(x, steps) {
  grid <- make_grid(x)
  size <- nrow(grid)
  for (i in steps) {
    step(grid)
  }
}

# flash: points that have flashed
# newflash: points to flash in this round
step <- function(grid) {
  flash <- list(x = integer(), y = integer()) # maybe this should be a list of points?
  # increase grid by 1
  grid <- grid + 1L
  while (TRUE) {
    # find flashers for this round
    flashed <- grid > 9
    # stop if no more flashers
    if (!any(flashed)) break

    flash <- which(flash %in% new_flash)
    for (i in flash) {
      neighbors <- neighbors(location)
      ifelse(neighbor not in flashed, neighbor +1, neighbor) # not sure if needed

    }
  }
  # increase neigbors by 1, flash if > 9, repeat until none > 9
  # set all that flashed to 0
  grid <- ifelse(flash, 0, grid)


}
neighbors <- function(size, x, y) {
  x_s <- c(rep(x-1, 3), rep(x+1,3), x, x)
  y_s <- c((y-1):(y+1), (y-1):(y+1), y-1, y+1)
  tibble(x = x_s, y = y_s) %>%
    filter(!(x<1 | y<1 | x>size | y >size))
}

# who are we spreading the flash to?
spread <- function(grid, flash, newflash) {
  # union flash with newflash
  # if neighbors haven't flashed, increment grid value by 1
  # return flash and grid

}
#' @rdname day11
#' @export
f11b <- function(x) {

}

make_grid <- function(x) {
  str_split(x, '') |>
    unlist() |>
    as.integer() |>
    matrix(nrow = length(x))
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day11
#' @export
example_data_11 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
