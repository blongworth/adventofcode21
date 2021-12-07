library(adventofcode21)
options(scipen=999)
x <- readLines("./inst/input06.txt")

p1 <- f06a(x, 80)
p2 <- f06a(x, 256)

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
