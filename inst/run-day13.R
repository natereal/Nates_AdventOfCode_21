library(natesAdventofcode21)
input <- readLines("./inst/input13.txt") %>%
  f13_process()

ex1 <- example_data_13(1) %>%
  process()

f13a(ex1)
f13b(ex1)

p1 <- f13a(input)
p2 <- f13b(input)

stopifnot(p1 == aoc_solutions$day13a)
stopifnot(p2 == aoc_solutions$day13b)
