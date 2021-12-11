install.packages("devtools")
install.packages(c("tidyverse", "rmarkdown"))
remotes::install_github("tjmahr/aoc")

options(aoc.package = "natesAdventofcode21")

# aoc::use_day(1)

renv::snapshot()
