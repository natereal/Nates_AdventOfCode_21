#' Day 13: Transparent Origami
#'
#' [Transparent Origami](https://adventofcode.com/2021/day/13)
#'
#' @name day13
#' @rdname day13
#' @details
#'
#' **Part One**
#'
#' You reach another volcanically active part of the cave. It would be nice
#' if you could do some kind of thermal imaging so you could tell ahead of
#' time which caves are too hot to safely enter.
#'
#' Fortunately, the submarine seems to be equipped with a thermal camera!
#' When you activate it, you are greeted with:
#'
#'     Congratulations on your purchase! To activate this infrared thermal imaging
#'     camera system, please enter the code found on page 1 of the manual.
#'
#' Apparently, the Elves have never used this feature. To your surprise,
#' you manage to find the manual; as you go to open it, page 1 falls out.
#' It\'s a large sheet of [transparent
#' paper](https://en.wikipedia.org/wiki/Transparency_(projection))! The
#' transparent paper is marked with random dots and includes instructions
#' on how to fold it up (your puzzle input). For example:
#'
#'     6,10
#'     0,14
#'     9,10
#'     0,3
#'     10,4
#'     4,11
#'     6,0
#'     6,12
#'     4,1
#'     0,13
#'     10,12
#'     3,4
#'     3,0
#'     8,4
#'     1,10
#'     2,14
#'     8,10
#'     9,0
#'
#'     fold along y=7
#'     fold along x=5
#'
#' The first section is a list of dots on the transparent paper. `0,0`
#' represents the top-left coordinate. The first value, `x`, increases to
#' the right. The second value, `y`, increases downward. So, the coordinate
#' `3,0` is to the right of `0,0`, and the coordinate `0,7` is below `0,0`.
#' The coordinates in this example form the following pattern, where `#` is
#' a dot on the paper and `.` is an empty, unmarked position:
#'
#'     ...#..#..#.
#'     ....#......
#'     ...........
#'     #..........
#'     ...#....#.#
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     .#....#.##.
#'     ....#......
#'     ......#...#
#'     #..........
#'     #.#........
#'
#' Then, there is a list of *fold instructions*. Each instruction indicates
#' a line on the transparent paper and wants you to fold the paper *up*
#' (for horizontal `y=...` lines) or *left* (for vertical `x=...` lines).
#' In this example, the first fold instruction is `fold along y=7`, which
#' designates the line formed by all of the positions where `y` is `7`
#' (marked here with `-`):
#'
#'     ...#..#..#.
#'     ....#......
#'     ...........
#'     #..........
#'     ...#....#.#
#'     ...........
#'     ...........
#'     -----------
#'     ...........
#'     ...........
#'     .#....#.##.
#'     ....#......
#'     ......#...#
#'     #..........
#'     #.#........
#'
#' Because this is a horizontal line, fold the bottom half *up*. Some of
#' the dots might end up overlapping after the fold is complete, but dots
#' will never appear exactly on a fold line. The result of doing this fold
#' looks like this:
#'
#'     #.##..#..#.
#'     #...#......
#'     ......#...#
#'     #...#......
#'     .#.#..#.###
#'     ...........
#'     ...........
#'
#' Now, only `17` dots are visible.
#'
#' Notice, for example, the two dots in the bottom left corner before the
#' transparent paper is folded; after the fold is complete, those dots
#' appear in the top left corner (at `0,0` and `0,1`). Because the paper is
#' transparent, the dot just below them in the result (at `0,3`) remains
#' visible, as it can be seen through the transparent paper.
#'
#' Also notice that some dots can end up *overlapping*; in this case, the
#' dots merge together and become a single dot.
#'
#' The second fold instruction is `fold along x=5`, which indicates this
#' line:
#'
#'     #.##.|#..#.
#'     #...#|.....
#'     .....|#...#
#'     #...#|.....
#'     .#.#.|#.###
#'     .....|.....
#'     .....|.....
#'
#' Because this is a vertical line, fold *left*:
#'
#'     #####
#'     #...#
#'     #...#
#'     #...#
#'     #####
#'     .....
#'     .....
#'
#' The instructions made a square!
#'
#' The transparent paper is pretty big, so for now, focus on just
#' completing the first fold. After the first fold in the example above,
#' `17` dots are visible - dots that end up overlapping after the fold is
#' completed count as a single dot.
#'
#' *How many dots are visible after completing just the first fold
#' instruction on your transparent paper?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f13a(x)` returns .... For Part Two,
#'   `f13b(x)` returns ....
#' @export
#' @examples
#' f13a(example_data_13())
#' f13b()
f13a <- function(x) {
 fold(get_matrix(x), x$folds$axis[1]) %>%
    sum()
}


#' @rdname day13
#' @export
f13b <- function(x) {
  output <- input_matrix
  for(i in input$folds$axis) {
    output <- output %>%
      fold(i)
  }
  print_mat(output)
  return(sum(output))
}


f13_process <- function(input_string) {
  folds <- input_string %>%
    stringr::str_subset("fold along") %>%
    stringr::str_remove("fold along ") %>%
    stringr::str_split("=", simplify = TRUE) %>%
    `colnames<-`(c("axis", "value")) %>%
    tibble::as_tibble() %>%
    mutate(value = as.numeric(value))

  coords <- input_string %>%
    stringr::str_subset("^[:digit:]+") %>%
    stringr::str_split(",", simplify = TRUE) %>%
    apply(MARGIN = c(1,2), FUN = as.numeric)

  colnames(coords) <- c("row", "column")

  return(list(
    "coords" = coords,
    "folds"  = folds
  ))
}

get_matrix <- function(input) {
  coords <- input$coords
  # R counts arrays from 1
  coords <- coords + 1
  # x_min <- min(coords[, "row"])
  x_max <- max(coords[, "row"])
  # y_min <- min(coords[, "column"])
  y_max <- max(coords[, "column"])

  mat <- matrix(FALSE, nrow = x_max, ncol = y_max)

  coords <- as.data.frame(coords)

  for(i in seq_len(nrow(coords))) {
    mat[coords$row[i], coords$column[i]] <- TRUE
  }

  #mat[1:20, 1:50]
  return(mat)
}

invert_rows <- function(matrix) {
  return(apply(matrix, MARGIN = 2, FUN = rev))
}
invert_cols <- function(matrix) {
  N <- nrow(matrix)

  invert_rows(matrix) |>
    rev() |>
    matrix(nrow = N)
}

fold <- function(matrix, axis) {
  if(axis == "x") {
    half <- floor(nrow(matrix) / 2)
    matrix <- matrix | invert_rows(matrix)
    matrix <- matrix[1:half, ]
  } else if(axis == "y") {
    half <- floor(ncol(matrix) / 2)
    matrix <- matrix | invert_cols(matrix)
    matrix <- matrix[, 1:half]
  } else{
    stop("axis must be one of 'x' or 'y'")
  }
  return(matrix)
}

print_mat <- function(matrix) {
  matrix[matrix == TRUE] <- "#"
  matrix[matrix == FALSE] <- "."

  matrix %>%
    apply(MARGIN = 2, FUN = paste, collapse = "") %>%
    as.matrix() %>%
    print()
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day13
#' @export
example_data_13 <- function(example = 1) {
  l <- list(
    a = c(
      "6,10",
      "0,14",
      "9,10",
      "0,3",
      "10,4",
      "4,11",
      "6,0",
      "6,12",
      "4,1",
      "0,13",
      "10,12",
      "3,4",
      "3,0",
      "8,4",
      "1,10",
      "2,14",
      "8,10",
      "9,0",

      "fold along y=7",
      "fold along x=5"
    )
  )
  l[[example]]
}
