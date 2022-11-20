#' Day 05: Hydrothermal Venture
#'
#' [Hydrothermal Venture](https://adventofcode.com/2021/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' You come across a field of [hydrothermal
#' vents](https://en.wikipedia.org/wiki/Hydrothermal_vent) on the ocean
#' floor! These vents constantly produce large, opaque clouds, so it would
#' be best to avoid them if possible.
#'
#' They tend to form in *lines*; the submarine helpfully produces a list of
#' nearby [lines of vents]{title="Maybe they're Bresenham vents."} (your
#' puzzle input) for you to review. For example:
#'
#'     0,9 -> 5,9
#'     8,0 -> 0,8
#'     9,4 -> 3,4
#'     2,2 -> 2,1
#'     7,0 -> 7,4
#'     6,4 -> 2,0
#'     0,9 -> 2,9
#'     3,4 -> 1,4
#'     0,0 -> 8,8
#'     5,5 -> 8,2
#'
#' Each line of vents is given as a line segment in the format
#' `x1,y1 -> x2,y2` where `x1`,`y1` are the coordinates of one end the line
#' segment and `x2`,`y2` are the coordinates of the other end. These line
#' segments include the points at both ends. In other words:
#'
#' -   An entry like `1,1 -> 1,3` covers points `1,1`, `1,2`, and `1,3`.
#' -   An entry like `9,7 -> 7,7` covers points `9,7`, `8,7`, and `7,7`.
#'
#' For now, *only consider horizontal and vertical lines*: lines where
#' either `x1 = x2` or `y1 = y2`.
#'
#' So, the horizontal and vertical lines from the above list would produce
#' the following diagram:
#'
#'     .......1..
#'     ..1....1..
#'     ..1....1..
#'     .......1..
#'     .112111211
#'     ..........
#'     ..........
#'     ..........
#'     ..........
#'     222111....
#'
#' In this diagram, the top left corner is `0,0` and the bottom right
#' corner is `9,9`. Each position is shown as *the number of lines which
#' cover that point* or `.` if no line covers that point. The top-left pair
#' of `1`s, for example, comes from `2,2 -> 2,1`; the very bottom row is
#' formed by the overlapping lines `0,9 -> 5,9` and `0,9 -> 2,9`.
#'
#' To avoid the most dangerous areas, you need to determine *the number of
#' points where at least two lines overlap*. In the above example, this is
#' anywhere in the diagram with a `2` or larger - a total of `5` points.
#'
#' Consider only horizontal and vertical lines. *At how many points do at
#' least two lines overlap?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'

parse_input_05 <- function(input) {
  input |>
    stringr::str_remove_all(pattern = " ") |>
    stringr::str_split(pattern = "->|,", simplify = TRUE) |>
    apply(x, MARGIN = c(1,2), FUN = as.numeric)
}

solve <- function(lines, include_diagonal = FALSE, test = FALSE) {
  size <- max(lines) + 1
  diagram <- matrix(0, nrow = size, ncol = size)
  colnames(lines) <- c("x1", "y1", "x2", "y2")

  for(i in seq_along(1:nrow(lines))) {
    row_index <- column_index <- NULL
    diagonal <- FALSE
    if (lines[i, "y1"] == lines[i, "y2"]){
      row_index <- lines[i, "x1"]:lines[i, "x2"] + 1
      column_index <- lines[i, "y1"] + 1
    }else if (lines[i, "x1"] == lines[i, "x2"]){
      column_index <- lines[i, "y1"]:lines[i, "y2"] + 1
      row_index <- lines[i, "x1"] + 1
    }else if (include_diagonal){
      row_index <- lines[i, "x1"]:lines[i, "x2"] + 1
      column_index <- lines[i, "y1"]:lines[i, "y2"] + 1
      diagonal <- TRUE
    }
    if(diagonal){
      diag(diagram[row_index, column_index]) <- diag(diagram[row_index, column_index]) + 1
    } else {
      diagram[row_index, column_index] <- diagram[row_index, column_index] + 1
    }
  }

  solution <- sum(diagram > 1)

  if(test) {
    return(t(diagram))
  }

  return(solution)
}


#' @param x some data
#' @return For Part One, `f05a(x)` returns .... For Part Two,
#'   `f05b(x)` returns ....
#' @export
#' @examples
#' f05a(example_data_05())
#' f05b()
f05a <- function(x) {
  x |>
    parse_input_05() |>
    solve()
}

#' @rdname day05
#' @export
f05b <- function(x) {
  x |>
    parse_input_05() |>
    solve(include_diagonal = TRUE)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    a = c(
      "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2"
    )
  )
  l[[example]]
}
