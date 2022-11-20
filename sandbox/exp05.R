test_input <- c(
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
test_input

# One string
"0,9 -> 5,9" |>
  stringr::str_remove_all(pattern = " ") |>
  stringr::str_split(pattern = "->|,", simplify = TRUE) |>
  as.numeric()

# 2+ string
x <- c("0,9 -> 5,9", "8,0 -> 0,8") |>
  stringr::str_remove_all(pattern = " ") |>
  stringr::str_split(pattern = "->|,", simplify = TRUE)

# convert to numeric
x <- apply(x, MARGIN = c(1,2), FUN = as.numeric)

# data frame?
tibble::as_tibble(x)



lines <- parse_input(test_input)

solve <- function(lines, include_diagonal = FALSE, test = FALSE) {
  size <- max(lines) + 1
  diagram <- matrix(0, nrow = size, ncol = size)
  # when x1 == x2, then create a set of co-ords


  colnames(lines) <- c("x1", "y1", "x2", "y2")

  # lines[lines[, "y1"] == lines[, "y2"],   ]
  #
  # lines[1,1]:lines[1,3]
  #
  # m <- diagram
  # m[0:5, 9] <- -1
  # m

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

lines |>
  solve(include_diagonal = TRUE, test = TRUE)

lines |>
  solve(include_diagonal = TRUE)

readLines("inst/input05.txt") |>
  parse_input() |>
  solve()

readLines("inst/input05.txt") |>
  parse_input() |>
  solve(include_diagonal = TRUE)
