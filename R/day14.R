#' Day 14: Extended Polymerization
#'
#' [Extended Polymerization](https://adventofcode.com/2021/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' The incredible pressures at this depth are starting to put a strain on
#' your submarine. The submarine has
#' [polymerization](https://en.wikipedia.org/wiki/Polymerization) equipment
#' that would produce suitable materials to reinforce the submarine, and
#' the nearby volcanically-active caves should even have the necessary
#' input elements in sufficient quantities.
#'
#' The submarine manual contains [instructions]{title="HO
#'
#' HO -> OH"} for finding the optimal polymer formula; specifically, it
#' offers a *polymer template* and a list of *pair insertion* rules (your
#' puzzle input). You just need to work out what polymer would result after
#' repeating the pair insertion process a few times.
#'
#' For example:
#'
#'     NNCB
#'
#'     CH -> B
#'     HH -> N
#'     CB -> H
#'     NH -> C
#'     HB -> C
#'     HC -> B
#'     HN -> C
#'     NN -> C
#'     BH -> H
#'     NC -> B
#'     NB -> B
#'     BN -> B
#'     BB -> N
#'     BC -> B
#'     CC -> N
#'     CN -> C
#'
#' The first line is the *polymer template* - this is the starting point of
#' the process.
#'
#' The following section defines the *pair insertion* rules. A rule like
#' `AB -> C` means that when elements `A` and `B` are immediately adjacent,
#' element `C` should be inserted between them. These insertions all happen
#' simultaneously.
#'
#' So, starting with the polymer template `NNCB`, the first step
#' simultaneously considers all three pairs:
#'
#' -   The first pair (`NN`) matches the rule `NN -> C`, so element `C` is
#'     inserted between the first `N` and the second `N`.
#' -   The second pair (`NC`) matches the rule `NC -> B`, so element `B` is
#'     inserted between the `N` and the `C`.
#' -   The third pair (`CB`) matches the rule `CB -> H`, so element `H` is
#'     inserted between the `C` and the `B`.
#'
#' Note that these pairs overlap: the second element of one pair is the
#' first element of the next pair. Also, because all pairs are considered
#' simultaneously, inserted elements are not considered to be part of a
#' pair until the next step.
#'
#' After the first step of this process, the polymer becomes `NCNBCHB`.
#'
#' Here are the results of a few steps using the above rules:
#'
#'     Template:     NNCB
#'     After step 1: NCNBCHB
#'     After step 2: NBCCNBBBCBHCB
#'     After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
#'     After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
#'
#' This polymer grows quickly. After step 5, it has length 97; After step
#' 10, it has length 3073. After step 10, `B` occurs 1749 times, `C` occurs
#' 298 times, `H` occurs 161 times, and `N` occurs 865 times; taking the
#' quantity of the most common element (`B`, 1749) and subtracting the
#' quantity of the least common element (`H`, 161) produces
#' `1749 - 161 = 1588`.
#'
#' Apply 10 steps of pair insertion to the polymer template and find the
#' most and least common elements in the result. *What do you get if you
#' take the quantity of the most common element and subtract the quantity
#' of the least common element?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'

f14_get_template <- function(x) {
  return(x[1])
}

f14_get_rule <- function(x) {
  x %>%
    stringr::str_subset(pattern = "->") %>%
    stringr::str_split("->", simplify = TRUE) %>%
    `colnames<-`(c("search", "insertion")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(stringr::str_trim)
}

get_subt <- function(string, n) {
  stringr::str_sub(string, n, n+1)
}

insert_step <- function(string, rules) {
  N <- nchar(string)
  string %>%
    get_subt(1:N) %>%
    tibble::as_tibble() %>%
    #df %>%
    dplyr::left_join({
      rules %>%
        dplyr::mutate(insertion = stringr::str_c(stringr::str_sub(search, 1, 1), insertion))
    }, by = c("value" = "search")) %>%
    dplyr::mutate(insertion = dplyr::if_else(is.na(insertion), value, insertion)) %>%
    dplyr::pull(insertion) %>%
    stringr::str_c(collapse = "")
}

insert_n_steps <- function(string, rules, n, print = FALSE) {
  out <- string
  for(i in seq_len(n)) {
    out <- df_insert(out, rules)
    print(i)
    if(print) print(out)
  }
  return(out)
}

count_string <- function(string) {
  letters <- string %>%
    stringr::str_split(pattern = "", simplify = TRUE) %>%
    as.vector() %>%
    unique()

  df <- tibble::tibble(LETTER = letters,
                       COUNT = stringr::str_count(string, letters))

  return(df)
}

matrix_exp <- function(matrix, n) {
  stopifnot(n > 1)

  for(i in seq_len(n-1)) {
    if(i == 1) {
      result <- matrix %*% matrix
    } else{
      result <- result %*% matrix
    }
  }
  return(result)
}

#' @param x some data
#' @return For Part One, `f14a(x)` returns .... For Part Two,
#'   `f14b(x)` returns ....
#' @export
#' @examples
#' f14a(example_data_14())
#' f14b()
f14a <- function(x) {
  r <- f14_get_rule(x)
  string <- f14_get_template(x)

  output <- insert_n_steps(string, r, 10)

  count_string(output) %>%
    dplyr::arrange(desc(COUNT)) %>%
    dplyr::summarise(solution = diff(range(COUNT)))
}


#' @rdname day14
#' @export
f14b <- function(x, n = 40) {
  rules <- f14_get_rule(x)
  string <- f14_get_template(x)

  rules <- rules %>%
    mutate(new_pair_l = stringr::str_c(stringr::str_sub(search, 1, 1), insertion),
           new_pair_r = stringr::str_c(insertion, stringr::str_sub(search, 2, 2)))

  pairs <- rules$search
  num_pairs <- length(pairs)

  m <- matrix(0, num_pairs, num_pairs, dimnames = list(pairs, pairs))
  m[cbind(pairs, rules$new_pair_l)] <- 1
  m[cbind(pairs, rules$new_pair_r)] <- 1
  #m[cbind(rules$search, rules$new_pair_r)] <- m[cbind(rules$search, rules$new_pair_r)] + 1


  patterns_present <- stringr::str_count(string = string, pattern = possible_pairs)
  counts <- patterns_present %*% matrix_exp(m, n)

  first_string_char <- stringr::str_sub(string, 1, 1)

  tibble(n = c(counts), pair = colnames(counts)) %>%
    mutate(second_char = stringr::str_sub(pair, 2, 2)) %>%
    group_by(second_char) %>%
    summarise(n = sum(n)) %>%
    mutate(n = if_else(second_char == first_string_char, n+1, n)) %>%
    summarise(solution = diff(range(n)))
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_data_14 <- function(example = 1) {
  l <- list(
    a = c(
      "NNCB",
      "CH -> B",
      "HH -> N",
      "CB -> H",
      "NH -> C",
      "HB -> C",
      "HC -> B",
      "HN -> C",
      "NN -> C",
      "BH -> H",
      "NC -> B",
      "NB -> B",
      "BN -> B",
      "BB -> N",
      "BC -> B",
      "CC -> N",
      "CN -> C"
    )
  )
  l[[example]]
}
