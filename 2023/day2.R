library(stringr)
source("utils.R")

## part 1 ----
input <- get_aoc_input(2)

# input <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
inp <- strsplit(input, "\n")[[1]]

make_name <- function(x){
  x <- matrix(x, nrow = 2)
  y <- setNames(as.numeric(x[1, ]), x[2, ])
  c(
    red = max(y[names(y) == "red"]),
    blue = max(y[names(y) == "blue"]),
    green = max(y[names(y) == "green"])
  )
}

max_cube <- c(red = 12, green = 13, blue = 14)
check_max <- function(x) x["red"] <= 12 && x["green"] <= 13 && x["blue"] <= 14
tallied <- gsub("Game \\d+: ", "", inp) |>
  str_split("; ") |>
  lapply(str_split, pattern = ", ") |>
  lapply(unlist) |>
  lapply(str_split, pattern = " ") |>
  lapply(unlist)

tallied |>
  lapply(make_name) |>
  sapply(check_max) |>
  which() |>
  sum()

## part 2 ----
make_prod <- function(x){
  x <- matrix(x, nrow = 2)
  y <- setNames(as.numeric(x[1, ]), x[2, ])
  max(y[names(y) == "red"]) *
    max(y[names(y) == "blue"]) *
    max(y[names(y) == "green"])
}
tallied |>
  lapply(make_prod) |>
  unlist() |>
  sum()
