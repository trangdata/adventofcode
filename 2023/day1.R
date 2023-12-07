library(tidyverse)
library(stringr)
library(stringi)
library(english)
source("utils.R")

## part 1 ----
# input <- "1a234bc2
# pqr3s234tu28vwx
# a1b223c3d4e5f
# treb7uchet"

# input <- "two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen"

input <- get_aoc_input(day = 1, cookie = Sys.getenv("COOKIE_2023"))
inp <- strsplit(input, "\n")[[1]]

# Extract the first number
first_num <- function(s) str_sub(gsub("\\D", "", s), 1, 1)
last_num <- function(s) str_sub(gsub("\\D", "", s), -1)

sum(as.numeric(paste0(first_num(inp), last_num(inp))))

## part 2 ----
patterns <- english(1:1000)
reps <- as.numeric(patterns) |>
  as.character() |>
  setNames(patterns)

pats <- patterns |>
  as.character() |>
  setNames(as.numeric(patterns))

words <- str_replace_all(inp, pats)
patterns_col <- paste(patterns, collapse = "|")
pattern_rev_col <- paste(stri_reverse(patterns), collapse = "|")

x <- str_extract(words, patterns_col) |>
  `[`(reps, j = _) |>
  first_num()

y <- stri_reverse(words) |>
  str_extract(pattern_rev_col) |>
  stri_reverse() |>
  `[`(reps, j = _) |>
  last_num()

sum(as.numeric(paste0(x, y)))
