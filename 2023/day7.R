library(tidyverse)
source("utils.R")

## part 1 ----
input <- get_aoc_input(7)
# input = "32T3K 765
# T55J5 684
# KK677 28
# KTJJT 220
# QQQJA 483"

ord <- c(
  setNames(2:9, 2:9),
  "T" = 10,
  "J" = 11,
  "Q" = 12,
  "K" = 13,
  "A" = 14
)
get_score <- \(x) sum(ord[x] * 14^(4:0))
get_type <- \(x) paste(sort(unname(table(x)), decreasing = TRUE), collapse = "")

inp <- data.frame(input = strsplit(input, "\n")[[1]]) |>
  separate_wider_delim(input, names = c("set", "bid"), delim = " ") |>
  rowwise() |>
  mutate(
    cards = strsplit(set, ""),
    type = get_type(cards),
    score = get_score(cards)
  ) |>
  arrange(type, score) |>
  rownames_to_column("rank") |>
  mutate(wins = as.numeric(rank) * as.numeric(bid))
inp
sum(inp$wins)

## part 2 ----

ord2 <- ord
ord2["J"] <- 1

get_score2 <- \(x) sum(ord2[x] * 14^(4:0))
get_type2 <- function(x) {
  if (identical(x, rep("J", 5))) {
    return("5")
  }
  y <- table(x)
  taby <- sort(y[names(y) != "J"], decreasing = TRUE)
  if (!is.na(y["J"])) {
    taby[1] <- taby[1] + y["J"]
  }
  paste(taby, collapse = "")
}

inp <- data.frame(input = strsplit(input, "\n")[[1]]) |>
  separate_wider_delim(input, names = c("set", "bid"), delim = " ") |>
  rowwise() |>
  mutate(
    cards = strsplit(set, ""),
    type = get_type2(cards),
    score = get_score2(cards)
  ) |>
  arrange(type, score) |>
  rownames_to_column("rank") |>
  mutate(wins = as.numeric(rank) * as.numeric(bid))
inp
sum(inp$wins)
