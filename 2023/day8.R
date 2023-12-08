source("utils.R")
library(stringr)
library(purrr)

## part 1 ----
input <- get_aoc_input(8)
# input = "RL
#
# AAA = (BBB, CCC)
# BBB = (DDD, EEE)
# CCC = (ZZZ, GGG)
# DDD = (DDD, DDD)
# EEE = (EEE, EEE)
# GGG = (GGG, GGG)
# ZZZ = (ZZZ, ZZZ)"
# input = "LR
#
# 11A = (11B, XXX)
# 11B = (XXX, 11Z)
# 11Z = (11B, XXX)
# 22A = (22B, XXX)
# 22B = (22C, 22C)
# 22C = (22Z, 22Z)
# 22Z = (22B, 22B)
# XXX = (XXX, XXX)"
directions <- c("R" = 2, "L" = 1)
inp0 <- strsplit(input, "\n")[[1]]
dir <- directions[strsplit(inp0[[1]], "")[[1]]] |>
  unname()
n <- length(dir)
inp <- strsplit(inp0[-(1:2)], " = ")
inst0 <- setNames(map_chr(inp, 2), map_chr(inp, 1))
inst <- lapply(
  strsplit(inst0, ", "),
  \(x) gsub("\\(|\\)", "", x)
)
go <- function(x0, d) {
  map_chr(inst[x0], d)
}

## part 1 ----
x0 <- "AAA"
for (i in 1:10000000) {
  x0 <- go(x0, dir[(i - 1) %% n + 1])
  if (x0 == "ZZZ") {
    break
  }
}
i

## part 2 ----
allx0 <- names(inst)
x0s <- allx0[str_sub(allx0, -1) == "A"]
out <- function(x0) {
  for (i in 1:100000) {
    x0 <- go(x0, dir[(i - 1) %% n + 1])
    if (str_sub(x0, -1) == "Z") {
      return(i)
    }
  }
}

l <- vector("numeric")
for (x0 in x0s) {
  l <- c(l, out(x0))
}
l
Reduce(gmp::lcm.bigz, l)
