source("utils.R")

## part 1 ----
input <- get_aoc_input(6)

# input = "Time:      7  15   30
# Distance:  9  40  200"

inp <- strsplit(input, "\n")[[1]] |>
  gsub(".*: ", "", x = _)

inl <- strsplit(inp, "\\s") |>
  lapply(setdiff, "") |>
  lapply(as.numeric) |>
  do.call(rbind, args = _)

dist <- function(x, a) x * (a - x)
wins <- function(x) sum(dist(0:x[1], x[1]) > x[2])
l <- vector("numeric", length = ncol(inl))
for (i in seq.int(ncol(inl))) {
  l[i] <- wins(inl[, i])
}
prod(l)

## part 2 ----

solv <- function(u, v) {
  ceiling((-u + sqrt(u^2 - 4 * v)) / 2 - 1) -
    floor((-u - sqrt(u^2 - 4 * v)) / 2 + 1) + 1
}
do.call(solv, as.list(as.numeric(gsub("\\s", "", inp))))

# check for part 1
prod(sapply(1:4, \(i) do.call(solv, as.list(inl[, i]))))
