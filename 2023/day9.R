source("utils.R")

## part 1 ----
input <- get_aoc_input(9)

# input <- "0 3 6 9 12 15
# 1 3 6 10 15 21
# 10 13 16 21 30 45"

inp <- strsplit(input, "\n")[[1]] |>
  strsplit(" ") |>
  lapply(as.integer)

ls <- vector("numeric")
for (i in inp) {
  l <- tail(i, 1)
  while (!all(i == 0)) {
    i <- diff(i)
    l <- l + tail(i, 1)
  }
  ls <- c(ls, l)
}
sum(ls)


## part 2 ----

ls <- vector("numeric")
for (i in inp) {
  l <- head(i, 1)
  s <- 1
  while (!all(i == 0)) {
    i <- diff(i)
    s <- s * (-1)
    l <- l + s * head(i, 1)
  }
  ls <- c(ls, l)
}
sum(ls)
