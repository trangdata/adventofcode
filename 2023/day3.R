library(stringr)
source("utils.R")

## part 1 ----
input <- get_aoc_input(3)

# input <- "467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598.."
inp <- strsplit(input, "\n")[[1]]
n <- nchar(inp[[1]])
mychar <- strsplit(inp, "")
inmat <- mychar |>
  unlist() |>
  matrix(ncol = n, byrow = TRUE)

symbols <- setdiff(unique(unlist(mychar)), c(".", as.character(0:9)))
all_nums <- str_extract_all(inp, "\\d+")
all_locs <- str_locate_all(inp, "\\d+")

sub_idx <- function(all_locs, n, i) {
  numloc <- all_locs[[i]]
  row_x <- max(1, i - 1)
  row_y <- min(n, i + 1)
  col_x <- pmax(1, numloc[, "start"] - 1)
  col_y <- pmin(n, numloc[, "end"] + 1)
  list(row_x:row_y, purrr::map2(col_x, col_y, seq))
}

l <- c()
for (i in seq_along(inp)) {
  indx <- sub_idx(all_locs, n, i)
  js <- indx[[2]]
  for (j in seq_along(js)) {
    if (any(symbols %in% inmat[indx[[1]], js[[j]]])) {
      l <- c(l, all_nums[[i]][[j]])
    }
  }
}
head(l)
sum(as.numeric(l))

## part 2 ----

l <- list()
for (i in seq_along(inp)) {
  indx <- sub_idx(all_locs, n, i)
  js <- indx[[2]]
  rows <- indx[[1]]
  for (j in seq_along(js)) {
    star_ind <- which("*" == inmat[rows, js[[j]]], arr.ind = TRUE)
    if (nrow(star_ind) > 0) {
      pos <- c(rows[[1]], js[[j]][[1]]) + star_ind - 1
      id <- as.character(pos[1] * n + pos[2])
      l[[id]] <- c(l[[id]], all_nums[[i]][[j]])
    }
  }
}
lout <- l[lengths(l) > 1] |>
  lapply(as.numeric)

sum(sapply(lout, `[`, 1) * sapply(lout, `[`, 2))
