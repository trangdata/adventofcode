source("utils.R")
options(digits = 10)

input <- get_aoc_input(11)
# input = "...#......
# .......#..
# #.........
# ..........
# ......#...
# .#........
# .........#
# ..........
# .......#..
# #...#....."
inp <- strsplit(input, "\n")[[1]] |>
  strsplit("")

## part 1
expansion_factor <- 1

## part2
expansion_factor <- 10^6 - 1

inmat <- matrix(unlist(inp), byrow = TRUE, nrow = length(inp[[1]]))

expands <- inmat == "."
row_expa <- which(rowSums(expands) == ncol(inmat))
col_expa <- which(colSums(expands) == nrow(inmat))
galaxies <- which(inmat == "#", arr.ind = T)

new_rows <- sapply(
  galaxies[, "row"],
  \(x) x + expansion_factor * sum(row_expa < x)
)
new_cols <- sapply(
  galaxies[, "col"],
  \(x) x + expansion_factor * sum(col_expa < x)
)

d1 <- expand.grid(x = new_rows, y = new_rows)
d2 <- expand.grid(x = new_cols, y = new_cols)

(sum(abs(d1$x - d1$y)) +
  sum(abs(d2$x - d2$y))
) / 2
