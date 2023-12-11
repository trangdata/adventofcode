# library(tidyverse)
# library(stringr)
library(purrr)
source("utils.R")
source("day10-funcs.R")
## part 1 ----
input <- get_aoc_input(10)

# input <- "7-F7-
# .FJ|7
# SJLL7
# |F--J
# LJ.LJ"
# input = "-L|F7
# 7S-7|
# L|7||
# -L-J|
# L|-JF"
# input = "..........
# .S------7.
# .|F----7|.
# .||OOOO||.
# .||OOOO||.
# .|L-7F-J|.
# .|II||II|.
# .L--JL--J.
# .........."
#
input <- ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."
input = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"
inp <- strsplit(input, "\n")[[1]] |>
  strsplit("")
m <- length(inp)
n <- length(inp[[1]])
inmat <- inp |>
  unlist() |>
  matrix(byrow = TRUE, ncol = n)

system.time({
  animals <- part1(inmat)
})
length(animals)/2

outmat <- matrix(0, nrow = m, ncol = n)
for (p in animals){
  outmat[p[[1]], p[[2]]] <- 1
}
inpipe <- inmat
inpipe[outmat == 0] <- "."


## EXPANDING inpipe2 ----
m <- m * 2
n <- n * 2
inpipe2 <- matrix(".", nrow = m, ncol = n)
for (r in seq(2, m, 2)) {
  for (col in seq(2, n, 2)) {
    inpipe2[r, col] <- inpipe[r / 2, col / 2]
  }
}

for (r in seq(2, m, 2)) {
  for (col in seq(2, n, 2)) {
    rcol <- slic(inpipe2, c(r, col))
    rcol2 <- slic(inpipe2, c(r, col + 2))
    r2col <- slic(inpipe2, c(r + 2, col))
    if (rcol %in% c("-", "F", "L") ||
      rcol2 %in% c("-", "7", "J")) {
      inpipe2[r, col + 1] <- "-"
    }

    if (rcol %in% c("|", "F", "7") ||
      r2col %in% c("|", "J", "L")) {
      inpipe2[r + 1, col] <- "|"
    }
  }
}


## Initialize ABmat with A at corners ----
ABmat <- matrix(NA, ncol = n, nrow = m)
indices <- list(c(1, 1), c(1, n), c(m, 1), c(m, n))
for (idx in indices) {
  ABmat[idx[[1]], idx[[2]]] <- "A"
}
i <- 1
while (i <= length(indices)) {
  a <- indices[[i]]
  neighs <- neighbor_simple(a)
  for (ne in neighs) {
    if (is.na(slic(ABmat, ne)) && slic(inpipe2, ne) == ".") {
      ABmat[ne[[1]], ne[[2]]] <- "A"
      indices <- c(indices, list(ne))
    }
  }
  i <- i + 1
}



## EXTRAPOLATE with larger matrix inmat ----
inmat <- inpipe2
npipes <- length(animals)
is <- seq.int(npipes)
animalsb <- vector("list", length = npipes *2)
animalsb[2*(is-1) + 1] <- lapply(animals[is], `*`, y = 2)
for (i in seq(2, 2*npipes, 2)){
  animalsb[[i]] <- (animalsb[[i-1]] + animalsb[[(i+1)%%(2*npipes)]])/2
}

# k = 14
# profvis::profvis({
for (k in 2:length(animalsb)) {
  a <- animalsb[[k]]
  neighs <- neighbors_more(a)
  pipe <- slic(inpipe2, a)
  adjs <- group_pipes(pipe)

  for (i in 1:2) {
    neighi <- neighs[adjs[[i]]]
    adjAB <- neighbors(neighi) |>
      sapply(\(x) slic(ABmat, x)) |>
      # unlist() |>
      na.omit()

    if (length(adjAB)) {
      for (ne in neighi) {
        # if (!is.na(slic(outmat, ne))) {
        # if (is_inside(ne)){
          ABmat[ne[[1]], ne[[2]]] <- adjAB[[1]] # should be unique values
        # }
      }
    }
  }
  for (i in 1:2) {
    neighi <- neighs[adjs[[i]]]
    if (any(is.na(lapply(neighi, \(x) slic(ABmat, x))))) {
      for (ne in neighi) {
        opposite_ind <- neighs[[adjs[[3 - i]][[1]]]]
        opposite_val <- slic(ABmat, opposite_ind)
        if (!is.na(opposite_val)) {
          ABmat[ne[[1]], ne[[2]]] <- revAB[[opposite_val]]
        }
      }
    }
  }
}
# })

ABmat2 <- ABmat
ABmat2[is.na(ABmat2)] <- inpipe2[is.na(ABmat2)]

# fill in ABmat again with either A or B
i <- 1
indices <- which(is.na(ABmat) & !inpipe2 != ".", arr.ind = TRUE)
while (i <= nrow(indices)) {
  a <- indices[i, ]

  adjAB <- neighbor_simple(a) |>
    lapply(\(x) slic(ABmat, x)) |>
    unlist() |>
    na.omit()

  if (length(adjAB) > 0) {
    ABmat[a[[1]], a[[2]]] <- adjAB[[1]] # should be unique values
  }
  i <- i + 1
}

# easy view of ABmat
ABmat2 <- ABmat
ABmat2[is.na(ABmat2)] <- inpipe2[is.na(ABmat2)]

ABreduced <- matrix(".", nrow = m / 2, ncol = n / 2)
for (r in seq(2, m, 2)) {
  for (col in seq(2, n, 2)) {
    ABreduced[r/2, col/2] <- ABmat[r, col]
  }
}
sum(ABreduced == "B", na.rm = TRUE)

