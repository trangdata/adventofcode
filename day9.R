library(aocodeR)

source('utils.R')
inday9 <- get_aoc_input(9, 2020, Sys.getenv('COOKIE_PATH'))
input <- as.numeric(strsplit(inday9, '\n')[[1]])

for (i in 26:length(input)){
  prev25 <- input[(i - 25):(i - 1)]
  # prev25 <- prev25[prev25 < val]
  added <- outer(prev25, prev25, '+')
  if (!input[i] %in% added[lower.tri(added)]) break
}

inval <- input[i]
inval_idx <- i

for (i in seq.int(inval_idx - 1)) {
  mysum <- cumsum(input[i:(inval_idx - 1)])
  if (inval %in% mysum) {
    first_idx <- i
    increments <- match(inval, mysum) - 1
    break
  }
}

cont_vec <- input[first_idx:(first_idx + increments)]
max(cont_vec) + min(cont_vec)
