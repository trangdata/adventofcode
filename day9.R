library(aocodeR)

source('utils.R')
inday9 <- get_aoc_input(9, 2020, Sys.getenv('COOKIE_PATH'))
in_num <- as.numeric(strsplit(inday9, '\n')[[1]])

for (i in 26:length(in_num)){
  val <- in_num[i]
  prev25 <- in_num[(i - 25):(i - 1)]
  prev25 <- prev25[prev25 < val]
  added <- outer(prev25, prev25, '+')
  if (!val %in% added[lower.tri(added)]){
    print(val)
    inval <- val
    inval_idx <- i
    break
  }
}

for (i in seq.int(inval_idx - 1)) {
  mysum <- cumsum(in_num[i:(inval_idx - 1)])
  if (inval %in% mysum) {
    first_idx <- i
    increments <- which(inval == mysum) - 1
    break
  }
}

cont_vec <- in_num[first_idx:(first_idx + increments)]
max(cont_vec) + min(cont_vec)
