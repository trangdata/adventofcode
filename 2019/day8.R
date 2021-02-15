source('utils.R')

w = 25
h = 6
inday <- get_aoc_input(8, 2019, Sys.getenv('COOKIE_PATH'))
input = strsplit(inday, '')[[1]] %>%
  matrix(nrow = w*h)
col = input[, which.min(colSums(input == '0'))]
sum(col == '1') * sum(col == '2')

# part 2
library(plot.matrix)
get_non_trans <- function(i){
  rlei = rle(input[i, ])
  rlei$values[rlei$values != 2][1]
}

sapply(1:nrow(input), get_non_trans) %>%
  as.numeric() %>%
  matrix(ncol = w, byrow = T) %>%
  plot()
