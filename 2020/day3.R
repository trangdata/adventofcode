source('utils.R')
library(tidyverse)

inday3 <- get_aoc_input(3, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday3, '\n')[[1]] %>%
  strsplit(split = '') %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  data.matrix() %>%
  {.}

loopj <- ncol(input)

total_trees = 1
for (slope in c(1, 3, 5, 7)){
  i = 1
  j = 1
  ntrees = 0
  while (i <= nrow(input)){
    ntrees = ntrees + input[i, j] - 1
    i = i + 1
    j = j + slope
    if (j > loopj) j = j %% loopj
  }
  total_trees = total_trees*ntrees
}

i = 1
j = 1
ntrees = 0
while (i <= nrow(input)){
  ntrees = ntrees + input[i, j] - 1
  i = i + 2
  j = j + 1
  if (j > loopj) j = j %% loopj
}
total_trees = total_trees*ntrees
