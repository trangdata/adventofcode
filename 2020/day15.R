source('utils.R')
library(tidyverse)

inday15 <- get_aoc_input(15, 2020, Sys.getenv('COOKIE_PATH'))
input = strsplit(inday15, ',')[[1]] %>% as.numeric()

# n = 2020 # part 1
n = 3*10^7 # part 2
say = input
x = length(say)

temp = tail(say, 1) + 1
cache_env = vector(mode = 'integer', length = n)
cache_env[say[-x] + 1] = 1:(x-1)

system.time(for (i in ((x + 1):(n + 1))) {
  previous = cache_env[temp]
  cache_env[temp] = i - 1

  if (previous != 0) {
    temp = i - previous
  } else {
    temp = 1
  }
})

which(cache_env == n) - 1
