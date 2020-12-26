source('utils.R')
library(tidyverse)
library(hash)

# inday15 <- get_aoc_input(15, 2020, Sys.getenv('COOKIE_PATH'))
input = strsplit(inday15, ',')[[1]] %>% as.numeric()

n = 30000000
say = input
x = length(say)

cache = (as.list(seq.int(x)) %>% setNames(say))[-x]
temp = tail(say, 1)
cache_env <- hash(names(cache), cache)

system.time(for (i in ((x + 1):(n + 1))) {
  newcache = as.character(temp)
  previous = cache_env[[newcache]]
  cache_env[[newcache]] = i - 1

  if (!is.null(previous)) {
    temp = i - 1 - previous
  } else {
    temp = 0
  }
})
temp
names(cache_env)[values(cache_env)==n]
