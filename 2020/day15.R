source('utils.R')
library(tidyverse)

inday15 <- get_aoc_input(15, 2020, Sys.getenv('COOKIE_PATH'))
input = strsplit(inday15, ',')[[1]] %>% as.numeric()
# If that was the first time the number has been spoken, the current player says 0.
# Otherwise, the number had been spoken before;
# the current player announces how many turns apart
# the number is from when it was previously spoken.

ex = '0,3,6'
# ex = '3,1,2'
#
input = strsplit(ex, ',')[[1]] %>% as.numeric()

n = 202000

say = input
i = length(say)
cache = (as.list(seq.int(i)) %>% setNames(say))[-i]
cache# say
# cache
temp = tail(say, 1)
system.time(
repeat{
  i = i+1
  newcache = as.character(temp)
  previous = cache[[newcache]]
  cache[[newcache]] = i-1

  if (!is.null(previous)){
    temp = i - 1 - previous
  } else {
    temp = 0
  }
  # say <- c(say, temp)

  if (i > n) break
}
)
# say
# cache
# temp

names(cache[cache == n])
# look at print last value

