source('utils.R')
library(dplyr)

inday13 <- get_aoc_input(13, 2020, Sys.getenv('COOKIE_PATH'))
input = strsplit(inday13, '\n')[[1]]

## part 1
intime = input[1] %>% as.integer()
buses = setdiff(strsplit(input[2], ',')[[1]], 'x') %>% as.numeric()
waittimes = sapply(buses, function(x) x - mod(intime, x))
min(waittimes) * buses[which.min(waittimes)]

## part 2
buses = strsplit(input[2], ',')[[1]]
constraints = data.frame(buses = buses, val = 1:length(buses) - 1) %>%
  filter(buses != 'x') %>%
  mutate(buses = as.integer(buses),
         modulo = mod(val, buses)) %>%
  arrange(desc(buses))

facteur = 1
timei = 0
jnext = constraints[1, 'modulo']
facteur_next = constraints[1, 'buses']
stepi = 1

for (i in seq.int(nrow(constraints))){
  repeat{
    timei = timei + stepi
    if (mod(timei + jnext, facteur_next) == 0) break
  }
  j = constraints[i, 'modulo']
  jnext = constraints[i + 1, 'modulo']
  facteur = constraints[i, 'buses']
  facteur_next = constraints[i + 1, 'buses']
  stepi = stepi * facteur
}

print(timei, digits = 16)
