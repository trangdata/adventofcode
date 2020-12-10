source('utils.R')

inday6 <- get_aoc_input(6, 2020, Sys.getenv('COOKIE_PATH'))

## Part 1
input <- strsplit(strsplit(inday6, '\n\n')[[1]], '\n|')
lapply(input, function(x)
  length(setdiff(unique(x), ''))) %>% unlist() %>% sum()

## Part 2
input <- strsplit(strsplit(inday6, '\n\n')[[1]], '\n')
lapply(input, function(x)
  strsplit(x, "") %>%
    Reduce(intersect, .) %>%
    length()) %>%
  unlist() %>%
  sum()
