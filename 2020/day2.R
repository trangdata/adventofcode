source('utils.R')
library(tidyverse)

inday2 <- get_aoc_input(2, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday2, '\n')[[1]] %>%
  gsub(':', '', .) %>%
  strsplit(' ') %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  separate(X1, c('min_let', 'max_let'), '-') %>%
  mutate(min_let = as.numeric(min_let),
         max_let = as.numeric(max_let),
         n_lets = str_count(X3, X2),
         valid = n_lets >= min_let & n_lets <= max_let,
         str_min = str_sub(X3, min_let, min_let),
         str_max = str_sub(X3, max_let, max_let),
         new_valid = xor(str_min == X2, str_max == X2))

sum(input$valid)
sum(input$new_valid)
