
source('utils.R')
library(tidyverse)

inday7 <- get_aoc_input(7, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(strsplit(inday7, '\n')[[1]], ' contain ') %>%
  do.call(rbind, .) %>%
  `colnames<-`(c('big_bag', 'small_bags')) %>%
  as.data.frame() %>%
  separate_rows(small_bags, sep = ', ') %>%
  mutate(small_bags = gsub('bag|bags|\\.', '', small_bags) %>% str_trim(),
         big_bag = gsub('bags', '', big_bag) %>% str_trim())

input_new <- str_split_fixed(input$small_bags, ' ', n = 2) %>%
  as.data.frame() %>%
  `colnames<-`(c('n_bags', 'small_bag')) %>%
  cbind(input, .) %>%
  mutate(n_bags = n_bags %>% recode(`no` = '0') %>% as.numeric()) %>%
  select(- small_bags)

input[is.na(input_new$n_bags),]

## Part 1
mybag <- 'shiny gold'
parent = c(mybag)
parents = vector(mode = 'character')
repeat{
  parent = input_new %>%
    filter(small_bag %in% parent) %>%
    pull(big_bag) %>%
    unique()

  if (length(setdiff(parent, parents)) == 0) break
  parents = unique(c(parents, parent))
}

length(parents)

## Part 2
child = input_new %>%
  filter(big_bag == mybag) %>%
  mutate(n_bags_all = n_bags)
n_bigs <- 0
repeat{
  n_bigs <- n_bigs + sum(child$n_bags_all, na.rm = TRUE)

  child = child %>%
    select(n_bags_all, big_bag = small_bag) %>%
    left_join(input_new, by = c('big_bag')) %>%
    mutate('n_bags_all' = n_bags_all*n_bags)

  if (sum(child$small_bag %in% c(NA, 'other')) == nrow(child)) break
}

n_bigs