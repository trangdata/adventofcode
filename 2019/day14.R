source('utils.R')
library(tidyverse)

ex <- get_aoc_input(14, 2019, Sys.getenv('COOKIE_PATH'))


ex = '10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL'

input0 <- strsplit(ex, '\n')[[1]] %>%
  strsplit(' => ') %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  `colnames<-`(c('material', 'result')) %>%
  separate_rows(material, sep = ', ') %>%
  separate(material, into = c('num_mat', 'mat'), sep = ' ') %>%
  separate(result, into = c('num_res', 'res'), sep = ' ') %>%
  mutate(num_mat = as.integer(num_mat),
         num_res = as.integer(num_res)) %>%
  {.}

# resi = 'FUEL'
dat = input0 %>%
  filter(res %in% 'FUEL') %>%
  select(num_mat, mat)
sum_ores = 0
first_materials = input0 %>%
  filter(mat == 'ORE') %>%
  pull(res)

repeat{
  dat = input0 %>%
    # filter(res %in% dat$mat) %>%
    right_join(dat, by = c('res' = 'mat'))

  if (nrow(dat) <= 1 && is.na(dat$mat))
    break

  dat = dat %>%
    mutate(num_mat = num_mat.x * ceiling(num_mat.y/num_res)) %>%
    # select(num_mat, mat) %>%
    group_by(mat) %>%
    summarise(num_mat = sum(num_mat), .groups = 'drop')

  sum_ores = sum_ores +
    dat %>% filter(mat == 'ORE') %>% pull(num_mat) %>% sum()
}
sum_ores
