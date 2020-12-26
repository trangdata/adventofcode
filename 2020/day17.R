source('utils.R')
library(tidyverse)

inday17 <- get_aoc_input(17, 2020, Sys.getenv('COOKIE_PATH'))
n_sims = 6

input0 = strsplit(inday17, '\n')[[1]] %>%
  gsub('#', 1, .) %>%
  gsub('\\.', 0, .) %>%
  strsplit(split = '')
n_cols = length(input0)


input = input0 %>%
  unlist() %>%
  as.integer() %>%
  data.frame(
    val = ., x = rep(0:(n_cols - 1), each = n_cols),
    y = rep(0:(n_cols - 1), n_cols), z = 0) %>%
  filter(val == 1)
neighbor_delta = crossing(xd = -1:1, yd = -1:1, zd = -1:1) %>%
  filter(!(xd==0 & yd == 0 & zd == 0))

dat = input %>% select(x, y, z, val)

for (i in 1:6){

  proc_dat = dat %>%
    crossing(neighbor_delta) %>%
    mutate(x_neighbor = x + xd,
           y_neighbor = y + yd,
           z_neighbor = z + zd)

  all_neighbors = proc_dat %>%
    select(x = x_neighbor, y = y_neighbor, z = z_neighbor) %>%
    distinct(x, y, z) %>%
    anti_join(select(proc_dat, x, y, z)) %>%
    mutate(val = 0) %>% crossing(neighbor_delta) %>%
    mutate(x_neighbor = x + xd,
           y_neighbor = y + yd,
           z_neighbor = z + zd)

  proc_dat = proc_dat %>%
    bind_rows(all_neighbors) %>%
    select(-ends_with('d')) %>%
    left_join(dat, by = c('x_neighbor' = 'x',
                          'y_neighbor' = 'y',
                          'z_neighbor' = 'z')) %>%
    replace_na(list(val.y = 0)) %>%
    group_by(val.x, x, y, z) %>%
    summarise(sum_neighbors = sum(val.y), .groups = 'drop') %>%
    mutate(val = case_when(
      val.x == 1 & sum_neighbors != 3 & sum_neighbors != 2 ~ 0,
      val.x == 0 & sum_neighbors == 3 ~ 1,
      TRUE ~ val.x
    ))

  dat = proc_dat %>% filter(val == 1) %>% select(x, y, z, val)

}

sum(proc_dat$val)


## part 2

input = input0 %>%
  unlist() %>%
  as.integer() %>%
  data.frame(
    val = ., x = rep(0:(n_cols - 1), each = n_cols),
    y = rep(0:(n_cols - 1), n_cols), z = 0, w = 0) %>%
  filter(val == 1)
neighbor_delta = crossing(xd = -1:1, yd = -1:1, zd = -1:1, wd = -1:1) %>%
  filter(!(xd==0 & yd == 0 & zd == 0 & wd == 0))

dat = input %>% select(x, y, z, w, val)
for (i in 1:6){
  proc_dat = dat %>%
    crossing(neighbor_delta) %>%
    mutate(x_neighbor = x + xd,
           y_neighbor = y + yd,
           z_neighbor = z + zd,
           w_neighbor = w + wd)

  all_neighbors = proc_dat %>%
    select(x = x_neighbor, y = y_neighbor,
           z = z_neighbor, w = w_neighbor) %>%
    distinct(x, y, z, w) %>%
    anti_join(select(proc_dat, x, y, z, w)) %>%
    mutate(val = 0) %>% crossing(neighbor_delta) %>%
    mutate(x_neighbor = x + xd,
           y_neighbor = y + yd,
           z_neighbor = z + zd,
           w_neighbor = w + wd)

  proc_dat = proc_dat %>%
    bind_rows(all_neighbors) %>%
    select(-ends_with('d')) %>%
    left_join(dat, by = c('x_neighbor' = 'x',
                          'y_neighbor' = 'y',
                          'z_neighbor' = 'z',
                          'w_neighbor' = 'w')) %>%
    replace_na(list(val.y = 0)) %>%
    group_by(val.x, x, y, z, w) %>%
    summarise(sum_neighbors = sum(val.y), .groups = 'drop') %>%
    mutate(val = case_when(
      val.x == 1 & sum_neighbors != 3 & sum_neighbors != 2 ~ 0,
      val.x == 0 & sum_neighbors == 3 ~ 1,
      TRUE ~ val.x
    ))

  dat = proc_dat %>% filter(val == 1) %>% select(x, y, z, w, val)

}

sum(proc_dat$val)
