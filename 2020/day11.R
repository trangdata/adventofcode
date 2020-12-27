source('utils.R')
library(purrr)
library(tidyverse)
inday11 <- get_aoc_input(11, 2020, Sys.getenv('COOKIE_PATH'))

adj_seats <- function(inpair) {
  i = inpair[,1]
  j = inpair[,2]
  seats = expand.grid(max(1, i - 1):min(inrow, i + 1),
                      max(1, j - 1):min(incol, j + 1))
  seats = seats[seats[,1] != i | seats[, 2] != j, ]
  return(seats)
}

input0 <- strsplit(strsplit(
  inday11 %>%
    gsub('L', '0', .) %>%
    gsub('#', '1', .) %>%
    gsub('\\.', '2', .),
  split = '\n'
)[[1]], '')

input = input0 %>%
  do.call(rbind, .) %>%
  `class<-`('integer')

inrow = nrow(input)
incol = ncol(input)

alllocs = split(expand.grid(1:inrow, 1:incol), seq(inrow*incol))
allseats = lapply(alllocs, adj_seats)
system.time(
repeat {
  oriin = input
  empty = which(oriin == 0, arr.ind = T)
  if (nrow(empty) > 0) {
    for (idx in (1:nrow(empty))) {
      i = empty[idx, 1]
      j = empty[idx, 2]
      seats = allseats[[(j-1)*inrow + i]]
      if (any(diag(oriin[seats[, 1], seats[, 2]]) == 1))
        next
      input[i, j] = 1
    }
  }
  filled = which(oriin == 1, arr.ind = T)
  if (nrow(filled) > 0) {
    for (idx in 1:nrow(filled)) {
      i = filled[idx, 1]
      j = filled[idx, 2]
      seats = allseats[[(j-1)*inrow + i]]
      if (sum(diag(oriin[seats[, 1], seats[, 2]]) == 1) >= 4)
        input[i, j] = 0
    }
  }

  if (identical(input, oriin))
    break
}
)
sum(input == 1)


# Part 2
sum_all_neighbors = function(i, j){
  sum(get_east(i, j),
      get_ne(i, j),
      get_north(i, j),
      get_nw(i, j),
      get_west(i, j),
      get_sw(i, j),
      get_south(i, j),
      get_se(i, j), na.rm = TRUE)
}

get_first_seat = function(x) x[x != 2][1]
get_last_seat = function(x) tail(x[x != 2], 1)
get_east = function(i, j){
  if (j == incol)
    return(0)
  get_first_seat(input[i, (j+1):incol])
}
get_west = function(i, j){
  if (j == 1)
    return(0)
  get_last_seat(input[i, 1:(j-1)])
}
get_north = function(i, j){
  if (i == 1)
    return(0)
  get_last_seat(input[1:(i-1), j])
}
get_south = function(i, j){
  if (i == inrow)
    return(0)
  get_first_seat(input[(i+1):inrow, j])
}
get_se = function(i, j){
  if (i == inrow || j == incol)
    return(0)
  k = min(inrow - i, incol - j)
  get_first_seat(input[cbind(i + 1:k, j + 1:k)])
}
get_nw = function(i, j){
  if (i == 1 || j == 1)
    return(0)
  k = min(i, j)
  get_first_seat(input[cbind(i - 1:k, j - 1:k)])
}
get_sw = function(i, j){
  if (i == inrow || j == 1)
    return(0)
  k = min(inrow - i, j)
  get_first_seat(input[cbind(i + 1:k, j - 1:k)])
}
get_ne = function(i, j){
  if (i == 1 || j == incol)
    return(0)
  k = min(i, incol - j)
  get_first_seat(input[cbind(i - 1:k, j + 1:k)])
}

## FLIP incol and inrow because IT IS WHAT IT IS
incol = length(input0)
inrow = lengths(input0)[1]

dat = input0 %>%
  unlist() %>%
  as.integer() %>%
  data.frame(
    val = ., i = rep(1:inrow, incol),
    j = rep(1:incol, each = inrow))

input = dat %>%
  pivot_wider(names_from = 'j', values_from = 'val') %>%
  select(-i) %>%
  as.matrix()

repeat{
  oriin = input
  dat = dat %>%
    mutate(sum_neighbors = select(., i, j) %>% pmap_dbl(sum_all_neighbors),
           new_val = case_when(
             val == 1L & sum_neighbors >= 5 ~ 0L,
             val == 0L & sum_neighbors == 0 ~ 1L,
             TRUE ~ val))

  dat = dat %>%
    select(val = new_val, i, j)

  input = dat %>%
    pivot_wider(names_from = 'j', values_from = 'val') %>%
    select(-i) %>%
    as.matrix()

    if (identical(input, oriin))
    break
}

sum(input == 1)

