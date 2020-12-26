source('utils.R')
library(R.utils)
library(tidyverse)
inday14 <- get_aoc_input(14, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday14, '\n')[[1]]

bitsToInt <- function(x) sum(2^(which(rev(x))-1))

masking <- function(x, y){
  a = strsplit(x, '')[[1]]
  b = strsplit(y, '')[[1]]
  a[a == 'X'] <- b[a == 'X']
  bitsToInt(as.logical(as.numeric(a)))
}

mask_address <- function(x, y){
  a = strsplit(x, '')[[1]]
  b = strsplit(y, '')[[1]]
  a[a == '0'] <- b[a == '0']

  floats = do.call(expand.grid, rep(list(0:1), sum(a == 'X'))) %>%
    mutate(filled_dec = select(., matches('Var')) %>%
             purrr::pmap_dbl(fill_float, a = a))

  paste(floats$filled_dec, collapse = ',')
}

fill_float <- function(a, ...){
  a[a == 'X'] <- c(...)
  bitsToInt(as.logical(as.numeric(a)))
}

dat = strsplit(input, ' = ') %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  mutate(mask = ifelse(X1 == 'mask', X2, NA)) %>%
  fill(mask) %>%
  filter(X1 != 'mask') %>%
  mutate(X1 = gsub('mem|\\[|\\]', '', X1))

df1 = dat %>%
  group_by(X1) %>%
  mutate(last = ifelse(row_number() == n(), 1, 0)) %>%
  ungroup() %>%
  filter(last == 1) %>%
  mutate(X2_bin = intToBin(X2) %>% str_pad(36, 'left', pad = '0')) %>%
  rowwise() %>%
  mutate(masked = masking(mask, X2_bin)) %>%
  ungroup()

print(sum(df1$masked), digits = 15)


## part 2

df2 = dat %>%
  mutate(X1_bin = intToBin(X1) %>% str_pad(36, 'left', pad = '0')) %>%
  rowwise() %>%
  mutate(masked_add = mask_address(mask, X1_bin)) %>%
  ungroup() %>%
  separate_rows(masked_add, sep = ',') %>%
  group_by(masked_add) %>%
  mutate(last = ifelse(row_number() == n(), 1, 0)) %>%
  ungroup() %>%
  filter(last == 1) %>%
  {.}

sum(df2$X2 %>% as.numeric()) %>% print(digit = 15)
