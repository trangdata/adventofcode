source('utils.R')
library(tidyverse)

# inday21 <- get_aoc_input(21, 2020, Sys.getenv('COOKIE_PATH'))

input = strsplit(strsplit(inday21, '\n')[[1]], split = "\\(") %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  rownames_to_column('food') %>%
  separate_rows(X1, sep = " ") %>%
  filter(X1 != "") %>%
  mutate(X2 = gsub("contains |)", "", X2)) %>%
  separate_rows(X2, sep = ", ") %>%
  {.}

multi_foods = unique(input$X2)

allermap = list()
old_allermap = allermap

while (TRUE){
  for (aller in c(multi_foods)){
    if (length(allermap[[aller]]) == 1) next
    aller_df = input %>%
      filter(X2 == aller,
             !(X1 %in% allermap[lengths(allermap) == 1])) %>%
      add_count(X1) %>%
      filter(n == max(n))
    allermap[[aller]] = unique(aller_df$X1)
  }
  if (identical(old_allermap, allermap)) break
  old_allermap = allermap
}

allermap

input %>%
  distinct(food, X1) %>%
  filter(!(X1 %in% unique(unlist(allermap)))) %>%
  pull(X1) %>%
  length()

## part 2
allermap[order(names(allermap))] %>% unlist() %>% unname() %>% cat(sep = ',')

# smfz,vhkj,qzlmr,tvdvzd,lcb,lrqqqsg,dfzqlk,shp

