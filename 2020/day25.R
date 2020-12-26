source('utils.R')
library(tidyverse)

inday25 <- get_aoc_input(25, 2020, Sys.getenv('COOKIE_PATH'))
input = strsplit(inday25, '\n')[[1]] %>% as.numeric()

val = 1
s = 7

for (i in 1:10^7){
  val = mod(val * s, 20201227)
  if (val == input[1]){
    print(i)
    break
  }
}

val = 1
s = input[2]
for (j in 1:i){
  val = mod(val * s, 20201227)
}
val
