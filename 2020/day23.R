source('utils.R')
library(tidyverse)

inday23 <- get_aoc_input(23, 2020, Sys.getenv('COOKIE_PATH'))
input = as.numeric(strsplit(inday23, '')[[1]])

myarray = c(input[-1], input[1])[order(input)]
min_val = min(myarray)
max_val = max(myarray)

this_val = input[1]

for (i in 1:100){
  first = myarray[this_val]
  second = myarray[first]
  third = myarray[second]

  dest_val = this_val - 1
  if (dest_val == 0)
    dest_val = max_val
  while (dest_val %in% c(first, second, third)) {
    dest_val = dest_val - 1
    if (dest_val < min_val)
      dest_val = max_val
  }
  fourth = myarray[dest_val]
  myarray[dest_val] = first
  myarray[this_val] = myarray[third]
  myarray[third] = fourth
  this_val = myarray[this_val]
}
print(myarray)


## part 2
ninputs = 10^6
modified = c(input, (max(input) + 1):ninputs)
myarray = c(modified[-1], modified[1])[order(modified)]
min_val = min(myarray)
max_val = max(myarray)
this_val = modified[1]

for (i in 1:10^7){
  first = myarray[this_val]
  second = myarray[first]
  third = myarray[second]

  dest_val = this_val - 1
  if (dest_val == 0)
    dest_val = max_val
  while (dest_val %in% c(first, second, third)) {
    dest_val = dest_val - 1
    if (dest_val < min_val)
      dest_val = max_val
  }
  fourth = myarray[dest_val]
  myarray[dest_val] = first
  myarray[this_val] = myarray[third]
  myarray[third] = fourth
  this_val = myarray[this_val]
}

myarray[1]*myarray[myarray[1]]
