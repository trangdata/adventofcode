source('utils.R')

inday1 <- get_aoc_input(1, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday1, '\n')[[1]] %>% as.numeric
added <- outer(input, input, '+')
input[which(added == 2020, arr.ind = TRUE)[1,]] %>% prod()

small_input <- input[input < 674]
large_input <- input[input >= 674]
addlarge <- outer(large_input, large_input, '+') %>% c()
addlarge <- addlarge[addlarge < 2020]
addall <- outer(addlarge, small_input, '+')
which(addall == 2020, arr.ind = TRUE)

addsmall <- outer(small_input, small_input, '+')
addall <- outer(c(addsmall), large_input, '+')
which(addall == 2020, arr.ind = TRUE)
addsmal[6]
large_input[24]
small_input[which(addsmall == addsmall[6], arr.ind = T)]
277*337*1406
