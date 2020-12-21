source('utils.R')
inday10 <- get_aoc_input(10, 2020, Sys.getenv('COOKIE_PATH'))
input <- as.numeric(strsplit(inday10, '\n')[[1]])

## Part 1
newin <- c(0, sort(input), max(input) + 3)
x <- newin %>% diff()
table(x)

## Part 2
count_seq <- rle(x)$lengths[rle(x)$values == 1]
count_seq[count_seq == 4] <- 7
count_seq[count_seq == 3] <- 4
count_seq[count_seq == 2] <- 2
prod(count_seq)
