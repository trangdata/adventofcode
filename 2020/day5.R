source('utils.R')

inday5 <- get_aoc_input(5, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday5, '\n')[[1]]

convert_seat <- function(x) {
  c(
    substr(x, 1, 7) %>% gsub('F', '0', .) %>%  gsub('B', '1', .) %>% strtoi(base = 2),
    substr(x, 8, 10) %>% gsub('L', '0', .) %>%  gsub('R', '1', .) %>% strtoi(base = 2)
  )
}

indf <- lapply(input, convert_seat) %>%
  do.call(rbind, .) %>%
  as.data.frame()
seats <- indf$V1 * 8 + indf$V2
max(seats)
sort(seats)[(sort(seats) %>% diff() == 2)] + 1
