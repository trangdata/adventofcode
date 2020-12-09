library(magrittr)
library(httr)
get_aoc_input <- function(day, year = 2020, cookie, encoding = "UTF-8") {
  GET(
    paste0("https://adventofcode.com/", year, "/day/", day,
           "/input"),
    set_cookies(session = cookie)
  ) %>% content("text",
                encoding = encoding) %>% trimws()

}
