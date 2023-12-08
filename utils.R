library(httr)
get_aoc_input <- function(day, year = 2023, cookie = Sys.getenv("TRANG_COOKIE"), encoding = "UTF-8") {
  GET(
    paste0("https://adventofcode.com/", year, "/day/", day, "/input"),
    set_cookies(session = cookie)
  ) |>
    content("text", encoding = encoding) |>
    trimws()
}

bitsToInt <- function(x) sum(2^(which(rev(x))-1))
