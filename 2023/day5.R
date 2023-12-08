library(stringr)
source("utils.R")

## part 1 ----

input = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"
input <- get_aoc_input(5)
inp0 <- strsplit(input, "\n\n")[[1]]
inp <- gsub(".* map:\n", "", x = inp0)

maps <- list()
for (map_ind in 1:7){
  maps[[map_ind]] <- strsplit(inp[map_ind + 1], "\n")[[1]] |>
    strsplit(" ") |>
    lapply(as.numeric)
}

get_mapped <- function(s, type){
  map <- maps[[type]]
  out <- s
  for (m in map){
    tf <- s >= m[[2]] & s <  m[[2]] + m[[3]]
    vec <- m[[1]] + s - m[[2]]
    out[tf] <- vec[tf]
  }
  out
}

seeds <- as.numeric(strsplit(inp[1], " ")[[1]][-1])
locations <- purrr::reduce(1:7, get_mapped, .init = seeds)
min(locations)


## part 2 ----

seed_df <- data.frame(matrix(seeds, ncol = 2, byrow = TRUE)) |>
  dplyr::mutate(X3 = X1 + X2 - 1)
seed_df
rev_mapped <- function(s, type){
  map <- maps[[type]]
  out <- s
  for (m in map){
    tf <- s >= m[[1]] & s <  m[[1]] + m[[3]]
    vec <- m[[2]] + s - m[[1]]
    out[tf] <- vec[tf]
  }
  out
}
size <- 10^5
for (i in 1:100){
  print(i)
  small_locs <- seq(size*(i-1) + 1, size*i)
  small_seeds <- purrr::reduce(7:1, rev_mapped, .init = small_locs)
  for (i in seq.int(nrow(seed_df))){
    found <- dplyr::between(small_seeds, seed_df$X1[[i]], seed_df$X3[[i]])
    if (any(found)){
      print("Seed found!")
      found_seed <- small_seeds[found]
      break
    }
  }
  if (any(found)){
    break
  }
}
head(found_seed)
locations <- purrr::reduce(1:7, get_mapped, .init = found_seed)
min(locations)
