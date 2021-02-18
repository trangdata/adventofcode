source('utils.R')
inday2 <- get_aoc_input(2, 2019, Sys.getenv('COOKIE_PATH'))
input0 <- strsplit(inday2, ',')[[1]] %>% as.numeric()

nv = expand.grid(1:100, 1:100)
for (row in 1:nrow(nv)) {
  input = input0

  input[2] = nv[row, 1]
  input[3] = nv[row, 2]

  for (j in seq(1, length(input), 4)) {
    oriin = input
    if (input[j] == 99) {
      break
    } else if (input[j] == 1) {
      input[oriin[j + 3] + 1] =
        input[oriin[j + 1] + 1] +
        input[oriin[j + 2] + 1]
    } else {
      input[oriin[j + 3] + 1] =
        input[oriin[j + 1] + 1] *
        input[oriin[j + 2] + 1]
    }
  }

  if (input[1] == 19690720) {
    print(row)
    break
  }
}
sum(nv[row, ]*c(100, 1))
