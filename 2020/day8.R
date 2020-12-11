library(aocodeR)


source('utils.R')
inday8 <- get_aoc_input(8, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(strsplit(inday8, '\n')[[1]], ' ') %>%
  do.call(rbind, .) %>%
  `colnames<-`(c('op', 'val')) %>%
  as.data.frame()
input$val <- as.numeric(input$val)

### Part 1

acc <- 0
i <- 1
i_vec <- vector()
repeat{
  i_vec <- c(i_vec, i)
  if (input[i, 'op'] == 'nop'){
    i <- i + 1
  } else if (input[i, 'op'] == 'jmp'){
    i <- i + input[i, 'val']
  } else {
    acc <- acc + input[i, 'val']
    i <- i + 1
  }

  if (i %in% i_vec) break
}

acc

### Part 2
input_ori = input
input_ori$new_op <- ifelse(
  input$op == 'nop', 'jmp',
  ifelse(input$op == 'jmp', 'nop', 'acc')
)
j = 0
repeat{
  j = j + 1
  # print(j)
  input = input_ori
  input[j, 'op'] = input[j , 'new_op']
  i_vec <- vector()
  i = 1
  acc = 0
  repeat{
    i_vec <- c(i_vec, i)
    if (input[i, 'op'] == 'nop'){
      i <- i + 1
    } else if (input[i, 'op'] == 'jmp'){
      i <- i + input[i, 'val']
    } else {
      acc <- acc + input[i, 'val']
      i <- i + 1
    }
    if (i == nrow(input)) break
    if (i %in% i_vec) break
  }
  if (i == nrow(input)) break
  if (j == nrow(input)) break
}

acc
