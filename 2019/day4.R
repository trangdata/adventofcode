source('utils.R')
inday4 <- get_aoc_input(4, 2019, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday4, '-')[[1]]

low = input[1]
high = input[2]
# part 1
check_pw <- function(x, one_detail = FALSE){
  xchar = as.integer(x) %>% as.character() %>% strsplit('') %>% `[[`(1)
  cont_diff = diff(as.numeric(xchar))
  if (any(cont_diff < 0))
    return(FALSE)

  if (any(cont_diff == 0)){
    if (one_detail){
      rles = rle(cont_diff)
      zero_len = rles$lengths[rles$values == 0]
      if (any(zero_len == 1))
        return(TRUE)
    } else {
      return(TRUE)
    }
  }
  return(FALSE)
}

n_pws = 0
for (j in low:399999){
  if (check_pw(j))
    n_pws = n_pws + 1
}
for (i in 4:8){
  for (j in (i*111111):((i+1)*10^5-1)){
    if (check_pw(j))
      n_pws = n_pws + 1
  }
}
n_pws

# part 2
n_pws2 = 0
for (j in low:399999){
  if (check_pw(j, TRUE))
    n_pws2 = n_pws2 + 1
}

for (i in 4:8){
  for (j in (i*111111):((i+1)*10^5-1)){
    if (check_pw(j, TRUE))
      n_pws2 = n_pws2 + 1
  }
}
n_pws2
