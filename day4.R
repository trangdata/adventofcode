source('utils.R')
library(tidyverse)
inday4 <- get_aoc_input(4, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday4, '\n\n')[[1]] %>%
  gsub('\n', ' ', .)

input_proc <- input %>% gsub('cid:', '', .)
valid_lens <- lengths(regmatches(input_proc, gregexpr(":", input_proc)))
sum(valid_lens == 7)

## Part 2
validity_check <- function(i){
  res = strsplit(newin[[i]], ':')
  names(res) = lapply(res, `[[`, 1) %>% unlist()
  res = lapply(res, tail, 1)

  hcl = res$hcl
  if (!grepl('\\#[a-z0-9]{6}', hcl))
    return(FALSE)

  ecl = res$ecl
  if (!ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'))
    return(FALSE)

  pid = res$pid
  if (!(nchar(pid) == 9 && grepl('\\d{9}$', pid)))
    return(FALSE)

  byr = res$byr
  if (!(nchar(byr) == 4 &&
        as.numeric(byr) >= 1920 &&
        as.numeric(byr) <= 2002))
    return(FALSE)

  iyr = res$iyr
  if (!(nchar(iyr) == 4 &&
        as.numeric(iyr) >= 2010 &&
        as.numeric(iyr) <= 2020))
    return(FALSE)

  eyr = res$eyr
  if (!(nchar(eyr) == 4 &&
        as.numeric(eyr) >= 2020 &&
        as.numeric(eyr) <= 2030))
    return(FALSE)

  hgt = res$hgt
  hgt_u = substr(hgt, nchar(hgt)-1, nchar(hgt))
  hgt_val = substr(hgt, 1, nchar(hgt)-2)

  if (hgt_u == 'cm'){
    if (hgt_val < 150 || hgt_val > 193)
      return(FALSE)
  } else if (hgt_u == 'in'){
    if (hgt_val < 59 || hgt_val > 76)
      return(FALSE)
  } else {
    return(FALSE)
  }

  return(TRUE)
}

valid_idx = which(valid_lens == 7)
newin <- strsplit(input, ' ')
n_val = 0
for (i in valid_idx){
  n_val = n_val + as.numeric(validity_check(i))
  print(n_val)
}

n_val