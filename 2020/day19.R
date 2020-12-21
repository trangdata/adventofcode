source('utils.R')
library(tidyverse)

# inday19 <- get_aoc_input(19, 2020, Sys.getenv('COOKIE_PATH'))

input = strsplit(inday19, '\n\n')[[1]]
ex='0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb'
input = strsplit(ex, '\n\n')[[1]]

rules = strsplit(strsplit(input[1], '\n')[[1]], ': ') %>%
  do.call(rbind, .) %>% data.frame()

mssgs = strsplit(input[2], '\n')[[1]]
head(rules)

m = mssgs[4]


m
i = 1

current_rule = rules %>% filter(X1 == '0') %>% pull(X2)
check_basic(m, "1", 1)
# current_rule = "1"
current_char = 1

if (grepl('\\|', current_rule)){
  current_rule = current_rule %>%
    strsplit(' \\| ') %>%
    `[[`(1)
  x = strsplit(current_rule[1], split = ' ')[[1]]
  y = strsplit(current_rule[2], split = ' ')[[1]]
  for (j in x){
    print(j)
    rulej = rules %>% filter(X1 == j) %>% pull(X2)
    res = check_basic(mssgs[i], rulej, current_char)
    if (!res[[1]]) break
  }
  # check_basic(current_rule[1]) || check_basic(current_rule[2])
} else {
  x = strsplit(current_rule, split = ' ')[[1]]
  for (j in x){
    print(j)
    rulej = rules %>% filter(X1 == j) %>% pull(X2)
    res = check_basic(mssgs[i], rulej, current_char)
    if (!res[[1]]) break
    current_char = res[[2]]
  }
}


current_rule = rules %>% filter(X1 == '2') %>% pull(X2)


# current_char = current_char + length(x)
# not good enough what if x[1] has rules that are complicated?
j = '4' # 4th rule: "a"
# i = 1
# m

check_basic(m, "4 1 5", 1)
check_basic <- function(m, current_rule, current_char){
  if (grepl('\"', current_rule)){
    res = grepl(
      current_rule,
      paste0('\"', substr(m, current_char, current_char), '\"'))
    if (!res) return(list(res = FALSE, current_char = 0))
    current_char = current_char + 1
  } else if (grepl('\\|', current_rule)){
    current_rule = strsplit(current_rule, ' \\| ')[[1]]
    x = strsplit(current_rule[1], split = ' ')[[1]]
    y = strsplit(current_rule[2], split = ' ')[[1]]
    for (j in x){
      # print(j)
      rulej = rules %>% filter(X1 == j) %>% pull(X2)
      resx = check_basic(mssgs[i], rulej, current_char)
      if (!resx[[1]]) break
      current_char = res[[2]]
    }

    if (resx[[1]]){
      return(list(res = TRUE, current_char = current_char))
    } else {
      for (j in y){
        # print(j)
        rulej = rules %>% filter(X1 == j) %>% pull(X2)
        resy = check_basic(m, rulej, current_char)
        if (!resy[[1]]) return(list(res = FALSE, current_char = 0))
        current_char = res[[2]]
      }
    }

    # check_basic(current_rule[1]) || check_basic(current_rule[2])
  } else {
    x = strsplit(current_rule, split = ' ')[[1]]
    for (j in x){
      # print(j)
      rulej = rules %>% filter(X1 == j) %>% pull(X2)
      res = check_basic(m, rulej, current_char)
      if (!res[[1]]) return(list(res = FALSE, current_char = 0))
      current_char = res[[2]]
    }
  }
  # res = check_basic(m, current_rule, current_char)
  # if (!res) return(list(res = FALSE, current_char = 0))
  # current_char = res[[2]]

  return(list(res = TRUE, current_char = current_char))
}

if (grepl('\"', current_rule)){
  # res = last_rule(current_rule, mssgs[i], current_char)

}



current_rule = rules %>%
  filter(X1 == '4') %>%
  pull(X2) %>%
  # strsplit(' | ') %>%
  # `[[`(1) %>%
  {.}

current_char = 4

current_rule = rules %>%
  filter(X1 == '2') %>%
  pull(X2) %>%
  # strsplit(' | ') %>%
  `[[`(1)






res
mssgs[i]

# head(input, 200)
# tail(input)

last_rule = function(current_rule, mssg, current_char){
  grepl(
    current_rule,
    paste0('\"',
           substr(mssg,
                  current_char, current_char), '\"'))
}

