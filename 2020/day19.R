source('utils.R')
library(tidyverse)

inday19 <- get_aoc_input(19, 2020, Sys.getenv('COOKIE_PATH'))
input = strsplit(inday19, '\n\n')[[1]]

check_basic <- function(m, this_rule, this_char) {
  max_char = nchar(m) + 1

  if (grepl('\"', this_rule)) {
    rule_char = nchar(this_rule) - 2
    res = grepl(this_rule,
                paste0('\"', substr(m, this_char, this_char + rule_char - 1), '\"'))
    if (!res)
      return(list(res = FALSE, this_char = 0))
    this_char = this_char + rule_char

  } else if (grepl('\\|', this_rule)) {
    this_rule = strsplit(this_rule, ' \\| ')[[1]]
    x = strsplit(this_rule[1], split = ' ')[[1]]
    y = strsplit(this_rule[2], split = ' ')[[1]]

    charx = this_char
    for (j in x) {
      rulej = rules %>% filter(X1 == j) %>% pull(X2)
      resx = check_basic(m, rulej, charx)
      if (!resx[[1]])
        break
      charx = resx[[2]]
      if (charx > max_char)
        break
    }

    if (resx[[1]]) {
      return(list(res = TRUE, this_char = resx[[2]]))
    } else {
      chary = this_char
      for (j in y) {
        rulej = rules %>% filter(X1 == j) %>% pull(X2)
        resy = check_basic(m, rulej, chary)
        if (!resy[[1]])
          return(list(res = FALSE, this_char = 0))
        chary = resy[[2]]
        if (chary > max_char)
          return(list(res = FALSE, this_char = 0))
      }
      return(list(res = TRUE, this_char = resy[[2]]))
    }
  } else {
    x = strsplit(this_rule, split = ' ')[[1]]
    for (j in x) {
      rulej = rules %>% filter(X1 == j) %>% pull(X2)
      res = check_basic(m, rulej, this_char)
      if (!res[[1]])
        return(list(res = FALSE, this_char = 0))
      this_char = res[[2]]
    }
  }

  return(list(res = TRUE, this_char = this_char))
}

check811 = function(mssg, fix_length = 8) {
  mchar = nchar(mssg)
  if (mchar > 24)
    return(FALSE)

  return(substr(mssg, 1, 8) %in% poss8 &&
           substr(mssg, 9, 24) %in% poss11)
}

check4231 = function(mssg, fix_length = 8) {
  mchar = nchar(mssg)
  last_check = substr(mssg, mchar - fix_length + 1, mchar) %in% poss31
  if (!(last_check))
    return(FALSE)

  char = 1
  while (char < mchar) {
    temp = substr(mssg, char, char + fix_length - 1) %in% poss42
    if (!temp)
      break
    char = char + fix_length
  }

  if (char <= (mchar / 2) + 1)
    return(FALSE)

  while (char < mchar) {
    temp = substr(mssg, char, char + fix_length - 1) %in% poss31
    if (!temp)
      return(FALSE)
    char = char + fix_length
  }

  return(TRUE)
}

collapse_char = function(x, fixed_rule) {
  lapply(x, function(x)
    fixed_rule %>% filter(X1 == x) %>% pull(X2)) %>%
    do.call(expand.grid, .) %>%
    mutate(new_phrase = select(., matches('Var')) %>%
             purrr::pmap_chr(paste0)) %>%
    pull(new_phrase)
}

generate_rules = function(rules) {
  fixed_rule = data.frame()

  fixed_rule = rules %>%
    filter(grepl('\"', X2)) %>%
    rownames_to_column('X1') %>%
    mutate(X2 = gsub('\"', '', X2))

  for (times in 1:10) {
    # 10 is enough for this problem
    remaining_rules = setdiff(rownames(rules), fixed_rule$X1)
    if (length(remaining_rules) == 1)
      break
    for (rulei in remaining_rules) {
      this_rule = rules[rulei, 'X2']

      if (grepl('\\|', this_rule)) {
        this_rule = strsplit(this_rule, ' \\| ')[[1]]
        x = strsplit(this_rule[1], split = ' ')[[1]]
        y = strsplit(this_rule[2], split = ' ')[[1]]

        if (all(c(x, y) %in% fixed_rule$X1)) {
          fixed_rule = rbind(
            fixed_rule,
            data.frame(
              X1 = rulei,
              X2 = c(
                collapse_char(x, fixed_rule),
                collapse_char(y, fixed_rule)
              )
            ))
        }
      } else {
        x = strsplit(this_rule, split = ' ')[[1]]
        if (all(x %in% fixed_rule$X1)) {
          fixed_rule = rbind(
            fixed_rule,
            data.frame(X1 = rulei, X2 = collapse_char(x, fixed_rule)))
        }
      }
    }
  }
  fixed_rule
}

rules = strsplit(strsplit(input[1], '\n')[[1]], ': ') %>%
  do.call(rbind, .) %>% data.frame()
mssgs = strsplit(input[2], '\n')[[1]]
rules = strsplit(strsplit(input[1], '\n')[[1]], ': ') %>%
  do.call(rbind, .) %>% data.frame() %>%
  column_to_rownames('X1')
mssgs = strsplit(input[2], '\n')[[1]]

## part 1
fixed_rule = generate_rules(rules)
poss8 = fixed_rule %>% filter(X1 == '8') %>% pull(X2)
poss11 = fixed_rule %>% filter(X1 == '11') %>% pull(X2)
sum(sapply(mssgs, check811, fix_length = 8))


## part 2
rules['8', 'X2'] = '42 | 42 8'
rules['11', 'X2'] = '42 31 | 42 11 31'
fixed_rule = generate_rules(rules)

poss31 = fixed_rule %>% filter(X1 == '31') %>% pull(X2)
poss42 = fixed_rule %>% filter(X1 == '42') %>% pull(X2)

sum(sapply(mssgs, check4231, fix_length = 8))
