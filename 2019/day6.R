source('utils.R')

# part 1
ex = get_aoc_input(6, 2019, Sys.getenv('COOKIE_PATH'))
input = strsplit(ex, '\n|)')[[1]] %>%
  matrix(byrow = T, ncol = 2) %>%
  as.data.frame()

out_list = list(COM = 0)
parents = names(out_list)

repeat{
  childrens = vector(mode = 'character')
  for (parent in parents){
    children = input %>% subset(V1 == parent) %>% `$`(V2)
    if (length(children) == 0)
      next
    childrens = c(childrens, children)
    out_list[children] = 1 + out_list[[parent]]
  }

  if (length(childrens) == 0)
    break
  parents = childrens
}
sum(unlist(out_list))

# part 2
get_all_pars <- function(child){
  parents = vector(mode = 'character')
  repeat{
    child = input %>% subset(V2 == child) %>% `$`(V1)
    parents = c(parents, child)
    if (child == 'COM')
      break
  }
  parents
}

match_vec = match(get_all_pars('YOU'), get_all_pars('SAN'))
min(match_vec, na.rm = T) + sum(is.na(match_vec)) - 1
