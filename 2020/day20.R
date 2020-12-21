library(dplyr)
source('utils.R')
inday20 <- get_aoc_input(20, 2020, Sys.getenv('COOKIE_PATH'))
input <- strsplit(strsplit(inday20, '\n\n')[[1]], '\n')

finrow = 10
nmat = length(input)
mynames = sapply(input, `[`, 1)

mats = lapply(input, function(x) strsplit(x[-1], '') %>%
  do.call(rbind, .)) %>%
  setNames(mynames)

get_edges <- function(mat){
  edges = rbind(mat[1,], mat[,1], mat[finrow,], mat[,finrow])
  edges = rbind(edges, edges[, finrow:1])
  split(edges, 1:8) %>%
    setNames(c('top', 'left', 'bottom', 'right',
               'top_vertical', 'left_horizontal',
               'bottom_vertical', 'right_horizontal'))
}

edge_list = lapply(mats, get_edges)

check_pos = function(x, k) all(x == inter[[k]])

interlens = vector('integer', length = nmat)
names(interlens) = mynames
layout = data.frame()

for (i in seq.int(nmat - 1)){
  for (j in (i + 1):nmat){
    edgei = edge_list[[i]]
    edgej = edge_list[[j]]
    inter = intersect(edgei, edgej)
    if (length(inter) > 0){
      for (k in seq.int(length(inter))){
        layout = rbind(
          layout,
          data.frame(
            tile1 = names(edge_list)[i],
            tile2 = names(edge_list)[j],
            edge1 = names(edgei)[sapply(edgei, check_pos, k = k)],
            edge2 = names(edgej)[sapply(edgej, check_pos, k = k)]))
      }
    }
    interlen = length(inter)
    interlens[i] = interlens[i] + interlen
    interlens[j] = interlens[j] + interlen
  }
}

as.numeric(gsub('Tile |:', '', names(interlens)))[interlens/2 == 2] %>%
  prod() %>%
  print(digits = 16)


## part 2
fromthetop1 = c('top', 'right', 'bottom_vertical', 'left_horizontal')
fromthetop2 = c('top_vertical', 'right_horizontal', 'bottom', 'left')

rotate = function(start, times){
  if (start %in% fromthetop1){
    fromthetop = fromthetop1
  } else {
    fromthetop = fromthetop2
  }
  fromthetop[mod(which(fromthetop == start) + times - 1, 4) + 1]
}

clockwise = function(m, times, finrow = 10){
  if (times == 2){
    return(m[finrow:1, finrow:1])
  } else if (times == 1){
    m = t(m)
    return(m[ , finrow:1])
  } else if (times == 3){
    m = t(m)
    return(m[finrow:1, ])
  }
  return(m)
}

flip = function(m, axis = c('horizontal', 'vertical')){
  m = switch(
    axis,
    horizontal = m[finrow:1, ],
    vertical = m[, finrow:1])
  m
}

flip_edge = function(edge, mustflip, axis){
  if (!mustflip) return(edge)

  if (axis == 'horizontal'){
    if (grepl('_horizontal', edge)){
      return(strsplit(edge, '_')[[1]][1])
    } else if (grepl('right|left', edge)) {
      return(paste(edge, 'horizontal', sep = '_'))
    } else if (grepl('top', edge)){
      return(gsub('top', 'bottom', edge))
    } else if (grepl('bottom', edge)){
      return(gsub('bottom', 'top', edge))
    }
  } else {
    if (grepl('_vertical', edge)){
      return(strsplit(edge, '_')[[1]][1])
    } else if (grepl('top|bottom', edge)) {
      return(paste(edge, 'vertical', sep = '_'))
    } else if (grepl('left', edge)){
      return(gsub('left', 'right', edge))
    } else if (grepl('right', edge)){
      return(gsub('right', 'left', edge))
    }
  }
  return(edge)
}

to_left = function(toedge = 'left', beginedge){
  short_left = strsplit(toedge, "_")[[1]][1]
  if (beginedge %in% fromthetop2){
    left_loc = grep(short_left, fromthetop2)
    times_to_left = mod(left_loc - grep(beginedge, fromthetop2), 4)
    mustflip = toedge %in% fromthetop1
  } else {
    left_loc = grep(short_left, fromthetop1)
    times_to_left = mod(left_loc - grep(beginedge, fromthetop1), 4)
    mustflip = toedge %in% fromthetop2
  }

  return(list(times_to_left = times_to_left,
              mustflip = mustflip))
}

fill_row = function(tiles_connect, rotated){
  inner = rotated[2:9, 2:9]
  this_tile = tiles_connect$til1[1]

  for (i in seq.int(11)){
    this_row = filter(tiles_connect, grepl('right', rotated_edg1))
    next_tile = this_row$til2
    ops = to_left(gsub('right', 'left', this_row$rotated_edg1), this_row$edg2)
    rotated_next = clockwise(mats[[next_tile]], ops$times_to_left) %>%
      {if (ops$mustflip) flip(., 'horizontal') else .}
    rotated_next
    inner = cbind(inner, rotated_next[2:9, 2:9])

    tiles_connect = update_connections(this_tile, next_tile, ops, 'horizontal')
    this_tile = next_tile
  }
  inner
}

update_connections = function(this_tile, next_tile, ops, axis){
  newlay %>%
    filter(tile1 == next_tile| tile2 == next_tile,
           tile1 != this_tile, tile2 != this_tile) %>%
    mutate(swap = tile2 == next_tile,
           til1 = ifelse(swap, tile2, tile1),
           til2 = ifelse(swap, tile1, tile2),
           edg1 = ifelse(swap, edge2, edge1),
           edg2 = ifelse(swap, edge1, edge2)) %>%
    rowwise() %>%
    mutate(rotated_edg1 =
             rotate(edg1, ops$times_to_left) %>%
             flip_edge(mustflip = ops$mustflip, axis))
}

newlay = layout %>% distinct(tile1, tile2, .keep_all = TRUE)
corners = names(interlens)[interlens/2 == 2]
this_tile = corners[1]

tile0 = newlay %>%
  filter(tile1 == this_tile| tile2 == this_tile) %>%
  mutate(swap = tile2 %in% corners,
         til1 = ifelse(swap, tile2, tile1),
         til2 = ifelse(swap, tile1, tile2),
         edg1 = ifelse(swap, edge2, edge1),
         edg2 = ifelse(swap, edge1, edge2)) %>%
  rowwise() %>%
  mutate(rotated_edg1 = rotate(edg1, 2))

this_row = filter(tile0, grepl('right', rotated_edg1))
ops = to_left('right_horizontal', this_row$edg1)

first_rotated = clockwise(mats[[this_tile]], ops$times_to_left)
inners = fill_row(tile0, first_rotated)
far_left_cont = tile0

# filling columns:
for (j in seq.int(11)){
  this_col = filter(far_left_cont, grepl('bottom', rotated_edg1))
  next_tile = this_col$til2
  ops = to_left(gsub('bottom', 'top', this_col$rotated_edg1), this_col$edg2)
  rotated_next = clockwise(mats[[next_tile]], ops$times_to_left) %>%
    {if (ops$mustflip) flip(., 'vertical') else .}

  far_left_cont = update_connections(this_tile, next_tile, ops, 'vertical')
  this_tile = next_tile

  inners = rbind(inners, fill_row(far_left_cont, rotated_next))
}

# find sea monsters
monster = '                  # \n#    ##    ##    ###\n #  #  #  #  #  #   '

monstermat = strsplit(strsplit(monster, '\n')[[1]], '') %>%
  do.call(rbind, .) %>%
  gsub(' ', '0.5', .) %>%
  gsub('#', '1', .) %>%
  `class<-`('numeric')

myimg = inners %>%
  gsub('\\.', '0', .) %>%
  gsub('#', '1', .) %>%
  `class<-`('numeric')

for (rotations in 0:3) {
  nmons = 0
  rotated_img = clockwise(myimg, rotations, 96)

  for (i in seq.int(96 - 2)) {
    for (j in seq.int(96 - 19)) {
      if (sum(monstermat - rotated_img[i:(i + 2), j:(j + 19)] == 1) == 0)
        nmons = nmons + 1
    }
  }
  if (nmons > 0)
    break
}

sum(myimg) - sum(monstermat == 1) * nmons
