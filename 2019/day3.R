source('utils.R')
inday3 <- get_aoc_input(3, 2019, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday3, '\n')[[1]]

getco <- function(in1df, no = 1){
  x = 0
  y = 0
  coords <- data.frame()
  for (i in 1:nrow(in1df)){
    xold = x
    yold = y
    if (in1df[i, 1] == 'R'){
      x = x + in1df[i, 2]
      newco = cbind(x = (xold + 1):x, y)
    } else if (in1df[i, 1] == 'L'){
      x = x - in1df[i, 2]
      newco = cbind(x = (xold - 1):x, y)
    } else if (in1df[i, 1] == 'U'){
      y = y + in1df[i, 2]
      newco = cbind(x, y = (yold + 1):y)
    } else {
      y = y - in1df[i, 2]
      newco = cbind(x, y = (yold - 1):y)
    }
    coords = rbind(coords, newco)
  }
  coords = coords %>%
    mutate(wire = no, step = row_number()) %>%
    distinct(x, y, .keep_all = TRUE)
}

in1 = strsplit(input[1], ',')[[1]]
in1df = data.frame(
  key = substr(in1, 1, 1),
  val = as.numeric(in1 %>% substr(2, nchar(in1))))

in2 = strsplit(input[2], ',')[[1]]
in2df = data.frame(
  key = substr(in2, 1, 1),
  val = as.numeric(in2 %>% substr(2, nchar(in2))))
coco = rbind(getco(in1df, 1), getco(in2df, 2))
dupco = coco[duplicated(coco[, c('x', 'y')]), ]
min(abs(dupco[,1]) + abs(dupco[,2]))

# part 2

dupidx = duplicated(coco[, c('x', 'y')]) |
  duplicated(coco[, c('x', 'y')], fromLast = TRUE)

coco[dupidx, ] %>%
  dplyr::group_by(x, y) %>%
  dplyr::summarise(sum_ind = sum(step), .groups = 'drop') %>%
  dplyr::arrange(sum_ind)

# or
# coco2 = coco %>%
#   add_count(V1, V2) %>%
#   filter(n == 2) %>%
#   group_by(V1, V2) %>%
#   mutate(group_id = cur_group_id(),
#          total_steps = sum(step)) %>%
#   ungroup() %>%
#   arrange(total_steps)
# min(coco2$total_steps)
