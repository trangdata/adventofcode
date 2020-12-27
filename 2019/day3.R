source('utils.R')
inday3 <- get_aoc_input(3, 2019, Sys.getenv('COOKIE_PATH'))
input <- strsplit(inday3, '\n')[[1]]

# ex = 'R75,D30,R83,U83,L12,D49,R71,U7,L72
# U62,R66,U55,R34,D71,R55,D58,R83'
# ex = 'R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
# U98,R91,D20,R16,D67,R40,U7,R15,U6,R7'
ex = 'R8,U5,L5,D3
U7,R6,D4,L4'
input <- strsplit(ex, '\n')[[1]]

in1 = strsplit(input[1], ',')[[1]]
in1df = data.frame(
  key = substr(in1, 1, 1),
  val = as.numeric(in1 %>% substr(2, nchar(in1))))

getco <- function(in1df){
  x = 0
  y = 0
  coords <- data.frame()
  for (i in 1:nrow(in1df)){
    xold = x
    yold = y
    if (in1df[i, 1] == 'R'){
      x = x + in1df[i, 2]
    } else if (in1df[i, 1] == 'L'){
      x = x - in1df[i, 2]
    } else if (in1df[i, 1] == 'U'){
      y = y + in1df[i, 2]
    } else {
      y = y - in1df[i, 2]
    }
    newco = cbind(xold:x, yold:y)
    coords = rbind(coords, newco)
  }
  data.frame(coords[!duplicated(coords), ])
}


in2 = strsplit(input[2], ',')[[1]]
in2df = data.frame(
  key = substr(in2, 1, 1),
  val = as.numeric(in2 %>% substr(2, nchar(in2))))
co1 <- getco(in1df)
co2 <- getco(in2df)
coco = data.frame(
  rbind(co1[-1, ], co2),
  index = c(2:nrow(co1), 1:nrow(co2)))
dupco = coco[duplicated(coco[, c('V1', 'V2')]), ]
min(abs(dupco[,1]) + abs(dupco[,2]))

dupidx = duplicated(coco[, c('V1', 'V2')]) |
  duplicated(coco[, c('V1', 'V2')], fromLast = TRUE)

coco[dupidx, ] %>%
  dplyr::arrange(V1, V2) %>%
  dplyr::group_by(V1, V2) %>%
  dplyr::summarise(sum_ind = sum(index) - 2, .groups = 'drop') %>%
  dplyr::arrange(sum_ind)

