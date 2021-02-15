source('utils.R')
library(dplyr)

ex <- get_aoc_input(10, 2019, Sys.getenv('COOKIE_PATH'))

input = strsplit(ex, '\n')[[1]] %>%
  sapply(function(x)
    strsplit(x, '')[[1]])
coords = which(input == '#', arr.ind = T)

# part 1
out_list = list()
for (i in seq.int(nrow(coords))) {
  diff = apply(coords[-i,], 1, function(x)
    x - coords[i,])
  out_list[[i]] = nrow(distinct(data.frame(
    slope = (diff[2, ] / diff[1, ]),
    signs = sign(diff[1, ])
  )))
}
k = which.max(out_list) # 266
out_list[[k]] # or max(unlist(out_list))
coords[k, ] - c(1, 1)

# part 2
rownames(coords) <- 1:nrow(coords)
diff = apply(coords[-k,], 1, function(x)
  x - coords[k,])
dat = data.frame(id = colnames(diff),
                 x = diff[1, ],
                 y = diff[2, ]) %>%
  cbind(coords[-k,] - 1) %>%
  mutate(slope = y / x,
         signs = if_else(x == 0, 1, sign(x))) %>%
  group_by(slope, signs) %>%
  mutate(rotation = rank(order(abs(x), abs(y)))) %>%
  ungroup() %>%
  arrange(rotation, desc(signs), slope)

my200 = dat[200, ]
my200$row * 100 + my200$col
