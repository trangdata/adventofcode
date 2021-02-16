source('utils.R')

ex <- get_aoc_input(12, 2019, Sys.getenv('COOKIE_PATH'))

input = gsub('x=|y=|z=|<|>| ', '', ex) %>%
  strsplit(split = ',|\n') %>%
  `[[`(1) %>%
  as.numeric() %>%
  matrix(byrow = T, ncol = 3) %>%
  as.data.frame() %>%
  `names<-`(c('x', 'y', 'z'))

# part 1
dat = input
dimens = c('x', 'y', 'z')
pairs = combn(1:4, 2, simplify = F)
n_steps = 1000

dat_new = list()
vel_new = list()
for (dimen in dimens) {
  velocities = rep(0, 4)
  dat_dim = dat[, dimen]
  for (t in seq.int(n_steps)) {
    for (pairi in pairs) {
      diff = dat_dim[pairi[2]] - dat_dim[pairi[1]]

      if (diff != 0) {
        velocities[pairi] = velocities[pairi] +
          (2 * (diff > 0) - 1) * c(1,-1)
      }
    }
    dat_dim = dat_dim + velocities
    dat_new[[dimen]] = dat_dim
    vel_new[[dimen]] = velocities
  }
  ts = c(ts, t)
}

sum(rowSums(abs(dplyr::bind_cols(dat_new))) *
      rowSums(abs(dplyr::bind_cols(vel_new))))


# part 2
n_steps = 200000
dat = input
ts = vector(mode = 'numeric')

for (dimen in dimens) {
  velocities = rep(0, 4)
  dat_dim = dat[, dimen]
  for (t in seq.int(n_steps)) {
    for (pairi in pairs) {
      diff = dat_dim[pairi[2]] - dat_dim[pairi[1]]

      if (diff != 0) {
        velocities[pairi] = velocities[pairi] +
          (2 * (diff > 0) - 1) * c(1,-1)
      }
    }
    dat_dim = dat_dim + velocities
    if (identical(velocities, rep(0, 4)))
      break
  }
  ts = c(ts, t)
}

print(Reduce(pracma:::Lcm, ts)*2, digits = 15)
