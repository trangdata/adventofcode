source('utils.R')
# library(dplyr)

inday12 <- get_aoc_input(12, 2020, Sys.getenv('COOKIE_PATH'))

nsew = function(carte, dir, val){
  if (dir == 'N'){
    return(c(carte[1], carte[2] + val))
  } else if (dir == 'S'){
    return(c(carte[1], carte[2] - val))
  } else if (dir == 'E'){
    return(c(carte[1] + val, carte[2] ))
  } else if (dir == 'W'){
    return(c(carte[1] - val, carte[2] ))
  }
}

counterclockwise = c('E', 'N', 'W', 'S')
rotate = function(start, clockdir, deg){
  counterclockwise[
    mod(which(counterclockwise == start) - deg/90 +
          (clockdir == 'L')*2*deg/90 - 1, 4) + 1]
}

input = strsplit(inday12, '\n')[[1]] %>%
  data.frame(input = .) %>%
  dplyr::mutate(instr = substr(input, 1, 1),
         val = as.integer(stringr::str_sub(input, 2, -1)))

pointing = 'E'
carte = c(0, 0)

for (i in seq.int(nrow(input))){
  inti = input[i, 'instr']
  vali = input[i, 'val']
  if (inti == 'F'){
    carte = nsew(carte, pointing, vali)
  } else if (inti %in% c('L', 'R')){
    pointing = rotate(pointing, inti, vali)
  } else {
    carte = nsew(carte, inti, vali)
  }
}

sum(abs(carte))

## part 2

rotate_wave = function(carte, diff, clockdir, deg){
  rotatemod = mod(- deg/90 + (clockdir == 'R')*2*deg/90, 4)
  if (rotatemod == 2){
    return(carte - diff)
  } else if (rotatemod == 1){
    return(carte + rev(diff*c(-1,1)))
  } else if (rotatemod == 3){
    return(carte + rev(diff*c(1,-1)))
  }
}

carte = c(0, 0)
wavepoint = c(10, 1)
diff = wavepoint - carte

for (i in seq.int(nrow(input))){
  inti = input[i, 'instr']
  vali = input[i, 'val']
  if (inti == 'F'){
    carte = carte + vali*diff
    wavepoint = carte + diff
  } else if (inti %in% c('L', 'R')){
    wavepoint = rotate_wave(carte, diff, inti, vali)
  } else {
    wavepoint = nsew(wavepoint, inti, vali)
  }
  diff = wavepoint - carte
}
sum(abs(carte))
