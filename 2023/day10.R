library(tidyverse)
library(stringr)
library(purrr)
source("utils.R")
source("day10-funcs.R")
## part 1 ----
input <- get_aoc_input(10)

input <- "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"
# input = "-L|F7
# 7S-7|
# L|7||
# -L-J|
# L|-JF"
# input = "..........
# .S------7.
# .|F----7|.
# .||OOOO||.
# .||OOOO||.
# .|L-7F-J|.
# .|II||II|.
# .L--JL--J.
# .........."

input = ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."
inp <- strsplit(input, "\n")[[1]] |>
  strsplit("")
m <- length(inp)
n <- length(inp[[1]])
inmat <- inp |>
  unlist() |>
  matrix(byrow = TRUE, ncol = n)

outmat <- matrix(0, ncol = ncol(inmat), nrow = nrow(inmat))
animal <- which(inmat == "S", arr.ind = TRUE)
animals <- list(as.vector(animal))
outmat[animal[[1]], animal[[2]]] <- 1

i <- 1
animalsb <- list(as.vector(animal))
while (i <= length(animals)) {
  a <- animals[[i]]
  possible_dirs <- get_dirs(slic(inmat, a))
  for (d in possible_dirs) {
    a1 <- a + dirs[[d]]
    dif <- delta(
      pipe = slic(inmat, a1),
      dir = d
    )
    if (!is.null(dif)){
      a2 <- a + dif
      outmat[a1[[1]], a1[[2]]] <- 1
      outmat[a2[[1]], a2[[2]]] <- 1

      if (!(list(a2) %in% animals)) {
        animalsb <- c(animalsb, list(a1, a2))
        animals <- c(animals, list(a2))
      }
    }}
  i <- i + 1
}
animalsb <- c(animalsb, list(a1))
length(animals)
length(animalsb)
sum(outmat)/2

# saveRDS(animals, file = "animals.rds")
# saveRDS(outmat, file = "outmat.rds")

pipemat <- outmat
inpipe <- inmat
inpipe[pipemat == 0] <- "."


## EXPANDING ----
outmat <- matrix(0, ncol = 2*ncol(pipemat), nrow = 2*nrow(pipemat))
for (r in 1:nrow(pipemat)){
  for (col in 1:ncol(pipemat)){
    outmat[2*r, 2*col] <- pipemat[r, col]
  }
}

inpipe2 <- matrix(".", ncol = 2*ncol(inpipe), nrow = 2*nrow(inpipe))
for (r in 1:nrow(inpipe)){
  for (col in 1:ncol(inpipe)){
    inpipe2[2*r, 2*col] <- inpipe[r, col]
  }
}

for (r in 1:(m)){
  for (col in 1:(n)){
    rcol <- slic(inpipe2, c(2*r, 2*col))
    rcol2 <- slic(inpipe2, c(2*r, 2*col + 2))
    r2col <- slic(inpipe2, c(2*r + 2, 2*col))
    if (rcol %in% c("-", "F", "L") ||
        rcol2 %in% c("-", "7", "J")){
      inpipe2[2*r, 2*col + 1] <- "-"
    }

    if (rcol %in% c("|", "F", "7") ||
        r2col %in% c("|", "J", "L")){
      inpipe2[2*r + 1, 2*col] <- "|"
    }
  }
}
m <- m*2
n <- n*2


head(animalsb)

## FILL IN ABmat with A ----
ABmat <- matrix(NA, ncol = ncol(inpipe2), nrow = nrow(inpipe2))

indices <- list(c(1,1),
                c(1, ncol(inpipe2)),
                c(nrow(inpipe2), 1),
                c(nrow(inpipe2), ncol(inpipe2)))
for (idx in indices){
  ABmat[idx[[1]],idx[[2]]] <- "A"
}


i = 1

while (i <= length(indices)){
  a <- indices[[i]]

  neighs <- neighbor_simple(a)
  for (ne in neighs){
    if (all(ne > c(0, 0)) && all(ne <= c(m,n))){
      if (extract_inpipe2(ne) == "." && is.na(extractAB(ne))){
        ABmat[ne[[1]], ne[[2]]] <- "A"
        indices <- c(indices, list(ne))
      }
    }
  }
  i <- i + 1
}

## RERUN with larger matrix inmat ----
inmat <- inpipe2
animal <- which(inmat == "S", arr.ind = TRUE)
animals <- list(as.vector(animal))
i <- 1
outmat <- matrix(0, ncol = 2*ncol(pipemat), nrow = 2*nrow(pipemat))
outmat[animal[[1]], animal[[2]]] <- 1
animalsb <- list(as.vector(animal))

while (i <= length(animals)) {
  a <- animals[[i]]
  possible_dirs <- get_dirs(inmat[a[[1]], a[[2]]])
  for (d in possible_dirs) {
    nextid <- a + dirs[[d]]
    pipe <- get_pipe(nextid)
    nexta <- newxy(
      row = a[[1]],
      col = a[[2]],
      pipe = pipe,
      dir = d
    )
    if (!is.na(nexta[[1]])){
      outmat[nextid[[1]], nextid[[2]]] <- 1
      outmat[nexta[[1]], nexta[[2]]] <- 1

      animalsb <- c(animalsb, list(nextid))
      if (!(list(nexta) %in% animals)) {
        animalsb <- c(animalsb, list(nexta))
        animals <- c(animals, list(nexta))
      }
    }}
  i <- i + 1
}

## FINALLY calculate ABmat ----

animalsb <- animalsb[!duplicated(animalsb)]
sum(outmat)
length(animalsb)

# ABmat[87, 67] <- "A"
# k = 14
for (k in 2:length(animalsb)){

a <- animalsb[[k]]


a
neighs <- neighbors_more(a)
pipe <- slice2(inpipe2, a)
adjs <- group_pipes(pipe)



for (i in 1:2){
  adj1 <- adjs[[i]]
  neighi <- neighs[adj1]
  adjAB <- neighbors(neighi) |>
    lapply(extractAB) |>
    unlist() |>
    na.omit()

  if (length(adjAB)){
    for (ne in neighi){
      if (extract_outmat(c(ne[[1]], ne[[2]])) != 1){
        ABmat[ne[[1]], ne[[2]]] <-  adjAB[[1]] # should be unique values

      }
    }
  }
}
for (i in 1:2){
  adj1 <- adjs[[i]]
  neighi <- neighs[adj1]
  if (any(is.na(lapply(neighi, extractAB)))){
    for (ne in neighi){
      if (!is.na(extractAB(neighs[[adjs[[3-i]][[1]]]])) &&
          extract_outmat(c(ne[[1]], ne[[2]])) != 1){
        ABmat[ne[[1]], ne[[2]]] <- revAB[[extractAB(neighs[[adjs[[3-i]][[1]]]])]]
      }
    }
  }
}


}


ABmat2 <- ABmat
ABmat2[is.na(ABmat2)] <- inpipe2[is.na(ABmat2)]

# fill in ABmat again with either A or B

i = 1
indices <- which(is.na(ABmat)&!inpipe2 != ".", arr.ind = TRUE)
while (i <= nrow(indices)){
  # while (i < 10000){
  a <- indices[i,]

  adjAB <- neighbor_simple(a) |>
    lapply(extractAB) |>
    unlist() |>
    na.omit()

  if (length(adjAB) > 0){

    ABmat[a[[1]], a[[2]]] <-  adjAB[[1]] # should be unique values
  }
  i <- i + 1
}


ABmat2 <- ABmat
ABmat2[is.na(ABmat2)] <- inpipe2[is.na(ABmat2)]
sum(ABmat2 == ".")



ABreduced <- matrix(".", ncol = ncol(ABmat)/2, nrow = nrow(ABmat)/2)
for (r in 1:nrow(ABreduced)){
  for (col in 1:ncol(ABreduced)){
    ABreduced[r, col] <- ABmat[r*2, col*2]
  }
}
sum(ABreduced == "B", na.rm = TRUE)



