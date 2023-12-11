delta <- function(pipe, dir) {
  if ((!length(pipe)) || is.na(pipe)) {
    return(NULL)
  }
  pd <- paste0(pipe, dir)
  switch(pd,
    `|U` = c(-2, 0),
    `|D` = c(2, 0),
    `-L` = c(0, -2),
    `-R` = c(0, 2),
    `LD` = c(1, 1),
    `LL` = c(-1, -1),
    `JD` = c(1, -1),
    `JR` = c(-1, 1),
    `7R` = c(1, 1),
    `7U` = c(-1, -1),
    `FL` = c(1, -1),
    `FU` = c(-1, 1)
  )
}

deltab <- function(pipe, dir) {
  if ((!length(pipe)) || is.na(pipe)) {
    return(NULL)
  }
  pd <- paste0(pipe, dir)
  switch(pd,
         `|U` = c(-1, 0),
         `|D` = c(1, 0),
         `-L` = c(0, -1),
         `-R` = c(0, 1),
         `LD` = c(0, 1),
         `LL` = c(-1, 0),
         `JD` = c(0, -1),
         `JR` = c(0, 1),
         `7R` = c(1, 0),
         `7U` = c(-1, 0),
         `FL` = c(1, 0),
         `FU` = c(0, 1)
  )
}
dirs <- list(
  "L" = c(0, -1),
  "R" = c(0, 1),
  "U" = c(-1, 0),
  "D" = c(1, 0)
)
opposite_dir <- list(
  "L" = "R",
  "R" = "L",
  "U" = "D",
  "D" = "U"
)


dirs_more <- c(
  dirs,
  list(
    UL = c(-1, -1),
    UR = c(-1, 1),
    DR = c(1, 1),
    DL = c(1, -1)
  )
)

slic <- function(matrix, id) {
  # safe matrix slicing
  if (is.null(id) || any(id == 0)) {
    return(NA)
  }
  tryCatch(
    matrix[id[[1]], id[[2]]],
    error = function(e) {
      return(NA)
    }
  )
}

get_dirs <- function(pipe) {
  switch(pipe,
    "|" = c("D", "U"),
    "-" = c("L", "R"),
    "L" = c("U", "R"),
    "J" = c("U", "L"),
    "7" = c("D", "L"),
    "F" = c("D", "R"),
    names(dirs) #
  )
}

neighbors <- function(a) {
  d <- expand.grid(dirs, a)
  k <- purrr::map2(d$Var1, d$Var2, `+`)
  k[!duplicated(k)]
}
is_inside <- \(x) all(x > c(0, 0)) && all(x <= c(m, n))
neighbor_simple <- function(a) {
  ne <- lapply(dirs, \(x) x + a)
  inside <- sapply(ne, is_inside)
  ne[inside]
}
neighbors_more <- function(a) {
  ne <- lapply(dirs_more, "+", a)
  inside <- sapply(ne, is_inside)
  ne[inside]
}
revAB <- list(A = "B", B = "A")
group_pipes <- function(pipe) {
  switch(pipe,
    "|" = list("L", "R"),
    "-" = list("U", "D"),
    "L" = list(c("D", "L", "DR", "DL", "UL"), "UR"),
    "J" = list(c("D", "R", "DL", "DR", "UR"), "UL"),
    "7" = list(c("U", "R", "UL", "UR", "DR"), "DL"),
    "F" = list(c("U", "L", "UL", "UR", "DL"), "DR"),
    list(NULL)
  )
}

part1 <- function(inmat, m = nrow(inmat), n = ncol(inmat)) {
  animal <- which(inmat == "S", arr.ind = TRUE)
  animals <- vector("list", length = sum(inmat != "."))
  animals[[1]] <- as.vector(animal)
  i <- 1
  while (i <= length(animals)) {
    # while (i < 3){
    a <- animals[[i]]
    if (is.null(a)) {
      break
    }
    possible_dirs <- get_dirs(slic(inmat, a))
    for (d in possible_dirs) {
      a1 <- a + dirs[[d]]
      dif <- delta(
        pipe = slic(inmat, a1),
        dir = d
      )
      if (!is.null(dif)) {
        a2 <- a + dif
        if (!(list(a2) %in% animals)) {
          animals[[i + 1]] <- a1
          animals[[i + 2]] <- a2
        } else if (all(animal == a2) && !(list(a1) %in% animals)) {
          animals[[i + 1]] <- a1
        }
      }
    }
    i <- i + 2
  }
  # head(animals)
  animals |>
    discard(is.null)
  # animalsb <- animalsb[!duplicated(animalsb)]
}


part1b <- function(inmat, m = nrow(inmat), n = ncol(inmat)) {
  animal <- which(inmat == "S", arr.ind = TRUE)
  animals <- vector("list", length = sum(inmat != "."))
  animals[[1]] <- as.vector(animal)
  i <- 1
  d <- "D"
  # while (i <= length(animals)) {
    # while (i < 3){
    a <- animals[[i]]
    if (is.null(a)) {
      break
    }
    pipe = slic(inmat, a)
    # find d

    # possible_dirs <- get_dirs(slic(inmat, a))
    # for (d in possible_dirs) {
      a1 <- a + dirs[[d]]
      # dif <- delta(
      #   pipe = slic(inmat, a1),
      #   dir = d
      # )
      # if (!is.null(dif)) {
      #   a2 <- a + dif
        # if (!(list(a2) %in% animals)) {
          animals[[i + 1]] <- a1
          # animals[[i + 2]] <- a2
        # } else if (all(animal == a2) && !(list(a1) %in% animals)) {
        #   animals[[i + 1]] <- a1
        # }
      }
    # }
    i <- i + 2
  # }
  # head(animals)
  animals |>
    discard(is.null)
  # animalsb <- animalsb[!duplicated(animalsb)]
}

