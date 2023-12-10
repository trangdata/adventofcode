newxy <- function(row, col, pipe, dir) {
  x <- col
  y <- row
  # dir = c("U", "D", "L", "R")
  if (!length(pipe)) {
    return(NA)
  }
  if (pipe == "|") {
    if (dir == "U") {
      return(c(y - 2, x))
    } else if (dir == "D") {
      return(c(y + 2, x))
    } else {
      return(NA)
    }
  }

  if (pipe == "-") {
    if (dir == "L") {
      return(c(y, x - 2))
    } else if (dir == "R") {
      return(c(y, x + 2))
    } else {
      return(NA)
    }
  }

  if (pipe == "L") {
    if (dir == "D") {
      return(c(y + 1, x + 1))
    } else if (dir == "L") {
      return(c(y - 1, x - 1))
    } else {
      return(NA)
    }
  }

  if (pipe == "J") {
    if (dir == "D") {
      return(c(y + 1, x - 1))
    } else if (dir == "R") {
      return(c(y - 1, x + 1))
    } else {
      return(NA)
    }
  }

  if (pipe == "7") {
    if (dir == "R") {
      return(c(y + 1, x + 1))
    } else if (dir == "U") {
      return(c(y - 1, x - 1))
    } else {
      return(NA)
    }
  }

  if (pipe == "F") {
    if (dir == "L") {
      return(c(y + 1, x - 1))
    } else if (dir == "U") {
      return(c(y - 1, x + 1))
    } else {
      return(NA)
    }
  }
  NA
}

delta <- function(pipe, dir) {
  if ((!length(pipe)) || is.na(pipe)) {
    return(NULL)
  }
  pd <- paste0(pipe, dir)
  switch(
    pd,
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

dirs <- list(
  "L" = c(0, -1),
  "R" = c(0, 1),
  "U" = c(-1, 0),
  "D" = c(1, 0)
)

dirs_more <- c(
  dirs,
  list(
    UL = c(-1, -1),
    UR = c(-1, 1),
    DR = c(1,1),
    DL = c(1,-1)
  )
)

# get_pipe <- function(nextid) {
#   if (any(nextid < 1) || any(nextid > n)) {
#     return(NULL)
#   }
#   inmat[nextid[[1]], nextid[[2]]]
# }

slic <- function(matrix, id) {
  # safe matrix slicing
  tryCatch(
    matrix[id[[1]], id[[2]]],
    error = function(e) {
      return(NA)
    }
  )
}

get_dirs <- function(pipe){
  if (pipe == "|") {
    return(c("D", "U"))
  }

  if (pipe == "-") {
    return(c("L", "R"))
  }

  if (pipe == "L") {
    return(c("U", "R"))
  }

  if (pipe == "J") {
    return(c("U", "L"))
  }

  if (pipe == "7") {
    return(c("D", "L"))
  }

  if (pipe == "F") {
    return(c("D", "R"))
  }
  names(dirs)
}
rev_dir <- function(d){
  if (d == "U") return("D")
  if (d == "D") return("U")
  if (d == "L") return("R")
  if (d == "R") return("L")
}

neighbors <- function(a){
  # lapply(dirs, "+", a)
  expand.grid(dirs, a) |>
    rowwise() |>
    mutate(add = list(Var1 + Var2)) |>
    distinct(add) |>
    pull(add)
}
neighbor_simple <- function(a){
  lapply(dirs, \(x)x + a)
}
neighbors_more <- function(a){
  lapply(dirs_more, "+", a)
}
revAB <- list(A = "B", B = "A")
slice2 <- function(matx, a) matx[a[[1]], a[[2]]]
group_pipes <- function(pipe){
  if (pipe == "|") {
    return(list("L", "R"))
  }

  if (pipe == "-") {
    return(list("U", "D"))
  }

  if (pipe == "L") {
    return(list(c("D", "L", "DR", "DL", "UL"), "UR"))
  }

  if (pipe == "J") {
    return(list(c("D", "R", "DL", "DR", "UR"), "UL"))
  }

  if (pipe == "7") {
    return(list(c("U", "R", "UL", "UR", "DR"), "DL"))
  }

  if (pipe == "F") {
    return(list(c("U", "L", "UL", "UR", "DL"), "DR"))
  }
}

extractAB <- function(x){
  if (all(x > c(0, 0)) && all(x <= c(m,n))){
    return(ABmat[x[[1]],x[[2]]])
  }
  NA
}



extractABreduced <- function(x){
  if (all(x > c(0, 0)) && all(x <= c(m,n))){
    return(ABreduced[x[[1]],x[[2]]])
  }
  NA
}

extract_outmat <- function(x){
  if (all(x > c(0, 0)) && all(x <= c(nrow(outmat),ncol(outmat)))){
    return(outmat[x[[1]],x[[2]]])
  }
  1
}

extract_inpipe2 <- function(x){
  if (all(x > c(0, 0)) && all(x <= c(nrow(inpipe2),ncol(inpipe2)))){
    return(inpipe2[x[[1]],x[[2]]])
  }
  1
}
