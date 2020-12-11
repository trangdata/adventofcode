source('utils.R')
inday1 <- get_aoc_input(1, 2019, Sys.getenv('COOKIE_PATH'))
input <- as.numeric(strsplit(inday1, '\n')[[1]])

## Part 1
sum(floor(input/3) - 2)

## Part 2
# recursive function

createMemFuel <- function() {
  res <- 0
  memFuel <- function(n) {
    if (n <= 6) return(0)

    #grow res if necessary
    if (length(res) < n) res <<- `length<-`(res, n)

    #return pre-calculated value
    if (!is.na(res[n])) return(res[n])

    #calculate new values
    n_new <- floor(n/3) - 2
    res[n] <<- n_new + memFuel(n_new)
    res[n]
  }
  memFuel
}
memFuel <- createMemFuel()
sum(sapply(input, memFuel))
