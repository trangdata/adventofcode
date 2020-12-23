source('utils.R')
library(tidyverse)
library(purrr)

inday22 <- get_aoc_input(22, 2020, Sys.getenv('COOKIE_PATH'))
input = strsplit(strsplit(inday22, '\n\n')[[1]], '\n')
input_ls = lapply(input, function(x) as.numeric(x[-1]))

# part 1
combat = function(input_ls) {
  repeat {
    card1 = input_ls[[1]][1]
    card2 = input_ls[[2]][1]

    if (card1 > card2) {
      input_ls[[1]] = c(input_ls[[1]][-1], card1, card2)
      input_ls[[2]] = input_ls[[2]][-1]
    } else {
      input_ls[[2]] = c(input_ls[[2]][-1], card2, card1)
      input_ls[[1]] = input_ls[[1]][-1]
    }

    if (is_empty(input_ls[[1]]) || is_empty(input_ls[[2]]))
      return(input_ls)
  }

}

input_ls = combat(input_ls)
sum(input_ls[[2]] * (length(input_ls[[2]]):1))


## part 2
input_ls = lapply(input, function(x) as.numeric(x[-1]))

trampoline <- function(f, ...) {
  function(...) {
    ret <- f(...)
    while (inherits(ret, "recursion")) {
      ret <- eval(as.call(c(f, unclass(ret))))
    }
    ret
  }
}

recur <- function(...) {
  structure(list(...), class = "recursion")
}

n_cards = sum(lengths(input_ls))
# WOW HOW LONG DID THIS TAKE
recursive_combat = trampoline(function(input_ls, endgame = FALSE) {

  deck10 = c(input_ls[1])
  deck20 = c(input_ls[2])
  i = 0
  repeat {
    card1 = input_ls[[1]][1]
    card2 = input_ls[[2]][1]
    if (!(length(input_ls[[1]]) - 1 >= card1 &&
          length(input_ls[[2]]) - 1 >= card2)){
      # play normally
      winner = (card2 > card1) + 1L

    } else { # play a subgame
      i = i + 1
      print(i)
      input_sub = input_ls
      input_sub[[1]] = input_sub[[1]][2:(card1 + 1)]
      input_sub[[2]] = input_sub[[2]][2:(card2 + 1)]
      combat_res = recursive_combat(input_sub)
      winner = combat_res$winner
    }

    if (winner == 1) {
      input_ls[[1]] = c(input_ls[[1]][-1], card1, card2)
      input_ls[[2]] = input_ls[[2]][-1]
    } else {
      input_ls[[2]] = c(input_ls[[2]][-1], card2, card1)
      input_ls[[1]] = input_ls[[1]][-1]
    }

    if (input_ls[1] %in% deck10 && input_ls[2] %in% deck20){
      return(list(input_ls = input_ls, winner = 1))
    }

    deck10 = c(deck10, input_ls[1])
    deck20 = c(deck20, input_ls[2])
    if (is_empty(input_ls[[1]]) || is_empty(input_ls[[2]]))
        return(list(input_ls = input_ls, winner = is_empty(input_ls[[1]]) + 1))

    if (n_cards %in% lengths(input_ls)){
      print('END GAME')
      endgame = TRUE
      return(list(input_ls = input_ls,
                  winner = which(n_cards %in% lengths(input_ls))))
    }
  }
  # }
}
)

combat_result = recursive_combat(input_ls)
windeck = combat_result$input_ls
sum(windeck[[2]] * (length(windeck[[2]]):1))
sum(windeck[[1]] * (length(windeck[[1]]):1))
