source('utils.R')

# ex = '1002,4,3,4,33'
# ex = '3,9,8,9,10,9,4,9,99,-1,8'
ex <- get_aoc_input(5, 2019, Sys.getenv('COOKIE_PATH'))
input0 <- strsplit(ex, ',')[[1]] %>% as.numeric()
get_val <- function(input, para, mode){
  if (mode == 0){ # position mode
    return(input[para + 1])
  } else { # immediate mode
    return(para)
  }
}

input = input0
input_i = 1
j = 1
output = NA

while (j < length(input)){
  oriin = input
  opcode = input[j]

  if (opcode == 99)
    break
  if (opcode == 3){
    # Opcode 3 takes a single integer as input and
    # saves it to the position given by its only parameter.
    input[oriin[j + 1] + 1] = input_i
    j = j + 2
  } else if (opcode == 4){
    # Opcode 4 outputs the value of its only parameter.
    # For example, the instruction 4,50 would output the value at address 50.
    output = oriin[oriin[j + 1] + 1]
    j = j + 2
  } else {
    mode1 = (opcode %% 1000) %/% 100
    mode2 = (opcode %% 10000) %/% 1000
    mode3 = opcode %/% 10000 # should always be 0
    op = opcode %% 100
    if (op == 1) {
      input[oriin[j + 3] + 1] =
        get_val(input, oriin[j + 1], mode1) +
        get_val(input, oriin[j + 2], mode2)
    } else if (op == 2) {
      input[oriin[j + 3] + 1] =
        get_val(input, oriin[j + 1], mode1) *
        get_val(input, oriin[j + 2], mode2)
    }
    j = j + 4
  }
}


# part 2

input = input0
input_i = 5
j = 1
output = NA
while (j < length(input)){
  oriin = input
  opcode = input[j]
  cat(i, j, opcode, '\n')

  if (opcode == 99)
    break
  if (opcode == 3){
    # Opcode 3 takes a single integer as input and
    # saves it to the position given by its only parameter.
    input[oriin[j + 1] + 1] = input_i
    j = j + 2
  } else if (opcode == 4){
    # Opcode 4 outputs the value of its only parameter.
    # For example, the instruction 4,50 would output the value at address 50.

    output = oriin[oriin[j + 1] + 1]
    cat('op 4', output)
    j = j + 2
  }
  else {
    mode1 = (opcode %% 1000) %/% 100
    mode2 = (opcode %% 10000) %/% 1000
    mode3 = opcode %/% 10000 # should always be 0
    op = opcode %% 100
    cat(op, mode1, mode2, mode3, '\n')
    if (op == 1) {
      input[oriin[j + 3] + 1] =
        get_val(input, oriin[j + 1], mode1) +
        get_val(input, oriin[j + 2], mode2)
      j = j + 4
    } else if (op == 2) {
      input[oriin[j + 3] + 1] =
        get_val(input, oriin[j + 1], mode1) *
        get_val(input, oriin[j + 2], mode2)
      j = j + 4
    } else if (op == 5){
      print(get_val(input, oriin[j + 1], mode1))
      if (get_val(input, oriin[j + 1], mode1) != 0)
        j = get_val(input, oriin[j + 2], mode2) + 1
      else
        j = j + 3
    } else if (op == 6){
      if (get_val(input, oriin[j + 1], mode1) == 0)
        j = get_val(input, oriin[j + 2], mode2) + 1
      else
        j = j + 3
    } else if (op == 7){
      if (get_val(input, oriin[j + 1], mode1) <
          get_val(input, oriin[j + 2], mode2))
        input[oriin[j + 3] + 1] = 1
      else
        input[oriin[j + 3] + 1] = 0
      j = j + 4
    } else if (op == 8){
      if (get_val(input, oriin[j + 1], mode1) ==
          get_val(input, oriin[j + 2], mode2))
        input[oriin[j + 3] + 1] = 1
      else
        input[oriin[j + 3] + 1] = 0
      j = j + 4
    }

  }
}
output
