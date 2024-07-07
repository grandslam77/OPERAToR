hundreds_to_word <- function(n) {
    hundreds <- c("", "sto", "dwieście", "trzysta", "czterysta", "pięćset", "sześćset", "siedemset", "osiemset", "dziewięćset")
    return(hundreds[n + 1])
}