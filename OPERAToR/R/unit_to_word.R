unit_to_word <- function(n) {
    units <- c("zero", "jeden", "dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć")
    return(units[n + 1])
}
