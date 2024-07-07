teen_to_word <- function(n) {
    teens <- c("dziesięć", "jedenaście", "dwanaście", "trzynaście", "czternaście", "piętnaście", "szesnaście", "siedemnaście", "osiemnaście", "dziewiętnaście")
    return(teens[n - 10 + 1])
}