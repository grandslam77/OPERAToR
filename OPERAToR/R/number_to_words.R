number_to_words <- function(n) {
    if (n < 0) {
        return(paste("minus", number_to_words(-n)))
    }
    
    if (n <= 9) {
        return(unit_to_word(n))
    } else if (n <= 19) {
        return(teen_to_word(n))
    } else if (n <= 99) {
        return(paste(tens_to_word(n %/% 10), if (n %% 10 != 0) unit_to_word(n %% 10) else ""))
    } else if (n <= 999) {
        return(paste(hundreds_to_word(n %/% 100), if (n %% 100 != 0) number_to_words(n %% 100) else ""))
    } else if (n <= 999999) {
        thousands_part <- n %/% 1000
        return(paste(number_to_words(thousands_part), thousands_to_word(thousands_part), if (n %% 1000 != 0) number_to_words(n %% 1000) else ""))
    } else if (n <= 999999999) {
        millions_part <- n %/% 1000000
        return(paste(number_to_words(millions_part), millions_to_word(millions_part), if (n %% 1000000 != 0) number_to_words(n %% 1000000) else ""))
    } else if (n <= 999999999999) {
        billions_part <- n %/% 1000000000
        return(paste(number_to_words(billions_part), billions_to_word(billions_part), if (n %% 1000000000 != 0) number_to_words(n %% 1000000000) else ""))
    }
    
    return(as.character(n))  # Obsługa większych liczb
}