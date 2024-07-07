
#' billions_to_word
#'
#' Opis funkcji
#' @param x Opis parametru x
#' @return Opis zwracanego wyniku
#' @export

billions_to_word <- function(n) {
    if (n == 1) {
        return("miliard")
    } else if (n %% 10 >= 2 && n %% 10 <= 4 && (n %% 100 < 10 || n %% 100 >= 20)) {
        return("miliardy")
    } else {
        return("miliardów")
    }
}