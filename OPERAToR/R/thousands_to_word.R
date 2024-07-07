thousands_to_word <- function(n) {
    if (n == 1) {
        return("tysiąc")
    } else if (n %% 10 >= 2 && n %% 10 <= 4 && (n %% 100 < 10 || n %% 100 >= 20)) {
        return("tysiące")
    } else {
        return("tysięcy")
    }
}