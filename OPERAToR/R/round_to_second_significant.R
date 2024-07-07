round_to_second_significant <- function(number) {
    # Sprawdzenie czy liczba jest zero
    if (number == 0) {
        return("0.0")
    }
    
    # Pobranie części dziesiętnej liczby
    decimal_part <- abs(number - floor(number))
    
    # Znalezienie pierwszej cyfry znaczącej po przecinku
    significant_position <- -floor(log10(decimal_part))
    
    # Zaokrąglenie liczby do drugiej cyfry znaczącej po pierwszej cyfrze niebędącej zerem
    rounded_number <- round(number, significant_position + 1)
    
    # Konwersja na format dziesiętny bez notacji naukowej
    decimal_format <- format(rounded_number, scientific = FALSE)
    
    return(decimal_format)
}