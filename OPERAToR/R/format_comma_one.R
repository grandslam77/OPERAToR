format_comma_one <- function(number) {
    # Formatuje liczbę do dwóch miejsc po przecinku i zamienia kropkę na przecinek
    formatted_number <- gsub("\\.", ",", sprintf("%.1f", number))
    return(formatted_number)
}