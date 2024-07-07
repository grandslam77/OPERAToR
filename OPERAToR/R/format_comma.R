format_comma <- function(number) {
    # Formatuje liczbę do dwóch miejsc po przecinku i zamienia kropkę na przecinek
    formatted_number <- gsub("\\.", ",", sprintf("%.2f", number))
    return(formatted_number)
}