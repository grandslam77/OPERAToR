get_lista_ramek <- function(parent_list, sublist_name) {
    # Sprawdź, czy podana sublista istnieje w globalnej liście
    if (sublist_name %in% names(parent_list)) {
        sublist <- parent_list[[sublist_name]]
        
        # Sprawdź, czy sublista zawiera 'lista_ramek'
        if ("lista_ramek" %in% names(sublist)) {
            return(sublist$lista_ramek)
        } else {
            stop(paste("Sublista", sublist_name, "nie zawiera 'lista_ramek'."))
        }
    } else {
        stop(paste("Sublista", sublist_name, "nie istnieje w podanej liście."))
    }
}