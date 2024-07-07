find_cluster_with_fixed_centroid_opt <- function(data, lp_value, desired_size=4) {
    # Upewnijmy się, że kolumna `LP.` jest dostępna
    if (!"LP." %in% names(data)) {
        stop("Kolumna 'LP.' nie istnieje w ramce danych")
    }
    
    # Znalezienie wiersza odpowiadającego nieruchomości z LP. = lp_value
    fixed_centroid <- data %>% dplyr::filter(`LP.` == lp_value)
    data_without_fixed <- data %>% dplyr::filter(`LP.` != lp_value)
    
    # Liczba prób
    max_attempts <- 100
    
    for (k in 2:50) { # Zakładamy, że nie potrzebujemy więcej niż 50 klastrów
        # set.seed(123)
        
        attempt <- 0
        
        while (attempt < max_attempts) {
            # Wybór pozostałych k-1 centroidów losowo z reszty danych
            initial_centers <- rbind(fixed_centroid, data_without_fixed[sample(1:nrow(data_without_fixed), k-1), ])
            
            # Upewnijmy się, że wszystkie dane są numeryczne
            data_for_kmeans <- data[ , -c(1, 2)] %>% mutate_all(as.numeric)
            initial_centers_for_kmeans <- initial_centers[ , -c(1, 2)] %>% mutate_all(as.numeric)
            
            # Sprawdzenie, czy początkowe centroidy są unikalne
            if (nrow(unique(initial_centers_for_kmeans)) == k) {
                # Klasteryzacja K-means
                kmeans_result <- kmeans(data_for_kmeans, centers = initial_centers_for_kmeans, nstart = 1, iter.max = 20)
                
                # Sprawdzenie liczby obserwacji w każdym klastrze
                cluster_sizes <- table(kmeans_result$cluster)
                
                # Sprawdzenie, czy fixed_centroid jest w odpowiednim klastrze
                fixed_cluster <- kmeans_result$cluster[which(data$`LP.` == lp_value)]
                
                # Sprawdzenie, czy istnieje klaster o dokładnie 4 elementach i czy fixed_centroid jest w takim klastrze
                if (cluster_sizes[fixed_cluster] == desired_size) {
                    return(list(kmeans_result = kmeans_result, k = k))
                }
            }
            
            attempt <- attempt + 1
        }
    }
    return(NULL)
}
