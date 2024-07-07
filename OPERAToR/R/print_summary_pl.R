print_summary_pl <- function(model) {
    s <- summary(model)
    cat("Wyniki regresji liniowej wielorakiej:\n")
    #cat("Reszty:\n")
    #print(s$residuals)
    cat("\nWspółczynniki:\n")
    print(coef(s))
    cat("\nOgólne statystyki modelu:\n")
    cat(sprintf("Błąd standardowy reszt: %f\n", s$sigma))
    cat(sprintf("R-kwadrat: %f\n", s$r.squared))
    cat(sprintf("R-kwadrat skorygowany: %f\n", s$adj.r.squared))
    cat(sprintf("Statystyka F: %f\n", s$fstatistic[1]))
    cat(sprintf("Wartość p: %f\n", pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)))
}