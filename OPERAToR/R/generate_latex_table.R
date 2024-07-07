generate_latex_table <- function(df) {
    latex_code <- ""
    
    current_dzial <- ""
    current_nadcecha <- ""
    
    for (i in 1:nrow(df)) {
        if (df$DZIAŁ[i] != current_dzial) {
            current_dzial <- df$DZIAŁ[i]
            latex_code <- paste0(latex_code, "\\multicolumn{2}{|>{\\raggedright\\arraybackslash}X|}{\\textbf{", current_dzial, "}} \\\\ \\hline\n")
        }
        
        if (!is.na(df$NADCECHA[i]) && df$NADCECHA[i] != current_nadcecha) {
            current_nadcecha <- df$NADCECHA[i]
            latex_code <- paste0(latex_code, "\\multicolumn{2}{|>{\\raggedright\\arraybackslash}X|}{\\textbf{", current_nadcecha, "}} \\\\ \\hline\n")
        }
        
        if (df$`WARTOŚĆ CECHY`[i] == "BRAK WPISÓW") {
            latex_code <- paste0(latex_code, "\\multicolumn{2}{|>{\\raggedright\\arraybackslash}X|}{", df$CECHA[i], df$`WARTOŚĆ CECHY`[i], "} \\\\ \\hline\n")
        } else {
            latex_code <- paste0(latex_code, df$CECHA[i], " & ", df$`WARTOŚĆ CECHY`[i], " \\\\ \\hline\n")
        }
    }
    
    return(latex_code)
}