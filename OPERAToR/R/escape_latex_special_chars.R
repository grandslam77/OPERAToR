escape_latex_special_chars <- function(text) {
    text <- gsub("\\\\", "\\\\textbackslash{}", text)
    text <- gsub("\\$", "\\\\$", text)
    text <- gsub("\\{", "\\\\{", text)
    text <- gsub("\\}", "\\\\}", text)
    text <- gsub("\\_", "\\\\_", text)
    text <- gsub("\\#", "\\\\#", text)
    text <- gsub("\\%", "\\\\%", text)
    text <- gsub("\\&", "\\\\&", text)
    text <- gsub("\\^", "\\\\textasciicircum{}", text)
    text <- gsub("~", "\\\\textasciitilde{}", text)
    return(text)
}