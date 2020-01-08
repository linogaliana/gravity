#' Significance stars
#' 
#' @param pvalue pvalue

signif_stars <- function(pvalue){
  if (is.na(pvalue)) return("")
  if (pvalue<0.01) return("$^{***}$")
  if (pvalue<0.05) return("$^{**}$") 
  if (pvalue<0.1) return("$^{*}$") 
  return("")
}
