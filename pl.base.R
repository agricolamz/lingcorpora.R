pl.base <- function(x){
  out <- c()
  for(j in seq_along(x)) {
    if(grepl("\\[*:", x[j])){
      out[j] <- unlist(strsplit(unlist(strsplit(x[j], "\\["))[2], "\\:"))[1]
    } else {
      warning(paste0('No tags were found in the query "', x, '"'))
    }}
  return(out)
}

regexpr("\\[*:", "asdasd [dasd:14asd:asdasd]")

library(gsubfn)
strapply("asdasd [dasd:14asd:asdasd]", "\\[(.*?):")
