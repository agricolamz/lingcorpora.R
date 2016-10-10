pl.base <- function(x){
  out <- list()
  for(j in seq_along(x)) {
    if(grepl("\\[*:", x[j])){
      sub <- substring(x[j], 1:nchar(x[j]), 1:nchar(x[j]))
      n.br <- grep("\\[", sub)
      n.fin <- c()
      for(i in 1:length(n.br)){
        n.fin[i] <- grep("\\:", sub)[grep("\\:", sub) > n.br[i]][1]}
      df <- data.frame(b = n.br+1, e = n.fin-1)
      results <- c()
      for(i in 1:nrow(df)){
        results[i] <- paste(sub[df[i,1]:df[i,2]], collapse = "")}
      out[[j]] <- results
    } else {
        warning(paste0('No tags were found in the query "', x, '"'))
    }}
  out
  }