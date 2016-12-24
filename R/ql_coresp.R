#'Query-lines correspondencies
#'
#'Corpuses from http://baltoslav.eu/ can have two query tags in one result example. This function get query-lines correspondencies. It returns index of the query words for corect splitting of result lines into kwic parts.
#'@param query_word Vector with a query words
#'@param lines Vector with a result lines
#'@author George Moroz <agricolamz@gmail.com>
#'@examples
#' ql_coresp(c("a", "b", "c"), c("a b c", "c"))
#'

ql_coresp <- function(query_word, lines){
  corespondences <- c()
  i = 1; j = 1
  while(j <= length(lines)){
    if(grepl(query_word[i], lines[j])){
      corespondences[j] <- i
      i <- i+1
      j <- j+1
    } else { i = i+1}}
  return(corespondences)
}
