#' Abkhaz search
#'
#' Post query to the Abkhaz Text Corpus (http://baltoslav.eu/apsua/index.php, 2,010,052 words). Don't forget to check main page for advanced query language.
#' @param query Vector of length 1 which contain query.
#' @param n_results Integer. Defines number of examples from the corpus. By default is 10
#' @param kwic Logical. KWIC (key word in context) is the format for resulted lines. If TRUE, then it returns a dataframe with query in the middle and left and right contexts. If FALSE, then it returns each result in one string. By default is TRUE.
#' @param write This argument writes a file in the working derictory (see function getwd() and setwd() for more information). If FALSE, then it creates a dataframe in Global Environment. Otherwise function writes a .tsv file with the name frome the argument value. By default is FALSE.
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' abk_corpus("бызшәа")
#' abk_corpus("*омызт")
#' abk_corpus("бызшәа", n_results = 13)
#' abk_corpus("аҧс(ацәа|уаа)", kwic = F)
#' @export
#' @import utils
#'

abk_corpus <- function(query, n_results = 20, kwic = TRUE, write = FALSE){
  if(length(query) != 1){
    warning('x must be of length 1. If you want a dataframe with different queries try \n do.call("rbind.data.frame", sapply(x, abk_corpus, simplify = F))')
  }

  # get urls
  my.url <- paste0("http://baltoslav.eu/apsua/index.php?xxx=",
                   query,
                   "&mova=en&ks=ca&litery=&qp=",
                   0:(round(n_results/20)-1)*20) # each page

  # get data
  results <- do.call(rbind, lapply(my.url, abk_one.page))

  if(nrow(results) > n_results){
    results <- results[1:n_results,]}

  # kwic argument
  if(kwic == F){
    results <- apply(results, 1, function(x) paste(x, collapse = ""))
  }

  # write argument
  if(write != FALSE){
    utils::write.table(results, paste0(write, ".csv"), row.names = F, sep = "\t")
  } else {return(results)}
}
