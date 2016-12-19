#' Avar search
#'
#' Post query to the Avar Text Corpus (http://baltoslav.eu/avar/index.php). Don't forget to check main page for advanced query language.
#' @param query Vector of length 1 which contain query.
#' @param kwic Logical. KWIC (key word in context) is the format for resulted lines. If TRUE, then it returns a dataframe with query in the middle and left and right contexts. If FALSE, then it returns each result in one string. By default is TRUE.
#' @param write This argument writes a file in the working derictory (see function getwd() and setwd() for more information). If FALSE, then it creates a dataframe in Global Environment. Otherwise function writes a .tsv file with the name frome the argument value. By default is FALSE.
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' ava_corpus("магIарул")
#' ava_corpus("магIарул*")
#' ava_corpus("ва(ц|с)азе", kwic = F)
#' @export
#' @import xml2
#' @import selectr
#' @import httr
#' @import rvest
#' @import stringr

ava_corpus <- function(query, kwic = TRUE, write = FALSE){
  if(length(query) != 1){
    warning('x must be of length 1. If you want a dataframe with different queries try \n do.call("rbind.data.frame", sapply(x, ava.corpus, simplify = F))')
  }

  # get url
  my.url <- paste0("http://baltoslav.eu/avar/index.php?xxx=",
                  query,
                  "&met=kon&ks=ca&f=d&mova=en#wierch")

  # get text
  xml2::read_html(my.url) %>%
    html_nodes("td:nth-child(1) > div:nth-child(2) > div:nth-child(6) > div:nth-child(2) > div:nth-child(1)") %>%
    html_text() ->
    lines

  # get query result
  xml2::read_html(my.url) %>%
    html_nodes("td:nth-child(1) > div:nth-child(2) > div:nth-child(6) > div:nth-child(2) > div:nth-child(1) > b:nth-child(2)") %>%
    html_text() ->
    query_word

  # get final dataframe
  text_parts <- unlist(stringr::str_split(lines, paste0(" ", query_word, " ")))
  results <- data.frame(left.part = text_parts[1:20*2-1],
                        center.part = query_word,
                        right.part = text_parts[1:20*2])

  # kwic argument
  if(kwic == F){
    results <- apply(results, 1, function(x) paste(x, collapse = ""))
  }

  # write argument
  if(write != FALSE){
    write.table(results, paste0(write, ".csv"), row.names = F, sep = "\t")
  } else {return(results)}
  }
