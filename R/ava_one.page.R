#' Avar one page search
#'
#' Post query to the Avar Text Corpus (http://baltoslav.eu/avar/index.php). Don't forget to check main page for advanced query language.
#' @param my.url Vector with a link
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' ava_one.page.get("http://baltoslav.eu/avar/index.php?xxx=магIарул*&met=kon&ks=ca&f=d&mova=en#wierch")
#'
#' @import xml2
#' @import selectr
#' @import httr
#' @import rvest


ava_one.page <- function(my.url){
  # get page
  page <- xml2::read_html(my.url)
  # get context
  page %>%
    html_nodes("td:nth-child(1) > div:nth-child(2) > div:nth-child(6) > div:nth-child(2) > div:nth-child(1)") %>%
    html_text() ->
    lines

  # get query result
  page %>%
    html_nodes("td:nth-child(1) > div:nth-child(2) > div:nth-child(6) > div:nth-child(2) > div:nth-child(1) > b:nth-child(2)") %>%
    html_text() ->
    query_word

  # get final dataframe
  text_parts <- unlist(stringr::str_split(lines, paste0(" ", query_word, " ")))
  results <- data.frame(left.part = text_parts[1:20*2-1],
                        center.part = query_word,
                        right.part = text_parts[1:20*2])

  return(results)
  }
