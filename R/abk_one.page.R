#' Abkhaz one page search
#'
#' Post query to the Abkhaz Text Corpus (http://baltoslav.eu/apsua/index.php). Don't forget to check main page for advanced query language.
#' @param my.url Vector with a link
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' ava_one.page.get("http://baltoslav.eu/apsua/index.php?xxx=%D0%B1%D1%8B%D0%B7%D1%88%D3%99%D0%B0&met=kon&ks=ca&mova=en#wierch")
#' @import xml2
#' @import selectr
#' @import httr
#' @import rvest
#' @import stringr


abk_one.page <- function(my.url){
  # get page
  page <- xml2::read_html(my.url)
  # get context
  page %>%
    html_nodes("div.cyry") %>%
    html_text() ->
    lines

  # get query result
  page %>%
    html_nodes("div.cyry > b") %>%
    html_text() ->
    query_word

  query_word <- query_word[ql_coresp(query_word, lines)]

  # get final dataframe
  text_parts <- unlist(stringr::str_split(lines, paste0(" ", query_word, " ")))
  results <- data.frame(left.part = text_parts[1:20*2-1],
                        center.part = query_word,
                        right.part = text_parts[1:20*2])

  return(results)
}
