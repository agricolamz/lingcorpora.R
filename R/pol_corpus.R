#' Polish search
#'
#' Post query to the National Corpus of Polish (nkjp.pl). Don't forget to check http://nkjp.pl/poliqarp/help/plse3.html#x4-50003 for advanced query language.
#' @param query Vector of length 1 which contain query.
#' @param tag Logical. Do you need morphological tags? By default is FALSE.
#' @param n_results Integer. Defines number of examples from the corpus. By default is 10
#' @param corpus Vector with a type of the corpus: "nkjp300", "nkjp1800", "nkjp1M", "ipi250", "ipi030", "frequency-dictionary"
#' @param kwic Logical. KWIC (key word in context) is the format for resulted lines. If TRUE, then it returns a dataframe with query in the middle and left and right contexts. If FALSE, then it returns each result in one string. By default is TRUE.
#' @param write This argument writes a file in the working derictory (see function getwd() and setwd() for more information). If FALSE, then it creates a dataframe in Global Environment. Otherwise function writes a .tsv file with the name frome the argument value. By default is FALSE.
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' pol_corpus("boisz się")
#' pol_corpus("boisz się", tag = TRUE, n_results = 40)
#' pol_corpus("An*a")
#' pol_corpus("[base = 'strzyc']")
#' @export
#' @import xml2
#' @import selectr
#' @import httr
#' @import rvest

pol_corpus <- function(query, tag = F, n_results = 10, corpus = "nkjp300", kwic = T, write = F){
    if(length(query) != 1){
      warning('x must be of length 1. If you want a dataframe with different queries try \n do.call("rbind.data.frame", sapply(x, pol.corpus, simplify = F))')
    }
    # show tags?
    if(tag == T){
      tag.variable <- "slt"
    } else {tag.variable <- "s"}

    # set nkjp.pl settings
    settings <- httr::POST(url = "http://nkjp.pl/poliqarp/settings/",
                           body = list(
                             random_sample = "0",
                             random_sample_size = "50",
                             sort_column = "rm",
                             sort_type = "afronte",
                             sort_direction = "asc",
                             show_in_match = tag.variable,
                             show_in_context = tag.variable,
                             left_context_width = "10",
                             right_context_width = "10",
                             wide_context_width = "50",
                             results_per_page = as.character(n_results),
                             `next` = "/poliqarp/"))

    # get request data
    request <- httr::POST(url = "http://nkjp.pl/poliqarp/query/",
                          body = list(query = query, corpus = corpus),
                          set_cookies(domain = "#HttpOnly_nkjp.pl",
                                      flag = "FALSE",
                                      path = "/",
                                      secure = "FALSE",
                                      expiration = "<NA>",
                                      name = "sessionid",
                                      value = as.character(settings$cookies[7]))) # cookies from settings

    # get data
    my.url <- httr::GET(url = "http://nkjp.pl/poliqarp/nkjp300/query/",
                        set_cookies(domain = "#HttpOnly_nkjp.pl",
                                    flag = "FALSE",
                                    path = "/",
                                    secure = "FALSE",
                                    expiration = "<NA>",
                                    name = "sessionid",
                                    value = as.character(request$cookies[7]))) # cookies from request

    # check for errors
    xml2::read_html(my.url) %>%
      html_nodes(".errorlist > li:nth-child(1)") %>%
      html_text() ->
      errorlist

    if(length(errorlist) > 0){
      warning(errorlist)
    }

    # check for no result
    xml2::read_html(my.url) %>%
      html_nodes(".query-results > p:nth-child(2)") %>%
      html_text() ->
      number.results
    if(length(number.results) > 1){
      if(grepl(pattern = "Found 0 results", number.results)){
        warning(paste('No results were returned by the query "', query, '"', sep = ""))
      }}


    # get left part
    xml2::read_html(my.url) %>%
      html_nodes("td:nth-child(2)") %>%
      html_text() ->
      left.part
    left.part <- gsub("\\s+", " ", left.part) # clean data

    # get query word
    xml2::read_html(my.url) %>%
      html_nodes("td:nth-child(3)") %>%
      html_text() ->
      center.part
    center.part <- gsub("\\s+", " ", center.part) # clean data

    # get right part
    xml2::read_html(my.url) %>%
      html_nodes("td:nth-child(4)") %>%
      html_text() ->
      right.part
    right.part <- gsub("\\s+", " ", right.part) # clean data

    results <- data.frame(left.part, center.part, right.part, stringsAsFactors = F)

    # diparse kwic format into plain text
    if(kwic == F){
      results <- apply(results, 1, function(x) paste(x, collapse = ""))
    }

    # write argument
    if(write != F){
      write.table(results, paste0(write, ".csv"), row.names = F, sep = "\t")
    } else {return(results)}
}
