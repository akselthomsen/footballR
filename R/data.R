
#' @export

download_new_result <- function(){

  url <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"

  x <- url %>%
    httr::GET() %>%
    httr::stop_for_status() %>%
    httr::content() %>%
    readr::read_csv()

  return(x)
}
