#' Download national team results
#'
#' Results are downloaded from: https://github.com/martj42/international_results
#'
#' @return tibble.
#' @examples
#' download_int_results()
#' @export

download_int_results <- function(){

  url <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"

  x <- url %>%
    httr::GET() %>%
    httr::stop_for_status() %>%
    httr::content() %>%
    readr::read_csv() %>%
    assert_results()

  return(x)
}

#' @keywords internal

assert_results <- function(x){

  a <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = x,
                               types = c("Date", "character","numeric","logical"),
                               any.missing = FALSE,
                               ncols = 9,
                               min.rows = 40000,
                               null.ok = FALSE,
                               col.names = "named",
                               add = a)

  checkmate::assert_names(names(x),
                          identical.to = c("date", "home_team", "away_team",
                                           "home_score","away_score", "tournament",
                                           "city", "country", "neutral"),
                          add = a)

  # country_regex <- "^[:upper:]([:alnum:]| |-|\\.)*$"
  country_regex <- "([:alnum:]| |-|\\.)*"

  checkmate::assert_date(x[["date"]], upper = Sys.Date(), add = a)
  checkmate::assert_character(x[["home_team"]], pattern = country_regex, add = a)
  checkmate::assert_character(x[["away_team"]], pattern = country_regex, add = a)
  checkmate::assert_numeric(x[["home_score"]], lower = 0, add = a)
  checkmate::assert_numeric(x[["away_score"]], lower = 0, add = a)
  checkmate::assert_character(x[["tournament"]], add = a)
  checkmate::assert_character(x[["city"]], add = a)
  checkmate::assert_character(x[["country"]], pattern = country_regex, add = a)
  checkmate::assert_logical(x[["neutral"]])

  checkmate::reportAssertions(a)

  return(invisible(x))
}
