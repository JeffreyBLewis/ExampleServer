#' @export
post_result <- function(id = paste0("Student_",
                                      paste0(
                                        sample(LETTERS, 4, replace = TRUE),
                                        collapse = "")),
                         example = "eg00_toy_example",
                         result,
                         url = the_url) {
    pl <- list(id = id,
               payload = result,
               eg = example)
    res <- httr::POST(url, body = pl, encode = "json")
    if (res$status_code != 200) {
      error(sprintf("Failed to post to server (Returned status code %s).",
                    res$status_code))
    }
    res
}

#' @export
get_eg_data <- function( eg = "eg00_toy_example",
                        url = the_url) {
  dget(paste0(url, "/", eg, ".Rdata"))
}
