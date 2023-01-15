#' @import tidyverse
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export
eg00_toy_example <- list(
  title = "Estimating a mean",
  description = "Data are 1000 draws from a uniform distribution on the zero-ten interval. Report back your estimate of the
  mean of the population mean from which the data were drawn using a random subsample of 10 cases.",
  get_data = function(){data.frame(y=runif(10000, 0, 10))},
  sim_bot = function(data){
    jsonlite::toJSON(
      list(id = paste0("Bot_",
                       paste0(sample(LETTERS, 4, replace = TRUE), collapse = "")),
           payload = list(mean=mean(sample(data$y, 10, replace=TRUE))),
           eg = "eg00_toy_example")
    )
  },
  parse_results = function(results_json) {
    rawdata <- jsonlite::fromJSON(results_json, flatten=TRUE)
    if (class(rawdata) == "list") {
      res <- data.frame(
        id = character(0),
        mean = numeric(0),
        eg = character(0))
    } else {
      res <- rawdata %>%
        rename("mean" = "payload.mean") %>%
        tidyr::unnest(cols=everything())
    }
    res
  },
  make_table = function(results_data) results_data,
  make_summary = function(results_data){
                            data.frame(
                              Quantity = c("Average estimated mean",
                                           "Std. dev. of the estimated means",
                                           "Number of estimates"),
                              Value = c(mean(results_data$mean, na.rm=TRUE),
                                        sd(results_data$mean, na.rm=TRUE),
                                        length(results_data$mean))
                            )},
  make_plot = function(results_data) {
    gg <- ggplot(results_data %>%
                   mutate(isbot = ifelse(stringr::str_detect(id, "^Bot_"),
                                         "Bot", "Student")) %>%
                   arrange(isbot),
                 aes(x=mean, text=id, fill=isbot, color=isbot)) +
      geom_histogram(show.legend=FALSE) +
      theme_minimal()
    ggplotly(gg, tooltip = "id")
  },
  seed_eg = function() {set.seed(123)}
)

test_eg00_toy_example <- function() {
  eg00_toy_example$seed_eg()
  dat <- eg00_toy_example$get_data()
  json <- sprintf("[%s]",
            paste0(
              purrr::map_chr(1:1000,
                function(z) eg00_toy_example$sim_bot(dat)),
                collapse=","))
  rd<- eg00_toy_example$parse_results(json)
  print(eg00_toy_example$make_summary(rd))
  print( eg00_toy_example$make_plot(rd) )
  rd
}

#z <- test_eg00_toy_example()
