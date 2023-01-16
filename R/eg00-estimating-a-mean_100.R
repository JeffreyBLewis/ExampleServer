#' @import tidyverse
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export
eg00_estimating_a_mean_100 <- list(
  seed_eg = function() set.seed(123),
  title = "Estimating a mean",
  description = "Data are 1000 draws from a uniform distribution on the zero-ten interval. Report back your estimate of the
  the mean of the population from which the data were drawn using a random subsample of 100 cases.",

  # Function to load data
  get_data = function(){data.frame(y=runif(1000, 0, 10))},

  # Function to create bot respondent
  sim_bot = function(data){
    jsonlite::toJSON(
      list(id = paste0("Bot_",
                       paste0(sample(LETTERS, 4, replace = TRUE), collapse = "")),
           payload = data.frame(mean=mean(sample(data$y, 100, replace=TRUE))),
           eg = "eg00_estimating_a_mean_100")
    )
  },

  # Function to parse the submitted results
  parse_results = function(results_json) {
    rawdata <- jsonlite::fromJSON(results_json, flatten=TRUE)
    if (class(rawdata) == "list") {
      res <- data.frame(
        id = character(0),
        mean = numeric(0),
        eg = character(0))
    } else {
      res <- rawdata %>%
        tidyr::unnest(cols=everything())
    }
    res
  },

  # Functions to make result displays
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
                 aes(x = mean, text = id, fill = isbot, color = isbot)) +
      geom_histogram(show.legend = FALSE, bins=101) +
      xlim(0, 10) +
      theme_minimal()
    ggplotly(gg, tooltip = "id")
  }
)

#' @export
test_eg <- function(eg = eg00_estimating_a_mean) {
  eg$seed_eg()
  dat <- eg$get_data()
  json <- sprintf("[%s]",
                  paste0(
                    purrr::map_chr(1:1000,
                                   function(z) eg$sim_bot(dat)),
                    collapse=","))
  rd <- eg$parse_results(json)
  print(glimpse(rd))
  print(eg$make_summary(rd))
  print(eg$make_plot(rd) )
  rd
}

#rd <- test_eg()
