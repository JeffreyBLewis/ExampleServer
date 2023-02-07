#' @import tidyverse
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export
eg03_bivariate_regression <- list(
  seed_eg = function() set.seed(1234),
  title = "Estimating a bivariate regression",
  description = "Estimate the regression of US two-party presidential popular vote on
  presidential approval 10 months earlier",

  # Function to load data
  get_data = function(){
    data("approval_and_votes")
    approval_and_votes
  },

  # Function to create bot respondent
  sim_bot = function(data) {
    res_lm <- lm(incumbent_party_vote ~ approve, data = data)
    sim_dat <- data %>%
                 mutate(incumbent_party_vote = res_lm$fitted.values +
                                               res_lm$residuals[sample(1:NROW(data),
                                                                       replace=FALSE)])
    sim_lm <- lm(incumbent_party_vote ~ approve, data = sim_dat)
    res = data.frame(b0 = coef(sim_lm)[1],
               b1 = coef(sim_lm)[2],
               se0 = sqrt(vcov(sim_lm)[1,1]),
               se1 = sqrt(vcov(sim_lm)[2,2]),
               cov01 = vcov(sim_lm)[1,2],
               r2 = summary(sim_lm)$r.squared)

    jsonlite::toJSON(list(
      id = paste0("Bot_",
                  paste0(
                    sample(LETTERS, 4, replace = TRUE), collapse = ""
                  )),
      payload = res,
      eg = "eg03_bivariate_regression"
    ))
  },

  # Function to parse the submitted results
  parse_results = function(results_json) {
    rawdata <- jsonlite::fromJSON(results_json, flatten = TRUE)
    print(rawdata)
    if (class(rawdata) == "list") {
      res <- data.frame(id = character(0),
                        b0 = numeric(0),
                        b1 = numeric(0),
                        se0 = numeric(0),
                        se1  = numeric(0),
                        cov01  = numeric(0),
                        r2 = numeric(0),
                        eg = character(0))
    } else {
      res <- rawdata %>%
        tidyr::unnest(cols = everything()) %>%
        select(id, b0, b1, se0, se1, cov01, r2, eg)
    }
    res
  },

  # Functions to make result displays
  make_table = function(results_data)
    results_data,
  make_summary = function(results_data) {
    data.frame(
      Quantity = c(
        "Average b0",
        "Average b1",
        "sd(b0)",
        "sd(b1)",
        "Average st. se(b0)",
        "Average est. se(b1)",
        "Cov(b0, b1)",
        "Average est. cov(b0, b1)",
        "Number of estimates"
      ),
      Value = c(
        mean(results_data$b0, na.rm = TRUE),
        mean(results_data$b1, na.rm = TRUE),
        sd(results_data$b0, na.rm = TRUE),
        sd(results_data$b1, na.rm = TRUE),
        mean(results_data$se0, na.rm = TRUE),
        mean(results_data$se1, na.rm = TRUE),
        cov(results_data$b0, results_data$b1),
        mean(results_data$cov01),
        length(results_data$b0)
      )
    )
  },
  make_plot = function(results_data) {
     gg <- ggplot(
      results_data %>%
        mutate(isbot = ifelse(
          stringr::str_detect(id, "^Bot_"),
          "Bot", "Student"
        )),
      aes(
        x = b0,
        y = b1,
        color = isbot)) +
      geom_point() +
      theme_minimal() #+
    ggplotly(gg)
  }
)

#' @export
test_eg03 <- function(eg = eg03_bivariate_regression) {
 # eg$seed_eg()
  dat <- eg$get_data()
  print( eg$sim_bot(dat) )
  json <- sprintf("[%s]",
                  paste0(purrr::map_chr(1:100,
                                        function(z)
                                          eg$sim_bot(dat)),
                         collapse = ","))
  #print(json)

  rd <- eg$parse_results(json)
  print(cov(rd$b0, rd$b1))
  print(glimpse(rd))
  print(eg$make_summary(rd))
  print(eg$make_plot(rd))
  rd
}

#devtools::load_all(); test_eg03()
