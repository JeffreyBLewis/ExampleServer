#' @import tidyverse
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export
eg01_constructing_a_confidence_interval <- list(
  seed_eg = function() set.seed(1234),
  title = "Calculating an average treatment effect confidence interval",
  description = "Estimate one effect from the poverty relief program
  studied in Banerjee et al. 'A multifaceted program causes lasting progress for the very poor:
  Evidence from six countries' (Science, 2015)",

  # Function to load data
  get_data = function(){
    data("lasting_progress")
    lasting_progress
  },

  # Function to create bot respondent
  sim_bot = function(data) {
    res <- data %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      group_by(treatment) %>%
      summarize(mean = mean(ctotal_pcmonth_end - ctotal_pcmonth_bsl, na.rm = TRUE),
                sd = sd(ctotal_pcmonth_end - ctotal_pcmonth_bsl, na.rm = TRUE),
                n = n(),
                .groups = "drop") %>%
      mutate(se = sd/sqrt(n)) %>%
      summarize( ate = (mean[treatment == "Treatment"] -
                        mean[treatment == "Control"])/
                        sd[treatment == "Control"],
                 se  = sqrt(sum(se^2))/sd[treatment == "Control"]) %>%
      mutate(ci_low = ate - 1.96*se,
             ci_high = ate + 1.96*se)

    jsonlite::toJSON(list(
      id = paste0("Bot_",
                  paste0(
                    sample(LETTERS, 4, replace = TRUE), collapse = ""
                  )),
      payload = res,
      eg = "eg01_constructing_a_confidence_interval"
    ))
  },

  # Function to parse the submitted results
  parse_results = function(results_json) {
    rawdata <- jsonlite::fromJSON(results_json, flatten = TRUE)
    if (class(rawdata) == "list") {
      res <- data.frame(id = character(0),
                        ate = numeric(0),
                        se = numeric(0),
                        ci_low = numeric(0),
                        ci_high = numeric(0),
                        eg = character(0))
    } else {
      res <- rawdata %>%
        tidyr::unnest(cols = everything()) %>%
        select(id, ate, se, ci_low, ci_high, eg)
    }
    res
  },

  # Functions to make result displays
  make_table = function(results_data)
    results_data,
  make_summary = function(results_data) {
    data.frame(
      Quantity = c(
        "Average estimated treatment effect",
        "Observed std. error of the estimated treatment effect",
        "Average estimated standard error",
        "Number of estimates"
      ),
      Value = c(
        mean(results_data$ate, na.rm = TRUE),
        sd(results_data$ate, na.rm = TRUE),
        mean(results_data$se, na.rm = TRUE),
        length(results_data$ate)
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
        ymin = ci_low,
        ymax = ci_high,
        x = id,
        linetype = isbot,
        # 0.0113 is the estiamted/pseudo-pop value for the full dataset
        color = ifelse(ci_low < 0.0113 &
                       ci_high > 0.0113,
                       "Covers", "Doesn't \"cover\"")
      )
    ) +
      geom_linerange(show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      xlab("")
    ggplotly(gg, tooltip="id")
  }
)

#' @export
test_eg01 <- function(eg = eg01_constructing_a_confidence_interval) {
 # eg$seed_eg()
  dat <- eg$get_data()
  print( eg$sim_bot(dat) )
  json <- sprintf("[%s]",
                  paste0(purrr::map_chr(1:10,
                                        function(z)
                                          eg$sim_bot(dat)),
                         collapse = ","))
  print(json)

  rd <- eg$parse_results(json)
  print(glimpse(rd))
  print(eg$make_summary(rd))
  print(eg$make_plot(rd))
  rd
}

#rd <- test_eg()
