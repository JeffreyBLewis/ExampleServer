#' @import tidyverse
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export

eg02_a_simple_hypothesis_test <- list(
  seed_eg = function() set.seed(1234),
  title = "Calculating a simple hypothesis test",
  description = "Construct a simple hypothesis regarding one effect from the poverty relief program
  studied in Banerjee et al. 'A multifaceted program causes lasting progress for the very poor:
  Evidence from six countries' (Science, 2015)",

  # Function to load data
  get_data = function(){
    data("lasting_progress")
    lasting_progress
  },

  # Function to create bot respondent
  sim_bot = function(data) {
    data <- data %>%
      slice_sample(prop=1, replace = TRUE) %>%
      mutate(delta_consumption =
               ctotal_pcmonth_end - ctotal_pcmonth_bsl)
    r1 <- t.test(delta_consumption ~ treatment, data = data)
    r2 <- t.test(delta_consumption ~ treatment,
                 data = data %>%
                          mutate(treatment =
                                  treatment[sample(1:n(), replace=TRUE)]))
    jsonlite::toJSON(list(
      id = paste0("Bot_",
                  paste0(
                    sample(LETTERS, 4, replace = TRUE), collapse = ""
                  )),
      payload = data.frame(z_data = r1$statistic,
                           p_data = r1$p.value,
                           z_sim = r2$statistic,
                           p_sim = r2$p.value),
      eg = "eg02_a_simple_hypothesis_test"
    ))
  },

  # Function to parse the submitted results
  parse_results = function(results_json) {
    rawdata <- jsonlite::fromJSON(results_json, flatten = TRUE)
    if (class(rawdata) == "list") {
      res <- data.frame(id = character(0),
                        z_data = numeric(0),
                        p_data = numeric(0),
                        z_sim = numeric(0),
                        p_sim = numeric(0))
    } else {
      res <- rawdata %>%
        tidyr::unnest(cols = everything()) %>%
        select(id, z_data, p_data, z_sim, p_sim)
    }
    res
  },

  # Functions to make result displays
  make_table = function(results_data)
    results_data,
  make_summary = function(results_data) {
    data.frame(
      Quantity = c(
        "Average z from data",
        "Average z from simulation",
        "Average p value from data",
        "Average p value from simulation",
        "Fraction of real-data p values < 0.05",
        "Fraction of simulated-data p values < 0.05",
        "Number of estimates"
      ),
      Value = c(
        mean(results_data$z_data, na.rm = TRUE),
        mean(results_data$z_sim, na.rm = TRUE),
        mean(results_data$p_data, na.rm = TRUE),
        mean(results_data$p_sim, na.rm = TRUE),
        mean(results_data$p_data < 0.05, na.rm = TRUE),
        mean(results_data$p_sim < 0.05, na.rm = TRUE),
        length(results_data$z_data)
      )
    )
  },
  make_plot = function(results_data) {
     gg <- ggplot(
      results_data %>%
        pivot_longer(-id,
                     names_to="var",
                     values_to = "val") %>%
        separate(var, into=c("quant", "Data"), sep="_") %>%
        mutate(quant = ifelse(quant == "p", "P-value", "Z-statistic")),
      aes(
        x = val,
        y = ..density..,
        col = Data,
        fill = Data
      )
    ) +
      facet_wrap(~quant + Data, scales = "free") +
      geom_histogram(alpha = 0.25,
                     position="identity",
                     bins = 11,
                     show.legend = FALSE) +
      theme_minimal() +
      theme(legend.position='none') +
      xlab("")
    ggplotly(gg, tooltip="id")
  }
)

#' @export
test_eg02 <- function(eg = eg01_constructing_a_confidence_interval) {
 # eg$seed_eg()
  dat <- eg$get_data()
  print( eg$sim_bot(dat) )
  json <- sprintf("[%s]",
                  paste0(purrr::map_chr(1:1000,
                                        function(z)
                                          eg$sim_bot(dat)),
                         collapse = ","))
  print(json[[1]])

  rd <- eg$parse_results(json)
  print(glimpse(rd))
  print(eg$make_summary(rd))
  print(eg$make_plot(rd))
  rd
}

#rd <- test_eg02()
