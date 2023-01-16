#' Launch example server
#'
#` @port port number for serve
#' @import tidyverse
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @export
launch_server <- function(port = 4567,
                          title = "ps200b: Example server",
                          eg_str = "eg00_estimating_a_mean",
                          ...) {

  ## Setup: Get example and associated data
  datafile <- tempfile(fileext = "json")
  jsonlite::write_json(list(), datafile)
  eg <- eval(str2lang(eg_str))
  data <- eg$get_data()

  ## Handle requests for data and submission of results
  req_handler <- function(req, response) {
    ## Handle download of (over-sampled data) request
    if (stringr::str_detect(req$PATH_INFO,
                            paste0("/", eg_str, ".Rdata$"))) {
      samp_dat <- data %>%
                    slice_sample(prop = 1,
                                 replace = TRUE) %>%
                    dput() %>%
                    capture.output() %>%
                    paste0(collapse="")

      return(shiny:::httpResponse(status = 200,
                                  content_type = "application/gzip",
                                  content = samp_dat))
    }
    ## Handle posting of a student response...
    if (identical(req$REQUEST_METHOD, "POST")) {
      print("Received post...")
      pdata <- req$rook.input$read(1e5)
      pdata <- jsonlite::fromJSON(rawToChar(pdata))
      if (is.null(pdata$id)) {
        return(response)
      } else {
        print("Parsing result post..")
        x <- append(list(pdata), jsonlite::read_json(datafile))
        jsonlite::write_json(x, datafile, auto_unbox=TRUE)
        # return all okay
        return(shiny:::httpResponse(200, "text/plain", "OK\n"))
      }
    }
    response
  }

  options(shiny.http.response.filter = req_handler)

  ui <- fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        h3(textOutput("title")),
        textOutput("eg_str"),
        br(),
        textOutput("description"),
        br(),
        actionLink("add_bots", "Add bot responses"),
        br(),
        actionLink("clear", "Clear responses"),
        br(),
        br(),
        textOutput("count")
      ),
      mainPanel(
        tabsetPanel(type="tab",
                    tabPanel("Data", tableOutput('data')),
                    tabPanel("Estimates", tableOutput('table')),
                    tabPanel("Summary", tableOutput('summary')),
                    tabPanel("Plot", plotlyOutput('plot')))
      )
    )
  )

  server <- function(input, output, session) {
    ## Add bots (simulated student responses...)
    observeEvent(input$add_bots, {
      json <- readr::read_file(datafile) %>%
               stringr::str_replace_all("(^\\[)|(\\]\\n*$)", "")
      cat(sprintf("[%s]",
                 paste0(
                   json,
                   ifelse(nchar(json)==0, "", ","),
                   paste0(
                     purrr::map_chr(1:500,
                                  function(z) eg$sim_bot(data)),
                     collapse=","))),
          file = datafile)
    })

    # Clear all responses
    observeEvent(input$clear, {
        cat("[]", file = datafile)
    })

    # Poll JSON file for new responses
    results_data <- reactiveFileReader(1000, session, datafile, function(x) {
        eg$parse_results(x)
    })

    # Generate page content to flow into UI
    output$title <- renderText(eg$title)
    output$eg_str <- renderText(sprintf("(%s)", eg_str))
    output$description <- renderText(eg$description)
    output$data <- renderTable(data %>%
                                 mutate(`Obs no.` = 1:dplyr::n()) %>%
                                 select(`Obs no.`, everything()))
    output$table <- renderTable(eg$make_table(results_data()))
    output$plot <- renderPlotly(eg$make_plot(results_data()))
    output$summary <- renderTable(eg$make_summary(results_data()))
    output$count <- renderText(sprintf("Estimates reported: %i", NROW(results_data())))
  }

  shinyApp(ui, server,
         options = list(port = port, ...))
}


