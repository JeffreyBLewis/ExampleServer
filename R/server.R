eg_list <- function() {
  ls.str("package:ExampleServer") %>%
    as.character() %>%
    str_subset("^eg\\d{2}\\_")
  }

#' Launch example server
#'
#` @port port number for serve
#' @import tidyverse
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @export
launch_server <- function(port = 4567,
                          title = "ps200b: Example server",
                          eg_str = "eg00_estimating_a_mean_10",
                          ...) {

  ## Setup: Get example and associated data
  datafile <- tempfile(fileext = "json")
  jsonlite::write_json(list(), datafile)

  # Using these global variables make data download work...
  eg <- eval(str2lang(eg_str))
  eg$seed_eg()
  data <- eg$get_data()

  ## Handle requests for data and submission of results
  req_handler <- function(req, response) {

    ## Pass through AJAX call made by DT
    if (!is.na(req$HEADERS['x-requested-with']) &
        req$HEADERS["x-requested-with"] == "XMLHttpRequest") {
      return(response)
    }

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
      if (is.null(pdata$payload) || is.null(pdata$id)) {
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
        textOutput("count"),
        br(),
        selectInput("example", "Select example:", eg_list()),
      ),
      mainPanel(
        tabsetPanel(type="tab",
                    tabPanel("Data",
                             DT::dataTableOutput('data')),
                    tabPanel("Estimates", DT::dataTableOutput('table')),
                    tabPanel("Summary", tableOutput('summary')),
                    tabPanel("Plot", plotlyOutput('plot')))
      )
    )
  )

  server <- function(input, output, session) {
    egp <- reactiveValues( eg_str = eg_str,
                           eg = eval(str2lang(eg_str)),
                           data = data)

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

    # Switch examples
    observeEvent(input$example, {
      cat(sprintf("Switching to %s...\n", input$example))
      eg_str <<- egp$eg_str <- input$example
      eg <<- egp$eg <- eval(str2lang(eg_str))
      data <<- egp$data <- eg$get_data()
      eg$seed_eg()
      cat("[]", file = datafile)
    })

    # Poll JSON file for new responses
    results_data <- reactiveFileReader(1000, session, datafile, function(x) {
        eg$parse_results(x)
    })

    # Generate page content to flow into UI
    output$title <- renderText(egp$eg$title)
    output$eg_str <- renderText(sprintf("(%s)", egp$eg_str))
    output$description <- renderText(egp$eg$description)
    output$data <- DT::renderDataTable(egp$data %>%
                                         mutate(across(where(is.numeric), round, 3)),
                                       options = list(dom = 'tp'))
    output$table <- DT::renderDataTable(egp$eg$make_table(results_data()) %>%
                                          mutate(across(where(is.numeric), round, 3)),
                                        options = list(dom = 'tp'))
    output$plot <- renderPlotly(egp$eg$make_plot(results_data()))
    output$summary <- renderTable(egp$eg$make_summary(results_data()))
    output$count <- renderText(sprintf("Estimates reported: %i", NROW(results_data())))
  }

  shinyApp(ui, server,
           options = list(port = port, ...))
}


