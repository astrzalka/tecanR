#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = shinythemes::shinytheme("united"),
      titlePanel("Tecan Plate Reader"),
      fluidRow(
        column(2, style = "background-color: #f8f9fa;",
               radioButtons('exp_type', 'Choose experiment type',
                            choices = c("Mycobacterium" = "myco", "Streptomyces: luminescence" = "strepto",
                                        "Streptomyces: multiple days" = "strepto_multi"), selected = 'myco'),
               fileInput("plate_map_file", "Upload Plate Map"),
               fileInput("tecan_file", "Upload Tecan File"),
               radioButtons('plot_type', 'Choose values to plot', choices = c('OD' = 'od', 'Fluorescence' = 'fluo', 'Luminescence' = 'lumi'),
                            inline = TRUE),
               checkboxInput('norm', 'Normalize fluorescence/luminescense data by OD?', value = FALSE),
               downloadButton('download_plot', 'Download plot'),
               numericInput('width', 'Plot width [cm]', 20, min = 5, max = 35),
               numericInput('height', 'Plot height [cm]', 14, min = 5, max = 35),
               downloadButton("downloadData", "Download data")
        ),
        column(8,tabsetPanel(
          tabPanel('plot all', plotOutput("plot_1", height = "600px", width = '100%')),
          tabPanel('plot combined', plotOutput("plot_2", height = "600px", width = '100%'))
        )
        ),
        column(2, style = "background-color: #f8f9fa;",
               h5('Filters:'),
               numericInput('filter_time', 'Maximum time', value = NA, step = 50),
               numericInput('filter_value', 'Maximum value of OD/luminescence/fluorescence', value = NA, step = 100),
               uiOutput("strains_ui")

        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "tecanR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
