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
      titlePanel("Tecan Plate Reader Data Analysis"),
      sidebarLayout(
        sidebarPanel(
          fileInput("plate_map_file", "Upload Plate Map"),
          fileInput("tecan_file", "Upload Tecan File")
        ),
        mainPanel(tabsetPanel(
          tabPanel('plot all', plotOutput("plot_1")),
          tabPanel('plot combined', plotOutput("plot_2"))
        )

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
