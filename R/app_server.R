#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function(input, output, session) {

  library(magrittr)

  # Read in plate map and Tecan file
  plate_map <- reactive({
    req(input$plate_map_file)
    read_tecan_plate_map(input$plate_map_file$datapath)
  })

  tecan_data <- reactive({
    req(input$tecan_file)
    read_tecan_sheet_Kornel(input$tecan_file$datapath, sheet = 'Sheet0')
  })

  plot_tecan <- reactive({

    tecan_data() %>% dplyr::left_join(plate_map()) -> data_kornel_fluo

    data_kornel_fluo %>%
      ggplot2::ggplot(ggplot2::aes(x = time_od, y = OD, color = factor(rep)))+
      ggplot2::geom_line()+
      ggplot2::facet_wrap(~strain) -> p

    return(p)
  })

  output$plot_1 <- renderPlot(plot_tecan())

  plot_tecan2 <- reactive({

    tecan_data() %>% dplyr::left_join(plate_map()) -> data_kornel_fluo

    data_kornel_fluo %>%
      dplyr::group_by(strain, time_od) %>%
      dplyr::summarise(mean_od = mean(OD)) %>%
      tidyr::separate(strain, into = c('strain2', 'cond'), sep = '_', remove = FALSE) %>%
      ggplot2::ggplot(ggplot2::aes(x = time_od, y = mean_od, color = cond))+
      ggplot2::geom_line()+
      ggplot2::facet_wrap(~strain2) -> p

    return(p)
  })

  output$plot_2 <- renderPlot(plot_tecan2())

  }
