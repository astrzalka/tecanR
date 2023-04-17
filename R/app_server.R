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
    if(input$exp_type == 'myco'){
      res <- read_tecan_sheet_Kornel(input$tecan_file$datapath, sheet = 'Sheet0')
    }

    if(input$exp_type == 'strepto'){
      res <- read_tecan_sheet_Martyna(input$tecan_file$datapath, sheet = 'Sheet0')
    }


    return(res)
  })

  data_final <- reactive({

    tecan_data() %>% dplyr::left_join(plate_map()) -> data

    if(input$plot_type == 'od'){
      data %>% dplyr::rename(time = time_od, value = OD) -> data
    } else if(input$plot_type == 'fluo'){
      data %>% dplyr::rename(time = time_fluo, value = fluo) -> data
      if(input$norm){
        data %>% dplyr::mutate(value = value/OD) -> data
      }
    } else if(input$plot_type == 'lumi'){
      data %>% dplyr::rename(time = time_lumi, value = lumi) -> data
      if(input$norm){
        data %>% dplyr::mutate(value = value/OD) -> data
      }
    }

    data %>% dplyr::filter(strain %in% input$strains_ui) -> data
     if(!is.na(input$filter_time)){
       data %>% dplyr::filter(time/60 <= input$filter_time) -> data
     }

    if(!is.na(input$filter_value)){
      data %>% dplyr::filter(value <= input$filter_value) -> data
    }
    return(data)
  })

  plot_tecan <- reactive({

   data <- data_final()

    data %>%
      ggplot2::ggplot(ggplot2::aes(x = time/60, y = value, color = factor(rep)))+
      ggplot2::geom_line()+
      ggplot2::facet_wrap(~strain)+
      ggplot2::theme_minimal()+
      ggplot2::xlab('Time [min]')+
      ggplot2::theme(legend.position = 'bottom')-> p

    return(p)
  })

  output$plot_1 <- renderPlot(plot_tecan())

  plot_tecan2 <- reactive({

    data <- data_final()

    data %>%
      dplyr::group_by(strain, time) %>%
      dplyr::summarise(mean_value = mean(value)) %>%
      #tidyr::separate(strain, into = c('strain2', 'cond'), sep = '_', remove = FALSE) %>%
      ggplot2::ggplot(ggplot2::aes(x = time/60, y = mean_value, color = strain))+
      ggplot2::geom_line()+
      ggplot2::theme_minimal()+
      ggplot2::xlab('Time [min]')+
      ggplot2::theme(legend.position = 'bottom')-> p

    return(p)
  })

  output$plot_2 <- renderPlot(plot_tecan2())

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_final(), file, row.names = FALSE)
    }
  )

  output$download_plot <- downloadHandler(
    filename = function() { paste(input$dataset, '.tiff', sep='') },
    content = function(file) {
      tiff(file, res = 200, width = input$width, input$height, unit = 'cm', compression = 'lzw')
      print(plot_tecan2())
      dev.off()
    })

  output$strains_ui <- renderUI({
    plate <- plate_map()

    szczepy <- unique(plate$strain)

    checkboxGroupInput("strains_ui", label = ("Choose strains"),
                       choices = szczepy,
                       selected = szczepy)

  })

}
