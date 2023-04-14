#' Read data from a TECAN i-control sheet and format it into tidy format
#'
#' This function reads data from a TECAN i-control sheet in an Excel file and formats the data into a tidy format.
#' The function extracts the date of the measurement, luminescence data, and OD data from the sheet and combines them
#' into a single data frame.
#'
#' @param path The file path of the Excel file containing the TECAN i-control sheet
#' @param sheet The name of the sheet containing the TECAN i-control data
#' @return A data frame containing the formatted TECAN i-control data
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import lubridate
#' @export
#' @examples
#' read_tecan_sheet("path/to/tecan_sheet.xlsx", "Sheet1")
read_tecan_sheet <- function(path, sheet){

  # read excel file
  test <- readxl::read_excel(path, sheet = sheet, progress = FALSE)

  # get date of measurement
  date_row <- which(test$`Application: Tecan i-control` == 'Start Time:')
  date <- as.character(test[date_row[1],2])
  date_parsed <- lubridate::parse_date_time(date, orders = 'Ymd HMS')

  # get luminescence data
  data_lumi_row <- which(test$`Application: Tecan i-control` == '<>')
  data_lumi_row_end <- which(is.na(test$`Application: Tecan i-control`[data_lumi_row:(data_lumi_row+12)]))
  data_lumi <- test[data_lumi_row:(data_lumi_row+data_lumi_row_end[1]-2),]
  colnames(data_lumi) <- data_lumi[1,]
  data_lumi <- data_lumi[-1,]

  # pivot to tidy format
  data_lumi %>% dplyr::mutate(dplyr::across(!`<>`, as.numeric)) %>%
    tidyr::pivot_longer(cols = !`<>`, names_to = 'col', values_to = 'luminescence') %>%
    dplyr::mutate(Well = paste0(`<>`, col)) %>%
    dplyr::select(Well, luminescence) -> data_lumi

  # get od data
  data_od_row <- which(test$`Application: Tecan i-control` == 'Well')
  data_od_row_end <- which(test$`Application: Tecan i-control` == 'End Time:')[2] - 5

  data_od <- test[data_od_row:data_od_row_end,1:8]
  colnames(data_od) <- data_od[1,]
  data_od <- data_od[-1,]

  # combine datasets
  data_od %>% dplyr::left_join(data_lumi) %>%
    dplyr::select(Well, Mean, StDev, luminescence) -> data_final

  data_final$date <- date_parsed

  return(data_final)

}


#' Read a TECAN plate map and format data into long format
#'
#' This function reads a TECAN plate map from an Excel file and formats the data into long format.
#' If the plate map contains a column named "plate", the function assumes that the data contains
#' multiple plates and formats the data accordingly. If the plate map does not contain a column named
#' "plate", the function assumes that the data contains a single plate and formats the data accordingly.
#'
#' @param path The file path of the Excel file containing the TECAN plate map
#' @return A data frame containing the formatted TECAN plate map data
#' @export
#' @import tidyr
#' @import dplyr
#' @import readxl
#' @examples
#' read_tecan_plate_map("path/to/plate_map.xlsx")
read_tecan_plate_map <- function(path){

  plate_map <- readxl::read_excel(path, na = 'NA')

  if('plate' %in% colnames(plate_map)){
    plate_map %>% tidyr::pivot_longer(cols = !c(id, plate), names_to = 'col', values_to = 'strain') %>%
      dplyr::mutate(Well = paste0(id, col)) %>%
      dplyr::select(Well, rep_day = plate, strain) -> plate_data

  } else {
    plate_map %>% tidyr::pivot_longer(cols = !c(id), names_to = 'col', values_to = 'strain') %>%
      dplyr::mutate(Well = paste0(id, col)) %>%
      tidyr::drop_na() %>%
      dplyr::group_by(strain) %>%
      dplyr::mutate(rep = 1:dplyr::n()) %>%
      dplyr::select(Well, strain, rep) -> plate_data
  }
  return(plate_data)
}

#' Read Tecan file and plate map
#'
#' This function reads a Tecan file with multiple sheets and a Tecan plate map,
#' and returns a tidy data frame with the following columns:
#'
#' - \code{date}: Date and time of the measurement
#' - \code{sheet}: Name of the sheet in the Tecan file
#' - \code{rep_day}: Replicate number of the plate on the measurement day
#' - \code{strain}: Strain name of the microorganism
#' - \code{Well}: Well identifier in the plate map
#' - \code{luminescence}: Luminescence value
#' - \code{Mean}: Mean OD600 value of the well
#' - \code{StDev}: Standard deviation of the OD600 value of the well
#' - \code{lumi_od}: Luminescence/OD600 ratio of the well
#'
#' @param path Path to the Tecan file
#' @param path_map Path to the Tecan plate map
#' @return Tidy data frame with the results of the Tecan measurement
#' @import readxl
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @export
read_tecan_file <- function(path, path_map){

  test_sheets <- readxl::excel_sheets(path)
  test_sheets <- test_sheets[grepl('Sheet', test_sheets)]

  list_wyn <- list()

  for(i in 1:length(test_sheets)){
    list_wyn[[i]] <- read_tecan_sheet(path = path, sheet = test_sheets[i])
    list_wyn[[i]]$sheet <- test_sheets[i]

  }

  data_final <- do.call(rbind, list_wyn)

  # count plates per day
  data_final %>% dplyr::arrange(date) %>%
    dplyr::mutate(day = lubridate::floor_date(date, unit = 'days')) %>%
    dplyr::distinct(date, day, sheet) %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(rep_day = 1:dplyr::n()) -> date_rep_data

  # calculate luminescence OD ratio, add plate replicate data
  data_final %>% dplyr::arrange(date) %>%
    dplyr::left_join(date_rep_data) %>%
    dplyr::mutate(Mean = as.numeric(Mean),
           lumi_od = luminescence/`Mean`) -> data_final

  # read tecan plate map
  tecan_map <- read_tecan_plate_map(path_map)

  # combine tecan results with plate map
  data_final %>% dplyr::left_join(tecan_map, by = c("Well", "rep_day")) -> data_final

  return(data_final)
}

#' Read data from a Tecan plate reader excel sheet (Mycobacterium experiment)
#'
#' This function reads data from a Tecan plate reader excel sheet and returns a data frame with OD and, if available, fluorescence values.
#'
#' @param path Path to the excel sheet.
#' @param sheet Name of the sheet to read from.
#'
#' @return A data frame with columns for well, time, OD, and fluorescence (if available) values.
#'
#' @examples
#' \dontrun{
#' read_tecan_sheet_Kornel("path/to/excel", "Sheet1")
#' }
#'
#' @import readxl
#' @import tidyverse
#' @import lubridate
#' @export
read_tecan_sheet_Kornel <- function(path, sheet){

  # read excel file
  test <- readxl::read_excel(path, sheet = sheet, progress = FALSE)

  # get date of measurement
  date_row <- which(test$`Application: Tecan i-control` == 'Start Time:')
  date <- as.character(test[date_row[1],2])

  if(is.na(lubridate::parse_date_time(date, orders = 'Ymd HMS'))){
    date_parsed <- lubridate::parse_date_time(date, orders = 'dmY HMS')
  } else {
    date_parsed <- lubridate::parse_date_time(date, orders = 'Ymd HMS')
  }

  actions_row <-  which(test$`Application: Tecan i-control` == 'List of actions in this measurement script:')
  actions_row_end <- which(is.na(test$`Application: Tecan i-control`[actions_row:(actions_row+20)]))[1] + actions_row -2
  actions <- test$`Application: Tecan i-control`[actions_row:actions_row_end]

  # get od data
  data_od_row <- which(test$`Application: Tecan i-control` == 'Cycle Nr.')[1]
  data_od_row_end <- which(is.na(test$`Application: Tecan i-control`[data_od_row:(data_od_row+100)]))[1] + data_od_row - 2

  data_od <- test[data_od_row:data_od_row_end,]
  colnames(data_od) <- data_od[2,]
  data_od <- data_od[-(1:3),]

  od_rows <- data_od[1,] %>% as.numeric()
  if(sum(is.na(od_rows))>1){
    data_od <- data_od[,1:which(is.na(od_rows))[2]]
  }

  data_od %>%
    dplyr::mutate(dplyr::across(!`Time [s]`, as.numeric)) %>%
    tidyr::pivot_longer(cols = !`Time [s]`, names_to = 'time_od', values_to = 'OD') %>%
    dplyr::mutate(time_od = as.numeric(time_od)) %>%
    dplyr::rename(Well = `Time [s]`) -> data_od

  # get fluorescent data

  if('Fluorescence' %in% actions){

    data_fluo_row <- which(test$`Application: Tecan i-control` == 'Cycle Nr.')[2]
    data_fluo_row_end <- which(is.na(test$`Application: Tecan i-control`[data_fluo_row:(data_fluo_row+100)]))[1] + data_fluo_row - 2

    data_fluo <- test[data_fluo_row:data_fluo_row_end,]
    colnames(data_fluo) <- data_fluo[2,]
    data_fluo <- data_fluo[-(1:3),]

    data_fluo %>% dplyr::mutate(dplyr::across(!`Time [s]`, as.numeric)) %>%
      tidyr::pivot_longer(cols = !`Time [s]`, names_to = 'time_fluo', values_to = 'fluo') %>%
      dplyr::mutate(time_fluo = as.numeric(time_fluo)) %>%
      dplyr::rename(Well = `Time [s]`) -> data_fluo

  }
  # combine datasets

  if('Fluorescence' %in% actions){
    data_od %>% dplyr::bind_cols(data_fluo[,2:3]) -> data_final
  } else {
    data_od -> data_final
  }
  data_final$date <- date_parsed

  return(data_final)

}

#' Read data from a Tecan plate reader excel sheet (Streptomyces experiment)
#'
#' This function reads data from a Tecan plate reader excel sheet and returns a data frame with OD and luminescence values.
#'
#' @param path Path to the excel sheet.
#' @param sheet Name of the sheet to read from.
#'
#' @return A data frame with columns for well, time, OD, and luminescence values.
#'
#' @examples
#' \dontrun{
#' read_tecan_sheet_Martyna("path/to/excel", "Sheet1")
#' }
#'
#' @import readxl
#' @import tidyverse
#' @import lubridate
#' @export
read_tecan_sheet_Martyna <- function(path, sheet){

  # read excel file
  test <- readxl::read_excel(path, sheet = sheet, progress = FALSE)

  # get date of measurement
  date_row <- which(test$`Application: Tecan i-control` == 'Start Time:')
  date <- as.character(test[date_row[1],2])

  if(is.na(lubridate::parse_date_time(date, orders = 'Ymd HMS'))){
    date_parsed <- lubridate::parse_date_time(date, orders = 'dmY HMS')
  } else {
    date_parsed <- lubridate::parse_date_time(date, orders = 'Ymd HMS')
  }

  # get absorbance rows
  od_row <- which(test$`Application: Tecan i-control` == 'Cycles / Well')+1

  list_od <- list()

  for(i in 1:length(od_row)){

    od_table_small <- test[od_row[i]+3,]
    colnames(od_table_small) <- test[od_row[i]+1,]
    od_table_small %>% dplyr::select(!`NA`) %>%
      dplyr::select(!`Time [s]`) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'time', values_to = 'OD') %>%
      dplyr::mutate(Well = test$`Application: Tecan i-control`[od_row[i]]) -> list_od[[i]]
  }

  od_table <- do.call(rbind, list_od)

  # get luminescence data

  lumi_row <- which(test$`Application: Tecan i-control` == 'Label2')
  lumi_row_end <- which(test$`Application: Tecan i-control` == 'End Time:')

  lumi_table <- test[(lumi_row+4):(lumi_row_end-4),]
  colnames(lumi_table) <- test[lumi_row+2,]

  lumi_table %>% dplyr::select(!`NA`) %>%
    dplyr::mutate(dplyr::across(!`Time [s]`, as.numeric)) %>%
    tidyr::pivot_longer(cols = !`Time [s]`, names_to = 'time_lumi', values_to = 'lumi') %>%
    dplyr::mutate(time_lumi = as.numeric(time_lumi)) %>%
    dplyr::rename(Well = `Time [s]`) -> lumi_table

  # combine datasets
  od_table %>% dplyr::bind_cols(lumi_table[,2:3]) %>%
    dplyr::mutate(time = as.numeric(time)) -> tecan_results

  return(tecan_results)
}

