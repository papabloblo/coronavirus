#' 
#' ACTUALIZACIÓN AUTOMÁTICA DE LOS INFORMES DIARIOS
#' 
#' Descarga de los .csv actualizados diariamente en el repositorio
#' github.com/CSSEGISandData/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports
#' 
#' Se descarga un archivo .csv para cada día en data/raw/yyyy-mm-dd.csv
#' Solo se descargan aquellos .csv que no se hayan descargado previamente.
#' Se descargan hasta el día anterior al actual `Sys.Date() - 1`.
#' 

# DESCARGA ----------------------------------------------------------------

date_period <- seq(from = as.Date("2020-01-22"),
                   to = Sys.Date() - 1,
                   by = "day")

date_period <- as.character(date_period)

current_files <- list.files("data/raw")
current_files <- stringr::str_remove(current_files, ".csv") 

# Solo archivos que no se hayan descargado previamente
new_files <- setdiff(date_period, current_files)

download_daily_reports <- function(d, url_base){
  date_file <- as.Date(d)
  download.file(
    url = paste0(url_base,
                 as.character(date_file, format = "%m-%d-%Y"),
                 ".csv"),
    destfile = paste0("data/raw/",
                      as.character(date_file, format = "%Y-%m-%d"),
                      ".csv"
                      )
    )
}


url_base <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

for (d in new_files) {
  download_daily_reports(d, url_base = url_base)
}


