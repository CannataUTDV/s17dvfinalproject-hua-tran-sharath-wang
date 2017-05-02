require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)
online0 = FALSE

if(online0) {
  globals = query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="select Order_Date, Sales
    from SuperStoreOrders
    order by 1"
  ) 
} else {
  file_path = "www/Death.csv"
  df <- readr::read_csv(file_path) 
  globals <- df %>% dplyr::select(year, amt_death) %>% dplyr::distinct()
}
globals$Order_Date <- lubridate::year(globals$year)

