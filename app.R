library(shiny)
library(leaflet)
library(googlesheets4)

wainwrights=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1asnafeRHRpv9Ddf3HVg6q9Lr0cn3hevj4c2aq2hqcZc/edit?gid=0#gid=0")

ui <- fluidPage(
  leafletOutput("lakeDistrictMap", height = "800px")
)

server <- function(input, output, session) {
  output$lakeDistrictMap <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://os.openstreetmap.org/tiles/osgb/{z}/{x}/{y}.png",
        options = tileOptions(minZoom = 7, maxZoom = 18),
        attribution = paste0('Contains OS data &copy; Crown copyright and database rights ', format(Sys.Date(), "%Y"), '. Powered by OpenStreetMap contributors.')
      ) %>%
      setView(lng = -3.05, lat = 54.45, zoom = 10)
  })
}



shinyApp(ui, server)
