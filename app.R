
library(shiny)
library(leaflet)
library(googlesheets4)
library(dplyr)
library(flextable)

options(gargle_oauth_cache = ".secrets",
        gargle_oauth_email = TRUE)

gs4_auth(
  cache = ".secrets",
  email = "sebastiangbate@gmail.com"
)

wainwrights = googlesheets4::read_sheet(
  "https://docs.google.com/sheets/d/1asnafeRHRpv9Ddf3HVg6q9Lr0cn3hevj4c2aq2hqcZc/edit?gid=0#gid=0"
) %>%
  mutate(completion=3-(as.numeric(is.na(L))+as.numeric(is.na(B))+as.numeric(is.na(J))),
         completion_group = case_when(
           completion == 0 ~ "0",
           completion %in% c(1, 2) ~ "1-2",
           completion == 3 ~ "3",
           TRUE ~ NA_character_
         ),
         comments=paste0("<strong>",Name, "</strong><br><strong>L:</strong>", ifelse(is.na(L), "", L),"<br><strong>B:</strong>", ifelse(is.na(B), "", B), "<br><strong>J:</strong>", ifelse(is.na(J), "", J))
  )

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Map", 
             leafletOutput("lakeDistrictMap", height = "800px")),
    tabPanel("Stats",
             uiOutput("table",inline = TRUE, style = "margin:0px; padding:0px"))
  )
)

server <- function(input, output, session) {
  pal <- colorFactor(
    palette = c("blue", "yellow", "green"),
    levels = c("0", "1-2", "3")
  )
  
  output$lakeDistrictMap <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        options = tileOptions(minZoom = 7, maxZoom = 18),
        attribution = 'Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap contributors</a>'
      ) %>%
      setView(lng = -3.05, lat = 54.45, zoom = 10) %>%
      addCircleMarkers(
        data = wainwrights,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~comments,
        color = ~pal(completion_group),
        fillOpacity = 0.8,
        radius = 8
      )
  })
  
  output$table=renderUI({
    wainwrights %>% dplyr::select(Name, height, L,B,J) %>% 
      pivot_longer(cols=c(L,B,J), names_to = "person", values_to = "completion") %>% 
      mutate(completion=!is.na(completion),
             person=case_when(person=="L"~"Lawrence",
                              person=="B"~"Bob",
                              person=="J"~"John")) %>% 
      filter(completion) %>% 
      summarise(total=n(),
                total_height=sum(height), .by="person") %>% 
      mutate(test=1) %>% 
      merge(wainwrights %>% 
              mutate(test=1) %>% 
              summarise(total=n(),
                        total_height=sum(height, na.rm = T), .by = "test"), by="test", all=T  ) %>% 
      dplyr::select(-test) %>% 
      mutate(total_pc=paste0(sprintf("%.1f", 100*total.x/total.y)),
             height_pc=paste0(sprintf("%.1f", 100*total_height.x/total_height.y))) %>% 
      dplyr::select(person, total.x, total_pc, total_height.x, height_pc) %>% 
      flextable() %>% 
      set_header_labels(person="", total.x="Summits", total_pc="%",total_height.x="Total summit height",height_pc="%"  ) %>% 
      autofit() %>%
      font(fontname = "Arial", part="all") %>% 
      htmltools_value()
    
  })
}

shinyApp(ui, server)
