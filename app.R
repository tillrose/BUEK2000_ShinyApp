
### Startup ###
library(shiny)
library(gridlayout)
library(ggplot2)
library(readr)
library(dplyr)
library(forcats)
library(sf)
library(shadowtext)
library(ggspatial)
library(ggthemes)

source("library.R")

Soil_Germany <- read_rds("data/Bodenübersichtskarte_1_200000_only_agriculture")
complete_code <- read_rds("data/Bodenübersichtskarte_1_200000_code")

###############

# App template from the shinyuieditor
ui <- grid_page(
  layout = c(
    "header  header header",
    "sidebar dists  area3 ",
    ".       area4  area4 "
  ),
  row_sizes = c(
    "70px",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    item_alignment = "top",
    title = "Koordinaten",
    item_gap = "12px",
    numericInput(
      inputId = "NumericBreitengrad",
      label = "Breitengrad",
      value = 54.3158
    ),
    numericInput(
      inputId = "NumericLängengrad",
      label = "Längengrad",
      value = 9.9878
    )
  ),
  grid_card_text(
    area = "header",
    content = "Bodenübersichtskarte 2000",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card_plot(area = "dists"),
  grid_card(
    area = "area3",
    item_gap = "12px",
    tableOutput(outputId = "table")
  ),
  grid_card(
    area = "area4",
    plotOutput(
      outputId = "spatial",
      width = "100%",
      height = "400px"
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  textures <- reactive(getSoilTexture(geoLaenge = input$NumericLängengrad, geoBreite = input$NumericBreitengrad, BUEK2000_shape = Soil_Germany, BUEK2000_code = complete_code))
  
  output$table <- renderTable({
    
    textures() %>% 
      dplyr::select("Obere Grenze" = OTIEF, "Untere Grenze" = UTIEF, "Bodenart" = BOART)
    
    }) %>%
    bindCache(input$NumericLängengrad, input$NumericBreitengrad)

  output$dists <- renderPlot({
    
    textures <- textures() %>% 
      mutate(BOART = as.factor(BOART),
             BOART = fct_expand(BOART, "Ss", "Su2", "Sl2", "Sl3", "St2", "Su3", "Su4", "Slu", "Sl4", "St3", "Ls2", "Ls3", "Ls4", "Lt2", "Lts", "Ts4", "Ts3", "Uu", "Us", "Ut2", "Ut3", "Uls", "Ut4", "Lu", "Lt3", "Tu3", "Tu4", "Ts2", "Tl", "Tu2", "Tt"),
             BOART = fct_relevel(BOART, "Ss", "Su2", "Sl2", "Sl3", "St2", "Su3", "Su4", "Slu", "Sl4", "St3", "Ls2", "Ls3", "Ls4", "Lt2", "Lts", "Ts4", "Ts3", "Uu", "Us", "Ut2", "Ut3", "Uls", "Ut4", "Lu", "Lt3", "Tu3", "Tu4", "Ts2", "Tl", "Tu2", "Tt"))
    
    ggplot(textures) + aes() +
      theme_bw(base_size = 18) +
      geom_rect(aes(xmin = 1 -0.35, xmax = 1 + 0.35, ymin = UTIEF, ymax = OTIEF, group = HOR_NR, fill = BOART), colour = "white") +
      geom_shadowtext(aes(x = 1, y = UTIEF + (OTIEF - UTIEF)/2, label = BOART), colour = "white", bg.colour = "black", size = 5.5) +
      scale_y_reverse(expand = expansion(add = c(0, 0))) +
      scale_x_continuous(breaks = NULL, expand = expansion(add = c(0, 0))) +
      scale_fill_viridis_d(direction = -1, option = "rocket", end = 0.9) +
      labs(y = "Tiefe", fill = "Bodenart", x = "") +
      guides(fill = "none") +
      NULL
    
  }) %>%
    bindCache(input$NumericLängengrad, input$NumericBreitengrad)
  
  
  output$spatial <- renderPlot({
    
    point_coordinates <- data.frame("geoLaenge" = input$NumericLängengrad, "geoBreite" = input$NumericBreitengrad) %>% 
      st_as_sf(coords = c("geoLaenge", "geoBreite")) %>% 
      st_set_crs(value = "+proj=longlat +datum=WGS84") %>% 
      st_transform(crs = st_crs(Soil_Germany))
    
    boundary_box <- data.frame("geoLaenge" = c(input$NumericLängengrad - 0.125, input$NumericLängengrad - 0.125, input$NumericLängengrad + 0.125, input$NumericLängengrad + 0.125), "geoBreite" = c(input$NumericBreitengrad - 0.025, input$NumericBreitengrad + 0.025, input$NumericBreitengrad - 0.025, input$NumericBreitengrad + 0.025)) %>% 
      st_as_sf(coords = c("geoLaenge", "geoBreite")) %>% 
      st_set_crs(value = "+proj=longlat +datum=WGS84") %>% 
      st_transform(crs = st_crs(Soil_Germany)) %>% 
      st_bbox()
    
    Soil_Reduced <- Soil_Germany %>% 
      st_crop(boundary_box)
    
    ggplot(Soil_Reduced) +
      theme_map() +
      annotation_map_tile(zoom = 13) +
      geom_sf(aes(fill = Symbol), colour = "black", alpha = 0.25) +
      geom_sf(data = point_coordinates, colour = "white", fill = "red", shape = 21, size = 5) +
      scale_fill_viridis_d(option = "turbo") +
      coord_sf() +
      guides(fill = "none")
    

  }) %>%
    bindCache(input$NumericLängengrad, input$NumericBreitengrad)
  
  
}

shinyApp(ui, server)
