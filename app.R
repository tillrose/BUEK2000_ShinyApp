
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
library(raster)

source("library.R")

BUEK2000_Kartenblaetter <- read_rds("data/Bodenübersichtskarte_1_20000/Kartenblaetter_Box")
BUEK2000_path <- "data/Bodenübersichtskarte_1_20000/Kartenblaetter_RDS/"
BUEK2000_code <- read_rds("data/Bodenübersichtskarte_1_20000/Bodenübersichtskarte_1_200000_code")
                          
###############
### Options ###

# shinyOptions(cache = cachem::cache_disk(dir = "./myapp-cache", max_age = Inf))
             
###############
             
# App template from the shinyuieditor
ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar area5",
    "dists   area5"
  ),
  row_sizes = c(
    "70px",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    item_alignment = "top",
    title = "Koordinaten (WGS84)",
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
  grid_card(
    area = "area5",
    item_gap = "12px",
    tabsetPanel(
      tabPanel(
        title = "Boden",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            plotOutput(
                  outputId = "plotBoden",
                  width = "100%",
                  height = "400px"
                )
          ),
          mainPanel(
            textOutput(
              outputId = "text"
            ),
            tableOutput(
              outputId = "table"
            )
          )
      )),
      tabPanel(title = "Karte",
               plotOutput(
                 outputId = "plotRäumlich",
                 width = "100%",
                 height = "400px"
               ))
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  point_coordinates <- reactive(getPointCoordinates(geoLaenge = input$NumericLängengrad, geoBreite = input$NumericBreitengrad))
  soil_map <- reactive(getSoilMap(point_coordinates = point_coordinates(), BUEK2000_Kartenblaetter = BUEK2000_Kartenblaetter, BUEK2000_path = BUEK2000_path))
  soil_polygon <- reactive(getSoilPolygon(point_coordinates = point_coordinates(), Kartenblatt_spatial = soil_map(), BUEK2000_code = BUEK2000_code))
  textures <- reactive(getSoilTexture(soil_polygon = soil_polygon()))
  
  output$text <- renderText({
    
    textures() %>%
      pull(LE_TXT) %>%
      unique() %>%
      unlist()
    
  })
  
  output$table <- renderTable({
    
    textures() %>% 
      dplyr::select("Obere Grenze" = OTIEF, "Untere Grenze" = UTIEF, "Bodenart" = BOART)
    
    })

  output$plotBoden <- renderPlot({
    
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
    
  })

  
  output$plotRäumlich <- renderPlot({
    
    boundary_box <- data.frame("geoLaenge" = c(input$NumericLängengrad - 0.125, input$NumericLängengrad - 0.125, input$NumericLängengrad + 0.125, input$NumericLängengrad + 0.125), "geoBreite" = c(input$NumericBreitengrad - 0.025, input$NumericBreitengrad + 0.025, input$NumericBreitengrad - 0.025, input$NumericBreitengrad + 0.025)) %>%
      st_as_sf(coords = c("geoLaenge", "geoBreite")) %>%
      st_set_crs(value = "+proj=longlat +datum=WGS84") %>%
      st_transform(crs = st_crs("EPSG:25832")) %>%
      st_bbox()

    Soil_Reduced <- soil_map() %>%
      st_crop(boundary_box)

    ggplot(Soil_Reduced) +
      theme_map() +
      theme(plot.background = element_rect(colour = "grey")) +
      annotation_map_tile(zoom = 12) +
      geom_sf(colour = "red", size = 1, fill = "brown", alpha = 0.15) +
      geom_sf(data = point_coordinates(), colour = "white", fill = "red", shape = 21, size = 5) +
      scale_fill_viridis_d(option = "turbo") +
      coord_sf() +
      guides(fill = "none")


  })
  
}

shinyApp(ui, server)
