library(shiny)
library(rgdal)
library(maptools)
library(usmap)
library(readxl)
library(ggplot2)
library(tidyverse)
library(plotly)
library(formattable)
library(dichromat)


# Data processing ---------------------------------------------------------

abb <- read_xlsx("Top_energy_after_manual_changes.xlsx")
raw <- read_xlsx("Top_energy_after_manual_changes.xlsx", sheet = 2) %>%
  mutate(bdg = 100-bdg*100,
         shm = shm*100,
         shl = shl*100,
         shp = shp*100,
         atc = as.double(atc)*100,
         twd = twd*100,
         sfs = as.double(sfs)*100,
         aoz = aoz*100,
         apm = apm*100)
counties <- read_xls("counties_geo.xls")

size_full_names <- abb$Field[4:15]
size_names <- str_remove(size_full_names, "\\([^()]*\\)") %>% str_remove("[ \\t]+$")
size_var <- colnames(raw)[4:15]
col_full_names <- abb$Field[16:26]
col_names <- str_remove(col_full_names, "\\([^()]*\\)") %>% str_remove("[ \\t]+$")
col_var <- colnames(raw)[16:26]

count_geo <- counties %>% 
  select(GEOID, INTPTLAT, INTPTLONG) %>%
  rename(fips = GEOID, lat = INTPTLAT, lon = INTPTLONG)

cstate <- raw %>%
  mutate(county = str_remove(cou, "\\,[^,]*$")) %>%
  mutate(abbr = str_remove(cou, ".*,")) %>%
  rename(fips = fip)

cs_geo <- left_join(cstate, count_geo, by = "fips") %>%
  mutate(lat = replace(lat, fips == 46113, 43.3),        # Oglala Lakota, SD
         lon = replace(lon, fips == 46113, -102.55),     # recent name change
         lat = replace(lat, fips == 12025, 25.36),       # Miami-Dade, FL
         lon = replace(lon, fips == 12025, -80.29)) %>%  # recent fips change?
  drop_na(lon) %>%
  drop_na(lat)

cs_geo <- left_join(cs_geo,
                    usmap_transform(cs_geo %>% select(lon, lat)), by = c("lon", "lat"))

for (i in 1:12){
  varname <- paste0(size_var[i],"norm")
  cs_geo <- cs_geo %>% mutate( !!varname := get(size_var[i])/mean(get(size_var[i]), na.rm = T))
}


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".selectize-input {height: 10px; width: 200px; font-size: 12px;}
     label{font-size: 12px;}"))),
            fluidRow(
                column(3, h4()),
                column(3, align="center",
                      fluidRow(selectInput("size",
                                  "Circle size",
                                  size_names))
                      ),
                column(3, align="center",
                       selectInput("color",
                                   "Circle color",
                                   col_names)
                       # ,plotOutput("legend_col", height = 20)
                ),
                column(3, h4())
            )
            ,div(plotlyOutput("map"), align = "center")
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
      
      jiggle_spread <- function(x){
        x + 0.0001*(x-mean(x))
      }
      colfunc <- colorRampPalette(c("#52bd42", "#fc7e00", "#fc1d19"))
      
    output$map <- renderPlotly({
      sz <- size_var[which(size_names == input$size)]
      szname <- size_names[which(size_names == input$size)]
      sznorm <- paste0(size_var[which(size_names == input$size)],"norm")
      col <- col_var[which(col_names == input$color)]
      colname <- col_names[which(col_names == input$color)]
      
      gg <- plot_usmap(
        color = "#c1def5", 
        region = "counties",
        size = 0.1,
        text = "") + 
        geom_point(data = cs_geo %>% filter(get(sz) > 0 & !is.na(get(sz)) & !is.na(get(col))),
                   mapping = aes(x = lon.1, y = lat.1, 
                                 size = 2*round(get(sz)),
                                 color = cut(rank(get(col),ties.method = "first"),
                                             quantile(rank(get(col),ties.method = "first"), 
                                                      probs = seq(0, 1, by = .2)) %>% jiggle_spread()),
                                 text = paste0( 
                                   '</br> ', cou,
                                   '</br> ', szname, ": ", comma(get(sz), digit = 1),
                                   '</br> ', colname, ": ", comma(get(col), digit = 1))
                   ), 
                   alpha = 0.35) + 
        scale_size_continuous(name = "", range = c(1,17)) +
        scale_colour_manual(name = "", 
                            values = colfunc(5)) + 
        theme(legend.position = "top")
      
      p <- ggplotly(gg, tooltip = "text", width = 800, height = 550) %>%
        layout(modebar = list(orientation = "v"),
               legend = list(orientation = "h"   # show entries horizontally
                             ,xanchor = "center"  # use center of legend as anchor
                             ,x = 0.5             # put legend in center of x-axis
                             ,y = 1.073
                             ,trace = "constant"
                             ,font = list(size = 13)
               ),
               dragmode = 'pan'
               ) %>%
        config(scrollZoom = TRUE,
               displaylogo = FALSE,
               modeBarButtonsToRemove = c("lasso2d", "select2d", "zoom2d", "autoScale2d",
                                          "hoverClosestCartesian", "hoverCompareCartesian"))
      
      p$x$data[[3]]$name <- "0-20%"
      p$x$data[[4]]$name <- "20-40%"
      p$x$data[[5]]$name <- "40-60%"
      p$x$data[[6]]$name <- "60-80%"
      p$x$data[[7]]$name <- "80-100%"
      p$x$data[[1]]$hoverinfo <- "skip"
      p$x$data[[2]]$hoverinfo <- "skip"
      
      p
    })
}

# Run App -----------------------------------------------------------------



shinyApp(ui = ui, server = server)
