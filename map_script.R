# ========================================================
# Auslandstagegeld Interactive Map Visualization (ARVVwV 2025)
# Author: [Your Name]
# Description: Visualizing official German government per diem 
# and accommodation allowances for international travel.
# ========================================================

# ---------------------------
# 1. Install and Load Packages
# ---------------------------
install.packages(c(
  "leaflet", "dplyr", "readr",
  "rnaturalearth", "rnaturalearthdata",
  "sf", "htmltools", "htmlwidgets", "stringr"
))

library(leaflet)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(htmltools)
library(stringr)
library(htmlwidgets)

# ---------------------------
# 2. Load and Prepare Data
# ---------------------------
clean_data <- read.csv("clean-data.csv")

country_data <- clean_data %>%
  filter(ort == "sonst") %>%
  mutate(
    iso_a3       = str_trim(as.character(iso_a3)),
    country      = tolower(namen),
    display_name = str_to_title(namen)
  )

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso_a3 = str_trim(as.character(iso_a3)))

# ---------------------------
# 3. Data Diagnostics
# ---------------------------

# Check for missing ISO codes
# Using ISO - Code Alpha-3
missing_iso <- country_data %>% filter(is.na(iso_a3) | iso_a3 == "")
if (nrow(missing_iso) > 0) {
  print("Missing ISO Codes Detected:")
  print(missing_iso %>% select(namen, iso_a3))
} else {
  print("All ISO codes are present in country_data.")
}

# Check for unmatched ISO codes between data and shapefile
unmatched_iso <- anti_join(country_data, world, by = "iso_a3")
if (nrow(unmatched_iso) > 0) {
  print("Unmatched ISO Codes Found:")
  print(unmatched_iso %>% select(namen, iso_a3))
} else {
  print("All ISO codes successfully match with shapefile.")
}

# ---------------------------
# 4. Add Missing Countries Manually (France, Norway, Kosovo, French Guiana)
# This step is crucial since the shape file is NOT recognizing France, Norway or Kosovo.
# ---------------------------
template <- world[1, ]
forced_countries <- template[rep(1, 4), ]

forced_countries$iso_a3    <- c("FRA", "NOR", "XKX", "GUF")
forced_countries$name      <- c("frankreich", "norwegen", "kosovo", "französisch_guayana")
forced_countries$name_long <- c("Frankreich", "Norwegen", "Kosovo", "Französisch Guayana")

coords <- list(
  c(2.2137, 46.2276),   # > France
  c(8.4689, 60.4720),   # > Norway
  c(20.9020, 42.6026),  # > Kosovo
  c(-53.1258, 3.9339)   # > French Guiana
)

forced_countries$geometry <- st_sfc(
  st_point(coords[[1]]),
  st_point(coords[[2]]),
  st_point(coords[[3]]),
  st_point(coords[[4]]),
  crs = st_crs(world)
)

# Append manually created countries
world <- rbind(world, forced_countries)




# ---------------------------
# 5. Merge and Prepare Map Data
# ---------------------------
map_data <- left_join(world, country_data, by = "iso_a3")

poly_data  <- map_data %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
point_data <- map_data %>% filter(st_geometry_type(.) == "POINT")

# Define color bins for the choropleth map
bins <- c(0, 20, 30, 40, 50, 60, 70, 100)
pal_bin <- colorBin(
  palette = "YlOrRd",
  domain  = poly_data$auslandstagegeld,
  bins    = bins,
  pretty  = FALSE
)




# ---------------------------
# 6. Build and Save Interactive Map
# ---------------------------
map_object <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data        = poly_data,
    fillColor   = ~pal_bin(auslandstagegeld),
    weight      = 1,
    color       = "white",
    fillOpacity = 0.7,
    label       = lapply(
      paste0(
        "<strong>", poly_data$display_name, "</strong><br/>",
        "Tagegeld: €", poly_data$auslandstagegeld, "<br/>",
        "Übernachtungsgeld: €", poly_data$auslandsuebernachtungsgeld
      ),
      htmltools::HTML
    ),
    highlightOptions = highlightOptions(color = "black", weight = 2)
  ) %>%
  addCircleMarkers(
    data      = point_data,
    radius    = 6,
    fillColor = "blue",
    fillOpacity = 0.8,
    stroke    = FALSE,
    label     = lapply(
      paste0(
        "<strong>", point_data$display_name, "</strong><br/>",
        "Tagegeld: €", point_data$auslandstagegeld, "<br/>",
        "Übernachtungsgeld: €", point_data$auslandsuebernachtungsgeld
      ),
      htmltools::HTML
    )
  ) %>%
  addLegend(
    pal       = pal_bin,
    values    = poly_data$auslandstagegeld,
    title     = "Auslandstagegeld (€)",
    opacity   = 0.7,
    labFormat = labelFormat(prefix = "€")
  )




# Final step::::: Export as self-contained HTML for GitHub pages hosting
saveWidget(map_object, "index.html", selfcontained = TRUE)