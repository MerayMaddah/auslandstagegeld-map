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
# This step is crucial since the shape file is NOT recognizing France, Norway or Kosovo. I hard coded French Guiana to the csv file since it wasn't in the PDF document.
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




# ================================
# Auslandstagegeld Visualization/Plot Generation:
## Scatter and Faceted Plots for Countries With +1 Cities/Städte
# ================================

# Load Required Libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(scales)
library(ggplot2)

# 1. Load the csv.file:::
df <- read_csv("clean-data.csv") %>%
  mutate(
    display     = str_to_title(str_replace_all(namen, "_", " ")),
    city_label  = if_else(type == "Stadt", str_to_title(str_replace_all(ort, "_", " ")), NA_character_),
    type        = factor(type, levels = c("Land", "Stadt"))
  )

# Identify countries that have at least 1 city:
has_cities <- df %>%
  filter(type == "Stadt") %>%
  pull(display) %>%
  unique()
has_cities

# ========================
# 2. Plot 1: Overall Scatter Plot
# ========================
p1 <- ggplot(df, aes(
  x = auslandstagegeld,
  y = auslandsuebernachtungsgeld,
  colour = type,
  shape  = type
)) +
  geom_point(size = 3, alpha = 0.8) +
  # Add the non-overlapping city labels
  geom_text_repel(
    data = df %>% filter(type == "Stadt"),
    aes(label = str_to_title(str_replace_all(ort, "_", " "))), 
    size = 3.5, 
    color = "dodgerblue4",
    max.overlaps = 55,
    box.padding = 0.5,
    point.padding = 0.5
  ) +
  scale_colour_manual(values = c(Land = "brown4", Stadt = "dodgerblue4")) +
  scale_shape_manual(values = c(Land = 19, Stadt = 17)) +
  labs(
    x = "Tagesgeld (€)",
    y = "Übernachtungsgeld (€)",
    colour = "",
    shape  = "",
    title = "Tages- vs. Übernachtungsgeld\nLänder (Braun/Kreis) vs. Städte (Blau/Dreieck)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p1

## Warning message: Identify which countries have the NAs
## Belize, French Guiana, Somalia and Suriname do not have values in the official regulation
na_row <- df %>%
  filter(is.na(auslandstagegeld) | is.na(auslandstagegeld))
na_row


# ========================
# 3. Plot 2: Faceted Scatter Plot with Country-Specific Colors
# ========================

# Standardize facet axis ranges
x_range <- range(df$auslandstagegeld, na.rm = TRUE)
y_range <- range(df$auslandsuebernachtungsgeld, na.rm = TRUE)

# Assign a distinct color to each country with cities
country_colors <- hue_pal()(length(has_cities))
names(country_colors) <- has_cities

p2 <- ggplot(
  df %>% filter(display %in% has_cities),
  aes(
    x = auslandstagegeld,
    y = auslandsuebernachtungsgeld,
    colour = display,
    shape  = type
  )
) +
  geom_point(size = 2) +
  
  # Add city labels for cities/Städte only
  geom_text_repel(
    data = . %>% filter(type == "Stadt"),
    aes(label = str_to_title(str_replace_all(ort, "_", " "))),  
    size = 2.5,
    box.padding = 0.15,
    point.padding = 0.15,
    segment.size = 0.2,
    segment.alpha = 0.5,
    max.overlaps = 50,
    min.segment.length = 0,
    force = 0.3,
    force_pull = 0.5
  ) +
  
  scale_colour_manual(values = country_colors) +
  scale_shape_manual(values = c(Land = 19, Stadt = 17)) +
  
  # Standardized axis limits across facets
  facet_wrap(~ display, scales = "fixed", ncol = 4) +
  scale_x_continuous(limits = x_range) +
  scale_y_continuous(limits = y_range) +
  
  labs(
    x = "Tagesgeld (€)",
    y = "Übernachtungsgeld (€)",
    title = "Tages- vs. Übernachtungsgeld pro Land und seinen Städten"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text      = element_text(face = "bold")
  )

p2
