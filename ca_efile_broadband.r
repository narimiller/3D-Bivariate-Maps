# 1. PACKAGES
# ------------------
install.packages("remotes")

# bivariate maps
remotes::install_github(
  "chris-prener/biscale"
)

install.packages("tigris")
install.packages("rgl")
install.packages("pacman")

pacman::p_load(
  geodata,
  tidyverse,
  sf,
  terra,
  tigris,
  biscale,
  elevatr,
  gridGraphics,
  rayshader
)

# Ensure images directory exists
if (!dir.exists("images")) dir.create("images")

# 2. LOAD DATA
# ------------------

# IRS data
irs <- read_csv("data/22incyallnoagi.csv") |>
  filter(STATE == "CA") |>
  mutate(
    FIPS = paste0(STATEFIPS, COUNTYFIPS)
  ) |>
  select(FIPS, total_returns = N1, efiled = ELF) |>
  mutate(efile_rate = efiled / total_returns * 100)

head(irs, 10)
summary(irs$efile_rate)

# fcc data
fcc <- read_csv("data/county_tiers_201406_202406.csv") |>
  filter(Year == 2022) |>  # filter for the year 2022
  mutate(
    # Assign tier value midpoints for Tier_3 (≥25 Mbps)
    broadband_per_1000 = case_when(
      Tier_3 == 0 ~ 0,
      Tier_3 == 1 ~ 100,
      Tier_3 == 2 ~ 300,
      Tier_3 == 3 ~ 500,
      Tier_3 == 4 ~ 700,
      Tier_3 == 5 ~ 900,
      TRUE ~ NA_real_ # missing numeric value
    ),
    broadband_pct = broadband_per_1000 / 10  # Convert to percent of households
  ) |>
  select(FIPS, broadband_pct)  # estimate % of households

head(fcc, 10)
summary(fcc$broadband_pct)

# merge
df <- left_join(irs, fcc, by = "FIPS") |> drop_na() |>
  select(FIPS, efile_rate, broadband_pct)

# 3. GET POLYGONS (WITH FIPS CODES)
# ------------------
# download all U.S. counties as sf object
counties <- tigris::counties(cb = TRUE, class = "sf") |> 
  filter(STATEFP == "06") |>   # California only
  mutate(FIPS = GEOID)

# Define target CRS once
target_crs <- "EPSG:5070"  # Albers Equal Area Conic projection
counties_proj <- st_transform(counties, target_crs)

# join with data
map_data <- left_join(counties_proj, df, by = "FIPS")

# 4. BREAKS, PALETTE, AND THEME
# ------------------
breaks <- bi_class( # info about bivariate scale and distribution
  map_data,
  x = efile_rate, y = broadband_pct,
  style = "fisher", dim = 3
)

# Define color palette
custom_pal2 <- c(
  "1-1" = "#a5e9cc", # low x, low y - lightest green
  "2-1" = "#70ddaf", # medium x, low y 
  "3-1" = "#26cf90", # high x, low y - most green
  "1-2" = "#7fb8dd", # low x, medium y - light blue-green
  "2-2" = "#56aebe", # medium x, medium y 
  "3-2" = "#1da39c", # high x, medium y 
  "1-3" = "#8773e4", # low x, high y - lightest purple
  "2-3" = "#5c6dc4", # medium x, high y 
  "3-3" = "#1f66a1"  # high x, high y - darkest blue-purple
)

# define custom theme for map
custom_theme <- function(){
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.background = element_rect(
        fill = "white", color = NA
      ),
      plot.title = element_text(
        size = 16, 
        color = "grey10", hjust = 0.5,
        face = "bold", vjust = -1 # move down towards map
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5, vjust = -1
      ),
      plot.caption = element_text(
        size = 9, color = "grey20", 
        hjust = 0.5, vjust = 1
      ),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      panel.grid = element_blank()
    )
}

# 4. 2D BIVARIATE MAP
# ------------------

# create bivariate map using ggplot2
map <- ggplot(breaks) +
  geom_sf(
    aes(
      fill = bi_class # column for fill aesthetic
    ), show.legend = FALSE
  ) +
  biscale::bi_scale_fill( # how to fill values from bivariate scale
    pal = custom_pal2, dim = 3,
    flip_axes = FALSE,
    rotate_pal = FALSE
  ) +
  labs( 
      title = "CALIFORNIA: E-Filing Rate and Broadband Access by County",
      subtitle = "Electronic tax filing rates and estimated broadband access (≥25 Mbps), 2022",
      caption = "Source: IRS and FCC data, 2022",
      x = "", y = ""
  ) +
  coord_sf(
    crs = target_crs
  ) +
  custom_theme()

# create legend
legend <- biscale::bi_legend(
  pal = custom_pal2, 
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 3, 
  xlab = "E-file Rate (%)", 
  ylab = "Broadband Access (% of households)",
  size = 8.5 # size of label text in legend
)

# inspect output, get x and y ranges of map to help with legend placement
map_built <- ggplot_build(map)
str(map_built$layout$panel_params[[1]])

x_range <- c(-2391586, -1611187)
y_range <- c(1181840, 2513229)

# Calculate total range
x_total <- diff(x_range)  
y_total <- diff(y_range)  

# Convert to 0-1 scale (like cowplot) (x=0.05, y=0.13, width=0.25, height=0.25)
xmin <- x_range[1] + (0.62 * x_total)   
xmax <- x_range[1] + (1.00 * x_total)   
ymin <- y_range[1] + (0.54 * y_total)   
ymax <- y_range[1] + (0.92 * y_total)   

# combine map with legend
full_map <- map + 
  annotation_custom(
    grob = ggplotGrob(legend),
    xmin = xmin, xmax = xmax,  
    ymin = ymin, ymax = ymax   
  )

print(full_map)

#save as PNG
ggsave(
  filename = "images/ca_efile_broadband_2d_2.png",
  width = 7, height = 10, dpi = 600,
  device = "png", bg = "white", full_map
)

# 5. CREATE TERRAIN LAYER
# ------------------

# Get California elevation data
ca_dem <- get_elev_raster(
  locations = counties_proj, # clip to California counties
  z = 8, clip = "locations"
) |> 
terra::rast() |> # convert to SpatRaster
terra::mask(counties_proj)

terra::crs(ca_dem)

# project DEM and convert to dataframe
dem_df <- ca_dem |>
  terra::project(target_crs) |>
  as.data.frame(xy = TRUE, na.rm = TRUE) # remove NAs

# rename third column
names(dem_df)[3] <- "elevation"

# create terrain layer map
dem_map <- ggplot(
  dem_df, aes(x = x, y = y, fill = elevation)
  ) +
  geom_raster() +
  scale_fill_gradientn(colors = "white") + # required for rayshader
  guides(fill = "none") +
  labs( # labels
    title = "CALIFORNIA: E-Filing Rate and Broadband Access by County",
    subtitle = "Bivariate map of electronic tax filing rates and estimated broadband access (≥25 Mbps), 2022",
    caption = "Source: IRS and FCC data, 2022",
    x = "", y = ""
  ) +
  coord_sf(
    crs = target_crs
  ) +
  custom_theme() +
  theme(legend.position = "none") # remove legend

ggsave("images/check_dem_map_2.png", dem_map, width = 7, height = 10, dpi = 300)

# 6. RENDER 3D SCENE
# ------------------

rayshader::plot_gg(
  ggobj = full_map, # biv 2D map and legend
  ggobj_height = dem_map, # terrain layer
  width = 7, height = 10, # size of output in inches
  windowsize = c(700, 1000),
  scale = 100,
  shadow = TRUE,
  shadow_intensity = 1,
  phi = 87, theta = 0, zoom = 0.56,
  multicore = TRUE,
)

# 7. LIGHTS
# ------------------

url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/brown_photostudio_02_4k.hdr"
hdri_file <- basename(url)

download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb" 
)

# 8. RENDER 3D OBJECT
# ------------------
rayshader::render_highquality(
  filename = "images/ca_efile_broadband_3d_2.png",
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity = 1,
  rotate_env = 120, # control shadow direction
  parallel = TRUE,
  width = 2000, height = 2857,
  interactive = FALSE,
)
