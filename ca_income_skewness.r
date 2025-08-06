# 1. PACKAGES
# ------------------
# install.packages("remotes")

# bivariate maps
# remotes::install_github(
#  "chris-prener/biscale"
#)

# install.packages("tigris")
# install.packages("rgl")
# install.packages("pacman")

library(dplyr)

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
irs <- read_csv("data/22incyallagi.csv") |> 
  select(STATE, COUNTYNAME, agi_stub, N1, A00100, STATEFIPS, COUNTYFIPS)

head(irs,10)
summary(irs, A00100)

calc_median_agi <- function(stub, num_returns, total_agi, lower_bound, upper_bound) {
  if (sum(num_returns) == 0) return(NA)
  
  # Reorder vectors by stub
  sorted_idx <- order(stub)
  stub_sorted <- stub[sorted_idx]
  returns_sorted <- num_returns[sorted_idx]
  total_agi_sorted <- total_agi[sorted_idx]
  lower <- lower_bound[sorted_idx]
  upper <- upper_bound[sorted_idx]

  # Calculate median position
  total_returns <- sum(returns_sorted)
  median_position <- total_returns / 2

  # Locate median stub
  cumulative_returns <- cumsum(returns_sorted)
  median_stub_idx <- which(cumulative_returns >= median_position)[1]
  if (is.na(median_stub_idx)) return(NA_real_)

  # Handle open-ended bracket (stub 8)
  if (stub_sorted[median_stub_idx] == 8) {
    return(total_agi[median_stub_idx] / returns_sorted[median_stub_idx])
  }

  # Calculate median AGI for brackets
  prev_cumulative <- ifelse(median_stub_idx == 1, 0, cumulative_returns[median_stub_idx - 1])
  position_in_stub <- median_position - prev_cumulative
  proportion <- position_in_stub / returns_sorted[median_stub_idx]
  return(lower[median_stub_idx] + (proportion * (upper[median_stub_idx] - lower[median_stub_idx])))
}

# Define AGI stub ranges
stub_ranges <- data.frame(
  agi_stub = c(2, 3, 4, 5, 6, 7, 8),
  range_bottom = c(0.001, 10, 25, 50, 75, 100, 200),  
  range_top = c(10, 25, 50, 75, 100, 200, Inf)        
)

agi <- irs |> filter(!agi_stub %in% c(0, 1) & STATE == "CA" & COUNTYFIPS != "000") |>
  mutate(
    FIPS = paste0(STATEFIPS, COUNTYFIPS)
  ) |>
  left_join(stub_ranges, by = "agi_stub") |>
  group_by(FIPS) |>
  summarise(
    total_returns = sum(N1, na.rm = TRUE),
    avg_agi = sum(A00100, na.rm = TRUE) / total_returns,
    median_agi = calc_median_agi(agi_stub, N1, A00100, range_bottom, range_top),
    income_skewness = avg_agi / median_agi
  )

summary(agi$median_agi)
summary(agi$income_skewness)
head(agi, 10)

# Check a specific county
test_county <- agi |> slice(1)
cat("FIPS:", test_county$FIPS, "\n")
cat("Total returns:", test_county$total_returns, "\n") 
cat("Average AGI:", test_county$avg_agi, "\n")
cat("Median AGI:", test_county$median_agi, "\n")
cat("Ratio:", test_county$income_skewness, "\n")


# 3. GET POLYGONS (WITH FIPS CODES)
# ------------------
# Download counties as sf object
counties <- tigris::counties(cb = TRUE, class = "sf") |> 
  filter(STATEFP == "06") |>   # California only
  mutate(FIPS = GEOID)

# Define target CRS once
target_crs <- "EPSG:5070"  # Albers Equal Area Conic projection (equal-area projection)
counties_proj <- st_transform(counties, target_crs)

# Join with data, becomes sf object since sf comes first
map_data <- left_join(counties_proj, agi, by = "FIPS")

# 4. BREAKS, PALETTE, AND THEME
# ------------------
breaks <- bi_class( # info about bivariate scale and distribution
  map_data,
  x = median_agi, y = income_skewness,
  style = "quantile", dim = 4
)

# Define color palette
pal <- "PinkGrn" # bivariate palette from biscale

# Define custom theme for map
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

# 5. 2D BIVARIATE MAP
# ------------------

# Create bivariate map using ggplot2
map <- ggplot(breaks) + 
  geom_sf(
    aes(
      fill = bi_class # name of column needed for fill aesthetic
    ), show.legend = FALSE
  ) +
  biscale::bi_scale_fill( # how to fill values from bivariate scale
      pal = pal, dim = 4,
      flip_axes = FALSE,
      rotate_pal = FALSE
  ) +
  labs( # labels
      title = "CALIFORNIA: Wealth v. Income Skewness by County",
      subtitle = "Interpolated median AGI and income skewness ratio (mean/median AGI), 2022",
      caption = "Source: IRS SOI, 2022",
      x = "", y = ""
  ) +
  coord_sf(
    crs = target_crs
  ) +
  custom_theme()

# Create legend for bivariate map
legend <- biscale::bi_legend(
  pal = pal, 
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 4, 
  xlab = "Higher Median AGI", 
  ylab = "Higher Income Skewness Ratio",
  size = 8.5 # size of label text in legend
)

# Inspect output, get x and y ranges of map to help with legend placement
map_built <- ggplot_build(map)
str(map_built$layout$panel_params[[1]])

x_range <- c(-2391586, -1611187)
y_range <- c(1181840, 2513229)

# Calculate total range
x_total <- diff(x_range)  # 780399
y_total <- diff(y_range)  # 1331389

# Convert to 0-1 scale (like cowplot) (x=0.05, y=0.13, width=0.25, height=0.25)
xmin <- x_range[1] + (0.62 * x_total)   
xmax <- x_range[1] + (1.00 * x_total)   
ymin <- y_range[1] + (0.54 * y_total)   
ymax <- y_range[1] + (0.92 * y_total)   

# Map with legend
full_map <- map + 
  annotation_custom(
    grob = ggplotGrob(legend),
    xmin = xmin, xmax = xmax,  
    ymin = ymin, ymax = ymax   
  )

print(full_map)

# Save as PNG
ggsave(
  filename = "images/ca_income_skewness_quantile_2d.png",
  width = 7, height = 10, dpi = 600,
  device = "png", bg = "white", full_map
)

# 6. CREATE TERRAIN LAYER
# ------------------

# Get California elevation data
ca_dem <- get_elev_raster(
  locations = counties_proj, # clip to counties
  z = 8, clip = "locations"
) |> 
terra::rast() |> # convert to SpatRaster
terra::mask(counties_proj)

# Project DEM and convert to dataframe
dem_df <- ca_dem |>
  terra::project(target_crs) |>
  as.data.frame(xy = TRUE, na.rm = TRUE) # remove NAs

names(dem_df)[3] <- "elevation"

# Create terrain layer map
dem_map <- ggplot(
  dem_df, aes(x = x, y = y, fill = elevation)
  ) +
  geom_raster() +
  scale_fill_gradientn(colors = "white") + # required for rayshader
  guides(fill = "none") +
  labs( # labels
      title = "CALIFORNIA: Wealth v. Inequality by County",
      subtitle = "Interpolated median AGI and income skewness ratio (mean/median AGI), 2022",
      caption = "Source: IRS SOI, 2022",
      x = "", y = ""
  ) +
  coord_sf(
    crs = target_crs
  ) +
  custom_theme() +
  theme(legend.position = "none") # remove legend

ggsave("images/check_dem_map.png", dem_map, width = 7, height = 10, dpi = 300)

# 7. 3D SCENE
# ------------------

rayshader::plot_gg(
  ggobj = full_map, # biv scale and legend
  ggobj_height = dem_map, # terrain layer
  width = 7, height = 10, # size of output in inches
  windowsize = c(700, 1000),
  scale = 100,
  shadow = TRUE,
  shadow_intensity = 1,
  phi = 87, theta = 0, zoom = 0.56,
  multicore = TRUE,
)

# 8. LIGHTS
# ------------------

url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/brown_photostudio_02_4k.hdr"
hdri_file <- basename(url)

download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb" # write binary
)

# 9. RENDER 3D OBJECT
# ------------------
rayshader::render_highquality(
  filename = "images/ca_income_skewness_quantile_3d.png",
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity = 1,
  rotate_env = 120, # control shadow direction
  parallel = TRUE,
  width = 2000, height = 2857,
  interactive = FALSE,
)
