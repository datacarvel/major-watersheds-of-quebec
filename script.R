############## PRE-LOADING

libs <- c(
  "tidyverse", "sf", "giscoR", "patchwork", "ggtext", "ragg", "textshaping", "systemfonts"
)

installed_libraries <- libs %in% rownames(
  installed.packages() # checking whether they're installed
)

if(any(installed_libraries == F)) { # installing the non-installed ones
  install.packages(
    libs[!installed_libraries]
  )
}

invisible( #loading them
  lapply(
    libs, library,
    character.only = T
  )
)

##############

##############

fontss <- system_fonts()

# country borders 

get_country_borders <- function(){
  country_borders <- giscoR::gisco_get_countries(
    resolution = "3",
    country = "CA"
    )
  
  return(country_borders)
}

country_borders <- get_country_borders()

### basins data

### North America basins file
### https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_na_lev03_v1c.zip

### Canadian Arctic (including territories)
### https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_ar_lev03_v1c.zip

NA_basins_url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_na_lev03_v1c.zip"
AR_basins_url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_ar_lev03_v1c.zip"

get_basins <- function(url, file_name){
  #url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_na_lev03_v1c.zip"
  #file_name <- "hybas_na_lev03_v1c.zip"
  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_basins(NA_basins_url, "northamerica.zip")
get_basins(AR_basins_url, "arctic.zip")

list.files()

load_basins <- function(){
  filenames <- list.files(
    pattern = ".shp$",
    full.names = T
  )
  
  basins <- lapply(filenames, st_read)
  
  na_ar_basins <- do.call(rbind, basins)
  
  st_write(na_ar_basins, "na_ar_basins.shp")
  
  return(na_ar_basins)
}

na_ar_basins <- load_basins()

ggplot(na_ar_basins) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()

sf::sf_use_s2(F) #spherical options turned off

canada_basins <- na_ar_basins |>
  sf::st_intersection(
    country_borders
  ) |>
  dplyr::select(
    HYBAS_ID
  )

ggplot(canada_basins) +
  geom_sf(fill = "orange", color = "white") +
  theme_void()

### rivers data

### NA rivers
### https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip

### ARC rivers
### https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip

na_rivers_url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
ar_rivers_url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_ar_shp.zip"

get_rivers <- function(url, file_name){
  #url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_na_lev03_v1c.zip"
  #file_name <- "hybas_na_lev03_v1c.zip"
  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_rivers(na_rivers_url, "northamerica-rivers.zip")
get_rivers(ar_rivers_url, "arctic-rivers.zip")

list.files()
# HydroRIVERS_v10_na_shp #note that this is a folder (no file extension)
# HydroRIVERS_v10_ar_shp

load_rivers <- function(){
  filenames <- list.files(
    path = c("HydroRIVERS_v10_na_shp", "HydroRIVERS_v10_ar_shp"), 
    pattern = ".shp$",
    full.names = T
  )
  
  rivers <- lapply(filenames, st_read)
  
  na_ar_rivers <- do.call(rbind, rivers)
  
  st_write(na_ar_rivers, "na_ar_rivers.shp")
  
  return(na_ar_rivers)
}

na_ar_rivers <- load_rivers()

canada_rivers <- na_ar_rivers |>
  dplyr::select(
    ORD_FLOW
  ) |>
  sf::st_intersection(
    country_borders
  )

#### NOW HERE https://youtu.be/HugGwjogPv0?si=c0A7jqRB9udei2lO&t=2512

# determine every basin #     ...

canada_river_basin <- sf::st_intersection(
  canada_rivers,
  canada_basins 
)

# river width #     ...

unique(canada_river_basin$ORD_FLOW) # range of possible values, 9

canada_river_basin_width <- canada_river_basin |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 2 ~ .8, # largest ones
      width == 3 ~ .7,
      width == 4 ~ .6,
      width == 5 ~ .45,
      width == 6 ~ .35,
      width == 7 ~ .25,
      width == 8 ~ .2,
      width == 9 ~ .15,
      width == 10 ~ .1,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf()

# plot

unique(
  canada_river_basin_width$HYBAS_ID
)

hcl.pals("qualitative")

p <- ggplot() +
  geom_sf(
    data = canada_river_basin_width,
    aes(
      color = factor(
        HYBAS_ID
      ),
      size = width,
      alpha = width
    )
  ) +
  scale_color_manual(
    name = "",
    values = hcl.colors(
      30, "Dark 3",
      alpha = 1
    )
  ) +
  scale_size(
    range = c(.1, .7)
  ) +
  scale_alpha(
    range = c(.01, .7)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.caption = element_text(
      size = 9, color = "grey60",
      hjust = .1, vjust = 10
    ),
    plot.margin = unit(
      c(0, 0, 0, 0),
      "lines"
    ),
    plot.background = element_rect(
      fill = "black",
      color = NA
    ),
    panel.background = element_rect(
      fill = "black",
      color = NA
    )
  ) + 
  labs(
    title = "Canada basins",
    x = "",
    y = "",
    caption = "Made by whoever did this. Source : the data."
  )

ggsave(
  filename = "canada-basins-test.png",
  width = 7, height = 7.75, dpi = 600,
  bg = "white", device = "png", p
)
