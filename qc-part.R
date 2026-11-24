### example with giscoR

ITA <- gisco_get_nuts(country = "Italy", nuts_level = 1)

ggplot(ITA) +
  geom_sf() +
  geom_sf_text(aes(label = NAME_LATN)) +
  theme(axis.title = element_blank())

### ISOLATING QUEBEC

####################### wgs84 projection 

provinces <- st_read("qc_shape/lpr_000b16a_e.shp")
quebec_shape <- provinces %>% filter(PRENAME == "Quebec")

quebec_wgs84 <- st_transform(quebec_shape, crs = st_crs(country_borders))

intersected_qc <- st_intersection(country_borders, quebec_wgs84)

### ELLIPSE - SEE MAIN SCRIPT between lines ~48 through ~91

quebec_basins <- na_ar_basins |>
  sf::st_make_valid() |>  # Fixes geometry issues first
  sf::st_intersection(intersected_qc |> sf::st_make_valid()) |>  # Fixes both inputs
  dplyr::select(HYBAS_ID)

ggplot(quebec_basins) +
  geom_sf(fill = "orange", color = "white") +
  theme_void()

###################### lambert canonic something projection

country_lambert <- st_transform(country_borders, crs = st_crs(quebec_shape))

intersection_qc_lamb <- st_intersection(country_lambert, quebec_shape)

na_ar_basins_lamb <- st_transform(na_ar_basins, st_crs(quebec_shape))

### ELLIPSE - SEE MAIN SCRIPT between lines ~48 through ~91

quebec_basins_lamb <- na_ar_basins_lamb |>
  sf::st_make_valid() |>  # Fixes geometry issues first
  sf::st_intersection(intersection_qc_lamb |> sf::st_make_valid()) |>  # Fixes both inputs
  dplyr::select(HYBAS_ID)

ggplot(quebec_basins_lamb) +
  geom_sf(fill = "turquoise", color = "white") +
  theme_void()

## OK let's go for Lambert something

### ELLIPSE - SEE MAIN SCRIPT between lines ~109 through -149

# again projection conversion is necessary

quebec_rivers <- na_ar_rivers |> # rivers in WGS84 and quebec in WGS64 = works
  dplyr::select(
    ORD_FLOW
  ) |>
  sf::st_intersection(
    intersected_qc
  )

#### 28NOV : So this works and it took less than 10 secs?!?! Try vizzing now?
quebec_rivers_lamb <- st_transform(quebec_rivers, crs = st_crs(quebec_shape))
### BUT this might have been too early

#quebec_rivers_basins_lamb <- sf::st_intersection(
#  quebec_rivers_lamb,
#  quebec_basins_lamb
#) 
# doesnt work

quebec_rivers_basins <- sf::st_intersection(
  quebec_rivers,
  quebec_basins
)

unique(quebec_rivers_basins$ORD_FLOW) # range of possible values, 7, 2-8

quebec_rivers_basins_width <- quebec_rivers_basins |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 2 ~ 1, # largest ones
      width == 3 ~ .9,
      width == 4 ~ .8,
      width == 5 ~ .7,
      width == 6 ~ .6,
      width == 7 ~ .5,
      width == 8 ~ .4,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf()

#lamb proj

quebec_rivers_basins_width_lamb <- st_transform(
  quebec_rivers_basins_width, crs = 3347
)

unique(
  quebec_rivers_basins_width$HYBAS_ID
)

hcl.pals("qualitative")

pp

pp1 <- ggplot() +
  #geom_sf(
  #  data = quebec_basins_lamb,
  #  aes(
  #    fill = factor(
  #      HYBAS_ID
  #    )
  #    ),
  #  alpha = 0
  #  ) + 
  geom_sf(
    data = quebec_rivers_basins_width_lamb,
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
    values = fillfixed # from colors-iterations script
  ) +
  scale_fill_manual(
    name = "",
    values = fillfixed # from colors-iterations script
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
    plot.caption = element_markdown(size = 12, color = "lightblue", family = "Koblenz", hjust = 1, margin = margin(10, 5, 15, 0, unit = "pt")),
    plot.margin = unit(c(10, 15, 5, 15),"pt"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    plot.title = element_markdown(size = 27, color = "lightblue", family = "Koblenz", hjust = 0, margin = margin(10, 0, 15, 10, unit = "pt")),
    axis.title = element_blank()
  ) + 
  labs(
    title = "River basins of<br> Quebec",
    caption = "Source : WWF, HydroSHEDS database<br>Made by @stevecarufel.bsky.social from a tutorial by @milos_makes_maps"
  )
pp1

ggsave(
  filename = "qcbasins26.png",
  width = 4, height = 7, dpi = 600,
  bg = "black", device = "png", pp1
)
