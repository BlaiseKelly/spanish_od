
library(odf)
library(donutmaps)
library(tmap)
library(tidyverse)
library(spanishoddata)


districts_v1 <- spod_get_zones("dist", ver = 1)

ss_dom <- st_point(c( -1.96,43.319)) |> 
  st_sfc(crs = 4326) |> 
  st_buffer(10000)

dist_ll <- districts_v1 |> 
  st_transform(4326)

ss_districts <- dist_ll[ss_dom,] |> 
  transmute(id, name = district_names_in_v2, geometry) |> 
  st_centroid()

mapview(ss_districts)

dates <- c(start = "2024-03-04", end = "2024-03-04")
od_dist <- spod_get(type = "od", zones = "dist", dates = dates)

od <- od_dist |>
  group_by(hour, id_origin, id_destination) |>
  filter(id_origin %in% ss_districts$id & id_destination %in% ss_districts$id) |> 
  summarise(
    total_hourly_trips = round(sum(n_trips, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(id_origin = as.character(id_origin),
         id_destination = as.character(id_destination)) |> 
  collect() |> 
  select(id_origin, hour, id_destination, total_hourly_trips)

od_1 <- filter(od, hour == 8) |> 
  select(-hour)

top_id <- od_1 |> 
  arrange(desc(total_hourly_trips)) |> 
  slice(1:10)

top_origins <- unique(top_id$id_origin)[1:4]
top_dests <- unique(top_id$id_destination)

top_munz <- filter(ss_districts,id %in% top_origins)

# Create odf object
x = od(od_1, ss_districts, col_orig = "id_origin", col_dest = "id_destination", col_id = "id")

# Define color palette
CBS_pal = c("#d9328a", "#7d4791", "#da5914", "#53a31d", "#0581a2", "#B3B3B3")

unique(ss_districts$district_names_in_v2)

# Bake tasty donuts (all commuting traffic)
# Edges are incoming by default
tm = bake_donuts(x,
                 var = "total_hourly_trips",
                 groupname = "San Sebastian",
                 highlight = top_munz$name,
                 pal = CBS_pal,
                 donut_size_min = 3000, donut_size_max = 40000,
                 flow_th = 50, flow_max = 2000, flow_buffer = 50, flow_scale = 10,
                 donut_scale = 1.75)

# The result is a tmap object, which can best be shown in "view mode"
tmap_mode("view")

tmap_save(tm, "san_sebastian.html")
