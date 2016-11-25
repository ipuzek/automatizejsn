library(ggmap)
library(lubridate)

library(stringr)
library(magrittr)

library(purrr)

library(tibble); library(dplyr)

setwd("/home/ivan/Dropbox")

load("geok.udr")


udr.df <- do.call(rbind, udr.split) %>% as_data_frame()
rm(udr.split)

n_na <- function(x) sum(is.na(x))

n_na(udr.df$lon)

udr.df %>% 
  group_by(razbi) %>%
  summarize(missings = n_na(lon)) %>% 
  View()
  
# check missinga - grupa "razbi" s najviše missinga
# provedi geokodiranje na pouzdanom netu :D

geok_mutate_google <- function(x.df, lokacija, source = "google") {
  
  koords <- geocode(x.df[[lokacija]], source = source)
  
  cbind(x.df, koords) %>% 
    as_tibble()
  
}

ček <- udr.df %>% 
  filter(razbi == "d" & is.na(lon)) %>%
  select(-lon, -lat) %>% 
  geok_mutate_google(lokacija = "SJEDISTE")

ček2 <- udr.df %>% 
  filter(razbi == "p" & is.na(lon)) %>%
  select(-lon, -lat) %>% 
  geok_mutate_google(lokacija = "SJEDISTE")

(!is.na(ček2$lon)) %>% sum # ček OK, vrlo malo novih uspješnih geokodiranja
                           # nije stvar u netu

# ček_dsk <- udr.df %>% 
#   filter(razbi == "d" & is.na(lon)) %>%
#   select(-lon, -lat) %>% 
#   geok_mutate_google(lokacija = "SJEDISTE", source = "dsk")

# da vidimo gdje su neuspješna geokodiranja

razbi.miss <- udr.df %>% 
  group_by(razbi) %>%
  summarize(missings = n_na(lon))

razbi.miss.fulava <- razbi.miss %>% 
  arrange(-missings) %>%
  extract2("razbi") %>% 
  extract(1:10)

razbi.miss.uspijeva <- razbi.miss %>% 
  arrange(missings) %>%
  extract2("razbi") %>% 
  extract(1:10)

he.fulava <- filter(he, razbi %in% razbi.miss.fulava)
he.uspijeva <- filter(he, razbi %in% razbi.miss.uspijeva)

library(ggmap)
ggmap(map.hr) +
  geom_point(data = he.fulava, aes(lon, lat), size = .2, alpha = .5, colour = "pink2") +
  # geom_bin2d(data = okz, aes(lon, lat), binwidth = c(0.1, 0.1)) # radi, ali ružno
  geom_density2d(data = he.fulava, aes(lon, lat)) +
  labs(title = "geokodiranje NE uspijeva")

ggmap(map.hr) +
  geom_point(data = he.uspijeva, aes(lon, lat), size = .2, alpha = .5, colour = "pink2") +
  # geom_bin2d(data = okz, aes(lon, lat), binwidth = c(0.1, 0.1)) # radi, ali ružno
  geom_density2d(data = he.uspijeva, aes(lon, lat)) +
  labs(title = "geokodiranje uspijeva")

library(ggalt)
ggmap(map.hr) +
  geom_point(data = okz, aes(lon, lat), size = .2, alpha = .5, colour = "pink2") +
  # geom_bin2d(data = okz, aes(lon, lat), binwidth = c(0.1, 0.1)) # radi, ali ružno
  geom_bkde2d(data = okz, aes(lon, lat), ) +
  labs(title = now())

### nominatim via R
library(nominatim)

nominatim_naselje <- udr.df %>% 
  filter(is.na(lon)) %>% 
  select(REGISTARSKI_BROJ, SJEDISTE) %>% 
  mutate(SJEDISTE_naselje = str_split_fixed(SJEDISTE, ",", 2)[,1])

# osm_geocode(nominatim_naselje$SJEDISTE_naselje,
#             country_codes = "hr",
#             key = "FSMW1t398TEU44FmoOkH4TUjyMtXD53L")

# obični osm_geocode vraća glupu strukturu, hoću solidni dataframe!
osm_geocode_proper <- function(x, country_codes = "hr", key = "FSMW1t398TEU44FmoOkH4TUjyMtXD53L") {
  
  osm.geo.df <- osm_geocode(x, country_codes = country_codes, key = key)

  if (nrow(osm.geo.df) == 0) data_frame(lon = NA) else osm.geo.df 
}

dajjjj <- map_df(nominatim_naselje$SJEDISTE_naselje, osm_geocode_proper) %>% as_data_frame()

# mali check ====
ggmap(map.hr) +
  geom_point(data = dajjjj, aes(lon, lat), size = .2, alpha = .5, colour = "pink2") +
  # geom_bin2d(data = okz, aes(lon, lat), binwidth = c(0.1, 0.1)) # radi, ali ružno
  geom_density2d(data = dajjjj, aes(lon, lat)) +
  labs(title = "nominatim - samo naselja")
# kraj malog čeka =====

udr.df.nominatim <- nominatim_naselje %>% 
  cbind(dajjjj) %>% 
  select(REGISTARSKI_BROJ, SJEDISTE, SJEDISTE_naselje, lat, lon) %>% 
  as_tibble()

udr.df.google2 <- udr.df.nominatim %>% 
  filter(is.na(lon)) %>% 
  slice(1:2400) %>% 
  select(REGISTARSKI_BROJ, SJEDISTE, SJEDISTE_naselje) %>% 
  geok_mutate_google("SJEDISTE_naselje")



udr.df.google3 <- udr.df.nominatim %>% 
  filter(is.na(lon)) %>% 
  slice(2401:n()) %>% 
  select(REGISTARSKI_BROJ, SJEDISTE, SJEDISTE_naselje) %>% 
  geok_mutate_google("SJEDISTE_naselje")

rbind(udr.df.nominatim, udr.df.google2)

n_na(udr.df.google2$lon)
