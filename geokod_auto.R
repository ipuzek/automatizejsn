#library(readr)
#library(purrr)

#library(tibble); library(dplyr)


# SAMO PRVI PUT -----------------------------------------------------------
#udr <- read_csv2("Dropbox/udruge.csv", locale = locale(encoding = #"CP1250"))



# split udr
# nrow(udr) / 2315; nrow(udr) %% 2315

#udr$razbi <- c(unlist(map((letters[1:24]), rep, 2315)), rep("y", 2190))

#udr.split <- 
#  udr %>%
#  split(.$razbi) # za testove # %>% map(sample_n, 1)

#rm(udr)

#save(udr.split, file = "geok.udr")

# SVAKI PUT ---------------------------------------------------------------
library(ggmap)
library(lubridate)

library(stringr)

library(purrr)

library(tibble); library(dplyr)

setwd("/home/ivan/Dropbox")

load("geok.udr")

geok_mutate_google <- function(x.df, lokacija, source = "google") {
  
  koords <- geocode(x.df[[lokacija]], source = source)
  
  cbind(x.df, koords)
  
}

mii <- map(udr.split, function(x) is.null(x$lon)) %>% unlist()

udr.split[mii][[1]] <- geok_mutate_google(udr.split[mii][[1]], "SJEDISTE")

save(udr.split, file = "geok.udr")

ime.fajla <- paste(today(), "udruge_geokod.xlsx", sep = "__")
openxlsx::write.xlsx(udr.split[mii][[1]], ime.fajla)

# mapping do sada

library(readxl)

okz <- list.files()[str_detect(list.files(), "\\.xlsx")] %>% 
  map_df(read_excel)

lok.centar <- c(lon = mean(okz$lon, na.rm = TRUE), lat = mean(okz$lat, na.rm = TRUE) - 1)

map.hr <- get_map(location = lok.centar, zoom = 7)

ggmap(map.hr) +
  geom_point(data = okz, aes(lon, lat), size = .2, alpha = .5, colour = "pink2") +
  geom_density2d(data = okz, aes(lon, lat))

ggsave("/home/ivan/Dropbox/progress.pdf")