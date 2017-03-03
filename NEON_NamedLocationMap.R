library(httr)
library(jsonlite)
library(magrittr)
library(leaflet)
library(rgdal)
library(wellknown)

domainData <- readLines("data/NEON_Domains.geojson") %>% paste(collapse = "\n")
wkt.sampBounds <- srcfile(filename = "NEON_Field_Sampling_Boundaries_2016-11-16_15_09_08_653000.wkt")

psm <- readRDS("data/psm.rds")

totalm <- sum(psm$months)
sites <- unique(sites)
sites <- fromJSON("http://data.neonscience.org/api/v0/sites")
d <- data.frame(domain = NA, name = NA, lng = NA, lat = NA, elev = NA, desc = NA, parent = NA)
start <- Sys.time()
for(i in 1:length(sites$data$siteCode)){
  sitedataURL <- paste0("http://data.neonscience.org/api/v0/locations/", sites$data$siteCode[i])
  sitedata <- fromJSON(sitedataURL)
  for(j in 1:length(sitedata$data$locationChildren)){
    t <- fromJSON(paste0("http://data.neonscience.org/api/v0/locations/", sitedata$data$locationChildren[j]))
    d <- rbind(d, c(t$data$domainCode, t$data$locationName, t$data$locationDecimalLongitude, t$data$locationDecimalLatitude, 
                    t$data$locationElevation, t$data$locationDescription, t$data$locationParent))
  }
}
end <- Sys.time()

d <- d[-1,]
d$lat <- as.numeric(d$lat)
d$lng <- as.numeric(d$lng)
d <- d[-which(is.na(d$lat)),  ]
d$color <- character(nrow(d))
d$color[grep(pattern = "TOWER", x = d$name)] <- "grey"
d$color[grep(pattern = "MEGAPT", x = d$name)] <- "burlywood"
d$color[grep(pattern = "soil", x = d$name)] <- "brown"
d$color[grep(pattern = "mam", x = d$name)] <- "#AA3939"
d$color[grep(pattern = "bgc", x = d$name)] <- "#AA6C39"
d$color[grep(pattern = "bet", x = d$name)] <- "black"
d$color[grep(pattern = "hbp", x = d$name)] <- "#2D882D"
d$color[grep(pattern = "div", x = d$name)] <- "#88CC88"
d$color[grep(pattern = "mos", x = d$name)] <- "#D49A6A"
d$color[grep(pattern = "brd", x = d$name)] <- "#FFAAAA"
d$color[grep(pattern = "vst", x = d$name)] <- "springgreen"
d$color[grep(pattern = "dhp", x = d$name)] <- "#FFD1AA"
d$color[grep(pattern = "mfb", x = d$name)] <- "#226666"
d$color[grep(pattern = "cfc", x = d$name)] <- "#669999"
d$color[grep(pattern = "cdw", x = d$name)] <- "#11661"
d$color[grep(pattern = "ltr", x = d$name)] <- "#004400"
d$color[grep(pattern = "bbc", x = d$name)] <- "darkmagenta"
d$color[grep(pattern = "mpt", x = d$name)] <- "#804515"
d$color[grep(pattern = "tck", x = d$name)] <- "#552700"
d$color[grep(pattern = "sme", x = d$name)] <- "#550000"
d$color[grep(pattern = "phe", x = d$name)] <- "#55AA55"

d$type <- character(nrow(d))
d$type[grep(pattern = "TOWER", x = d$name)] <- "Tower"
d$type[grep(pattern = "MEGAPT", x = d$name)] <- "Megapit"
d$type[grep(pattern = "soil", x = d$name)] <- "Soil"
d$type[grep(pattern = "mam", x = d$name)] <- "Small Mammal"
d$type[grep(pattern = "bgc", x = d$name)] <- "Biogeochemistry"
d$type[grep(pattern = "bet", x = d$name)] <- "Beetle"
d$type[grep(pattern = "hbp", x = d$name)] <- "Herbaceous Biomass"
d$type[grep(pattern = "div", x = d$name)] <- "Plant Diversity"
d$type[grep(pattern = "mos", x = d$name)] <- "Mosquito"
d$type[grep(pattern = "brd", x = d$name)] <- "Bird"
d$type[grep(pattern = "vst", x = d$name)] <- "Vegetation Structure"
d$type[grep(pattern = "dhp", x = d$name)] <- "Digital Hemisphere Photos"
d$type[grep(pattern = "mfb", x = d$name)] <- "MFB"
d$type[grep(pattern = "cfc", x = d$name)] <- "CFC"
d$type[grep(pattern = "cdw", x = d$name)] <- "Coarse Downed Wood"
d$type[grep(pattern = "ltr", x = d$name)] <- "Litter"
d$type[grep(pattern = "bbc", x = d$name)] <- "Belowground Biomass Core"
d$type[grep(pattern = "mpt", x = d$name)] <- "Mosquito Pathogens"
d$type[grep(pattern = "tck", x = d$name)] <- "Tick"
d$type[grep(pattern = "sme", x = d$name)] <- "Soil Microbes"
d$type[grep(pattern = "phe", x = d$name)] <- "Plant Phenology"

#### Maps ####

#Without clustering
leaflet(d) %>% setView(lng = -100, lat = 40, zoom = 3) %>%
  addTiles() %>%
  addCircles(data = d, ~lng, ~lat, popup=d$name, weight = 3, radius=40, 
                   color=d$color, stroke = TRUE, fillOpacity = 1) %>%
  addGeoJSON(domainData, weight = 1, color = "#444444", fill = FALSE) %>%
  addLegend("bottomright", colors= unique(d$color), opacity = 1,labels=unique(d$type), title="NEON Named Locations") 

#With clustering
leaflet(d) %>% setView(lng = -100, lat = 40, zoom = 3) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat, popup=d$name, weight = 3, radius=10, 
                   color=d$color, stroke = TRUE, fillOpacity = 1, clusterOptions = markerClusterOptions()) %>% 
  addGeoJSON(domainData, weight = 1, color = "#444444", fill = FALSE) %>%
  addLegend("bottomright", colors= unique(d$color), opacity = 1,labels=unique(d$type), title="NEON Named Locations")
