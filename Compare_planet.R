library(ggplot2)
library(ggspatial)
library(dplyr)
library(sf)
library(stringr)
library(terra)
library(tidyterra)
library(patchwork)
library(gapminder)
library(tidyverse)
library(viridisLite)
library(forcats)


polygon_extent <- read_sf("D:\\Postdoc_work\\UMD\\API_query\\Biomass_polygon1.geojson")

scenes_folders <- list.files('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
                             full.names = T)

total_sites <- unique(polygon_extent$code)

planet_site_code <- stringr::str_extract(scenes_folders,'Onfarm_[A-Z]{3}') %>% 
  stringr::str_remove('Onfarm_')

### Sites where Planet images are available
rep1 <- polygon_extent %>% filter(rep==1)
rep2 <- polygon_extent %>% filter(rep==2)

df1 <- rep1[rep1$code %in% planet_site_code,]
df2 <- rep2[rep2$code %in% planet_site_code,]

planet_df <- rbind(df1, df2)

# map the polygon using 'leaflet'package
# planet_points <- planet_df %>%
#   st_as_sf(coords = c("longitude", "latitude")) %>%
#   st_sf(crs = 4326)

# long <- na.omit(planet_df$longitude)
# lat <- na.omit(planet_df$latitude)

library(leaflet)
leaflet() %>% 
  addTiles(group="One") %>%
  addProviderTiles(providers$Esri.WorldImagery,group="Two") %>%
  addPolygons(data = st_geometry(planet_df),color = "red") %>% 
  addLayersControl(baseGroups=c("One", "Two"),
                   options=layersControlOptions(collapsed=FALSE))

# # Load library 
# library(leaflet)
# library(dplyr)
# # Draw map
# leaflet() %>%
#   # Add first tile
#   addTiles(group="One") %>%
#   # Add second tile
#   addProviderTiles(providers$Esri.WorldImagery, group="Two") %>%
#   # Add first marker
#   addMarkers(lng=25.505206, lat=65.9767231, group="mark1") %>%
#   # Add second marker
#   addMarkers(lng=-45.445206, lat=45.5327231, group="mark2") %>%
#   # Add Layer controls
#   addLayersControl(baseGroups=c("One", "Two"), 
#                    overlayGroups=c("mark1", "mark2"), 
#                    options=layersControlOptions(collapsed=FALSE))

 

# BOX Plots
planet_df %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) 

p1 <- planet_df %>% 
  dplyr::filter(code %in% c("AHW","BGP","BRQ","BXQ","BZW","CRK","CWZ","DBS","DCN","DGF","DKP","DRK","DVW","DWV","EHC",
                            "EJU","EKG","FIY","FSX","FZT","GFU","GWP","HAP","HBM","HGU","HMI","HNZ","HOR","HXU","IFJ",
                            "INN","IQE","IWY","JBB","JEA","JVU","KNL","LHR","LZU","MGT","MJI","MLN","MWU","MZU","NBS",
                            "NVC","NYC")) %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
p1

p2 <- planet_df %>% 
  dplyr::filter(code %in% c("OFM","OZT","PHZ","PMZ","POQ","QAQ","QRD","QYI","RCA","RVP","SKF","SVC","TDI",
                            "TLF","TOI","TSH","TSN","TZM","UCE","UGB","UHC","ULW","URF","USV","VEZ","VMF","VPE","VZC",
                            "VZR","WBD","WOS","WPQ","XFS","XLS","XVY","YAA","YBR","YLM","YRV","YUO","ZEE","ZFW","ZJH",
                            "ZMK","ZTP")) %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
p2

## Histograms
# Histogram by group in ggplot2
library(viridis)
# h1 <- planet_df %>% 
#   dplyr::filter(code %in% c("AHW","BGP","BRQ","BXQ","BZW","CRK","CWZ","DBS","DCN","DGF","DKP","DRK","DVW","DWV","EHC",
#                             "EJU","EKG","FIY","FSX","FZT","GFU","GWP","HAP","HBM","HGU","HMI","HNZ","HOR","HXU","IFJ",
#                             "INN","IQE","IWY","JBB","JEA","JVU","KNL","LHR","LZU","MGT","MJI","MLN","MWU","MZU","NBS",
#                             "NVC","NYC")) %>%
#   ggplot( aes(x=uncorrected_cc_dry_biomass_kg_ha)) +
#   geom_histogram(alpha=0.6, binwidth = 44) +
#   scale_fill_viridis(discrete=TRUE) +
#   scale_color_viridis(discrete=TRUE) +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   xlab(" Biomass") +
#   ylab("Count")

h1 <- ggplot(planet_df, aes(x = uncorrected_cc_dry_biomass_kg_ha)) + 
  geom_histogram(colour = "black",bins = 92,
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity")
h1

h2 <- planet_df %>% 
    dplyr::filter(code %in% c("AHW","BGP","BRQ","BXQ","BZW","CRK","CWZ","DBS","DCN","DGF","DKP","DRK","DVW","DWV","EHC",
                              "EJU","EKG","FIY","FSX","FZT","GFU","GWP","HAP","HBM","HGU","HMI","HNZ","HOR","HXU","IFJ",
                              "INN","IQE","IWY","JBB","JEA","JVU","KNL","LHR","LZU","MGT","MJI","MLN","MWU","MZU","NBS",
                              "NVC","NYC")) %>%
  ggplot(aes(x = uncorrected_cc_dry_biomass_kg_ha)) + 
  geom_histogram(colour = "black",bins = 44,
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity")
h2

h3 <- ggplot(planet_df, aes(x = uncorrected_cc_dry_biomass_kg_ha)) + 
  geom_histogram(colour = "black",bins = 44,
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity")
h3









#################### Sites where Planet images are not available ##########################
rep1 <- polygon_extent %>% filter(rep==1)
rep2 <- polygon_extent %>% filter(rep==2)

df4 <- rep1[!(rep1$code %in% planet_site_code),]
df5 <- rep2[!(rep2$code %in% planet_site_code),]

nonplanet_df <- rbind(df4, df5)

library(leaflet)
leaflet() %>% 
  addTiles(group="One") %>%
  addProviderTiles(providers$Esri.WorldImagery,group="Two") %>%
  addPolygons(data = st_geometry(nonplanet_df),color = "black") %>% 
  addLayersControl(baseGroups=c("One", "Two"),
                   options=layersControlOptions(collapsed=FALSE))

# BOX Plots
nonplanet_df %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) 

p3 <- nonplanet_df %>% 
  dplyr::filter(code %in% c("AYR","BAR","BIG","BII","BRW","CFZ","COE","CUU","DAA","DAB","DAC","DAD","DAE","DET",
                            "DFV","DTN","DVH","DZJ","EBT","EDF","ELP","ERU","FJG","FLZ","GMG","GPM","GYK","GZS",
                            "HGI","HMT","HVQ","IFC","IMT","IVU","IWI","IYB","KGR","KIZ","KKD","KLU","KXR","LAB",
                            "LAC","LDT","LKB","LRH","LSS","LVF","MJH","MJT","NMO","NOY","NTK","NUR","NWE")) %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
p3

p4 <- nonplanet_df %>% 
  dplyr::filter(code %in% c("OEX","OJW","OOA","OQB","PKY","PML","POG","PRH","QKZ","QLR","QNE","RAB","RAD","REK","RKT",
                            "RPM","SAA","SAB","SHX","SOS","SOW","STA","SVA","TDP","TOX","TXE","UAN","UCG","UCT",
                            "UFH","UYD","VOP","VSJ","VWD","VYG","WFJ","XBM","XNM","XVB","XZY","YLT","ZGT")) %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
p4



## Box plots
h4 <- ggplot(nonplanet_df, aes(x = uncorrected_cc_dry_biomass_kg_ha)) + 
  geom_histogram(colour = "black",bins = 90,
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity")
  
h4







########## Planet total sites (177) map send to Jyoti

polygon_extent <- read_sf("D:\\Postdoc_work\\UMD\\API_query\\Biomass_polygon1.geojson")
total_sites <- unique(polygon_extent$code)

four_band_scenes_folders <- list.files('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\4band_onfarm_planet_sites',
                             full.names = T)
eight_band_scenes_folders <- list.files('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\8band_onfarm_planet_sites',
                                       full.names = T)

planet_site_codes_4band <- stringr::str_extract(four_band_scenes_folders,'Onfarm_[A-Z]{3}') %>% 
  stringr::str_remove('Onfarm_')
planet_site_codes_8band <- stringr::str_extract(eight_band_scenes_folders,'Onfarm_[A-Z]{3}') %>% 
  stringr::str_remove('Onfarm_')

all_bands_codes <- c(planet_site_codes_4band,planet_site_codes_8band)

#check difference and match in codes
setdiff(total_sites, all_bands_codes)
matching_codes <- intersect(total_sites, all_bands_codes)


polygon_extent_match <- polygon_extent %>% filter(code %in% matching_codes)

### Sites where Planet images are available
rep1 <- polygon_extent %>% filter(rep==1)
rep2 <- polygon_extent %>% filter(rep==2)

df1 <- rep1[rep1$code %in% matching_codes,]
df2 <- rep2[rep2$code %in% matching_codes,]

planet_df <- rbind(df1, df2)
unique(planet_df$affiliation)


library(sf)
library(dplyr)
library(leaflet)

states <- read_sf("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json")

planet_states <- states %>% 
  dplyr::filter(name %in% c("Virginia", "Alabama", "Florida", "North Carolina","Indiana", "Missouri", "Delaware", "New Hampshire","South Carolina",
                            "Maryland", "Vermont", "Kansas", "Pennsylvania", "Georgia", "Wisconsin", "Nebraska", "	Minnesota", "Arkansas"))

leaflet() %>% 
  addTiles(group="One") %>%
  addProviderTiles(providers$Esri.WorldImagery,group="Two") %>%
  addPolygons(data = st_geometry(polygon_extent_match ),color = "red") %>% 
  addLayersControl(baseGroups=c("One", "Two"),
                   options=layersControlOptions(collapsed=FALSE))

leaflet() %>% 
  addTiles(group="One") %>%
  addProviderTiles(providers$Esri.WorldImagery,group="Two") %>%
  addPolygons(data = st_geometry(planet_df),color = "red") %>% 
  addLayersControl(baseGroups=c("One", "Two"),
                   options=layersControlOptions(collapsed=FALSE)) %>% 
  addPolygons(data=planet_states, fill=F, color="white",opacity=0.5)











# BOX Plots
planet_df %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(1000, 8000))+
  geom_jitter(width=0.1,alpha=0.2) 

planet_df %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + 
  scale_y_continuous(limits=c(1000,8000)) +
  geom_jitter(width=0.1,alpha=0.2)


p1 <- planet_df %>% 
  dplyr::filter(code %in% c("AHW","BGP","BRQ","BXQ","BZW","CRK","CWZ","DBS","DCN","DGF","DKP","DRK","DVW","DWV","EHC",
                            "EJU","EKG","FIY","FSX","FZT","GFU","GWP","HAP","HBM","HGU","HMI","HNZ","HOR","HXU","IFJ",
                            "INN","IQE","IWY","JBB","JEA","JVU","KNL","LHR","LZU","MGT","MJI","MLN","MWU","MZU","NBS",
                            "NVC","NYC")) %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + 
  geom_jitter(width=0.1,alpha=0.2)
p1

p2 <- planet_df %>% 
  dplyr::filter(code %in% c("OFM","OZT","PHZ","PMZ","POQ","QAQ","QRD","QYI","RCA","RVP","SKF","SVC","TDI",
                            "TLF","TOI","TSH","TSN","TZM","UCE","UGB","UHC","ULW","URF","USV","VEZ","VMF","VPE","VZC",
                            "VZR","WBD","WOS","WPQ","XFS","XLS","XVY","YAA","YBR","YLM","YRV","YUO","ZEE","ZFW","ZJH",
                            "ZMK","ZTP")) %>% 
  ggplot(aes(x=code,y=uncorrected_cc_dry_biomass_kg_ha)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
p2



















