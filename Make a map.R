library(here)
library(rgdal)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

setwd(here::here())
source("Prepare data script.R")
data<-d
sites<-data[, grep("^Lat$|^Long$", names(data))]
names(sites)<-c("latitude","longitude")
sites$grp<-"Assemblages sampled"
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")



text.size<-12
shape.size<-3
map<-ggplot(data = world) +
  geom_sf(fill="gray") +
  geom_sf(data = sites, size = shape.size, shape = 23, color="black", aes(fill = grp)) +
  coord_sf(xlim = c(-170, 180), ylim = c(-50, 75), expand = FALSE) +
  theme_bw()+
  theme(text = element_text(size=text.size),
        legend.position = c(0.7, 0.1),
        legend.title=element_text(color=NA),
        legend.background = element_rect(fill=NA),
        legend.key=element_blank()) 


setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("map.pdf", map, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
