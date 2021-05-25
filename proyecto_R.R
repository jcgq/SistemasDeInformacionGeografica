install.packages("osmdata")
install.packages("tidyverse")
install.packages("sf")
install.packages("ggmap")
library(leaflet)
library(osmdata)
library(tidyverse)
library(sf)
library(ggmap)
library(ggplot2)



carreteras <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("primary")) %>%
  osmdata_sf()

getbb("Spain")









min <- c(-83.55, 30.84)
max <- c(-83.51, 30.88)
espania_df <- as.matrix(data.frame(min, max))
row.names(espania_df) <- c("x","y")


carreteras <- getbb("Almeria Spain")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("road", "motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .4,
          alpha = .8)+
  labs(title = "Almería",
       subtitle = "Red de carreteras")


calles <- getbb("Almería Spain")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()



ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .2,
          alpha = .8)+
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .8) +
  labs(title = "Almería")





ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .1,
          alpha = .8)+
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "grey40",
          size = .2,
          alpha = .8)+
  theme_void() +
  labs(title = "Almería") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white"))


rios <- getbb("Almeria Spain")%>%
  opq()%>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "canal")) %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .1,
          alpha = .8)+
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "grey40",
          size = .2,
          alpha = .8)+
  geom_sf(data = rios$osm_lines,
          inherit.aes = FALSE,
          color = "blue",
          size = .2,
          alpha = .8)+
  labs(title = "Almería")


bici <- getbb("Almería Spain")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("cycleway")) %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .1,
          alpha = .8)+
  geom_sf(data = bici$osm_lines,
          inherit.aes = FALSE,
          color = "blue",
          size = .2,
          alpha = .8)+
  labs(title = "Almería")

q <- getbb("Granada Spain") %>%
  opq() %>%
  add_osm_feature("shop", "tobacco")

cinema <- osmdata_sf(q)

available_tags("shop")


estancos <- getbb("Burgos Spain")%>%
  opq()%>%
  add_osm_feature(key = "shop", 
                  value = "hairdresser") %>%
  osmdata_sf()

mad_map <- get_map(getbb("Burgos Spain"), maptype = "toner-background")

ggmap(mad_map)+
  geom_sf(data = estancos$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)

mad_map

ggmap(mad_map)









ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .1,
          alpha = .8)+
  geom_sf(data = cinema$osm_points,
          inherit.aes = FALSE,
          colour = "blue",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(title = "Almería")
























available_tags("name")

#bounding box for the Iberian Peninsula
coordenadas <- c(-9.39288367353, 35.946850084, 3.03948408368, 43.7483377142)


q <- coordenadas %>% 
  opq() %>%
  add_osm_feature("name", "Eroski") %>%
  add_osm_feature("shop", "supermarket")

#query

eroskis <- osmdata_sf(q)

#final map
mad_map
mad_map <- get_map(getbb("Hamburg Germany"), maptype = "toner-background")

ggmap(mad_map)



ggplot(eroskis$osm_points)+
  geom_sf(colour = "#08519c",
          fill = "#08306b",
          alpha = .5,
          size = 1,
          shape = 21)+
  theme_void()




getbb("Almeria Spain")

min <- c(-3.037, 35.937)
max <- c(-2.202, 37.000)
almeria_df <- as.matrix(data.frame(min, max))
row.names(almeria_df) <- c("x","y")





ggplot() +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .4,
          alpha = .8)+
  labs(title = "Zaragoza",
       subtitle = "Red de carreteras")

carreteras

ggplot() +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .4,
          alpha = .8)+
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza",
       subtitle = "Red de carreteras")

min <- c(-0.95, 41.6)
max <- c(-0.8, 41.7)
zgz_df <- as.matrix(data.frame(min, max))
row.names(zgz_df) <- c("x","y")

carreteras_B <- zgz_df%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("road", "motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = carreteras_B$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .4,
          alpha = .8) +
  theme_void() +
  labs(title = "Zaragoza",
       subtitle = "Red de carreteras")

calles <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .3,
          alpha = .8)+
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza")

ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .2,
          alpha = .8)+
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .8) +
  labs(title = "Zaragoza")


ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .2,
          alpha = .8)+
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .8) +
  labs(title = "Almería")




getbb("Hamburgo Germany")

carreterasHamburgo <- getbb("Hamburg Germany")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("road", "motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

carreterasElEjido

callesHamburgor <- getbb("Hamburg Germany")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()




ggplot() +
  geom_sf(data = callesHamburgo$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .2,
          alpha = .8)+
  geom_sf(data = carreterasHamburgo$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .8)+
  coord_sf(xlim = c(8.1, 10.3), 
           ylim = c(53.4, 54.1),
           expand = FALSE) +
  theme_void() +
  labs(title = "Hamburgo")


rios <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "canal")) %>%
  osmdata_sf()


riosHamburgo <- getbb("Hamburgo Germany")%>%
  opq()%>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "canal")) %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = callesVictor$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .2,
          alpha = .8)+
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .8)+
  geom_sf(data = rios$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .8,
          alpha = .8) +
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza")


carreterasElEjido <- getbb("Hamburg Germany")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("road", "motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()



callesVictor <- getbb("Hamburg Germany")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

riosHamburgo <- getbb("Hamburgo Germany")%>%
  opq()%>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "canal")) %>%
  osmdata_sf()



ggplot() +
  geom_sf(data = callesVictor$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .2,
          alpha = .8)+
  geom_sf(data = carreterasElEjido$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .8)+
  geom_sf(data = riosHamburgo$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .8,
          alpha = .8) +
  coord_sf(xlim = c(8.1, 10.3), 
          ylim = c(53.4, 54.1),
           expand = FALSE) +
  theme_void() +
  labs(title = "Hamburgo")


bici <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("cycleway")) %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .4,
          alpha = .8) +
  geom_sf(data = bici$osm_lines,
          inherit.aes = FALSE,
          color = "springgreen",
          size = .4,
          alpha = .6)+
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza",
       subtitle = "Carriles bici") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white"),
        plot.subtitle= element_text(colour = "white"))

santander <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature("name","Santander")%>%
  add_osm_feature("amenity","bank") %>%
  osmdata_sf()

estancos <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature(key = "shop", 
                  value = "tobacco") %>%
  osmdata_sf()

supermarket <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature(key = "shop", 
                  value = "supermarket") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .4,
          alpha = .8) +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .1,
          alpha = .8) +
  geom_sf(data = santander$osm_points,
          colour="red",
          fill="red",
          alpha=.6,
          size=2,
          shape=21) +
  geom_sf(data = rios$osm_lines,
          inherit.aes = FALSE,
          color = "lightblue",
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza",
       subtitle = "Sucursales del Banco Santander") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white"),
        plot.subtitle= element_text(colour = "white"))




ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .4,
          alpha = .8) +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .1,
          alpha = .8) +
  geom_sf(data = santander$osm_points,
          colour="red",
          fill="red",
          alpha=.6,
          size=2,
          shape=21) +
  geom_sf(data = rios$osm_lines,
          inherit.aes = FALSE,
          color = "lightblue",
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza",
       subtitle = "Bancos santander") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white"),
        plot.subtitle= element_text(colour = "white"))


ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .4,
          alpha = .8) +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .1,
          alpha = .8) +
  geom_sf(data = supermarket$osm_points,
          colour="red",
          fill="red",
          alpha=.6,
          size=2,
          shape=21)+
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza",
       subtitle = "Supermercados") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white"),
        plot.subtitle= element_text(colour = "white"))

industria <- getbb("Zaragoza Spain")%>%
  opq()%>%
  add_osm_feature(key = "industrial", 
                  value = "depot") %>%
  osmdata_sf()


igle <- available_tags("industrial")
head(igle)


ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .4,
          alpha = .8) +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .1,
          alpha = .8) +
  geom_sf(data = industria$osm_points,
          colour="red",
          fill="red",
          alpha=.6,
          size=2,
          shape=21)+
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Zaragoza",
       subtitle = "Supermercados") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white"),
        plot.subtitle= element_text(colour = "white"))




min <- c(-0.1672, 51.4787)
max <- c(-0.0072, 51.5396)
lnd_df <- as.matrix(data.frame(min, max))
row.names(lnd_df) <- c("x","y")


bike_london_2 <- lnd_df %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("cycleway")) %>%
  osmdata_sf()


calles_london_2 <- lnd_df%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

ggplot(bike_london_2$osm_lines) +
  geom_sf(colour="orange",
          alpha=.5,
          size=0.5,
          shape=21)+
  geom_sf(data = calles_london_2$osm_lines,
          inherit.aes = F,
          color = "grey40",
          size = .2,
          alpha = .5)+
  theme_void() +
  labs(title = "Londres") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white", size = 16, hjust = 0.5))




getbb("Almeria Spain")

min <- c(-3.037, 35.937)
max <- c(-2.202, 37.000)
almeria_df <- as.matrix(data.frame(min, max))
row.names(almeria_df) <- c("x","y")

bike_almeria <- almeria_df %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("cycleway")) %>%
  osmdata_sf()


calles_almeria <- almeria_df%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

ggplot(bike_almeria$osm_lines) +
  geom_sf(colour="orange",
          alpha=.5,
          size=0.5,
          shape=21)+
  geom_sf(data = calles_almeria$osm_lines,
          inherit.aes = F,
          color = "grey40",
          size = .2,
          alpha = .5)+
  theme_void() +
  labs(title = "Almería") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white", size = 16, hjust = 0.5))


m <- c(-10, 30, 5, 46)

#building the query
q <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("name", "Mercadona") %>%
  add_osm_feature("shop", "supermarket")

#query
mercadona <- osmdata_sf(q)

#final map
ggplot(mercadona$osm_points)+
  geom_sf(colour = "#08519c",
          fill = "#08306b",
          alpha = .5,
          size = 1,
          shape = 21)+
  theme_void()

#building the query
q <- getbb("Madrid") %>%
  opq() %>%
  add_osm_feature("amenity", "cinema")

str(q) #query structure

cinema <- osmdata_sf(q)
cinema

#our background map
mad_map <- get_map(getbb("Madrid"), maptype = "toner-background")

#final map
ggmap(mad_map)+
  geom_sf(data = cinema$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")






bbox <- getbb("Manhattan New York")
bbox

bbox_poly_manhattan <- st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>% 
  filter(boro_name == "Manhattan") 

bbox_manhattan <- st_bbox(bbox_poly_manhattan)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = bbox_poly_manhattan)


callesMan <- opq(bbox_manhattan) %>% 
  add_osm_feature(key = "highway")

callesMan

callesMan <- callesMan %>% 
  osmdata_sf()

callesMan <- callesMan$osm_lines

callesMan <- st_intersection(callesMan, bbox_poly_manhattan)

ggplot() +
  geom_sf(data = callesMan)

callesManla <- callesMan %>%
  mutate(lanes = as.numeric(lanes),
         lanes = ifelse(is.na(lanes), 1, as.numeric(lanes)))

callesManla

callesManla <-callesMan %>% 
  mutate(color=case_when(lanes=="1" ~ "midnightblue",
                         lanes=="2" ~ "darkorange1",
                         lanes=="3" ~ "red4",
                         lanes=="4" ~ "brown4",
                         lanes=="5" ~ "darkmagenta",
                         lanes=="6" ~ "yellow3",
                         lanes=="7" ~ "green4", 
                         lanes== "NA" ~ "aliceblue")) 

ggplot() +
  geom_sf(data = bbox_poly_manhattan, aes(), color="grey80", size = 0.4) +
  geom_sf(data = callesManla, aes(fill = lanes)) +
  geom_sf(data = callesManla, aes(color = lanes), size = 0.5) +
  scale_color_identity() +
  theme_void() +
  labs(title = "Manhattan - NYC",
       subtitle = "Vías de circulación",
       fill = "Cantidad de carriles",
       caption = "Fuente: OpenStreetMap")


cafeMan <- opq(bbox_manhattan) %>% 
  add_osm_feature(key = "amenity", value = "cafe")

cafeMan <- cafeMan %>% 
  osmdata_sf()

cafeMan <- cafeMan$osm_points

cafeMan <- st_intersection(cafeMan, bbox_poly_manhattan)

ggplot() +
  geom_sf(data = callesMan,
          color = "gray60", alpha = 0.3) +
  geom_sf(data = cafeMan, color = "steelblue4") + 
  theme_void() +
  labs(title = "NYC",
       subtitle = "Café",
       caption = "fuente: OpenStreetMap")

