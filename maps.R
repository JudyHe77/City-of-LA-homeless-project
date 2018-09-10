library(ggplot2)
library(ggmap)



map <- get_googlemap("los angeles", markers = df, path = df, scale =2)

?get_googlemap

?revgeocode


map <- get_googlemap("los angeles", zoom = 10,
              size = c(640, 640), scale = 2, format = "png8", maptype = c("satellite"), language = "en-EN", sensor = FALSE, messaging = FALSE,
              urlonly = FALSE, filename = "ggmapTemp", color = c("color", "bw"),
              force = TRUE)


ggmap(map, extent = 'device')


?ggmap



map %>% qmplot(LONGITUDE, LATITUDE, data=crime$CRIME.CODE.DESCRIPTION, maptype="toner_lite", color=CRIME.CODE.DESCRIPTION, legend="topleft")


qmplot(LONGITUDE, LATITUDE, data = df, maptype = "toner-lite",
       color = offense, size = offense, legend = "topleft"
) +
  scale_colour_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
  scale_size_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder"),
                      range = c(1.75,6)) +
  guides(size = guide_legend(override.aes = list(size = 6))) +
  theme(
    legend.key.size = grid::unit(1.8,"lines"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  labs(colour = "Offense", size = "Offense")