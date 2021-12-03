#add letters to each panel

library(cowplot)
library(dplyr)
library(grid)
library(gridGraphics)
library(readr)
library(tidyr)

dat <- read_csv('analysis/data/2TrackSummaries 2021_09_30.csv') %>%
  arrange(ArrivalDSP) %>%
  mutate(i = row_number())

logo <- magick::image_read('analysis/figures/8Seal.PNG')

#################################################################### PANEL A

theme_white <- function(base_size = 12, base_family = "") {

  theme_grey(base_size = base_size, base_family = base_family) %+replace%

    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size, color = "black", lineheight = 0.9),
      axis.text.y = element_text(size = base_size, color = "black", lineheight = 0.9),
      axis.ticks = element_line(color = "black", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "black", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "black", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.position = "none",
      # Specify panel options
      panel.background = element_rect(fill = "white", color  =  NA),
      panel.border = element_rect(fill = NA, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text.x = element_text(size = base_size*0.8, color = "black"),
      strip.text.y = element_text(size = base_size*0.8, color = "black",angle = -90),
      # Specify plot options
      plot.background = element_rect(color = "white", fill = "white"),
      plot.title = element_text(size = base_size*1.2, color = "black"),
      plot.margin = unit(c(0, 0, 0, 0), "cm")

    )

}

library(viridis)

#load in time lat long
TLL <- read_csv("analysis/data/1TLL TV3 2021_09_30.csv") %>%
  #convert Lon-180:180 to Lon360
  mutate(Lon = Lon - ifelse(Lon > 0, 360, 0))

#next step, edit this to add points
library(ggmap)
library(mapdata)
library(patchwork)

ylim <- c(32,62) #find latitude limits of data to inform map
xlim <- c(-190,-121) #find longitude limits of data to inform map

#world map data
w <- map_data("worldHires", ylim = ylim, xlim = xlim) #extract map data

#make the plot
(fig1a <- ggplot() +
  geom_path(TLL,mapping=aes(x=Lon,y=Lat,group=TOPPID),colour="grey40",size=.2)+
  geom_point(dat,mapping=aes(x=TurnaroundLon,y=TurnaroundLat),color="black",size=2.3)+
  geom_point(dat,mapping=aes(x=TurnaroundLon,y=TurnaroundLat,color="Turnaround"),size=1.6)+
  geom_polygon(data=w,aes(x=long,y=lat,group=group),fill="grey10")+
  geom_point(dat,mapping=aes(x=-122,y=37.12),color="white",fill="black",pch=22,size=6)+
  annotate("text", x = -191, y = 62.5, hjust = 0, vjust = 1, label = "A", fontface="bold") +
  labs(x = "Longitude (°)",
       y = "Latitude (°)",
       color = "") +
  coord_fixed(1.5, xlim = xlim, ylim = ylim) +
  scale_color_manual(values = "goldenrod") +
  theme_white() +
  draw_image(logo, x = -180, y = 58, scale = 17,interpolate = TRUE))

################################################ PANEL B

fig1b_palette <- c(
  DepartureDSP = "#414487FF",
  TurnaroundDSP = "goldenrod",
  ArrivalDSP = "#22A884FF"
)

dat_long <- dat %>%
  pivot_longer(c(DepartureDSP, TurnaroundDSP, ArrivalDSP),
               names_to = "phenology",
               values_to = "DSP")

fig1b <- ggplot(dat_long, aes(i, DSP)) +
  geom_segment(aes(xend = i, y = DepartureDSP, yend = ArrivalDSP),
               data = dat) +
  geom_point(aes(fill = phenology), shape = 21) +
  annotate("text", x = 1, y = 50, label = "B", hjust = 0, vjust = 1, fontface = "bold") +
  scale_fill_manual(values = fig1b_palette) +
  scale_x_continuous("Seals", breaks = 1:max(dat_long$i)) +
  scale_y_continuous("Days Before Parturition",
                     limits = c(-250, 50),
                     breaks = seq(-250, 50, by = 50),
                     labels = c(-250, "", -150, "", -50, 0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "none")

fig1c <- ggplot(dat_long, aes(DSP)) +
  geom_density(aes(fill = phenology), size = 0.2) +
  annotate("text",
           x = c(-25, -110, -195),
           y = rep(0.21, 3),
           label = c("Arrival", "Turnaround", "Departure"),
           color = rev(fig1b_palette),
           hjust = 1) +
  annotate("text", x = 50, y = 0, label = "C", hjust = -1, vjust = 1, fontface = "bold") +
  scale_x_continuous(limits = c(-250, 50),
                     breaks = seq(-250, 50, by = 50)) +
  scale_y_continuous(limits = c(0, 0.23),
                     expand = c(0, 0)) +
  scale_fill_manual(values = fig1b_palette) +
  coord_flip() +
  labs(y = "Density") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

fig1d <- ggplot(dat, aes(TurnaroundDistanceKM, TurnaroundDSP)) +
  geom_point(shape = 21, bg = "goldenrod") +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE,
              linetype = 2,
              color = "black") +
  annotate("text", x = 105, y = 40, label = "D", hjust = 0, vjust = 1, fontface = "bold") +
  scale_x_continuous("Turnaround Distance (km)",
                     breaks = seq(0, 5000, by = 1000)) +
  scale_y_continuous("Turnaround Date\n(Days Before Parturition)",
                     limits = c(-250, 50),
                     breaks = seq(-250, 50, by = 50),
                     labels = c(-250, "", -150, "", -50, 0, 50)) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

fig1e <- ggplot(dat, aes(DriftRateSwitchDOY, TurnaroundDSP)) +
  geom_point(shape = 21, bg = "goldenrod") +
  annotate("text", x = 231, y = 40, label = "E", hjust = 1, vjust = 1, fontface = "bold") +
  scale_x_continuous("Drift Rate Change Day of Year",
                     breaks = seq(240, 340, by = 20)) +
  scale_y_continuous(limits = c(-250, 50),
                     breaks = seq(-250, 50, by = 50)) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())


fig1 <- plot_grid(
  fig1a,
  plot_grid(fig1b, fig1c, fig1d, fig1e,
            nrow = 2, align = "hv", axis = "tblr"),
  nrow = 2
)
ggsave("analysis/figures/fig1.png", height = 8, width = 6)
