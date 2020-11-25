

livecarbon = raster("data/raster/Biomass Total.tif")
livecarbon = aggregate(livecarbon, fact=4)
livecarbon_df = as.data.frame(livecarbon, xy=T) %>% filter(!is.na(Biomass_Total)) %>%
  rename("Amount"="Biomass_Total")

no_classes <- 8
quantiles <- quantile(livecarbon_df$Amount, probs = seq(0, 1, length.out = no_classes + 1))
live_breaks = c(15,30,45,60,90,120,150)
live_min = min(livecarbon_df$Amount)
live_max = max(livecarbon_df$Amount)
live_labels = c(live_breaks)

ggplot() +
  geom_sf(data=land_poly, aes(geometry=geometry), fill="grey99", color=NA) +
  geom_tile(data=livecarbon_df, aes(x=x, y=y, fill=Amount)) +
  geom_sf(data=states_poly, aes(geometry=geometry), fill=NA, color="grey20", size=0.2) +
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs",
           xlim=c(min(agemap_df$x), max(agemap_df$x)),
           ylim=c(min(agemap_df$y), max(agemap_df$y))) +
  theme(legend.position = "bottom") +
  theme_ipsum_rc() +
  labs(x=NULL, y=NULL, subtitle="Living Biomass Carbon Storage", fill=expression(Metric~Tons~C~ha^-1)) +
  scale_fill_fermenter(palette = "PuBuGn", type="seq", direction=1, guide="colorsteps", breaks=live_breaks) +
  theme(
    legend.position = c(0.2,0.1),
    legend.text.align = 0,
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_rect(size=0.2, fill=NA),
    panel.background = element_rect(fill="grey90"),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.ticks.length = unit(2, "mm"),
    legend.background = element_rect(fill="white", color=NA),
    legend.key.height = unit(2, "mm"),
    legend.key.width = unit(10, "mm")) +
  guides(fill = guide_colorsteps(direction = "horizontal",
                                 title.position = 'top',
                                 title.hjust = 0,
                                 label.hjust = 1,
                                 nrow = 1,
                                 byrow = T,
                                 reverse = F,
                                 label.position = "bottom"))


domcarbon = raster("data/raster/DOM Total.tif")
domcarbon = aggregate(domcarbon, fact=4)
domcarbon_df = as.data.frame(domcarbon, xy=T) %>% filter(!is.na(DOM_Total)) %>%
  rename("Amount"="DOM_Total")

no_classes <- 8
quantiles <- quantile(domcarbon_df$Amount, probs = seq(0, 1, length.out = no_classes + 1))
dom_breaks = c(80,100,120,140,180,220,260,300)
dom_min = min(domcarbon_df$Amount)
dom_max = max(domcarbon_df$Amount)
dom_labels = c(dom_breaks)

ggplot() +
  geom_sf(data=land_poly, aes(geometry=geometry), fill="grey99", color=NA) +
  geom_tile(data=domcarbon_df, aes(x=x, y=y, fill=Amount)) +
  geom_sf(data=states_poly, aes(geometry=geometry), fill=NA, color="grey20", size=0.2) +
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs",
           xlim=c(min(agemap_df$x), max(agemap_df$x)),
           ylim=c(min(agemap_df$y), max(agemap_df$y))) +
  theme(legend.position = "bottom") +
  theme_ipsum_rc() +
  labs(x=NULL, y=NULL, subtitle="Dead Organic Matter Carbon Storage", fill=expression(Metric~Tons~C~ha^-1)) +
  scale_fill_fermenter(palette = "YlOrBr", type="seq", direction=1, guide="colorsteps", breaks=dom_breaks) +
  theme(
    legend.position = c(0.2,0.1),
    legend.text.align = 0,
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_rect(size=0.2, fill=NA),
    panel.background = element_rect(fill="grey90"),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.ticks.length = unit(2, "mm"),
    legend.background = element_rect(fill="white", color=NA),
    legend.key.height = unit(2, "mm"),
    legend.key.width = unit(10, "mm")) +
  guides(fill = guide_colorsteps(direction = "horizontal",
                                 title.position = 'top',
                                 title.hjust = 0,
                                 label.hjust = 1,
                                 nrow = 1,
                                 byrow = T,
                                 reverse = F,
                                 label.position = "bottom"))
