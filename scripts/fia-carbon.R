#devtools::install_github('hunter-stanke/rFIA')
library(tidyverse)
library(rFIA)
library(sf)
library(dplyr)
library(tidyr)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(scales)
options(stringsAsFactors = FALSE)

state_poly = read_sf("data/vector/states.shp")
wa_poly = state_poly %>% filter(STATE=="Washington")

# Get FIA data
getFIA(states = c('WA'), dir = 'I:/GIS-Vector/FIA/', load = FALSE)

wa <- readFIA(dir = 'I:/GIS-Vector/FIA/', states = 'WA')

## Most recent subset
waMR <- clipFIA(wa)

## Group estimates by the areal units, and return as a dataframe
tpa_polys <- tpa(waMR, polys = wa_poly)

## Same as above, but return an sf mulitpolygon object (spatially enabled)
tpa_polysSF <- tpa(waMR, polys = wa_poly, returnSpatial = TRUE)

## Spatial plots with biomass
bio_pltSF <- carbon(waMR, byPlot = TRUE, returnSpatial = TRUE)

## Plot the results using default sf method
plot(bio_pltSF)

## Aboveground biomass/ acre (tons) for each plot
plot(bio_pltSF['CARB_ACRE'])

ggplot(data=filter(bio_pltSF, CARB_ACRE>0), aes(geometry=geometry, color=CARB_ACRE)) +
  geom_sf(size=0.5) +
  scale_color_viridis_c() +
  facet_wrap(~POOL)





##### IDW interpolation #####
# interpolate the data
crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
x = bio_pltSF %>% filter(POOL=="AG_LIVE")
x = st_as_sf(x)
x = st_transform(x, crs=crs)

r = raster(wa_poly)
r = projectRaster(r, res=25000, crs=crs)

library(gstat)
gs <- gstat(formula=CARB_ACRE~1, locations=x)
idw <- interpolate(r, gs)

idw = projectRaster(idw, bioag2020_wa)
idwr <- mask(idw, bioag2020_wa)
idwr = idwr*2.47105
plot(idwr)
plot(bioag2020_wa)

s = stack(idwr, bioag2020_wa)
sdf = as.data.frame(s, xy=T) %>% filter(!is.na(var1.pred)) %>% filter(stkg_441.it1.ts2020>0) %>%
  rename("fia" = "var1.pred", "lucas" = "stkg_441.it1.ts2020") %>%
  filter(lucas>10)
head(sdf)

ggplot(sdf, aes(x=lucas, y=fia)) +
  geom_jitter() +
  geom_smooth(method="lm")

r2 = lm(fia ~ lucas, data = sdf)
summary(r2)



# Load a LUCAS 2020 stock estimate
bioag2020 = datasheetRaster(myLibrary, "stsimsf_OutputSpatialStockGroup",
                          scenario = c(128),
                          timestep = c(2020),
                          iteration = 1,
                          subset = expression(grepl("Biomass: Aboveground", StockGroupID, fixed = TRUE)))

bioag2020_wa = crop(bioag2020, x)
bioag2020_wa = aggregate(bioag2020_wa, fact=25)
plot(bioag2020_wa)


fia_ag = bio_pltSF %>% filter(POOL=="AG_LIVE", PLOT_STATUS_CD==1, YEAR>2018)
fia_wa_extract = raster::extract(bioag2020_wa, fia_ag, na.rm=T, sp=T, buffer=1000, fun=mean)

fia_wa_extract = as.data.frame(fia_wa_extract) %>%
  filter(stkg_441.it1.ts2020>0) %>%
  mutate(CARB_Ha = CARB_ACRE * 2.47105)


ggplot(fia_wa_extract, aes(x=stkg_441.it1.ts2020, y=CARB_Ha)) +
  geom_jitter() +
  geom_smooth(method="lm")


z = lm(fia_wa_extract$CARB_Ha ~ fia_wa_extract$stkg_441.it1.ts2020)

# Function for Root Mean Squared Error
rmse <- function(error) { sqrt(mean(error^2)) }
rmse(z$residuals)
summary(z)


ggplot(fia_wa_extract, aes(x=coords.x1, y=coords.x2, color=CARB_Ha)) +
  geom_point()

ggplot(fia_wa_extract, aes(x=coords.x1, y=coords.x2, color=stkg_441.it1.ts2001)) +
  geom_point()







# Hexbin map
wa <- st_transform(wa_poly, crs)
hex25 <- st_make_grid(wa, cellsize=50000, what = 'polygons', square = FALSE, flat_topped = TRUE) %>%
  st_as_sf() %>%
  mutate(id=seq(1,nrow(hex25)))

# Process and filter FIA data
fia_ag = bio_pltSF %>% filter(POOL=="AG_LIVE", PLOT_STATUS_CD==1, YEAR>2018)
fia_ag = st_transform(fia_ag, crs)

# Extract FIA Aboveground values to hexagons
fia_ag_25 = st_join(hex25, fia_ag) %>%
  filter(YEAR==2019) %>%
  group_by(id) %>% summarise(CARB_ACRE=mean(CARB_ACRE)) %>%
  mutate(carb_ha = CARB_ACRE*2.47105) %>%
  rename("geometry"="x") %>%
  st_as_sf()

ggplot(fia_ag_25, aes(geometry=geometry, fill=carb_ha)) +
  geom_sf()


# Load a LUCAS 2020 stock estimate
bioag2020 = datasheetRaster(myLibrary, "stsimsf_OutputSpatialStockGroup",
                            scenario = c(128),
                            timestep = c(2020),
                            iteration = 1,
                            subset = expression(grepl("Biomass: Aboveground", StockGroupID, fixed = TRUE)))

bioag2020_wa = crop(bioag2020, hex25)
bioag2020_wa[bioag2020_wa==0] = NA
plot(bioag2020_wa)

# Create hexagon zonal attributes from LUCAS estimate
lucas_hex = raster::extract(bioag2020_wa, hex25, na.rm=T, sp=T, fun=mean)
lucas_hex = st_as_sf(lucas_hex) %>%
  rename("lucas"="stkg_441.it1.ts2020")

ggplot(lucas_hex, aes(geometry=geometry, fill=lucas)) +
  geom_sf()

# Join LUCAS and FIA hexagons
ag = st_join(fia_ag_25, lucas_hex, left=T) %>% filter(id.x==id.y) %>%
  mutate(diff = lucas-carb_ha)

ggplot(ag, aes(geometry=geometry, fill=diff)) +
  geom_sf() +
  scale_fill_fermenter(palette="Spectral")

ggplot(ag, aes(x=lucas, y=carb_ha)) +
  geom_point() +
  geom_smooth(method="lm")

reg = lm(carb_ha ~ lucas, data=ag)
summary(reg)
