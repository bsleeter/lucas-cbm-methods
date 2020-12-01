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

state_poly = read_sf("/Users/bsleeter/Documents/GitHub/lucas-cbm-methods/data/vector/states.shp")
wa_poly = state_poly %>% filter(STATE=="Washington")

# Get FIA data
getFIA(states = c('WA'),
       dir = 'Users/bsleeter/Downloads/R/',
       load = FALSE)

wa <- readFIA(dir = '/Users/bsleeter/Downloads/R/WA', states = 'WA')

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
r = projectRaster(r, res=10000, crs=crs)
plot(d)

library(gstat)
gs <- gstat(formula=CARB_ACRE~1, locations=x)
idw <- interpolate(r, gs)
## [inverse distance weighted interpolation]
idwr <- mask(idw, r)
plot(idw)
hist(idw)
