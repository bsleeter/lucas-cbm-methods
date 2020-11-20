Spatially explicit estimates of carbon stocks and fluxes for the United
States: a new approach linking the LUCAS and CBM-CFS3 models
================
Benjamin M. Sleeter
<<<<<<< HEAD
2020-11-20

Running Title: Ecosystem carbon balance in US Forests

Benjamin M. Sleeter<sup>1</sup>\*, Leonardo Frid<sup>2</sup>, Bronwyrn
Rayfield<sup>2</sup>, Paul C. Selmants<sup>3</sup>, Jinxun
Liu<sup>3</sup>, Colin J. Daniel<sup>2</sup>

#### Author Affiliations

<sup>1</sup>U.S. Geological Survey, Seattle, WA, USA;
<bsleeter@usgs.gov>; (253) 343-3363  
<sup>2</sup>Apex Resource Management Solutions Ltd., Ottawa, ON, CAN  
<sup>3</sup>U.S. Geological Survey, Menlo Park, CA, USA

## Abstract

**Background**

**Results**

**Conclusions**

**Keywords**

## Background

P1: Paragraph discussing the importance and need for ecosystem carbon
balance estimates and projections. • What are the important
considerations (e.g. LULC change, disturbances, climate variability and
change)? • Why are these estimates needed (e.g. meeting local to global
ghg-reduction targets). • What are the existing limitations of current
approaches other than the CBM and LUCAS models? P2: Review of the
CBM-CFS2 approach and its major limitations. P3: Review of the LUCAS
approach and its major limitations. P4: Describe in high-level terms
what the aim of this study was.

This research describes a new approach to estimating land use and land
cover change and their effect on ecosystem carbon dynamics by linking
the CBM-CFS3 and LUCAS models to produce spatially explicit projections
of ecosystem carbon balance in forested ecosystems of the conterminous
United States.

The LUCAS model (Land Use and Carbon Scenario Simulator) is a spatially
explicit state-and-transition simulation model (Daniel et al, 2016;
Daniel et al., 2018) developed to assess the impact of land use and land
cover change, ecosystem disturbance, and climate change on carbon stocks
and fluxes. LUCAS has been used to explore and impacts of a range of
plausible future scenarios on land use and carbon stocks (Sleeter et
al., 2018, Sleeter et al., 2019). LUCAS runs within the SyncroSim
software modeling environment (<http://syncrosim.com>).

The CBM-CFS3 model (Carbon Budget Model of the Canadian Forest Sector
v3) is an IPCC Tier 3 spatially referenced model of carbon stocks and
fluxes which can be applied at multiple scales (Kurz et al., 2009). The
CBM-CFS3 model estimates the impacts of a range of land-use and
disturbances on carbon stocks and fluxes. The approach relies on the
conversion of merchantable volume curves (user supplied) into carbon
biomass through the use of expansion factors. Turnover of live carbon
and decay and decomposition of dead organic matter (DOM) are specified
for each forest type and/or at a regional scale. CBM-CFS3 has been
applied and adapted in many other countries using regionally-specific
parameters. It’s transparent approach and robust methodology make it an
idea model to adapt for the U.S. forest sector.

## Results

## Discussion

## Conlucsion

## Methods

Model Structure To estimate carbon stocks and fluxes for CONUS, we
linked the CBM-CFS3 spatially referenced model of ecosystem carbon
dynamics with the LUCAS spatially explicit model of LULC change. The
CBM-CFS3 model was used to generate a set of forest species and
ecoregion-level carbon stock and flux rates which were then used within
the LUCAS model to produce spatially explicit maps of carbon stocks and
fluxes based on LULC change, disturbances, forest ageing, and climate
variability.

The conterminous U.S (CONUS) was partitioned into a regular grid of 1-km
x 1-km cells where each cell was assigned to a discrete
land-use/land-cover classes (LULC). The initial land cover map was based
on the 2001 National Land Cover Database (NLCD). We further modified the
classification system to partition the three NLCD forest classes into
forest type-groups based on the U.S. Forest Service classification
system. Furthermore, areas classified as shrubland or herbaceous
grassland in NLCD were assigned new classes based on the Landfire
Existing Vegetation Type (EVT) classification system. In total, the
LUCAS STSM contained 61 unique LULC classes (Fig 1). We modeled
transitions between state classes to represent major LULC change
processes, including urbanization, agricultural expansion and
contraction, forest management (i.e. clear-cut and selection harvest),
wildfire, and insect damage. For wildfire and insect damage, we
partitioned transition events into low, medium and high severity events.
For land use transitions (urbanization, agricultural expansion and
contraction), we generated spatial maps of the location of changes based
on the NLCD time-series maps for the years 2001, 2006, 2011, and 2016.
The 5-year change rates were then annualized, and the model was
parameterized to stochastically select cells to transition from one
class to another within the 5-year period. For the forest management and
disturbance transitions, we used the annual time-series maps from the
Landfire Disturbance database to identify areas of change. For
disturbance transitions, we assumed that all cells would transition back
to their original LULC class in the year following the disturbance
event. Forest mortality resulting from drought and/or insects were
derived from U.S. Forest Service Aerial Detection Surveys and were
modeled the same was as fire and harvest transitions.

We adopted the carbon stock and flux structure of the CBM-CFS3 model for
this study. The approach includes the use of five live carbon pools
(including above- and belowground pools) and 9 dead organic matter (DOM)
pools covering standing dead trees, down deadwood, litter and carbon
stored in soils. DOM pools are organized and named based on their rate
of decay (e.g. very fast, fast, medium, slow). Carbon transfers between
pools followed the same convention as the CBM-CFS3 model with two
important modifications. First, rather than using net growth increment,
we calculated net primary production (NPP) for each forest type-group
and age and used this to drive annual gross carbon accumulation. Second,
we introduced annual spatial multipliers to scale NPP based on
variations in local climate conditions. These modifications are
discussed in more detail below.

### CBM-CFS3 reference simulations

We used the CBM-CFS3 model (version 1.2) to generate estimates of carbon
stocks across all live and DOM pools for a 1-ha representative stand for
each forest type-group. Each forest type-group was assigned to the
closest forest type found in the CBM database; when no direct type was
available we used generic hardwood or softwood types from CBM-CFS3.
Additionally, each species was assigned to a representative ecozone and
administrative boundary. We assumed wildfire was the historic stand
replacing disturbance type and was the most recent disturbance while
using default historic return intervals from CBM-CFS3. No additional
disturbances were modeled for the reference simulation. Lastly, we
calculated the mean temperature across the range of each forest
type-group, using the value to modify the CBM-CFS3 reference simulation.

#### Merchantable volume curves

The CBM-CFS3 model relies on users to provide merchantable volume curves
for each tree species modeled. For each forest type-group, parameters
used to estimate merchantable volume were queried from the U.S. Forest
Service Forest Inventory and Analysis (FIA) database. We used the Von
Bertalanffy growth equation to estimate merchantable volume by age as:

\[ y = a * {1-} exp{(-b * age)}^3 \]

where *y* is the merchantable volume, *a* is the asymptote and *b* is
the rate of approach to the asymptote. The resulting estimates of
merchantable volume by age (0-300 years), along with default biomass
expansion factors from the CBM-CFS3 database, were used as inputs for
the reference simulations. The CBM-CFS3 model was then run on an annual
timestep for 300 years to estimate the amount of carbon stored in each
pool by age by applying species-specific expansion factors to estimate
biomass in each tree component. A carbon fraction was applied to
estimate the carbon portion of the biomass stock. We used the default
biomass expansion factors and carbon proportions from the CBM-CFS3
database.

![](lucas-cbm-methods-draft-v1_files/figure-gfm/crosswalk-1.png)<!-- -->

### Carbon flow rates: LUCAS Flow Pathways module

We developed a sub-module within LUCAS to calculate carbon flux
parameters based on output from the reference simulations and the
CBM-CFS3 database. The module uses the species crosswalk table from
above, along with crosswalk tables linking carbon stocks and disturbance
types used in LUCAS and CBM-CFS3. Within the LCUAS model we specified a
set of carbon Flow Pathways defining all of the carbon pools and fluxes
consistent with those used in the CBM-CFS3.

#### Net Primary Productivity

The module first uses results from the CBM-CFS3 reference simulation to
calculate net primary productivity (NPP) as the sum of net growth and
biomass turnover for a given age:

\[ NPP = \sum_{a} G_{(f,b,s,fr,cr)} + \sum_{a-1} T_{(f,b,s,fr,cr)}\]
where \(G\) is net growth, \(T\) is biomass turnover, \(f\) is foliage,
\(b\) is branches and other wood, \(s\) is merchantable stems, \(fr\) if
fine roots, \(cr\) is coarse roots and \(a\) is forest age. Thus,
outputs consist of an estimate of NPP by age (up to 300 years old) and
are stored as state attributes for each state class type (i.e. forest
type-group). Results are stored as a State Attribute within the LUCAS
model.

Output from the CBM-CFS3 reference simulation also allows us to
calculate the proportional allocation of NPP, by age, between the five
live tree component
stocks.

\[ pNPP_{s_i}^{a} = \frac{G_{s_i}^{a} + (S_{s_i}^{{a-1}} * T_{s_i})} {NPP^a}  \]
where \(pNPP_{s_i}^{a^t}\) is the proportion of NPP allocated to one of
five live carbon pools (\(s_i\)) for a given aged forest (\(a^t\));
\(G_{s_i}^{a^t}\) is the net growth of carbon pool \(i\) at age \(t\);
\(S_{s_i}^{a^{t-1}}\) is the amount of carbon stored in pool \(i\) at
age \(t-1\), \(T_{s_i}\) is the species and region specific carbon
turnover rate, and \(NPP^a\) is the total species and region specific
NPP for each forest age. Results are stored as Flow Multipliers within
the LUCAS model and used to allocate annual NPP State Attribute values
to each live tree component.

#### Carbon flux rates

The LUCAS model uses a set of flow multipliers to estimate carbon fluxes
representing biomass turnover, decay and decomposition, emissions, and
transfers of carbon resulting from LULC change and disturbance. The
LUCAS Flow Pathwyas module uses the set of crosswalk tables described
above to parameterize a flow multipliers table for each unique
combination of forest type-group and ecozone. Flow multipliers specify
the annual rate and proportion of carbon transferred from one stock type
to another. The ordering of carbon fluxes within the LUCAS model was
established to match that of the CBM-CFS3.

1.  Transfer of snag stems and branches to down deadwood pools
    (aboveground medium and fast, respectively),
2.  Emission and decay of standing (snag stems and branches) and down
    deadwood (aboveground medium),
3.  Emission from the belowground slow pool,
4.  Biomass turnover from live to DOM pools
5.  Emission and decay from DOM pools (belowground very fast,
    belowground fast, aboveground very fast, aboveground fast)
6.  Emission from aboveground slow
7.  Transfer from aboveground slow to belowground slow
8.  Growth of live pools.

In addition to the base carbon flux rates, the LUCAS Flow Pathways
module also parameterzies a set of transition-based flow multipliers
which control the rate at which carbon is transferred between pools when
a change in LULC or disturbance occurs. Transition triggered flows are
implemented at the end of each timestep after all base flows have
occured. For this study, we considered the effects of fire (high,
medium, and low severity), harvest (clearcut and selection),
drought/insect mortality, urbanization, agricultural expansion
(i.e. deforestation), and agricultural contraction
(i.e. reforestation). Flow rates were imported from transition
matricies from the CBM-CFS3 model.

### Model validation

Within the LUCAS model we ran a 300 year simulation with all cells
starting at age 0 using the carbon flux rates to estimate changes in
stocks developed in the previous step. A single 1-ha representative
stand was run for each forest group-type. No disturbances were
simulated. We compared individual carbon stock output from the LUCAS
simulation with the original output from the CBM-CFS3 runs to ensure we
could reliably reproduce results.

### Spin-up of Dead Organic Matter (DOM) Pools

We used the LUCAS model to spin-up DOM pools over a 3000 year simulation
period under two assumptions. Fire was assumed as the historical stand
replacing disturbance in both scenarios with a replacement interval
specified for each forest type-group and ecozone. We then imposed both
wildfire and clearcut harvest as the most recent disturbance and ran the
model for another 300 years in order to generate carbon over age look-up
tables for each assumption (harvest and clearcut as last disturbances).
The final 300 years worth of data were used to initialize carbon for
each land cell in the conterminous U.S..

### Mapping Initial Carbon Stocks in 2001

Results from the LUCAS spin-up were used to initialize carbon stocks for
each 1-km x 1-km cell in the conterminous U.S. for the year 2001. State
class maps (LULC + forest type-group) and forest age were developed and
each cell was initialized with carbon stocks for both fire and clearcut
harvest as the last disturbance. We used the National Interagency Fire
Consortium’s map of historical fire perimeters to identify cells which
should have their final initialization based on fire; if a cell was not
previously burned it was initiated with stocks from the clearcut harvest
simulation.
=======
2020-11-19

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](lucas-cbm-methods-draft-v1_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
>>>>>>> c9e36e9ee93091f889851cb51a16dac756293e41