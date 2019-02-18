---
title: "NEXUS Lanes and Local Economies (Prospect)"
author: "Cory Briar"
date: "2/7/2019"
output: 
  html_document:
    theme: flatly
    highlight: haddock 
    # code_folding: show
    toc: yes
    toc_depth: 4
    toc_float: yes
    keep_md: true
---





## Overview

#### Research Question
What is the impact of the implementation of NEXUS Border Crossing Lanes on business failures in US regions near the US-Canada border?

#### Hypothesis
To the extent that those who use NEXUS lanes are "day-trippers" looking to take advantage of price differentials between the US and Canada, we expect to see that large, non-local retailers (i.e. Costco, Walmart, Target) are the most likely to obtain the benefits of that commerce. While business is not a zero-sum game played between large and small companies, we might expect that this shift in clientele could adversely affect local small businesses in these areas. 

#### Goals
1. Contact CBP about NEXUS Lanes process (Who gets one, when, how?) -- Done
2. Import shape files from census bureau, integrate into single data frame; plot/investigate the data, etc. 
3. Obtain data on business failures by size, each county, each year 
4. Postulate a good regression equation

## Goal 1

I have reached out to Customs and Border Protection via email, but have yet to receive a response. Next time, I will put "SUSPECTED REFUGEE" in the subject line. 

## Goal 2



```r
# Bring in variables of interest with geometry

var_list = c(
  "B02001_001", # Population 
  "B19013_001", # Median Household Income
  "B02001_002", # White Population
  "B07009_005", # Population with Bachelor's or higher
  "B17001_001", # Population with income in last 12 months be
  "B00001_001" # Population in sample
)
cens <- get_acs(geography = "county", variables = var_list, year = 2010, output = "wide", geometry = T)
cens$year = rep(2010, 3221)
cens10 <- cens

# Repeat for all years
span <- 2011:2016

for(t in span){
  temp <- get_acs(geography = "county", variables = var_list, year = t, output = "wide", geometry = T)
  lngth <- length(temp$GEOID)
  temp$year = rep(t, lngth)
  cens <- rbind(cens, temp)
}
```


```r
# Eliminate margin of error columns
 cens <- cens %>% select(
   "GEOID",
   "NAME",
   "year",
   "B02001_001E", # Population
   "B19013_001E", # Median Household Income
   "B02001_002E", # White Population
   "B07009_005E", # Population with Bachelor's or higher
   "B17001_001E", # Population with income in last 12 months be
   "B00001_001E", # Survey Count
   "geometry" # Shape files
   )
 cens <- cens %>% rename(
   id = GEOID, # County id
   name = NAME, # County, State
   year = year, # Year of obs.
   pop = B02001_001E, # Population
   medinc = B19013_001E, # Median Household Income
   white_count =  B02001_002E, # White Population
   bach_count = B07009_005E, # Population with Bachelor's or higher
   pov_count = B17001_001E, # Population with income in last 12 months below poverty line
   surv_count = B00001_001E,  # Survey Count
   geometry = geometry # Duh
 )
```


```r
# Import border crossing data from shapefile
cross <- st_read("Canada_and_Mexico_Border_Crossings.shp")
```

```
## Reading layer `Canada_and_Mexico_Border_Crossings' from data source `/Users/bigfoot13770/Documents/UO ECON PROGRAM/REPRIEVE/nexus_lane_proj/nexus_lane/Canada_and_Mexico_Border_Crossings.shp' using driver `ESRI Shapefile'
## Simple feature collection with 171 features and 21 fields
## geometry type:  POINT
## dimension:      XY
## bbox:           xmin: -141.0014 ymin: 25.88342 xmax: -66.98008 ymax: 64.08552
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```r
# Eliminate Mexican border crossings, irrelevant vars.
cross <- cross %>%
   select(FID, PortCode, PortName, State, CP_Name, UNIQUEID, Personal_V, PV_Passeng, CLASS, geometry) %>%
   filter(!paste(State) %in% c("CA","AZ","NM","TX","AK")) 
# Convert cross$geometry to same as cens$geometry
cross$geometry <- cross$geometry %>% st_transform_proj(crs = "+proj=longlat +datum=NAD83 +no_defs")
cross
```

```
## Simple feature collection with 121 features and 9 fields
## geometry type:  POINT
## dimension:      XY
## bbox:           xmin: -123.4304 ymin: 42.31182 xmax: -66.98008 ymax: 49.00239
## epsg (SRID):    NA
## proj4string:    +proj=longlat +datum=NAD83 +no_defs
## First 10 features:
##    FID PortCode                       PortName State               CP_Name
## 1    1     3803               Sault Ste. Marie    MI      Sault Ste. Marie
## 2    2     3802                     Port Huron    MI            Port Huron
## 3    3     3801      Detroit-Ambassador Bridge    MI               Detroit
## 4    4     0901  Niagara Falls-Lewiston Bridge    NY Buffalo-Niagara Falls
## 5    5     0901 Niagara Falls-Whirlpool Bridge    NY Buffalo-Niagara Falls
## 6    6     0901   Niagara Falls-Rainbow Bridge    NY Buffalo-Niagara Falls
## 7    7     0901           Buffalo-Peace Bridge    NY Buffalo-Niagara Falls
## 8    8     0708                 Alexandria Bay    NY        Alexandria Bay
## 9    9     0701                     Ogdensburg    NY            Ogdensburg
## 10  10     0704                        Massena    NY               Massena
##    UNIQUEID Personal_V PV_Passeng CLASS                   geometry
## 1      3803    1003967    1856527     1 POINT (-84.36072 46.50844)
## 2      3802    2037430    4018331     1  POINT (-82.4236 42.99852)
## 3      3804          0          0     2 POINT (-83.07406 42.31182)
## 4      0904          0          0     2 POINT (-79.04446 43.15305)
## 5      0903          0          0     2 POINT (-79.05834 43.10924)
## 6      0902          0          0     2  POINT (-79.0677 43.09018)
## 7      0901    5847483   12553685     1 POINT (-78.90595 42.90694)
## 8      0708     674294    1494236     1 POINT (-75.98359 44.34723)
## 9      0701     388548     765758     1 POINT (-75.45775 44.73309)
## 10     0704     901987    1642028     1   POINT (-74.7395 44.9906)
```

### Problem
Since the lat-long's for county outlines and border crossings are from different sources, they don't over lap perfectly; thus, some border crossings are not contained by ANY US county. 

```r
# Assemble
US <- st_union(cens10$geometry)
```


```r
cross$in_us <- rep(0,length(cross$UNIQUEID))
for(c in 1:length(cross$UNIQUEID)){
   cross$in_us[c] <- ifelse(
      st_intersects(cross$geometry[c], US, sparse = F)[1,1] == TRUE,
      1, 0)
}

cross$in_us
```

```
##   [1] 1 1 0 1 1 1 0 1 1 0 0 0 0 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1
##  [36] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 0 1 1 0 0 1 1 0 0 0 0 1 0 0
##  [71] 0 1 1 1 1 1 1 1 0 1 1 0 0 0 0 0 0 1 0 1 1 0 1 1 1 1 1 0 1 1 1 0 0 0 1
## [106] 0 1 0 0 0 1 1 0 1 0 1 1 1 0 0 1
```

###Solution
They're close enough that a small lat-long buffer around the set of border crossings should take care of the issue (around 5 miles). This may mean that some counties that don't have a crossing may get labeled as having one if it's sufficiently close. I will check for this. 


```r
# Create dummy variable for county containing land crossing

b_x <- st_union(cross$geometry[1:121])
cens10$xing <- rep(0,3221)

for(c in 1:length(cens10$GEOID)){
  cens10$xing[c] <- ifelse(st_intersects(b_x, cens10$geometry[c], sparse = F)[1,1] == TRUE, 1, 0) 
}
```

 check
## Goal 3

In progress


## Goal 4

The key estimating equations shall be of the form

$$FAIL_{it}^q = \alpha + \delta_1TRIP_i+ \delta_2T_t + \delta_3(TRIP_i\cdot T_t) + D_{it}'\beta + \gamma_i + \tau_t + \varepsilon_{it} $$
Where $t = 0$ is the year of treatment for county $i$, and
$$
\begin{align*}
FAIL_{it}^q &\equiv \textrm{ number of business failures in county } i \textrm{ and year } t \textrm{ of size } q\\
TRIP_i &\equiv \textrm{dummy variable equal to 1 if the border of a county } i \textrm{ is within a "daytrip" distance of a NEXUS Lane}\\
T_t &\equiv \textrm{ dummy variable equal to 0 if } t<0 \textrm{ and 1 if } t\geq0.\\
D_{it} &\equiv\text { vector of demographic variabels such as median income, education, sector composition, etc.}\\
(\gamma_i, \tau_t) &\equiv \textrm{county and year fixed effects, respectively}\\
\end{align*}
$$
Given that small and large businesses within a county may be mutually dependent on each other for intermediate goods, clientele, etc., we would expect that the error terms between first of size $q$ are correlated. Thus, our preferred method of estimation will be seemingly unrelated regression. Apart from that note, it should be apparent that the regression will be a difference in difference estimator of the change in business failings between TRIP and non-TRIP counties before and after treatment, with $\delta_3$ being the diff-in-diff estimator. 







