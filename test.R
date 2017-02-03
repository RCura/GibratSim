######################################
##### GABAIX LOG(RANK - 1/2) OLS #####
######################################

BRICS %>%
    semi_join(group_by(.,system) %>% summarise(year = max(year)),
              by = c("system", "year")) %>%
    filter(!is.na(pop), pop >= 10E3) %>%
    group_by(system) %>%
    mutate(rank = min_rank(-pop),
           nbCities = n()) %>%
    group_by(system, year, nbCities) %>%
    tidyr::nest() %>%
    mutate(model = map(data, ~lm(log(pop) ~ log(rank), data = .)),
           slope = map(model, tidy) %>% map(., "estimate") %>% map_dbl(., ~ .[2]),
           R2 = map(model, glance) %>% map_dbl(., ~.[["r.squared"]]),
           ConfInt = map(model, confint, level = 0.95),
           lowerBound = map_dbl(ConfInt, ~ .["log(rank)",1]),
           upperBound = map_dbl(ConfInt, ~ .["log(rank)",2])
           ) %>%
    rename(Country = system,
           Year = year,
           Slope = slope,
           LowerBound = lowerBound,
           UpperBound = upperBound,
           NbCities = nbCities) %>%
    select(Country, Year, R2, UpperBound, LowerBound,NbCities)


#####################################
##### SPATIAL TRANSITION MATRIX #####
#####################################
library(tidyverse)    
IndiaLasts <- BRICS %>%
    filter(system == "India") %>%
    filter(year %in% c(2001, 2011))
India2001 <- IndiaLasts %>% filter(year == 2001)
India2011 <- IndiaLasts %>% filter(year == 2011)

library(geosphere)
distGeo(India2011 %>% select(Long, Lat), India2011 %>% select(Long, Lat))

library(spdep)
India2011SP <- SpatialPointsDataFrame(coords = India2011 %>% select(Long, Lat),
                                      data = India2011, 
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))
India2011SP <- spTransform(India2011SP, CRS("+init=epsg:3857"))
plot(India2011SP)
kNNIndia2011 <- knearneigh(x = India2011SP@coords, k = 5)
dist5India2011 <- nbdists(knn2nb(kNNIndia2011), India2011SP@coords)

meanDistIndia2011 <- dist5India2011 %>% map_dbl(mean) %>% tibble(dist = .)

meanDistIndia2011 %>%
    filter(dist > quantile(dist, 0.05),
           dist < quantile(dist, 0.95)) %>%
    ggplot(aes(dist)) +
    geom_histogram()

mean(meanDistIndia2011)
summary(meanDistIndia2011)
