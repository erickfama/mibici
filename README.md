# Bike Sharing System Clustering

## Context

Bike Sharing Systems (BSS) are a solution to traffic congestion. this systems are a service in which a user pays a fee to rent a bicycle for a short period of time. To accomplish this, these systems have a network of stations where users can pick up and return bycicles. 

## Problem

The logistics involved to achieve a correct geographical placement of new bycicle stations is a challenge. An improper location can lead people being unwilling to use the service. This, in turn, results in high economic costs for the service provider and social costs to the population due to underutilized infrastructure.

## Objective 

Analyze which are the main factors that influence the use of the BSS. The analysis is done by performing a clustering of bycicle stations to identify the individual characteristics of each station and to look out for differences in utilization between clusters. 

This approach facilitate an examination of the factors that make them similar and how station usage varies across different clusters. This proves valuable in optimizing the placement of new stations or enhancing the efficiency of existing stations.

## Data

### BSS 

The data used for this project is data from "MiBici" which is the BSS of the metropolitan area of Guadalajara, located in the state of Jalisco, Mexico. This BSS has 300 stations distributed across 3 municipalities: Guadalajara, Zapopan, and San Pedro Tlaquepaque.

There are three datasets regarding the BSS: The trips dataset and the stations dataset.

#### Trips

The trips dataset contains data from trips carried out during November, 2022. In total, 381,192 trips were registered during that month. For each trip, it holds demographic data about the user, like his age and gender, and information about the trip, like the time when it started and ended, and the station where the trip was initiaded and the station where the trip ended. 

#### Stations

The stations dataset contains data from the stations, like their names, ids, coordinates, operating status, and type of buildings and places of interest like coffee shops, schools, grocery stores, etc.

### Preprocessing

#### Trips

I needed to filter the dataset initially to consider only the trips that were registered during the week spanning from November 7th to November 13th, 2022. This time window was established because it doesn't include any holidays, thus avoiding the introduction of any disturbances caused by holidays.

Then, calculate and categorize trip durations into 7 different categories:

- 0 to 5 min
- 6 to 10 min
- 11 to 15 min
- 16 to 30 min
- 31 to 45 min
- 46 to 60 min
- More than 60 min

The code is the following:

```R
trips_clean <- trips_raw %>%
  filter(station_origin_id != station_destination_id, # Filter fot week span
         as_date(trip_start) >= "2022-11-07",
         as_date(trip_end) < "2022-11-14") %>%
  mutate(across(c("trip_start", "trip_end"), ~ as_datetime(.x)), # Convert to date
         duration_trip = difftime(trip_end, trip_start, units = "mins"), 
         time_schedule = ifelse(minute(trip_start) >= 55 & duration_trip > 5, 
                                hour(trip_end), 
                                hour(trip_start)), 
         duration_trip_cut = factor(
           case_when(duration_trip <= 5 ~ "0 to 5 min", # Categorize
                     duration_trip > 5 & duration_trip <= 10 ~ "6 to 10 min",
                     duration_trip > 10 & duration_trip <= 15 ~ "11 to 15 min",
                     duration_trip > 15 & duration_trip <= 30 ~ "16 to 30 min",
                     duration_trip > 30 & duration_trip <= 45 ~ "31 to 45 min",
                     duration_trip > 45 & duration_trip <= 60 ~ "46 to 60 min", 
                     duration_trip > 60 ~ "More than 60 min"), 
           levels = c("0 to 5 min", 
                      "6 to 10 min", 
                      "11 to 15 min", 
                      "16 to 30 min", 
                      "31 to 45 min", 
                      "46 to 60 min", 
                      "More than 60 min")),
         gender = factor(gender, levels = c("M", "F", "NULL"), 
                      labels = c("Male", "Female", "NULL"))) %>%
  filter(duration_trip > 5 & duration_trip < 360 &
           hour(trip_start) %in% c(0, seq(5, 23, 1)))
```

After cleaning the datasets, K-Means algorithm was applied to create 4 station clusters.

## Results 

Before clustering

![stations_map](https://github.com/erickfama/mibici/tree/main/figs/1_dist_estaciones.png)

After clustering

![stations_clusters](https://github.com/erickfama/mibici/tree/main/figs/7_estaciones_cluster.png)
