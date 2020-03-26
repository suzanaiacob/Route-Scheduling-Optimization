# load libraries 
library(tidyverse)
library(pracma)
library(data.table)
library(plyr) #for merging multiple datasets
library(readr) #for merging multiple datasets
library(dplyr)


########################################################################################################
# PART 1. Merge all data

# merge 9 months of data after creating a folder "DataAll" in workdir that includes all 9 monthly data (first three rows removed in each csv)
setwd("/Users/sharonxyan/DropboxMIT/2019Fall/A-Lab")
mydir = "DataAll"
myfiles = list.files(path=, pattern="*.csv", full.names=TRUE)
myfiles

alldata_csv = ldply(myfiles, read_csv)
alldata_csv # write.csv(alldata_csv, file="AllRoutes_9months.csv")


########################################################################################################
# PART 2. Identify eligible stops that...
# 1. appear in latest file (sept); 2. remove weekends & non active clients; 3. within boston metro area; 

# read in the September csv
sept <- read.csv("2019-09_RouteVisits.csv")
sept$zipcode <- as.character(sept$zipcode)
sept$clientkind <- as.character(sept$clientkind)

# remove weekends and non-active clients
sept <- sept %>%
  mutate(weekday=substr(sept$pickuptime, 0, 3)) %>%
  filter(weekday %in% c("Mon","Tue","Wed","Thu","Fri")) %>%
  filter(clientactive == 'Y') %>%
  filter(addressactive == 'Y') %>%
  select(-weekday)


# filter Boston Metro locations based on zipcodes 
sept_boston <- sept %>% 
  filter(zipcode %in% c("1431", "1432", "1434", "1450", "1460", "1463", "1464", "1469", "1474", "1503", "1701", 
                        "1702", "1718", "1719", "1720", "1721", "1730", "1731", "1740", "1741", "1742", "1746", 
                        "1748", "1749", "1752", "1754", "1760", "1770", "1773", "1775", "1776", "1778", "1801", 
                        "1803", "1810", "1821", "1824", "1826", "1827", "1830", "1832", "1833", "1834", "1835", 
                        "1840", "1841", "1843", "1844", "1845", "1850", "1851", "1852", "1854", "1860", "1862", 
                        "1863", "1864", "1867", "1876", "1879", "1880", "1886", "1887", "1890", "1901", "1902", 
                        "1904", "1905", "1906", "1907", "1908", "1913", "1915", "1921", "1922", "1923", "1929", 
                        "1930", "1937", "1938", "1940", "1944", "1945", "1949", "1950", "1951", "1952", "1960", 
                        "1966", "1969", "1970", "1982", "1983", "1984", "1985", "2019", "2021", "2025", "2026", 
                        "2030", "2032", "2035", "2038", "2043", "2045", "2047", "2050", "2052", "2053", "2054", 
                        "2056", "2061", "2062", "2066", "2067", "2071", "2072", "2081", "2090", "2093", "2108", 
                        "2109", "2110", "2111", "2113", "2114", "2115", "2116", "2118", "2119", "2120", "2121", 
                        "2122", "2124", "2125", "2126", "2127", "2128", "2129", "2130", "2131", "2132", "2134", 
                        "2135", "2136", "2138", "2139", "2140", "2141", "2142", "2143", "2144", "2145", "2148", 
                        "2149", "2150", "2151", "2152", "2155", "2163", "2169", "2170", "2171", "2176", "2180", 
                        "2184", "2186", "2188", "2189", "2190", "2191", "2199", "2203", "2210", "2215", "2301", 
                        "2302", "2322", "2324", "2330", "2332", "2333", "2338", "2339", "2341", "2343", "2346", 
                        "2347", "2351", "2359", "2360", "2364", "2366", "2367", "2368", "2370", "2379", "2382", 
                        "2420", "2421", "2445", "2446", "2451", "2452", "2453", "2457", "2458", "2459", "2460", 
                        "2461", "2462", "2464", "2465", "2466", "2467", "2468", "2472", "2474", "2476", "2478", 
                        "2481", "2482", "2492", "2493", "2494", "2532", "2538", "2558", "2562", "2571", "2576", 
                        "2738", "2739", "2762", "2767", "2770", "3032", "3034", "3036", "3037", "3038", "3042", 
                        "3044", "3053", "3077", "3079", "3087", "3103", "3104", "3106", "3225", "3234", "3261", 
                        "3263", "3290", "3291", "3801", "3811", "3819", "3820", "3823", "3824", "3825", "3826", 
                        "3827", "3833", "3835", "3839", "3840", "3841", "3842", "3844", "3848", "3851", "3852", 
                        "3854", "3855", "3856", "3857", "3858", "3861", "3862", "3865", "3867", "3868", "3869", 
                        "3870", "3871", "3873", "3874", "3878", "3884", "3885", "3887"))

sept_boston

# write.csv(sept_boston, file="septRoutes_BostonMetro.csv")

# Create a unique list of client stops, hubs, PSCs
# A stop is classified a hub if 'taskKind == HUB or addresskind == HUB'. Similar logic applies for PSC. 
# All other 'taskKind == PICKUP' are regular client stops. 
# The column MIT_stopType states how we interpreted / classified a stop (based on the above logic)
sept_boston_cleanstops <- sept_boston %>% 
  filter(taskKindCode %in% c('PICKUP','HUB','PSC')) %>%
  mutate(MIT_stopType=taskKindCode) %>%
  mutate(MIT_stopType=replace(MIT_stopType, addresskind=='HUB', 'HUB')) %>%
  mutate(MIT_stopType=replace(MIT_stopType, addresskind=='PSC', 'PSC')) %>%
  select(MIT_stopType, addressCode, clientcode, contactName1, addresskind, addressName1, address, cityName1, stateName, zipcode) %>%
  unique() %>%
  arrange(MIT_stopType) 
  
#used this to test that address code is indeed unique
  # group_by(addressCode) %>%
  #  summarize(n = n()) %>%
  #  filter(n>1)

#These address codes are duplicated and will have to be dealt with manually after outputting file:
  #FZM, QCA10470882, QCA49701163, QCA49702163B, QCA66003999G, QCAFQ8, QCASTATE, QCAVMY, Z4X

write.csv(sept_boston_cleanstops, file="boston_stops_v3.csv")

########################################################################################################
# PART 3. clean 9 months of data by selecting only eligible stops 

# create a new column combining "addressName1" and "zipcode" as the unique identifier of a location for 9 months of data 
#alldata_csv$identifier <- paste(alldata_csv$addressName1,alldata_csv$zipcode)
#alldata_csv

# filter eligible stops in 9 months of data 
alldata_cleanstop <- semi_join(alldata_csv, sept_boston_cleanstops, by = "addressCode")

alldata_cleanstop
alldata_cleanstop$pickuptime


########################################################################################################
# PART 4. clean 9 months of data by selecting only weekdays 

alldata_cleanstop$pickuptime <- as.character(alldata_cleanstop$pickuptime)

alldata_cleanstop$pickuptimeweekday <- substr(alldata_cleanstop$pickuptime, 0, 3)
alldata_cleanstop$pickuptimeweekday

alldata_cleanstop_weekday <- alldata_cleanstop %>% 
  filter(pickuptimeweekday %in% c("Mon","Tue","Wed","Thu","Fri"))
alldata_cleanstop_weekday 

# test <- filter(alldata_cleanstop_weekday, addressCode == 'QCA10037282')
# write.csv(test, file="test.csv")

write.csv(alldata_cleanstop_weekday, file="alldata_clean.csv")


########################################################################################################
# PART 5. total and average demand per client for all 9 months

#clean dates
    #currently in the format Mon, Sep 16,2019 8:32 PM
    #(a = weekday abbrev, b = month abbrev, p = PM/AM which is used in conjuction with I for the hour)
alldata_cleanstop_weekday$pickuptime <- as.POSIXct(alldata_cleanstop_weekday$pickuptime, format="%a, %b %d,%Y %I:%M %p") 
#calculate total client demand

total_client_demand <- alldata_cleanstop_weekday %>%
        mutate(pickupdate = format(alldata_cleanstop_weekday$pickuptime, '%b %d %Y')) %>%
        group_by(addressCode,pickupdate) %>%
        dplyr::summarise(totalDailySpecimensPicked = sum(specimenPicked)) %>%
        group_by(addressCode) %>%
        dplyr::summarise(totalDemand = sum(totalDailySpecimensPicked),
                         avgDailyDemand = mean(totalDailySpecimensPicked),
                         numDaysStopped = n())
  
total_client_demand <- merge(x = sept_boston_cleanstops, y = total_client_demand)  

write.csv(total_client_demand, file="boston_stops_v3 (with demand).csv")

  ########################################################################################################
# PART 6. average stop times at clients 

#clean dates, in the format Mon, Sep 16,2019 8:32 PM
#(a = weekday abbrev, b = month abbrev, p = PM/AM which is used in conjuction with I for the hour)
alldata_cleanstop_dates <- alldata_cleanstop_weekday
 
 alldata_cleanstop_dates$pickuptime <- as.POSIXct(alldata_cleanstop_dates$pickuptime, format="%a, %b %d,%Y %I:%M %p") 
 alldata_cleanstop_dates$pickupendtime <- as.POSIXct(alldata_cleanstop_dates$pickupendtime, format="%a, %b %d,%Y %I:%M %p") 
 alldata_cleanstop_dates <- mutate(alldata_cleanstop_dates,MIT_duration = difftime(alldata_cleanstop_dates$pickupendtime, alldata_cleanstop_dates$pickuptime, units = "mins"))
 average_stop_time <- alldata_cleanstop_dates %>%
    filter(MIT_stop)
 	group_by(addressCode) %>%
   
	summarize(num_stops = n(),
             average_stop_duration = mean(MIT_duration, na.rm=T),
             max_stop_duration = max(MIT_duration, na.rm=T),
             min_stop_duration = min(MIT_duration, na.rm=T),
             average_stop_duration = mean(serviceduration, na.rm=T))
   
 average_stop_time <- merge(x = sept_boston_cleanstops, y = average_stop_time)   

########################################################################################################
# PART 7. time window to visit clients

all_time_windows_sept_boston <- sept_boston %>%
  dplyr::group_by(addressCode,earlypickuptime,latepickuptime) %>%
  dplyr::summarise(count =n(), 
                   specimensPicked = sum(specimenPicked)) 
  

#removes time windows for stops that are not in our cleanstop list
clean_time_windows_sept_boston <- semi_join(all_time_windows_sept_boston,sept_boston_cleanstops,by="addressCode")

# test <- sept_boston_cleanstops %>%
#   dplyr::group_by(addressCode, MIT_stopType)

#grab MITstoptype column
clean_time_windows_sept_boston <- merge(x = clean_time_windows_sept_boston, y = sept_boston_cleanstops[ , c("addressCode", "MIT_stopType")], by = "addressCode", all.x=TRUE)


write.csv(clean_time_windows_sept_boston, file="client_time_windows.csv")

########################################################################################################



