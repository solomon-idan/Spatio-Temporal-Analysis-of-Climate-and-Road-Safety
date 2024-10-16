# set working directorate
setwd("D:/document/mphil")

#datawrangling for kumasi accidents
library(readxl)
kcrash<- read_excel("rta.xlsx",sheet = "Sheet8")

#recode police station to their respective district

library(dplyr)
kcrash$district <- recode(kcrash$district, 
                          "Central" = "KUMASI METROPOLITAN",
                          "Manhyia" = "KUMASI METROPOLITAN",
                          "Asawase" = "ASOKORE MAMPONG MUNICIPAL",
                          "Asokore" = "ASOKORE MAMPONG MUNICIPAL",
                          "Asokore Mampong" = "ASOKORE MAMPONG MUNICIPAL",
                          "Asokwa" = "ASOKWA  MUNICIPAL",
                          "Suame" = "SUAME MUNICIPAL",
                          "Old Tafo" = "OLD TAFO MUNICIPAL",
                          "Suntreso" = "KWADASO MUNICIPAL",
                          "Trede" = "KWADASO MUNICIPAL",
                          "UST" = "OFORIKROM MUNICIPAL",
                          "Oforikrom" = "OFORIKROM MUNICIPAL")


#create yearly aggregate from contributing district
kcrash <- kcrash %>%
  group_by(year, district)%>%
  summarise(fatal_injury=sum(`fatal injury`),
            severe_injury=sum(`severe injury`),
            minor_injury=sum(`minor injury`),
            total_crashes=sum(`total crashes`))%>% ungroup()

# repeat each row 12 times
kcrash <- kcrash[rep(1:nrow(kcrash), each = 12),]

# add a column for the month
kcrash$month <- rep(1:12, length.out = nrow(kcrash))

#data came as yearly aggregate however we want monthly aggrgate
#from the kumasi accident report for 2021, mothly ratios obtained

# define the ratios
ratios_crashes<- c(0.0758, 0.0510, 0.0718, 0.0885, 0.0989, 0.0837, 0.0813, 
                   0.0821, 0.0742, 0.0981, 0.0981, 0.0965)

ratios_injury <- c(0.0681,0.0681,	0.0932,	0.1004,	0.0986,
                   0.1022,	0.0806,	0.0932,	0.0466,	0.0950,	
                   0.0753,	0.0789)

ratios_fatal <- c(0.0500,	0.0750,	0.0438,	0.0750,	0.0750,	0.1063,
                  0.0875,	0.1188,	0.0750,	0.0875,	0.0688,	0.1375)



#attach ratios to datafame and repeat for each year
#assumning ratio remains same for study period
kcrash$ratio_tc <- rep(ratios_crashes, length.out = nrow(kcrash))

kcrash$ratio_smi <- rep(ratios_injury, length.out = nrow(kcrash))

kcrash$ratio_fi <- rep(ratios_fatal, length.out = nrow(kcrash))


#create values by multiplying ratios
kcrash <- kcrash %>% mutate(monthly_crashes= round(total_crashes*ratio_tc,0),
                            monthly_fata_injury=round(fatal_injury*ratio_fi,0),
                            monthly_severe_injury=round(severe_injury*ratio_smi,0),
                            monthly_minor_injury=round(minor_injury*ratio_smi,0))

#create date variable 
library(tidyr)
library(lubridate)
kcrash <- kcrash %>% mutate(Year=year, Month=month)

kcrash <- kcrash %>% unite(year_month, Year, Month, sep = "-") %>% 
  mutate(date=ym(year_month))

#trim dataframe to core variable
kcrash <- kcrash %>% select(date,year, month ,district,
                            monthly_crashes,monthly_fata_injury,
                            monthly_severe_injury, monthly_minor_injury)



# new table 
yr <- unique(kcrash$year)
new <- data.frame(year=yr,month = 1)
new <- new[rep(1:nrow(new), each = 12),]
new$month1 <- 1:nrow(new)
new$month <- rep(1:12, length.out = nrow(new))

#merge newtable to kcrash
mon_kcrash <- kcrash %>%
  left_join(new, by = c("year","month"))

#aggregate by the continues month, year and district
mon_kcrash <- mon_kcrash %>%
  group_by(year,month1,district,month,date)%>%
  summarise(monthly_fata_injury=sum(`monthly_fata_injury`),
            monthly_severe_injury=sum(`monthly_severe_injury`),
            monthly_minor_injury=sum(`monthly_minor_injury`),
            monthly_crashes=sum(`monthly_crashes`))%>% ungroup()

#create population and season
mon_kcrash <- mon_kcrash %>% mutate(popn = district)
mon_kcrash$popn <- recode(mon_kcrash$popn,
                    "ASOKORE MAMPONG MUNICIPAL"=191402,
                    "KUMASI METROPOLITAN"=443981,
                    "KWADASO MUNICIPAL"=154526,
                    "OFORIKROM MUNICIPAL"=213126,
                    "OLD TAFO MUNICIPAL"=114368,
                    "SUAME MUNICIPAL"=136290,"ASOKWA  MUNICIPAL"=125642)

#mon_kcrash <- mon_kcrash %>% mutate(popn=popn/1000) #scale population by 100,000
#index of the Public Health Risk (PHR) of road traffic crashes~100,000


mon_kcrash<- mon_kcrash %>% 
  mutate(season = case_when(month %in% c(12, 1:3) ~ "Dry season",
                            month %in% 4:11 ~ "Rainy season"),
         season=as.factor(season))

#create indirect standardization
library(SpatialEpi)

mon_kcrash$E1 <- expected(
  population = mon_kcrash$popn,
  cases = mon_kcrash$monthly_crashes,
  n.strata =1)

mon_kcrash$E2 <- expected(
  population = mon_kcrash$popn,
  cases = mon_kcrash$monthly_fata_injury,
  n.strata =1)

mon_kcrash$E3 <- expected(
  population = mon_kcrash$popn,
  cases = mon_kcrash$monthly_severe_injury,
  n.strata =1)

mon_kcrash$E4 <- expected(
  population = mon_kcrash$popn,
  cases = mon_kcrash$monthly_minor_injury,
  n.strata =1)

mon_kcrash$SIR1 <- mon_kcrash$monthly_crashes/ mon_kcrash$E1
mon_kcrash$SIR2 <- mon_kcrash$monthly_fata_injury/ mon_kcrash$E2
mon_kcrash$SIR3 <- mon_kcrash$monthly_severe_injury/ mon_kcrash$E3
mon_kcrash$SIR4 <- mon_kcrash$monthly_minor_injury/ mon_kcrash$E4

#---------------------------------------------------------------#
#call in climate data

#load climate data
kclim <- read_excel("kumasi.xlsx")

#add avg_temp
kclim <- kclim %>% mutate(avg_temp = (avg_min_temp  + avg_max_temp)/2)

#merge climate to crash 
kma_data <- mon_kcrash%>%
  left_join(kclim, by ="date")

kma_data <- as.data.frame(kma_data)

#create id for analysis
kma_data$idarea <- as.numeric(as.factor(kma_data$district))
kma_data$idarea1 <- kma_data$idarea
kma_data$idtime <- 1 + kma_data$month1 - min(kma_data$month1)

#save
library(writexl)
write_xlsx(kma_data, "kma_data.xlsx")
#---------------------------------------------------------------#
#load shape of kumasi metropolitan area
#load shapefile
library(sf)

kma_shapefile <-st_read("SHAPEFILES/kumasi shapefile.shp") 


kma_shapefile <- kma_shapefile %>% 
  select(OBJECTID,REGION,DISTRICT,Shape_Leng,Shape_Area,geometry) %>%
  mutate(district=DISTRICT)

#create coordinates
coord <- st_coordinates(st_centroid(kma_shapefile))
kma_shapefile$long <- coord[,1]
kma_shapefile$lat <- coord[,2]
kma_shapefile$ID <- 1:dim(kma_shapefile)[1]

#selection of variables for shapefile
kma_shapefile <- kma_shapefile %>% 
  select(ID,district,geometry,
         Shape_Leng,Shape_Area,long,lat)

#create neighbour
library(spdep)
nb <- poly2nb(kma_shapefile)

#create weight matrix for INLA package
library(INLA)
nb2INLA("map.adj", nb)
g<- inla.read.graph(filename = "map.adj")

#create weight matrix for BCARPTIME package
w <-  nb2mat(nb, style = "B", zero.policy = T)

#---------------------------------------------------------------#
#join dataframe
kma_full <- kma_data %>%
  left_join(kma_shapefile, by = c("district"))

kma_full <- as.data.frame(kma_full)
kma_full_sf <- st_as_sf(kma_full)

#save
library(writexl)
write_xlsx(kma_full_sf, "kma_full_sf.xlsx")
#---------------------------------------------------------------#

library(esquisse)
esquisser(kma_full_sf, viewer = "browser")

#---------------------------------------------------------------#
#spatiotemporal analysis with INLA [data=kma_data]

#monthly_crashes
formula.crash <- monthly_crashes ~ avg_temp + avg_rainfall +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

res.crash <- inla(formula.crash,
            family = "poisson", data = kma_data, E = E1,
            control.predictor = list(compute = TRUE))
summary(res.crash)

#monthly_fatal_injury
formula.fatal <-  monthly_fata_injury ~ avg_temp + avg_rainfall + 
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

res.fatal <- inla(formula.fatal,
                  family = "poisson", data = kma_data, E = E2,
                  control.predictor = list(compute = TRUE))
summary(res.fatal)

#monthly_severe_injury
formula.severe <-  monthly_severe_injury ~ avg_temp + avg_rainfall + 
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

res.severe <- inla(formula.severe,
                   family = "poisson", data = kma_data, E = E3,
                   control.predictor = list(compute = TRUE))
summary(res.severe)

#monthly minor injury
formula.minor <-   monthly_minor_injury ~ avg_temp + avg_rainfall + 
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

res.minor <- inla(formula.minor,
                  family = "poisson", data = kma_data, E = E4,
                  control.predictor = list(compute = TRUE))
summary(res.minor)


