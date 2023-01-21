#Confirmed COVID positive cases in Ontairo https://open.canada.ca/data/en/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27
#PHU Boundraries https://geohub.lio.gov.on.ca/datasets/ministry-of-health-public-health-unit-boundary/explore?location=49.342384%2C-84.749434%2C5.81&showTable=true
#Underlying HEalth Issues https://www150.statcan.gc.ca/n1/tbl/csv/13100777-eng.zip


#COVID Cases by FSA https://open.toronto.ca/dataset/covid-19-cases-in-toronto/
#Toronto Neighbourhood Boundraries https://open.toronto.ca/dataset/neighbourhoods/
#Census information https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/page_dl-tc.cfm?Lang=E
#School information and student demographics 
setwd("~/Google Drive/Health Equity Analysis")

library(data.table)
library(tidyverse)
library(readr)
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(ggspatial)
library(readxl)
library("scales")


rm(census_trunc)
##========================================================================================================================##
## IMPORT STATS FILES 
##========================================================================================================================##

#COVID_by_PHU <- read_csv("COVID by PHU.csv")
sex <- read_excel("Sex by FSA.csv.xlsx") #census data 
income <- read_excel("CRA Income.csv.xlsx") #census data 
tocases <- read_csv("COVID19 cases.csv")
sif <- read_excel("new_sif_data_table_2019_20prelim_en_february2022.xlsx")
poll <- read_csv("toronto_air_pollution_and_covid-19_data_by_neighbourhood-en.csv")

#Prep age and sex dataset
##========================================================================================================================##
#Remove the geo name Canada 
sex <- sex[!(sex$GEO_NAME=="Canada"),]

#Keep  data only if they are from Toronto
sex$first_letterfsa <- substr(sex$GEO_NAME, 1,1)
sex <- sex[which(sex$first_letterfsa=="M"),]
sex_trunc <- sex[!(sex$`DIM: Age (in single years) and average age (127)`=="Average age"|
                     sex$`DIM: Age (in single years) and average age (127)`=="Total - Age"),]
sex_trunc <- sex %>% 
  group_by(GEO_NAME) %>% 
  mutate(male_total = sum(`Dim: Sex (3): Member ID: [2]: Male`),
         female_total = sum(`Dim: Sex (3): Member ID: [3]: Female`))
sex_trunc <- sex_trunc %>% 
  select(c("GEO_CODE (POR)","male_total","female_total"))
sex_trunc <- distinct(sex_trunc)
sex_trunc$total <- sex_trunc$male_total + sex_trunc$female_total
sex_trunc$pos_female <- sex_trunc$female_total/sex_trunc$total #not too many differences in terms of sex distribution in FSA 
rm(sex)

age <- sex[which(sex$`DIM: Age (in single years) and average age (127)`=="Average age"),]
age <- age %>% 
  select(c("GEO_NAME","Dim: Sex (3): Member ID: [1]: Total - Sex"))
age$avg_age <- age$`Dim: Sex (3): Member ID: [1]: Total - Sex`
age$`Dim: Sex (3): Member ID: [1]: Total - Sex` <- NULL

#Prep income dataset
##========================================================================================================================##
income$first_letterfsa <- substr(income$FSA, 1,1)
income <- income[which(income$first_letterfsa=="M"),]
#income_trunc <- income %>% 
  #select(c("FSA","Average"))

income_trunc <- income[,c(2,24)]
rm(income)

#Prep case dataset
##========================================================================================================================##

case_info <- tocases %>% 
  group_by(FSA,`Ever Hospitalized`) %>% 
  count()
case_info <- dcast(case_info, FSA ~ `Ever Hospitalized`)
case_info[is.na(case_info)] <- 0
case_info$total_cases <- case_info$No + case_info$Yes
case_info$hospitalization_rate <- case_info$Yes/case_info$total_cases
rm(tocases)

#Prep School Info dataset
##========================================================================================================================##
sif$fsa <- substr(sif$`Postal Code`, 1,3)

sif$`Percentage of Students Who Are New to Canada from a Non-English Speaking Country` <- as.numeric(sif$`Percentage of Students Who Are New to Canada from a Non-English Speaking Country`)
sif$`Percentage of Students Whose First Language Is Not English` <- as.numeric(sif$`Percentage of Students Whose First Language Is Not English`)
sif$`Percentage of Students Whose Parents Have No Degree, Diploma or Certificate` <- as.numeric(sif$`Percentage of Students Whose Parents Have No Degree, Diploma or Certificate` )
  
sif_sum <- sif %>% 
  group_by(fsa) %>% 
  summarise(median_newcanada = median(`Percentage of Students Who Are New to Canada from a Non-English Speaking Country`),
            median_firstlangnoteng = median(`Percentage of Students Whose First Language Is Not English`),
            median_parentedu <- median(`Percentage of Students Whose Parents Have No Degree, Diploma or Certificate`))

rm(sif)

#Prep Pollution  dataset
##========================================================================================================================##
tobound <- st_read("Neighbourhoods.shp")
tobound$FIELD_2 <- gsub("\\s*\\([^\\)]+\\)","",as.character(tobound$FIELD_2))

poll <- poll[,c(1,11,17,20)]

poll2 <- merge(poll, tobound, by.x="Neighbourhood_name", by.y="FIELD_2", all=TRUE)
class(poll)
rm(poll2, tobound)

#========================================================================================================================##
## SET UP SHAPE FILES
##========================================================================================================================##

#Import Shape files 
#phuboundrary<-st_read("MOH_PHU_BOUNDARY.shp")
#hrboundary <-shapefile("HR_035a18a_e.shp")
fsaboundary <-st_read("lfsa000a16a_e.shp")

#Keep only Toronto FSA's from the shape file
fsaboundary <- fsaboundary[which(fsaboundary$CFSAUID %in% income_trunc$FSA),]

## Average Age by FSA Plot 
##========================================================================================================================##
age_bound <- merge(age,fsaboundary, by.x="GEO_NAME", by.y="CFSAUID", all=TRUE)
age_bound <- st_as_sf(age_bound)

ggplot() +
  geom_sf(age_bound, mapping=aes(fill=avg_age)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="Average Age") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top"))
 

## COVID Cases by FSA
##========================================================================================================================##
ncases <- case_info[,c(1,4)]
ncases <- merge(ncases,fsaboundary, by.x="FSA", by.y="CFSAUID", all=TRUE)
ncases <- st_as_sf(ncases)

ggplot() +
  geom_sf(ncases, mapping=aes(fill=total_cases)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="Case Count",direction= -1, option="A") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top"))

plot(ncases)

## Average Income by FSA Plot 
##========================================================================================================================##
inc_bound <- merge(income_trunc,fsaboundary, by.x="FSA", by.y="CFSAUID", all=TRUE)
inc_bound <- st_as_sf(inc_bound)
ggplot() +
  geom_sf(inc_bound, mapping=aes(fill=Average)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="Average Private Household Income",labels = comma, direction=-1, option="A") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top"))

## Prop Female by FSA Plot 
##========================================================================================================================##
prop_female <- sex_trunc[,c(1,6)]
prop_female <- merge(prop_female,fsaboundary, by.x="GEO_NAME", by.y="CFSAUID", all=TRUE)
prop_female <- st_as_sf(prop_female)

ggplot() +
  geom_sf(prop_female, mapping=aes(fill=pos_female)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="Proportion Female",labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top"))

## Hospitalizations by FSA
##========================================================================================================================##
hosp <- case_info[,c(1,5)]
hosp <- merge(hosp,fsaboundary, by.x="FSA", by.y="CFSAUID", all=TRUE)
hosp <- st_as_sf(hosp)
ggplot() +
  geom_sf(hosp, mapping=aes(fill=hospitalization_rate)) +
  scale_fill_viridis_c(trans = "log", alpha = .9, name="Hospitalization Rate",labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top")) 

## Student Demographics by FSA
##========================================================================================================================##
sif_sum <- merge(sif_sum,fsaboundary, by.x="fsa", by.y="CFSAUID", all=TRUE)
sif_sum <- st_as_sf(sif_sum)
ggplot() +
  geom_sf(sif_sum, mapping=aes(fill=median_newcanada)) +
  scale_fill_viridis_c(trans = "log", alpha = .9, name="Median % of Students New to Canada from a Non-English Speaking Country",labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10)) +
  guides(fill = guide_colorbar(title.position="top")) 

ggplot() +
  geom_sf(sif_sum, mapping=aes(fill=median_firstlangnoteng)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="Median % of Students First Language Not English",labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top")) 

ggplot() +
  geom_sf(sif_sum, mapping=aes(fill=median_firstlangnoteng)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="Median % Student with Parents no Degree/Diploma/Certificate",labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top")) 

##Demographics by Neighbourhood
##========================================================================================================================##
poll2 <- st_as_sf(poll2)
ggplot() +
  geom_sf(poll2, mapping=aes(fill=percent_people_of_colour)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="% of People of Colour",labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top")) 

ggplot() +
  geom_sf(poll2, mapping=aes(fill=percent_health_soc_service_ind)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .9, name="%",labels = comma) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width = unit(5, "line"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) +
  guides(fill = guide_colorbar(title.position="top")) 

