library(tidyverse)
library(tidycensus)
srd <- read_csv("Data/School Data/Data_4-13-2021---1003.csv")


# Filtering for Schools of interest

Academicsrd <- srd %>% 
  filter(`Sector of institution (HD2019)` != 0, # The school is not administrative
         `Institution is active in current year (HD2019)` == 1, #The School is active
         `Academic (IC2019)` == 1,#The School is Academic School
         `Degree-granting status (HD2019)` == 1, #Gives Degrees
         `Percent admitted - total (DRVADM2019)`> 75) # Broad Band access based on papers


#Renaming Variables of interest for coding convince
schooldf <- Academicsrd %>%
  rename(SchoolName = `Institution Name`,
         State = `State abbreviation (HD2019)`,
         Longitude = `Longitude location of institution (HD2019)`,
         Latitude = `Latitude location of institution (HD2019)`,
         Address = `Street address or post office box (HD2019)`,
         ZIPcode = `ZIP code (HD2019)`,
         PercentAdmittedTotal = `Percent admitted - total (DRVADM2019)`,
         PercentAdmittedMale = `Percent admitted - men (DRVADM2019)`,
         PercentAdmittedFemale = `Percent admitted - women (DRVADM2019)`,
         AverageNetPriceG=`Average net price-students awarded grant or scholarship aid  2018-19 (SFA1819)`,
         AverageNetPrice0_30 = `Average net price (income 0-30 000)-students awarded Title IV federal financial aid  2018-19 (SFA1819)`,
         AverageNetPrice30_48 = `Average net price (income 30 001-48 000)-students awarded Title IV federal financial aid  2018-19 (SFA1819)`,
         AverageNetPrice48_75 = `Average net price (income 48 001-75 000)-students awarded Title IV federal financial aid  2018-19 (SFA1819)`,
         AverageNetPrice75_110 = `Average net price (income 75 001-110 000)-students awarded Title IV federal financial aid  2018-19 (SFA1819)`,
         AverageNetPrice110_inf = `Average net price (income over 110 000)-students awarded Title IV federal financial aid  2018-19 (SFA1819)`,
         TotalPriceLivingOnCampusInState = `Total price for in-state students living on campus 2019-20 (DRVIC2019)`,
         TotalPriceLivingOnCampusOutState = `Total price for out-of-state students living on campus 2019-20 (DRVIC2019)`)

#Census Data
IncomeCounty <- read_csv("Data/IncomeByCounty/ACSST1Y2019.S1903_data_with_overlays_2021-04-12T132235.csv")


sa_acs<-get_acs(geography = "tract",
                state="TX",
                county = c("Bexar"),
                year = 2017,
                variables=c("DP03_0062", "DP03_0063") ,#Median62 and Mean63 House Income
                geometry = T, output = "wide")

IncomeCountyI <- IncomeCounty %>% 
  select(GEO_ID, NAME, S1903_C01_001E, S1903_C01_001M, S1903_C03_001E, S1903_C03_001M) %>% 
  .[-1,]#Gets rid of first row which is just column infomation
