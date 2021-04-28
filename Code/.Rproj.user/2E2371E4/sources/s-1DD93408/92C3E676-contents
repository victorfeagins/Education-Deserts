library(tidyverse)
library(tidycensus)
srd <- read_csv("Data/School Data Graduation Rates/Data_4-27-2021---301.csv")


# Filtering for Schools of interest

Academicsrd <- srd %>% 
  filter(`Sector of institution (HD2019)` != 0, # The school is not administrative
         `Institution is active in current year (HD2019)` == 1, #The School is active
         `Academic (IC2019)` == 1,#The School is Academic School
         `Degree-granting status (HD2019)` == 1) #Gives Degrees

GraduationRate <- Academicsrd %>% 
  rename(Percent_Admitted = `Percent admitted - total (DRVADM2019)`,
         GradRate_4_years = `Graduation rate - Bachelor degree within 4 years  total (DRVGR2019)`,
         GradRate_5_years = `Graduation rate - Bachelor degree within 5 years  total (DRVGR2019)`,
         GradRate_6_years = `Graduation rate - Bachelor degree within 6 years  total (DRVGR2019)`) %>% 
  select(`Institution Name`, Percent_Admitted, GradRate_4_years, GradRate_5_years, GradRate_6_years)


ggplot(GraduationRate, mapping = aes(Percent_Admitted,GradRate_4_years)) +
  geom_point() +
  xlab("Percent Admitted 2019")+
  ylab("Graduation Rate 4 Years")

ggplot(GraduationRate, mapping = aes(Percent_Admitted,GradRate_5_years)) +
  geom_point() +
  xlab("Percent Admitted 2019")+
  ylab("Graduation Rate 5 Years")

ggplot(GraduationRate, mapping = aes(Percent_Admitted,GradRate_6_years))  +
  geom_point() +
  xlab("Percent Admitted 2019")+
  ylab("Graduation Rate 6 Years")


x1 <- select(GraduationRate, Percent_Admitted,GradRate_4_years, GradRate_5_years, GradRate_6_years)
cormat <- round(cor(x1, use = "complete.obs"),2)


SchoolsInterest <-filter(GraduationRate, `Institution Name` == "The University of Texas at San Antonio")

#National Averages
xdomain = seq(1:100)
PredictedData <-  tibble(Percent_Admitted = xdomain)

#Creating Fits for each year
lm4.fit <- lm(GradRate_4_years ~ Percent_Admitted , GraduationRate)

lm5.fit <- lm(GradRate_5_years ~ Percent_Admitted , GraduationRate)

lm6.fit <- lm(GradRate_6_years ~ Percent_Admitted , GraduationRate)


#Making Tibbles for each grad rate
PredictedData4<- tibble(data.frame(predict(lm4.fit, PredictedData, interval = "confidence"))) %>% 
  rename(GradRate_4_years = fit) %>% 
  mutate(Percent_Admitted = xdomain)

PredictedData5<- tibble(data.frame(predict(lm5.fit, PredictedData, interval = "confidence"))) %>% 
  rename(GradRate_5_years = fit) %>% 
  mutate(Percent_Admitted = xdomain)

PredictedData6<- tibble(data.frame(predict(lm6.fit, PredictedData, interval = "confidence"))) %>% 
  rename(GradRate_6_years = fit) %>% 
  mutate(Percent_Admitted = xdomain)


#Plotting
ggplot(PredictedData4, mapping = aes(Percent_Admitted,GradRate_4_years)) +
  geom_line() +
  geom_line(mapping = aes(y = lwr), colour = "grey") +
  geom_line(mapping = aes(y = upr), colour = "grey") +
  geom_point(data = SchoolsInterest)+
  geom_text(data = SchoolsInterest, aes(label = `Institution Name`),hjust=1, vjust=0)+
  xlim(1, 100)+
  ggtitle("4 Year Graduation rate Mean 95% confidence Interval")


ggplot(PredictedData5, mapping = aes(Percent_Admitted,GradRate_5_years)) +
  geom_line() +
  geom_line(mapping = aes(y = lwr), colour = "grey") +
  geom_line(mapping = aes(y = upr), colour = "grey") +
  geom_point(data = SchoolsInterest)+
  geom_text(data = SchoolsInterest, aes(label = `Institution Name`),hjust=1, vjust=0)+
  xlim(1, 100)+
  ggtitle("5 Year Graduation rate Mean 95% confidence Interval")

ggplot(PredictedData6, mapping = aes(Percent_Admitted,GradRate_6_years)) +
  geom_line() +
  geom_line(mapping = aes(y = lwr), colour = "grey") +
  geom_line(mapping = aes(y = upr), colour = "grey") +
  geom_point(data = SchoolsInterest)+
  geom_text(data = SchoolsInterest, aes(label = `Institution Name`),hjust=1, vjust=0)+
  xlim(1, 100)+
  ggtitle("6 Year Graduation rate Mean 95% confidence Interval")
