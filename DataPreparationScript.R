#Libraries Loading
install.packages("tidyverse")
install.packages("gridExtra")
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Data loading in Variables
activity <- read.csv("dailyActivity_merged.csv")
calories <- read.csv("dailyCalories_merged.csv")
intensities <- read.csv("dailyIntensities_merged.csv")
steps <- read.csv("dailySteps_merged.csv")
hcalories <- read.csv("hourlyCalories_merged.csv")
hintensities <- read.csv("hourlyIntensities_merged.csv")
hsteps <- read.csv("hourlySteps_merged.csv")
dsleep <- read.csv("sleepDay_merged.csv")
heartrate <- read.csv("heartrate_seconds_merged.csv")
msleep <- read.csv("minuteSleep_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
mcalories <- read_csv("minuteCaloriesNarrow_merged.csv")
mintensities <- read_csv("minuteIntensitiesNarrow_merged.csv")
mmets <- read_csv("minuteMETsNarrow_merged.csv")
msteps <- read_csv("minuteStepsNarrow_merged.csv")

#Checking Data Validation 
colnames(activity)
colnames(calories)
colnames(intensities)
colnames(steps)
colnames(heartrate)
colnames(hcalories)
colnames(hintensities)
colnames(hsteps)
colnames(mcalories)
colnames(mintensities)
colnames(mmets)
colnames(msleep)
colnames(msteps)
colnames(weight)
colnames(dsleep)

length(unique(activity$Id))
length(unique(heartrate$Id))
length(unique(hcalories$Id))
length(unique(hintensities$Id))
length(unique(hsteps$Id))
length(unique(mcalories$Id))
length(unique(mintensities$Id))
length(unique(mmets$Id))
length(unique(msleep$Id))
length(unique(msteps$Id))
length(unique(weight$Id))
length(unique(calories$Id))
length(unique(intensities$Id))
length(unique(steps$Id))
length(unique(dsleep$Id))

# Choosing Which Tables to work With

head(activity)
head(calories)
head(dsleep)
head(steps)
head(weight)

# Understanding Our tables for Better Decision and Understanding Statistical Rates

summary(activity)
summary(calories)
summary(dsleep)
summary(steps)
summary(weight)

# Preparing to merge and work with final Data

activity <- activity %>% 
  rename(date = ActivityDate) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
calories <- calories %>% 
  rename(date = ActivityDay) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
dsleep <- dsleep %>% 
  rename(date = SleepDay) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
steps <- steps %>% 
  rename( date= ActivityDay) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
weight <- weight %>% 
  rename(date = Date) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
hcalories <- hcalories %>% 
  rename(date = ActivityHour) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))

#Memory allocation would be failure to join without Adjusting date

finaltable <- activity %>%
  left_join(hcalories, by = c("Id","date")) %>%
  left_join(calories, by = c("Id","date")) %>%
  left_join(steps, by = c("Id","date")) %>%
  left_join(weight, by = c("Id","date")) %>%
  left_join(dsleep, by = c("Id","date"), relationship = "many-to-many")
  
View(finaltable)


#Removing unnecessary columns

finaltable <- finaltable %>% 
  select(-c(StepTotal,BMI,WeightPounds,Fat,IsManualReport,LogId
            ,TrackerDistance,LoggedActivitiesDistance,Calories.x,Calories.y
          ))

#Final Clean Form to Work With

View(finaltable)

#Exporting Data Set for Clean work with Visualization
write_csv(finaltable, "FinalWorkingTable.csv")  # row.names = FALSE

#Visualization Relationship

#Undstanding Distance to Burning Calories

T1 <-  ggplot(finaltable, aes(x=VeryActiveDistance, y=Calories)) + geom_point(aes(color=TotalSteps)) + geom_smooth() + labs(x="Active Distance Covered", y="Calories Burned")
T2 <-  ggplot(finaltable, aes(x=ModeratelyActiveDistance, y=Calories)) + geom_point(aes(color=TotalSteps)) + geom_smooth() + labs(x="Moderate Activity Distance Covered", y="Calories Burned")
T3 <-  ggplot(finaltable, aes(x=LightActiveDistance, y=Calories)) + geom_point(aes(color=TotalSteps)) + geom_smooth() + labs(x="Light Activity Distance Covered", y="Calories Burned")
T4 <-  ggplot(finaltable, aes(x=SedentaryActiveDistance, y=Calories)) + geom_point(aes(color=TotalSteps)) + geom_smooth() + labs(x="Sedentary Activity Distance Covered", y="Calories Burned")

grid.arrange(T1,T2,T3,T4)

#Understanding Time used for Burning Calories


M1 <- ggplot(finaltable, aes(x=VeryActiveMinutes, y=Calories)) + geom_point(aes(color=VeryActiveMinutes)) + geom_smooth() + labs(x="Active Minutes Consumed", y="Calories Burned",color="Minutes")
M2 <- ggplot(finaltable, aes(x=FairlyActiveMinutes, y=Calories)) + geom_point(aes(color=FairlyActiveMinutes)) + geom_smooth() + labs(x="Fairly Active Minutes Consumed", y="Calories Burned",color="Minutes")
M3 <- ggplot(finaltable, aes(x=LightlyActiveMinutes, y=Calories)) + geom_point(aes(color=LightlyActiveMinutes)) + geom_smooth() + labs(x="Lightly Active Minutes Consumed", y="Calories Burned",color="Minutes")
M4 <- ggplot(finaltable, aes(x=SedentaryMinutes, y=Calories)) + geom_point(aes(color=SedentaryMinutes)) + geom_smooth() + labs(x="Sedentary Minutes Consumed", y="Calories Burned",color="Minutes")

grid.arrange(M1,M2,M3,M4)
# Understanding Affected Parameters to Burning Calories

Tsteps <- ggplot(finaltable, aes(x=TotalSteps, y=Calories)) + geom_point(aes(color=TotalSteps)) + geom_smooth() + labs(x="Steps", y="Calories Burned",color="Steps")
TDistance <- ggplot(finaltable, aes(x=TotalDistance, y=Calories)) + geom_point(aes(color=TotalDistance)) + geom_smooth() + labs(x="Total Distance", y="Calories Burned",color="Distance")
TMS <- ggplot(finaltable, aes(x=TotalMinutesAsleep, y=Calories)) + geom_point(aes(color=TotalMinutesAsleep)) + geom_smooth() + labs(x="Total Sleep in Minutes", y="Calories Burned",color="Total Sleeping in Minutes")
TTB <- ggplot(finaltable, aes(x=TotalTimeInBed, y=Calories)) + geom_point(aes(color=TotalTimeInBed)) + geom_smooth() + labs(x="Total Time in Bed", y="Calories Burned",color="Total Time In Bed")
WC <- ggplot(finaltable, aes(x=WeightKg, y=Calories)) + geom_point(aes(color=WeightKg)) + geom_smooth() + labs(x="Weight", y="Calories Burned",color="Weight")
TTZ <- ggplot(finaltable, aes(x=TotalTimeInBed, y=TotalMinutesAsleep)) + geom_step(direction="hv") + geom_smooth() + labs(x="Total Time in Bed", y="Total Time A Sleep",color="Difference Between Sleep and Spended time in Bed")
grid.arrange(Tsteps,TDistance,TMS,WC,TTB,TTZ)
 