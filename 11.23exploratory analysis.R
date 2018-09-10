
library(ggplot2)
library(dplyr)
library(readxl)

# exploratory analysis on crime
crime <- read.csv("crime_w_CTs20171102134814.csv")
str(calls)

crime %>%
  group_by(VICTIM.SEX) %>%
  summarise(sex_num=n()) %>%
  ggplot(aes(x=VICTIM.SEX,y=sex_num)) +
  geom_bar(stat="identity") +
  labs(title="Crime Victims by Sex",x="sex",y="number of victims")

crime %>%
  ggplot(aes(x=VICTIM.AGE)) +
  geom_histogram() +
  labs(title="Crime Victims by age",x="age",y="number of victims")

crime %>%
  group_by(CRIME.CODE.DESCRIPTION) %>%
  summarise(crime_num=n()) %>%
  arrange(-crime_num) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(CRIME.CODE.DESCRIPTION,-crime_num),y=crime_num)) +
  geom_bar(stat = "identity") +
  labs(title="Top 10 Crime Types",x="crimetype",y="victim number") +
  scale_x_discrete(labels=c("Deadly weapon","Battery","Intimate Partner Simple","Robbery","Theft Plain","Intimate Partner Aggravated","Rape","Brandish","Threats","Attempted Robbery"))

#analysis on 311 call
call <- read.csv("311_calls_w_CTs20171102134828.csv")
str(call)

call %>%
 group_by(STATUS) %>%
 summarise(statusnum=n()) %>%
 ggplot(aes(x=STATUS,y=statusnum)) +
 geom_bar(stat = "identity") +
  labs(title="311 call status",x="call status",y="number of calls")

call %>%
  group_by(REQUESTSOURCE) %>%
  summarise(sourcenum=n()) %>%
  ggplot(aes(x=REQUESTSOURCE,y=sourcenum)) +
  geom_bar(stat = "identity") +
  labs(title="311 call source",x="call source",y="number of calls")

# analysis on shelters
shelter <- read.csv("shelters_w_CTs20171102134808.csv")
str(shelter)

shelter %>%
  group_by(CITY) %>%
  summarize(sheltercity=n()) %>%
  arrange(-sheltercity) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(CITY,-sheltercity),y=sheltercity)) +
  geom_bar(stat="identity") +
  labs(title="Top 10 areas with most shelters",x="areas",y="number of shelters")

# analysis on homeless
homeless <- read.csv("Homeless2017.csv")
str(homeless)

homeless %>%
  group_by(City) %>%
  summarise(homelessnum=n()) %>%
  arrange(-homelessnum) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(City,-homelessnum),y=homelessnum)) +
  geom_bar(stat = "identity") +
  labs(title="Top 10 homeless areas",x="areas",y="number of homeless")

# correlation analysis
corr <- read.csv("combined_data_new.csv")

ggplot(corr,aes(x=totHomeless,y=totCrime)) +
  geom_point()

ggplot(corr,aes(x=totSheltHomeless,y=totCrime)) +
  geom_point()

ggplot(corr,aes(x=totUnsheltHomeless,y=totCrime)) +
  geom_point()

ggplot(corr,aes(x=totShelter,y=totCrime)) +
  geom_point()

ggplot(corr,aes(x=totCall,y=totCrime)) +
  geom_point()

# Trend for homeless from 2015-2017
homelesstrend <- read.csv("homeless trend.csv")

ggplot(homelesstrend,aes(x=as.character(year),y=Unsheltered,group=1)) +
  geom_line() +
  labs(title="Unsheltered homeless increased rapidly from 2015-2017",x="year") 

ggplot(homelesstrend,aes(x=as.character(year),y=Sheltered,group=1)) +
  geom_line() +
  labs(title="Sheltered homeless fluctuated from 2015-2017",x="year") 

ggplot(homelesstrend,aes(x=as.character(year),y=TotalHomeless,group=1)) +
  geom_line() +
  labs(title="Unsheltered homeless increased rapidly from 2015-2017",x="year") 

homelesstrend %>%
  mutate(shelt_percent=Sheltered/TotalHomeless*100) %>%
  ggplot(aes(x=as.character(year),y=shelt_percent,group=1)) +
  geom_line() +
  labs(title="Percentage of Sheltered decreased from 2015-2017",x="year",y="percent")

ggplot(homelesstrend,aes(x=as.character(year),y=Emergency.Shelter,group=1)) +
  geom_line() +
  labs(title="Emergency shelters increased rapidly from 2016 to 2017",x="year")

ggplot(homelesstrend,aes(x=as.character(year),y=Transitional.Housing,group=1)) +
  geom_line() +
  labs(title="Transitional Housing fluctuated from 2015-2017",x="year") 

ggplot(homelesstrend,aes(x=as.character(year),y=Safe.Havens,group=1)) +
  geom_line() +
  labs(title="Safe Havens declined rapidly from 2015-2017",x="year") 

# top 5 communities with most sheltered/unsheltered people
homeless2015 <- read.csv("Homeless2015.csv")

homeless2015_new=homeless2015[!is.na(homeless2015$STATE_FIPS),]

homeless2015_new %>%
  group_by(Community.Name) %>%
  summarize(unshelter=sum(Unsheltered)) %>%
  arrange(-unshelter) %>%
  slice(1:5) %>%
  ggplot(aes(x=reorder(Community.Name,-unshelter),y=unshelter)) +
  geom_bar(stat="identity") +
  labs(title="Top 5 communities with unsheltered people in 2015",x="community",y="number of unsheltered")

homeless2015_new %>%
  group_by(Community.Name) %>%
  summarize(shelter=sum(Sheltered)) %>%
  arrange(-shelter) %>%
  slice(1:5) %>%
  ggplot(aes(x=reorder(Community.Name,-shelter),y=shelter)) +
  geom_bar(stat="identity") +
  labs(title="Top 5 communities with sheltered people in 2015",x="community",y="number of sheltered")

homeless2017 <- read.csv("Homeless2017.csv")

homeless2017 %>%
  group_by(Community_Name) %>%
  summarize(unshelter=sum(totUnsheltPeople)) %>%
  arrange(-unshelter) %>%
  slice(1:5) %>%
  ggplot(aes(x=reorder(Community_Name,-unshelter),y=unshelter)) +
  geom_bar(stat="identity") +
  labs(title="Top 5 communities with unsheltered people in 2017",x="community",y="number of unsheltered")

homeless2017 %>%
  group_by(Community_Name) %>%
  summarize(shelter=sum(totSheltPeople)) %>%
  arrange(-shelter) %>%
  slice(1:5) %>%
  ggplot(aes(x=reorder(Community_Name,-shelter),y=shelter)) +
  geom_bar(stat="identity") +
  labs(title="Top 5 communities with sheltered people in 2017",x="community",y="number of sheltered")

homeless2016 <- read.csv("HC2016_Total_Counts_by_Census_Tract_LA_CoC_07132016.csv")

homeless2016_new=homeless2016[!is.na(homeless2016$tract),]

homeless2016_new %>%
  group_by(CommunityName) %>%
  summarize(unshelter=sum(totUnsheltPeople)) %>%
  arrange(-unshelter) %>%
  slice(1:5) %>%
  ggplot(aes(x=reorder(CommunityName,-unshelter),y=unshelter)) +
  geom_bar(stat="identity") +
  labs(title="Top 5 communities with unsheltered people in 2016",x="community",y="number of unsheltered")

homeless2016_new %>%
  group_by(CommunityName) %>%
  summarize(shelter=sum(totSheltPeople)) %>%
  arrange(-shelter) %>%
  slice(1:5) %>%
  ggplot(aes(x=reorder(CommunityName,-shelter),y=shelter)) +
  geom_bar(stat="identity") +
  labs(title="Top 5 communities with sheltered people in 2016",x="community",y="number of sheltered")
