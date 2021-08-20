# Descriptive Analysis
# Central VA Youth Volunteer Hours
# Redcross Internship 
# Yixin Tang (July 29th, 2021)

# Data Source: Red Cross Hour by Race (Calculated by Sophia Walton)
# NO OUTLIERS

# Load packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)

# Read in the data
race <- read_excel("/Users/yixintang/Redcross/Demographics/redcross_hoursbyrace.xlsx")

# Boxplot
boxplot(race$`hours worked since 8/1/20`, plot=TRUE)

ggplot(data=race,aes(x=`hours worked since 8/1/20`))+
  geom_boxplot()

ggplot(data=race,aes(x=race,y=`hours worked since 8/1/20`,color=factor(race)))+
  geom_boxplot()+
  scale_color_viridis(discrete=TRUE,name="Race")+
  labs(x="Race", 
       title="Boxplot: Hours by Race", 
       size=12)

# Remove outliers
outliers <-boxplot(race$`hours worked since 8/1/20`, plot=FALSE)$out

# Outliers info
outliers_info<-data.frame()

for (i in 1:length(outliers)){
  position<-which(race$`hours worked since 8/1/20`==outliers[i])
  x<-race[position,]
  outliers_info<-rbind(outliers_info,x)
}


# Remove Outliers
race<- race[-which(race$`hours worked since 8/1/20` %in% outliers),]
# before: 224 observations
# after: 192 observations
# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/

max(race$`hours worked since 8/1/20`)
min(outliers)

# Botplot without outliers
ggplot(data=race,aes(x=race,y=`hours worked since 8/1/20`,color=factor(race)))+
  geom_boxplot()+
  scale_color_viridis(discrete=TRUE,name="Race")+
  labs(x="Race", 
       title="Boxplot: Hours by Race", 
       size=12)




###################
###### RACE #######
###################

# Number of Volunteers by Race
ggplot(race, aes(x=forcats::fct_infreq(race),fill=factor(race)))+
  geom_bar(stat="count", width=0.7)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Race")+
  labs(x="Race", 
       title="Number of Volunteers by Race", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
# Reorder geom_bar from high to low when using stat=“count”
# https://stackoverflow.com/questions/56599684/reorder-geom-bar-from-high-to-low-when-using-stat-count

# Total Hours by Race
TH<-race %>% 
  group_by(race) %>%
  summarize(total_hour = sum(`hours worked since 8/1/20`, na.rm = TRUE))

ggplot(data=TH,aes(x=reorder(race,-total_hour),
                   y=total_hour,fill=factor(race))) + 
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Race")+
  labs(x="Race", 
       title="Total Hours by Race", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5)


# Averaged Hours by Race
race_count<-race %>%
  count(race)
TH$Average<-TH$total_hour/race_count$n

ggplot(data=TH,aes(x=reorder(race,-Average),
                   y=Average,fill=factor(race))) + 
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Race")+
  labs(x="Race", 
       title="Averaged Hours by Race", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5)

###################
##### GENDER ######
###################

# Number of Volunteers by Gender
ggplot(race, aes(x=forcats::fct_infreq(gender),fill=factor(gender)))+
  geom_bar(stat="count", width=0.5,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue','grey','yellow'),name="Gender")+
  labs(x="Race", 
       title="Number of Volunteers by Gender", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)

# Total Hours by Gender
TH_gender<-race %>% 
  group_by(gender) %>%
  summarize(total_hour = sum(`hours worked since 8/1/20`, na.rm = TRUE))

ggplot(data=TH_gender,aes(x=reorder(gender,-total_hour),
                          y=total_hour,fill=factor(gender))) + 
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue','grey','yellow'),name="Gender")+
  labs(x="Gender", 
       title="Total Hours by Gender", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5)

# Averaged Hours by Gender
gender_count<-race %>%
  count(gender)
TH_gender$Average<-TH_gender$total_hour/gender_count$n

ggplot(data=TH_gender,aes(x=reorder(gender,-Average),
                          y=Average,fill=factor(gender))) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue','grey','yellow'),name="Gender")+
  labs(x="Gender", 
       title="Averaged Hours by Gender", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5)
# Hispanic: highest, all female (male: potential)
# Black: male is higher than female
# Asian: female is high but male is lower
# Middle Eastern: only female
# White: male significantly higher, closed to Hispanic female


########################
##### RACE/GENDER ######
########################

# Number of Volunteers by Race/Gender
ggplot(race, aes(x=forcats::fct_infreq(race),fill=factor(gender)))+
  geom_bar(stat="count", width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue','grey','yellow'),name="Gender")+
  labs(x="Race", 
       title="Charlottesville Volunteers \nNumber of Volunteers by Race&Gender (General Population)", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.3)

# Total Hours by Race/Gender
TH_race_gender<-race %>% 
  group_by(race,gender) %>%
  summarize(total_hour = sum(`hours worked since 8/1/20`, na.rm = TRUE))

ggplot(data=TH_race_gender,aes(x=reorder(race,-total_hour),
                               y=total_hour,fill=factor(gender))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue','grey','yellow'),name="Gender")+
  labs(x="Gender", 
       title="Charlottesville Volunteers \nTotal Hours by Race&Gender (General Population)", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5,
            position=position_dodge(0.9))
# labels: overlap together
# have to fix it: https://stackoverflow.com/questions/40542685/how-to-jitter-remove-overlap-for-geom-text-labels



#############################
######## HIGHLIGHT ##########
#############################

# Averaged Hours by Race/Gender
race_gender_count<-race %>%
  count(race,gender)
TH_race_gender$Average<-TH_race_gender$total_hour/race_gender_count$n

ggplot(data=TH_race_gender,aes(x=reorder(race,-Average),
                               y=Average,fill=factor(gender))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue','grey','yellow'),name="Gender")+
  labs(x="Gender", 
       title="Charlottesville Volunteers \nAveraged Hours by Race&Gender (General Population)", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position=position_dodge(0.9))

# Boxplot Hours by Race/Gender
ggplot(data=race,aes(x=race,y=`hours worked since 8/1/20`,color=factor(gender)))+
  geom_boxplot()+
  scale_color_manual(values=c('red','blue','grey','yellow'),name="Gender")+
  labs(x="Race", 
       title="Charlottesville Volunteers \nBoxplot: Hours by Race&Gender (General Population)", 
       size=12)

########################
###### RACE/CLUB #######
########################

# Number of Volunteers by Race/Club
ggplot(race, aes(x=forcats::fct_infreq(race),fill=factor(`club?`)))+
  geom_bar(stat="count", width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Club")+
  labs(x="Race", 
       title="Number of Volunteers by Race/Club", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)

# Total Hours by Race/Club
TH_race_club<-race %>% 
  group_by(race,`club?`) %>%
  summarize(total_hour = sum(`hours worked since 8/1/20`, na.rm = TRUE))

ggplot(data=TH_race_club,aes(x=reorder(race,-total_hour),
                               y=total_hour,fill=factor(`club?`))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Club")+
  labs(x="Club", 
       title="Total Hours by Race/Gender", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5)

# Averaged Hours by Race/Club
race_club_count<-race %>%
  count(race,`club?`)
TH_race_club$Average<-TH_race_club$total_hour/race_club_count$n

ggplot(data=TH_race_club,aes(x=reorder(race,-Average),
                               y=Average,fill=factor(`club?`))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Club")+
  labs(x="Club", 
       title="Averaged Hours by Race/Club", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position=position_jitter(width=0.2,height=0.2))
