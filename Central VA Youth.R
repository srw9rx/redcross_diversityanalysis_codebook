# Descriptive Analysis
# Central VA Youth Volunteer Hours
# Redcross Internship 
# Yixin Tang (July 21st, 2021)

# Data Source: Hours Summary for Youth Volunteers (FY21)
# (In Query Tool type in 'youth' in name)

library(tidyverse)
library(ggplot2)
library(readxl)

youth <- read_excel("/Users/yixintang/Redcross/Central VA Youth/Central VA Youth.xlsx")

# Only 15 youth volunteers in Central Virginia
names<-unique(youth$`Account Name (Youth Under 18)`)
names
length(names)

total_hour <- data.frame()
TH<-data.frame()

for (i in 1:length(names)){
  position<-which(youth$`Account Name (Youth Under 18)`==names[i])
  rows<-youth[position,]
  x<-sum(rows$`Worked Hours`)
  total_hour<-rbind(total_hour,x)
  
  row<-youth[position[1],]
  TH<-rbind(TH,row)
}

# Get the total hour for each person
TH<-cbind(TH,total_hour)
names(TH)[15]<-"Total_Hour"
TH

# Change the Birthday format
TH$`Date of Birth`<-as.Date(TH$`Date of Birth`,origin='1900-01-01')-2
# https://stats.idre.ucla.edu/r/faq/how-does-r-handle-date-values/

TH<-TH[,-c(3,4)]

TH$Total_Hour

library(viridis)

# Monthly Worked Hours by Individual Volunteers
ggplot(data=TH,aes(x=`Month Hours Worked`,y=`Worked Hours`,color=factor(`Account Name (Youth Under 18)`))) + 
  geom_point(position=position_jitter(h=0.15, w=0.15), alpha = 0.8, size = 3)+
  theme(axis.text.x = element_text(angle = 15, size=8, vjust=1, hjust=1))+
  scale_color_viridis(discrete=TRUE,name="Name")+
  labs(x="Time(by month)", 
       title="Monthly Worked Hours by Individual Volunteers", 
       size=12)

# Monthly Worked Hours by Positions
ggplot(data=TH,aes(x=`Month Hours Worked`,y=`Worked Hours`,color=factor(`Position Name`))) + 
  geom_point(position=position_jitter(h=0.15, w=0.15), alpha = 0.8, size = 3)+
  theme(axis.text.x = element_text(angle = 15, size=8, vjust=1, hjust=1))+
  scale_color_viridis(discrete=TRUE,name="Position")+
  labs(x="Time(by month)", 
       title="Monthly Worked Hours by Positions", 
       size=12)
# Observations
# Red Cross Clubs: Mostly 2020 Oct and Nov, around 1-3 hours
# Blood Services: 2020 July, 2021 June (the big blood drive Bill mentioned before:
# one July one Dec?) more hours than the Clubs

# Club activity according to file: General Body Meeting

# Blood services activity according to file: 
# 1.American Red Cross Charlottesville Fixed Site: Donor Ambassador
# 2.DA Refresher Videos & Reading Materials

# Total Worked Hours by Positions
ggplot(data=TH,aes(x=reorder(`Account Name (Youth Under 18)`,Total_Hour),
                   y=Total_Hour,fill=factor(`Position Name`))) + 
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 90, size=8, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Position")+
  labs(x="Total Hour", y="Name",
       title="Total Hour Worked by Individual Volunteers", 
       size=12)

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(6)

pdf("/Users/yixintang/Redcross/Central VA Youth/Number_of Volunteers3.pdf")
# Number of Volunteers by Position/Race/Gender
ggplot(TH, aes(x=forcats::fct_infreq(Race),fill=factor(`Position Name`),color=factor(Gender)))+
  geom_bar(stat="count", width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=mycolors,name="Position")+
  scale_color_manual(values=c('red','blue','black'),name="Gender",na.value='black')+
  labs(x="Race", 
       title="Central VA Youth Volunteers \nNumber of Volunteers by Position&Race&Gender", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.3)
dev.off()

# Total Worked Hours by Positions

TH_race_position<-TH %>% 
  group_by(Race,Gender,`Position Name`) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

pdf("/Users/yixintang/Redcross/Central VA Youth/Total_Hour.pdf")
ggplot(data=TH_race_position,aes(x=reorder(Race,total_hour),
                   y=total_hour,fill=factor(`Position Name`),color=factor(Gender))) + 
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 45, size=8, vjust=1, hjust=1))+
  scale_fill_manual(values=mycolors,name="Position")+
  scale_color_manual(values=c('red','blue','black'),name="Gender",na.value='black')+
  labs(x="Total Hour", y="Name",
       title="Central VA Youth Volunteers \nTotal Hour Worked by Position&Race&Gender", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5,
            position=position_dodge(0.9))
dev.off()


TH_race_gender<-TH %>% 
  group_by(Race,Gender) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

# Total Hours by Race & Gender

ggplot(data=TH_race_gender,aes(x=reorder(Race,-total_hour),
                               y=total_hour,fill=factor(Gender))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('pink','light blue','dark grey'),name="Gender")+
  labs(x="Gender", 
       title="Charlottesville Youth Volunteers \nTotal Hours by Race&Gender", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5,
            position=position_dodge(0.9))
# labels: overlap together
# have to fix it: https://stackoverflow.com/questions/40542685/how-to-jitter-remove-overlap-for-geom-text-labels


# Averaged Hours by Race/Gender
race_gender_count<-TH %>%
  count(Race,Gender)
TH_race_gender$Average<-TH_race_gender$total_hour/race_gender_count$n

ggplot(data=TH_race_gender,aes(x=reorder(Race,-Average),
                               y=Average,fill=factor(Gender))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('pink','light blue','grey','yellow'),name="Gender")+
  labs(x="Gender", 
       title="Charlottesville Volunteers \nAveraged Hours by Race&Gender(Outliers)", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position=position_dodge(0.9))


# Highschools in Central Virginia chapter:
# https://en.wikipedia.org/wiki/List_of_high_schools_in_Virginia#Charlottesville_City
# https://www.doe.virginia.gov/directories/schools/school_info_by_regions.shtml#r5

# Is there a list of Highscool that has a Redcross club in the Chapter??? - yes: look into that
# Great Potential
# Analyse Capital Virginia? What their youth are doing? 

# Demographics info
# pretty balanced gender composition
# only one Asian, the rest are all White/NA
# No Hispanic

# Look into Capital Virginia Chapter?
# What their Redcross clubs are doing? What kind of activity?









