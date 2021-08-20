# Descriptive Analysis
# Capital VA Youth Volunteer Hours (No outliers)
# Redcross Internship 
# Yixin Tang (August 12th, 2021)

# Data Source: Red Cross Hour Summary by Youth Volunteer (FY21)
# From the original data source:
# 1.Filtered out the volunteers in Capital Virginia Chapter. 
# 2.Looked up their profiles one by one 
# 3.Collected all Hours record (not just FY21) and demographic info

library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)

capital_i <- read_excel("/Users/yixintang/Redcross/Capital VA Youth/individual_sums(activity).xlsx")

##############################
###### REMOVE OUTLIERS #######
##############################

# Individual
Individual<-capital_i %>% 
  group_by(Name,Race) %>%
  summarize(TH = sum(Total_Hour, na.rm = TRUE))

# Outliers
ggplot(data=Individual,aes(x=TH))+
  geom_boxplot()

# Boxplot/Outliers Total Hour by Race
ggplot(data=Individual,aes(x=Race,y=TH,color=factor(Race)))+
  geom_boxplot()+
  scale_color_viridis(discrete=TRUE,name="Race")+
  labs(x="Race", 
       title="Boxplot: Total Hour by Race", 
       size=12)+
  theme(axis.text.x = element_text(angle = 45, size=9, vjust=1, hjust=1))
# Asian, White: outliers

# Remove outliers
outliers <-boxplot(Individual$TH, plot=FALSE)$out
min(outliers)
length(outliers)
# over 22 hours per year: considered as outliers
# 16 outliers (130 youth volunteers in total)

# Outliers: Total Hours of all activities
outliers_info<-Individual[which(Individual$TH %in% outliers),]

# Outliers: Hours for each activity
OL_info<-capital_i[which(capital_i$Name %in% outliers_info$Name),]

# Remove Outliers from original data set
capital_i<- capital_i[-which(capital_i$Name %in% OL_info$Name),]
length(unique(capital_i$Name))
# before:  130 observations
# after: 114 observations
# 16 outliers in total
# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/



################################
###### RACE(no outliers) #######
################################

capital_r<-capital_i%>%
  select(Name,Race,Gender)%>%
  distinct()

length(capital_r$Name)
# 130 youth volunteers in Capital VA in total

# Number of Youth Volunteers by Race
ggplot(capital_r, aes(x=forcats::fct_infreq(Race),fill=factor(Race)))+
  geom_bar(stat="count", width=0.7)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Race",na.value = "grey")+
  labs(x="Race", 
       title="Number of Youth Volunteers by Race", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
# Reorder geom_bar from high to low when using stat=“count”
# https://stackoverflow.com/questions/56599684/reorder-geom-bar-from-high-to-low-when-using-stat-count

# Total hours by Race without outliers
TH<-capital_i %>% 
  group_by(Race) %>%
  summarize(Total = sum(Total_Hour, na.rm = TRUE))

ggplot(data=TH,aes(x=reorder(Race,-Total),
                   y=Total,fill=factor(Race))) + 
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Race",na.value = "grey")+
  labs(x="Race", 
       title="Total Hours by Race", 
       size=12)+
  geom_text(aes(label=Total), vjust=-0.3, size=3.5)


# Averaged Hours by Race
race_count<-capital_r %>%
  count(Race)

TH$Average<-TH$Total/race_count$n

ggplot(data=TH,aes(x=reorder(Race,-Average),
                   y=Average,fill=factor(Race))) + 
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Race",na.value = "grey")+
  labs(x="Race", 
       title="Averaged Hours by Race", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5)
# Average Hours
# African Americans actually have higher hours
# Asian actually is relatively low



###################
##### GENDER ######
###################

# Number of Volunteers by Gender
ggplot(capital_r, aes(x=forcats::fct_infreq(Gender),fill=factor(Gender)))+
  geom_bar(stat="count", width=0.5,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('pink','light blue'),
                    name="Gender",na.value = "grey")+
  labs(x="Race", 
       title="Number of Volunteers by Gender", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)

# Total Hours by Gender
TH_gender<-capital_i %>% 
  group_by(Gender) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

ggplot(data=TH_gender,aes(x=reorder(Gender,-total_hour),
                          y=total_hour,fill=factor(Gender))) + 
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('pink','light blue'),
                    name="Gender",na.value = "grey")+
  labs(x="Gender", 
       title="Total Hours by Gender", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5)

# Averaged Hours by Gender
gender_count<-capital_r %>%
  count(Gender)
TH_gender$Average<-TH_gender$total_hour/gender_count$n

ggplot(data=TH_gender,aes(x=reorder(Gender,-Average),
                          y=Average,fill=factor(Gender))) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('pink','light blue'),
                    name="Gender",na.value = "grey")+
  labs(x="Gender", 
       title="Averaged Hours by Race", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5)
# Only 4 males
# May not be that accurate


########################
##### RACE/GENDER ######
########################

# Number of Volunteers by Race/Gender
png("/Users/yixintang/Redcross/Capital VA Youth/Number_of_Volunteers.png")
ggplot(capital_r, aes(x=forcats::fct_infreq(Race),fill=factor(Gender)))+
  geom_bar(stat="count", width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue'),name="Gender",
                    na.value = "grey")+
  labs(x="Race", 
       title="Capital VA Youth Volunteers \nNumber of Volunteers by Race&Gender (General Population)", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
dev.off()
# Not much to say, mainly girls

# Total Hours by Race/Gender
TH_race_gender<-capital_i %>% 
  group_by(Race,Gender) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

png("/Users/yixintang/Redcross/Capital VA Youth/Total_Hour.png")
ggplot(data=TH_race_gender,aes(x=reorder(Race,-total_hour),
                               y=total_hour,fill=factor(Gender))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue'),
                    name="Gender",na.value = "grey")+
  labs(x="Gender", 
       title="Capital VA Youth Volunteers \nTotal Hours by Race&Gender (General Population)", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, 
            size=3.5,position = position_dodge(0.9))
dev.off()
# label: https://stackoverflow.com/questions/37587288/ggplot-label-bars-in-grouped-bar-plot

# Averaged Hours by Race/Gender
race_gender_count<-capital_r %>%
  count(Race,Gender)
TH_race_gender$Average<-TH_race_gender$total_hour/race_gender_count$n

png("/Users/yixintang/Redcross/Capital VA Youth/Average_Hour.png")
ggplot(data=TH_race_gender,aes(x=reorder(Race,-Average),
                               y=Average,fill=factor(Gender))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('red','blue'),
                    name="Gender",na.value = "grey")+
  labs(x="Gender", 
       title="Capital VA Youth Volunteers \nAveraged Hours by Race&Gender (General Population)", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position = position_dodge(0.9))
dev.off()
# Again, only 4 males
# may not be accurate

# Boxplot: Single Time Hour Entry by Race/Gender
ggplot(data=capital_i,aes(x=Race,y=Total_Hour,color=factor(Gender)))+
  geom_boxplot()+
  scale_color_manual(values=c('pink','light blue'),
                    name="Gender",na.value = "grey")+
  labs(x="Race", 
       title="Capital VA Youth Volunteers \nBoxplot: Single Activity Hour Entry by Race&Gender (General Population)", 
       size=12)+
  theme(axis.text.x = element_text(angle = 45, size=9, vjust=1, hjust=1))

#######################
###### Activity #######
#######################

# General Activity Category #
capital_simplified<-capital_i %>% 
  group_by(Activity_simplified) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(20)

png("/Users/yixintang/Redcross/Capital VA Youth/Act_total.png")
# Total Number of Hours Spent on Each Category
ggplot(data=capital_simplified,aes(x=reorder(Activity_simplified,-total_hour),
                               y=total_hour,fill=factor(Activity_simplified))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="General Activity Category",na.value = "grey")+
  labs(title="Capital VA Youth Volunteers \nActivity: Total Number of Hours (Eight Categories)", 
       size=12,
       x="Activity",
       y="Total Number of Hours")+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5)
dev.off()

png("/Users/yixintang/Redcross/Capital VA Youth/Act_number.png")
# Number of Occurrences(General Activity Category)
ggplot(capital_i, aes(x=forcats::fct_infreq(Activity_simplified),fill=factor(Activity_simplified)))+
  geom_bar(stat="count", width=0.7)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="General Activity Category",na.value = "grey")+
  labs(x="Activity", 
       title="Capital VA Youth Volunteers \nActivity: Number of Occurrences (Eight Categories)", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
dev.off()

# Averaged Hours (General Activity Category)
act_s_count<-capital_i %>%
  count(Activity_simplified)

capital_simplified$Average<-capital_simplified$total_hour/act_s_count$n

png("/Users/yixintang/Redcross/Capital VA Youth/Act_average.png")
ggplot(data=capital_simplified,aes(x=reorder(Activity_simplified,-Average),
                               y=Average,fill=factor(Activity_simplified))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="General Activity Category",na.value = "grey")+
  labs(x="Activity", 
       title="Capital VA Youth Volunteers \nActivity: Averaged Hours (Eight Categories)", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position = position_dodge(0.9))
# Meeting: 2 hours, not that long but may be too many
dev.off()

#####################
# Specific Activity #
#####################

act_specific<-capital_i %>% 
  filter(Activity_simplified=='Activity') %>% 
  group_by(Activity_specific) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

act_specific_full<-capital_i %>% 
  filter(Activity_simplified=='Activity')

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(10)

# Total Number of Hours Spent on Each Specific Activity
png("/Users/yixintang/Redcross/Capital VA Youth/Act_total(specific).png")
ggplot(data=act_specific,aes(x=reorder(Activity_specific,-total_hour),
                                   y=total_hour,fill=factor(Activity_specific))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=mycolors,name="Specific Activity",na.value = "grey")+
  labs(title="Capital VA Youth Volunteers \nTotal Number of Hours (Specific Activities)", 
       size=12,
       x="Activity",
       y="Total Number of Hours(Specific Activity)")+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5)
dev.off()

# Number of Occurrences(Specific Activity)
png("/Users/yixintang/Redcross/Capital VA Youth/Act_number(specific).png")
ggplot(act_specific_full, aes(x=forcats::fct_infreq(Activity_specific),fill=factor(Activity_specific)))+
  geom_bar(stat="count", width=0.7)+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Specific Activity",na.value = "grey")+
  labs(x="Activity", 
       title="Capital VA Youth Volunteers \nNumber of Occurrences (Specific Activities)", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
dev.off()

# Averaged Hours (Specific Activity)
act_specific_count<-act_specific_full %>%
  count(Activity_specific)

act_specific$Average<-act_specific$total_hour/act_specific_count$n

png("/Users/yixintang/Redcross/Capital VA Youth/Act_averaged(specific).png")
ggplot(data=act_specific,aes(x=reorder(Activity_specific,-Average),
                                   y=Average,fill=factor(Activity_specific))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="Specific Activity",na.value = "grey")+
  labs(x="Activity", 
       title="Capital VA Youth Volunteers \nAveraged Hours (Specific Activities)", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position = position_dodge(0.9))
dev.off()

############################
###### RACE/Activity #######
############################

act_specific<-capital_i %>% 
  filter(Activity_simplified=='Activity') %>% 
  group_by(Activity_specific) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

act_specific_full<-capital_i %>% 
  filter(Activity_simplified=='Activity')

#############################
# General Activity Category #
#############################

# Number of Occurrences by Race(General Activity Category)
png("/Users/yixintang/Redcross/Capital VA Youth/Act_number_r.png")
ggplot(capital_i, aes(x=forcats::fct_infreq(Race),
                      fill=factor(Activity_simplified)))+
  geom_bar(stat="count", width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="General Activity Category",na.value = "grey")+
  labs(x="Race", 
       title="Capital VA Youth Volunteers \nActivity: Number of Occurrences (General)", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
dev.off()

# Total Hours by Race(General Activity Category)
TH_race_act<-capital_i %>% 
  group_by(Race,Activity_simplified) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

png("/Users/yixintang/Redcross/Capital VA Youth/Act_total_r.png")
ggplot(data=TH_race_act,aes(x=reorder(Race,-total_hour),
                             y=total_hour,fill=factor(Activity_simplified))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="General Activity Category",na.value = "grey")+
  labs(x="Race", 
       title="Capital VA Youth Volunteers \nActivity: Total Hours (General)", 
       size=12)+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5,
            position = position_dodge(0.9))
dev.off()


# Averaged Hours by Race(General Activity Category)
race_act_count<-capital_i %>%
  count(Race,Activity_simplified)
TH_race_act$Average<-TH_race_act$total_hour/race_act_count$n

png("/Users/yixintang/Redcross/Capital VA Youth/Act_average_r.png")
ggplot(data=TH_race_act,aes(x=reorder(Race,-Average),
                             y=Average,fill=factor(Activity_simplified))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 70, size=9, vjust=1, hjust=1))+
  scale_fill_viridis(discrete=TRUE,name="General Activity Category",na.value = "grey")+
  labs(x="Race", 
       title="Capital VA Youth Volunteers \nActivity: Averaged Hours (General)", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position = position_dodge(0.9))
dev.off()

##############################
# Specific Activity Category #
##############################

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(10)

png("/Users/yixintang/Redcross/Capital VA Youth/Act_number_r(specific).png")
# Number of Occurrences by Race(General Activity Category)
ggplot(act_specific_full, aes(x=forcats::fct_infreq(Race),
                      fill=factor(Activity_specific)))+
  geom_bar(stat="count", width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=mycolors,name="Specific Activity",na.value = "grey")+
  labs(x="Race", 
       title="Capital VA Youth Volunteers \nActivity: Number of Occurrences (Specific)", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
dev.off()


# Total Number of Hours Spent on Each Specific Activity
act_specific_r<-capital_i %>% 
  filter(Activity_simplified=='Activity') %>% 
  group_by(Activity_specific,Race) %>%
  summarize(total_hour = sum(Total_Hour, na.rm = TRUE))

png("/Users/yixintang/Redcross/Capital VA Youth/Act_total_r(specific).png")
ggplot(data=act_specific_r,aes(x=reorder(Race,-total_hour),
                             y=total_hour,fill=factor(Activity_specific))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=mycolors,name="Specific Activity",na.value = "grey")+
  labs(title="Capital VA Youth Volunteers \nActivity: Total Hours (Specific)", 
       size=12,
       x="Race",
       y="Total Number of Hours(Specific Activity)")+
  geom_text(aes(label=total_hour), vjust=-0.3, size=3.5,
            position=position_dodge(0.9))
dev.off()

# Averaged Hours (Specific Activity)
act_specific_count_r<-act_specific_full %>%
  count(Activity_specific,Race)

act_specific_r$Average<-act_specific_r$total_hour/act_specific_count_r$n

png("/Users/yixintang/Redcross/Capital VA Youth/Act_average_r(specific).png")
ggplot(data=act_specific_r,aes(x=reorder(Race,-Average),
                             y=Average,fill=factor(Activity_specific))) + 
  geom_bar(stat="identity",width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=mycolors,name="Specific Activity",na.value = "grey")+
  labs(x="Activity", 
       title="Capital VA Youth Volunteers \nActivity: Averaged Hours (Specific)", 
       size=12)+
  geom_text(aes(label=round(Average,2)), vjust=-0.3, size=3.5,
            position = position_dodge(0.9))
dev.off()



##############################
###### GENDER/Activity #######
##############################

#############################
# General Activity Category #
#############################

ggplot(capital_i, aes(x=Activity_simplified,fill=factor(Gender)))+
  geom_bar(stat="count", width=0.7,position=position_dodge())+
  theme(axis.text.x = element_text(angle = 30, size=9, vjust=1, hjust=1))+
  scale_fill_manual(values=c('pink','light blue'),name="Gender",
                    na.value = "grey")+
  labs(x="Race", 
       title="Number of Occurrences by Gender(General Activity Category)", 
       size=12)+
  geom_text(aes(label=..count..),stat='count',
            position=position_dodge(0.9),vjust=-0.2)
