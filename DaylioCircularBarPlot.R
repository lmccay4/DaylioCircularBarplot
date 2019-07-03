#lmccay4


# Libraries
library(tidyverse)
library("colorspace") 

#import Daylio CSV file
moodData<-read.csv(file.choose())

#Assigns numeric value based on mood
moodData$value[moodData$mood=="awful"] <- 55 
moodData$value[moodData$mood=="bad"] <- 125
moodData$value[moodData$mood=="meh"] <- 225
moodData$value[moodData$mood=="good"] <- 325
moodData$value[moodData$mood=="rad"] <- 500

#Delete overlapping days between years (only want 365 days)
#You will have to change first number depending on your moodData
moodData = moodData[-377:-366,]

#Creates month column from month mentioned in date column
#May need to execute twice
moodData$month[grep("May", moodData$date)] <- "May"
moodData$month[grep("April", moodData$date)] <- "April"
moodData$month[grep("March", moodData$date)] <- "March"
moodData$month[grep("February", moodData$date)] <- "February"
moodData$month[grep("January", moodData$date)] <- "January"
moodData$month[grep("December", moodData$date)] <- "December"
moodData$month[grep("November", moodData$date)] <- "November"
moodData$month[grep("October", moodData$date)] <- "October"
moodData$month[grep("September", moodData$date)] <- "September"
moodData$month[grep("August", moodData$date)] <- "August"
moodData$month[grep("July", moodData$date)] <- "July"
moodData$month[grep("June", moodData$date)] <- "June"

#Creates monthNumber column and assigns numeric value 1-12 based on month from month column
#May need to execute twice
moodData$monthNumber[grep("May", moodData$month)] <- 5
moodData$monthNumber[grep("April", moodData$month)] <- 4
moodData$monthNumber[grep("March", moodData$month)] <- 3
moodData$monthNumber[grep("February", moodData$month)] <- 2
moodData$monthNumber[grep("January", moodData$month)] <- 1
moodData$monthNumber[grep("December", moodData$month)] <- 12
moodData$monthNumber[grep("November", moodData$month)] <- 11
moodData$monthNumber[grep("October", moodData$month)] <- 10
moodData$monthNumber[grep("September", moodData$month)] <- 9
moodData$monthNumber[grep("August", moodData$month)] <- 8
moodData$monthNumber[grep("July", moodData$month)] <- 7
moodData$monthNumber[grep("June", moodData$month)] <- 6

#New dataframe from original CSV with groupings by month
#You will need to change this depending on how many days you have recorded in each month
df=data.frame(
  moodData,
  group=c(rep('June 2019', 7), rep('May', 31), rep('April', 30), rep('March', 31), rep('February', 28), rep('January', 31), rep('December', 31), rep('November', 30), rep('October', 31), rep('September', 30), rep('August', 31), rep('July', 31), rep('June 2018', 23)) 
)


# Set a number of NA/empty bars to add to the end of each month group
empty_bar=0
to_add = data.frame( matrix(NA, empty_bar*nlevels(df$group), ncol(df)) )
colnames(to_add) = colnames(df) 
to_add$group=rep(levels(df$group), each=empty_bar) 
df=rbind(df, to_add)

#Arrange dataframe in chronological order to set sequential IDs based on date
df=df %>% arrange(group)
df<-df[order(df$full_date),]
df$id=seq(1, nrow(df))

#Order bars by ID number on base of graph
base_data=df %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

#Sort group and mood column level orders manually
levels(df$group)
df$group = factor(df$group, levels= c("January","February", "March", "April", "May", "June 2019", "June 2018", "July", "August", "September", "October", "November", "December"))
levels(df$group)


levels(df$mood)
#for Positive-centric
df$mood = factor(df$mood, levels = c("rad", "good", "meh", "bad", "awful"))
#for Negative-centric
df$mood = factor(df$mood, levels = c("awful", "bad", "meh", "good", "rad"))

levels(df$mood)

#Postive-centric graph (only need if created negative previously - to restore default values)
df$value[df$mood=="rad"] <- 500
df$value[df$mood=="good"] <- 325
df$value[df$mood=="meh"] <- 225
df$value[df$mood=="bad"] <- 125
df$value[df$mood=="awful"] <- 55

#Negative-centric graph (invert values)
df$value[df$mood=="awful"] <- 500
df$value[df$mood=="bad"] <- 325
df$value[df$mood=="meh"] <- 225
df$value[df$mood=="good"] <- 125
df$value[df$mood=="rad"] <- 55


#Creates plot 
#Change graph key with color property (mood for moods, value for numeric representation, group for months)
p = ggplot(df, aes(color= mood, x=as.factor(id), y=value)) + 
  
  #creates bars
  geom_bar(stat="identity", alpha=0.5, width = 0.7) +
  
  # Limits: Negative controls size of inner circle. Positive adds size ceiling to bars
  ylim(-400,510) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    #legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  
  # circular instead of cartesian plot
  coord_polar(start=0)

  p
  

  #Save plot as high quality JPG
  ggsave("daylio_Positive_Mood.jpg", dpi = 1000)

