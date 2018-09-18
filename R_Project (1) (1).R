#Exploratory Analysis

tor = read.csv("MCI_2014_to_2017.csv")
dim(tor) #131073 observations and 29 variables

#Data Preprocessing
anyDuplicated(tor) # None found
head(tor)
colnames(tor)

#Datacleaning and preperation
#Removing unecessary columns
to_drop = c("¯..X", "Y", "Index_", "event_unique_id", "ucr_code", "ucr_ext",
            "FID","reportedyear","reportedmonth", "reportedday", "reporteddayofyear",
            "reporteddayofweek", "reportedhour", "Hood_ID")
tor = tor[, !(names(tor) %in% to_drop)]
tor = tor[,2:16]

#observing structure of dataset
str(tor)

#Finding Factors
factor_or_not = as.data.frame(sapply(tor, is.factor))
print(factor_or_not)

#Converting non-factors to factors
tor$occurrenceyear = factor(tor$occurrenceyear)
tor$occurrencedayofyear = factor(tor$occurrencedayofyear)
tor$occurrencehour = factor(tor$occurrencehour)
tor$occurrenceday = factor(tor$occurrenceday)

#gsub()
tor$reporteddate = gsub("T"," ", tor$reporteddate)
tor$reporteddate = gsub("Z","", tor$reporteddate)
tor$occurrencedate = gsub("T"," ", tor$occurrencedate)
tor$occurrencedate = gsub("Z","", tor$occurrencedate)
#Changing character to date
tor$occurrencedate = as.Date(tor$occurrencedate, "%Y-%m-%d")
tor$reporteddate = as.Date(tor$reporteddate, "%Y-%m-%d")

#Locating Missing Values
tor[which(!complete.cases(tor)),]

#Finding Columns with NA values
colnames(tor)[apply(is.na(tor),2,any)]

#Dealing with missing values
#deleting all the NA values as all the NA values corrasponds
#to the crimes occured more than 10 years ago
tor = tor[!is.na(tor$occurrenceyear),]

dim(tor)
unique(tor$Neighbourhood)

#plotting
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(ggthemes)


# as crime numbers reported from 2000 to 2013 are significantly less I am only using 
#crime stats for 2014-2017
years = c(2014,2015,2016,2017)
tor = tor[tor$occurrenceyear %in% years,]
crime_by_year1 = count(tor, 'occurrenceyear')
ggplot(aes(x=occurrenceyear, y=freq),
       data = crime_by_year1)+
    geom_bar(stat = 'identity', width = 0.5, fill = "tomato") +
       ggtitle('Total Crimes by Years')+geom_text(aes(label = freq, vjust = 2, ))+
    xlab('Year')+
    ylab('Frequency')+
    theme_bw() +
    theme(plot.title = element_text(size = 14),
          axis.title = element_text(size = 10, face = "bold"))
    
#What are the major crimes between 2014 - 2017?
crime_by_MCI <- count(tor, "MCI")
ggplot(aes(x=reorder(MCI, freq), y=freq),
       data = crime_by_MCI)+
    geom_bar(stat = 'identity', width = 0.5, fill = "tomato") +
    coord_flip()+
    ggtitle('Major Crimes 2014-2017')+
    xlab('Major Crimes')+
    ylab('Frequency')+
    theme_bw() +
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))


#Crime bt time of the day
hour_group <- group_by(tor, occurrencehour)
crime_hour <- dplyr::summarise(hour_group, n=n())
ggplot(aes(x=occurrencehour, y=n), data = crime_hour) + 
    geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen", group=1) + 
    geom_point(size = 0.5) + 
    ggtitle('Total Crimes by Hour of Day in Toronto 2014-2017') +
    ylab('Number of Occurrences') +
    xlab('Hour(24-hour clock)') +
    theme_bw() +
    theme(plot.title = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"))

#crime by hour
hour_crime_group <- group_by(tor, occurrencehour, MCI)
hour_crime <- dplyr::summarise(hour_crime_group, n=n())
ggplot(aes(x=occurrencehour, y=n, color=MCI, group=MCI), data =hour_crime) + 
    geom_line(size=1.5) + 
    ggtitle('Crime Types by Hour of Day in Toronto 2014-2017') +
    ylab('Number of Occurrences') +
    xlab('Hour(24-hour clock)') +
    theme_bw() +
    theme(plot.title = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"))

#crime by month
month_crime_group <- group_by(tor, occurrencemonth)
month_crime <- dplyr::summarise(month_crime_group, n=n())
month_crime$occurrencemonth = factor(month_crime$occurrencemonth, 
                                     levels = c("January", "February","March"
                                                ,"April","May", "June",
                                                "July", "August","September",
                                                "October", "November", "December"))
ggplot(aes(x=occurrencemonth, y=n), data =month_crime) + 
    geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen", group=1)  + 
  ggtitle('Crimes by month of year in Toronto 2014-2017') +
  ylab('Number of Occurrences') +
  xlab('Month') +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face= "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle=90))

#Crime by Location
location_group = group_by(tor,Neighbourhood)
crime_by_location = dplyr::summarise(location_group,n=n())
crime_by_location = crime_by_location[order(crime_by_location$n,decreasing = T),]
top_20_locations = top_n(crime_by_location,20)
ggplot(data = top_20_locations, aes(x=reorder(Neighbourhood,n), y=n))+
    geom_bar(stat = 'identity', width = 0.6, fill="red")+
    coord_flip()+
    geom_text(aes(label=n), hjust = -0.1, size = 3)+
    ggtitle('Top 20 Neighbourhoods with most Crimes')+
    xlab("Neighbourhood")+
    ylab("Number of Crimes")+
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))


#Map of Toronto Crimes
library(ggmap)
lattitude = tor$Lat
longitude = tor$Long
crimes = tor$MCI
Y = data.frame(crimes,lattitude,longitude)
colnames(Y) <- c('crimes', 'lattitude', 'longitude')
tor_bbox <- make_bbox(lon = tor$Long, lat = tor$Lat, f = 0.01)
tor_map <- get_map(location = tor_bbox, maptype = "terrain", scale = 2, color="bw", zoom = 11)
ggmap(tor_map) +
    geom_point(data=Y, aes(x = longitude, y = lattitude, color = "#27AE60"), 
               size = 0.8, alpha = 0.5) +
    xlab('Longitude') +
    ylab('Latitude') +
    ggtitle('Location of Major Crime Indicators Toronto 2014-2017') +
    guides(color=FALSE)



