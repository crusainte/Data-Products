# Explore top 10 countries by year

library(dplyr)
library(tidyr)
require(rCharts)
# Read in the dataset
data<-read.csv("data/ArrivalsByCountry.csv",skip=4,header=FALSE,stringsAsFactors=FALSE)

# Remove Unwanted rows without data
data<-data[1:41,]
data<-data[,-ncol(data)]

# Convert " - " character to NA
data[data==" - "] <- NA

# Transpose data and prevent all rows from becoming factors
dataT<-data.frame(t(data),stringsAsFactors = FALSE)
#dataT<-data.frame(data,stringsAsFactors = FALSE)

# Removing whitespaces and clean up the column and row names
colnames(dataT)<-gsub(" ","",dataT[1,])
colnames(dataT)<-gsub(",.*","",colnames(dataT))
rownames(dataT)<-gsub("X","",rownames(dataT))
dataT<-dataT[-1,]

# Remove columns which are not countries
dataT<-select(dataT, -c(2:4))
dataT<-select(dataT,-contains("CIS"))
dataT<-select(dataT,-c(Europe,Scandinavia,Americas,Oceania,Africa))

# Convert the entire data frame minus the date column into numeric type
dataT[,-1] <- as.integer(gsub(",", "", as.matrix(dataT[,-1])))
dataT[,1] <-gsub("^ | $","",dataT[,1])

# Split out the date into year and month column
datecol<-data.frame(date = dataT[,1])
datecol<-datecol %>% separate(date,c("year","month"))

# Convert the year and month columns into integer
datecol$year<-as.integer(datecol$year)
datecol$month<-match(datecol$month,month.abb)

# Combine the year and month columns back to the dataset
dataT<-cbind(datecol,dataT[,-1])

# Remove artifacts from rownames
rownames(dataT)<-gsub("V","",rownames(dataT))

# Remove year 2015 from the data as it is not a complete year
dataT<-filter(dataT,year!=2015)

# Now the data is ready!

prepareRanking <- function(inputFrame){
    # Transpose the result
    results<-data.frame(total=t(inputFrame[,-c(1:2)]))
    # Extract the rownames as a country
    countries<-data.frame(countries=colnames(inputFrame[,-c(1:2)]))
    
    # cbind the countries
    results<-cbind(countries,results)
    results<-arrange(results,desc(total))
    
    results$countries<-factor(results$countries,levels=results[order(results$total,decreasing=TRUE),"countries"])
    results<-mutate(results,total=total/100)
    # Return top 10 results
    return(results)
}

rankCountriesByYearRange <- function(inputFrame,yearStart,yearEnd){
    
    # Sum total for each year, filter based on year range and sum again
    tempFrame <- inputFrame %>%
        group_by(year) %>% 
        summarise_each(funs(sum(., na.rm = TRUE))) %>%
        filter(year %in% c(yearStart:yearEnd)) %>%
        summarise_each(funs(sum(., na.rm = TRUE)))
    
    results<-prepareRanking(tempFrame)
    rm(tempFrame)
    
    return(results)
}

plotResults <- function(inputFrame){

    plot <- nPlot(total ~ countries,
                  data = inputFrame[1:10,],
                  type = 'discreteBarChart'        
    )

    # Stagger the labels so that they will not be cut off
    plot$xAxis(axisLabel = "Countries", staggerLabels = TRUE)
    # To properly format the nvd3 labels
    plot$yAxis(axisLabel = "Total Arrivals (in Thousands)", tickFormat = "#!d3.format(',f')!#")
    # To prevent yAxislabel from being cut off
    plot$chart(margin=list(left=100))

    # IMPORTANT: For identifying the plot at server.R
    plot$set(dom = "myChart", title="OH YEAH")
    return(plot)
}