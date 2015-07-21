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
    
    # Return top 10 results
    return(results)
}

# Major issue with this, will need to overhaul
rankCountriesByAllYear <- function(inputFrame){
    # Sum total for each country
    results <- inputFrame %>%
        group_by(year) %>% summarise_each(funs(sum(., na.rm = TRUE)))
    
    results<-prepareRanking(results)
    
    return(results)
}

rankCountriesByYear <- function(inputFrame, inputYear){
    # Sum total for each country
    results<- inputFrame %>%
        filter(year==inputYear) %>%
        group_by(year) %>% summarise_each(funs(sum(., na.rm = TRUE)))
    
    results<-prepareRanking(results)
    
    return(results)
}

plotAllResults <- function(inputFrame){
    plot <- nPlot(countries ~ total, 
                 #group = 'countries', 
                 data = inputFrame,
                 type = 'multiBarChart'
    )
    plot$addParams(dom = 'myChart')
    plot    
#     plot<-ggplot(inputFrame[1:10,],aes(x=countries,y=total)) +
#         geom_bar(stat="identity") + 
#         theme(axis.text.x = element_text(angle=60,hjust=1)) +
#         xlab("Country") +
#         ylab("No of Arrivals") +
#         ggtitle("Top 10 Airport Arrivals in Singapore by Country for 1978-2014"
#         )
#     plot
}

plotResultsByYear <- function(inputFrame, inputYear){

    plot <- nPlot(total ~ countries,
                  data = inputFrame[1:10,],
                  type = 'discreteBarChart'
    )
    #plot$xAxis(rotateLabels=-60)
    # IMPORTANT: For identifying the plot at server.R
    plot$set(dom = "myChart")
    #plot$chart(margin = list(left = 100))
    return(plot)
#     plot<-ggplot(inputFrame[1:10,],aes(x=countries,y=total)) +
#         geom_bar(stat="identity") + 
#         theme(axis.text.x = element_text(angle=60,hjust=1)) +
#         xlab("Country") +
#         ylab("No of Arrivals") +
#         ggtitle(paste("Top 10 Airport Arrivals in Singapore by Country for ",inputYear)
#         )
#     plot
}

# Sum total for each country
# dataTsum<- dataT %>%
#     filter(year==2014) %>%
#     group_by(year) %>% summarise_each(funs(sum(., na.rm = TRUE)))

# Transpose the result
# dataTsum2<-data.frame(total=t(dataTsum[,-c(1:2)]))
# Extract the rownames as a country
# countries<-data.frame(countries=colnames(dataTsum[,-c(1:2)]))

# cbind the countries
# dataTsum2<-cbind(countries,dataTsum2)
# dataTsum3<-arrange(dataTsum2,desc(total))
# dataTsum4<-dataTsum3
# 
# dataTsum4$countries<-factor(dataTsum4$countries,levels=dataTsum4[order(dataTsum4$total,decreasing=TRUE),"countries"])

# Get top 10
# plot<-ggplot(dataTsum4[1:10,],aes(x=countries,y=total)) +
#     geom_bar(stat="identity") + 
#     theme(axis.text.x = element_text(angle=60,hjust=1)) +
#     xlab("Country") +
#     ylab("No of Arrivals") +
#     ggtitle("Top 10 Airport Arrivals in Singapore by Country"
#     )
# plot