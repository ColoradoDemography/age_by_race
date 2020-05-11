# SYA by Race Chart Dashboard  Support functions
# Adam Bickford January 2020
# 

library(tidyverse, quietly=TRUE)
library(stringr)
library(readr)
library(readxl, quietly=TRUE)
library(RPostgreSQL)
library(plotly)
library(scales, quietly=TRUE)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(RColorBrewer)


# Additions for Database pool
library('pool') 
library('DBI')
library('stringr')
library('config')

# Set up database pool 
config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)

dbGetInfo(DOLAPool)



# Support Functions
# NumFmt formats a numberic variable to a whold number, comma separated value
#
NumFmt <- function(inval){
  outval <- format(round(inval ,digits=0),  big.mark=",")
  return(outval)
}

# simpleCap produces string in Proper case
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

#YrSelect  Generates a list of years
YrSelect <- function(DBPool) {
   yrStr <- paste0("SELECT DISTINCT year FROM estimates.county_sya_race_estimates;")
   f.yrLookup <- dbGetQuery(DBPool, yrStr) %>% arrange(year)
return(f.yrLookup)   
}
    
# popPlace list of county names
popPlace <- function(DBPool) {
 

  # Create Connection Strings
  clookupStr <- paste0("SELECT DISTINCT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE placefips = 0;")

    # f.cLookup contains the county records
    f.cLookup <- dbGetQuery(DBPool, clookupStr)
    
 # Counties   
    f.cLookup <- arrange(f.cLookup, countyfips)
    f.cLookup[,2] <- sapply(f.cLookup[,2], function(x) simpleCap(x))
    f.cLookup$municipalityname <- str_replace(f.cLookup$municipalityname,"Colorado State","Colorado")
    
   
  return(f.cLookup)
}

#listToFips retuns a fips code from a county name

listTofips <- function(df, inList1){
  # Function to produce a vector of FIPS codes from an input list of names and codes
  fipsl <- df[which(df$municipalityname == inList1),1]
  return(fipsl)
} #end listTofips


# genPlotData  returns the analysis dataset
genPlotData <- function(DBPool,fips,yr){

  if(fips == 0) {
       sqlSYARace <- paste0("SELECT * FROM estimates.county_sya_race_estimates WHERE year = ",yr,";")
  } else {
       sqlSYARace <- paste0("SELECT * FROM estimates.county_sya_race_estimates WHERE (county_fips = ",fips,"AND year = ",yr,");")
  }

 f.SYARace <-  dbGetQuery(DBPool, sqlSYARace) 
 
 # Assembling data file

#Dataset with the  85+    
       f.SYARaceH85 <- f.SYARace %>% 
             filter(ethnicity == "Hispanic Origin") %>%
             mutate(race = "Hispanic Origin") %>%
             group_by(race,age) %>%
             summarise(Population = sum(count)) 
   
   f.SYARaceNH85 <- f.SYARace %>% 
             filter(ethnicity != "Hispanic Origin") %>%
             group_by(race,age) %>%
             summarise(Population = sum(count))
 
    f.SYARace85 <- bind_rows(f.SYARaceH85, f.SYARaceNH85)
    f.SYARace85$Population <- ceiling(f.SYARace85$Population)
    
    names(f.SYARace85) <- c("Race", "Age", "Population")

    f.SYARace85$Age <- ifelse(f.SYARace85$Age == 85,"85+",str_pad(f.SYARace85$Age,2,"0", side="left"))


    f.SYARace85$Race <- plyr::revalue(f.SYARace85$Race, c("Hispanic Origin" = "Hispanic",
                                                  "American Indian" = "American Indian, Not Hispanic",
                                                  "Asian/Pacific Islander" = "Asian/Pacific Islander, Not Hispanic",
                                                  "Black" = "Black, Not Hispanic",
                                                  "White" = "White, Not Hispanic"))




    f.SYARace85$Race <- factor(f.SYARace85$Race,levels= c("White, Not Hispanic",
                                                  "Hispanic",
                                                  "Black, Not Hispanic",
                                                   "Asian/Pacific Islander, Not Hispanic",
                                                   "American Indian, Not Hispanic"))
    
    f.SYARaceOut <- f.SYARace85 %>% filter(Age != "85+")
    f.SYARaceOut$Age <- as.numeric(f.SYARaceOut$Age)
    
    outlist <- list("chartData"= f.SYARaceOut, "DLData" = f.SYARace85)

return(outlist)
}

# GenPlot returns the Plots
GenPlot <- function(DBPool,ctyfips, ctyname, datyear) {
  
ctysel <- listTofips(ctyfips,ctyname)
datalist <- genPlotData(DBPool = DBPool,fips = ctysel,yr = datyear)
f.SYARace <- datalist$chartData
f.SYARaceDL <- datalist$DLData


f.SYARace[is.na(f.SYARace)] <- 0

   outCAP <- paste0("Colorado State Demography Office, Date Printed: ",as.character(format(Sys.Date(),"%m/%d/%Y")))
   grTitle <- paste0("Single Year of Age by Race: ",ctyname,", ",datyear)  
    xAxis <- list(range=c(0,85), dtick = 5, tick0 = 5, tickmode = "linear", title = "Age")
    yAxis <- list(separators = ',.', title = 'Population')
    
    colorset <- c("blue", "orange", "green", "red", "purple")

    ggline <- f.SYARace %>% ggplot() +
              aes(x = Age, y = Population, color = Race) +
              geom_line() +
              scale_x_continuous(limits= c(0,85), breaks=seq(0,85, by=5)) +
              scale_y_continuous(labels = scales::comma) +
              scale_color_manual(values = colorset) +
              labs(x = 'Age',
               y = 'Population',
               color = "Race/Ethnicity",
               title = grTitle) +
      theme(plot.title = element_text(hjust = 0.5, size=12),
            axis.text=element_text(size=8, color="grey50"),
            axis.text.x = element_text(hjust = 1, size = 8),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(color="grey90"), panel.grid.minor = element_blank(),
            legend.position = "right")

       ggSYALINE <-ggplotly(ggline) %>% layout(yaxis = yAxis, xaxis=xAxis,
                                                hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                                                annotations = list(text = outCAP,
                                                                   font = list(size = 10), showarrow = FALSE, yref = 'paper', y = -0.4))
    
       wBar <- f.SYARace %>%
         filter(Race == "White, Not Hispanic") %>%
         ggplot() +
         geom_bar(aes(x = Age, y = Population), fill = "blue", stat = "identity", color="black") +
         scale_x_continuous(limits= c(0,85), breaks=seq(0,85, by=5)) +
         scale_y_continuous(labels = scales::comma) +
         labs(x = 'Age',
              y = 'Population') +
         theme(axis.text=element_text(size=8, color="grey50"),
               axis.text.x = element_text(hjust = 1, size = 8),
               panel.background = element_rect(fill = "white", colour = "grey50"),
               panel.grid.major = element_line(color="grey90"), panel.grid.minor = element_blank(),
               legend.position = "none")
       
       

ggSYABARW <-  ggplotly(wBar) %>%
              layout( title=list(text = paste0(grTitle,
                                    '<br>',
                                    '<sup>',
                                    'White, Not Hispanic',
                                    '</sup>'),titlefont=list(size=10)), 
              yaxis = yAxis, xaxis=xAxis,
              hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                          annotations = list(text = outCAP,
                          font = list(size = 10), showarrow = FALSE, yref = 'paper', y = -0.4))


hBar <- f.SYARace %>%
  filter(Race == "Hispanic") %>%
  ggplot() +
  geom_bar(aes(x = Age, y = Population), fill = "orange", stat = "identity", color="black") +
  scale_x_continuous(limits= c(0,85), breaks=seq(0,85, by=5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Age',
       y = 'Population') +
  theme(axis.text=element_text(size=8, color="grey50"),
        axis.text.x = element_text(hjust = 1, size = 8),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(color="grey90"), panel.grid.minor = element_blank(),
        legend.position = "none")

ggSYABARH <- ggplotly(hBar) %>%
     layout( title=list(text = paste0(grTitle,
                                    '<br>',
                                    '<sup>',
                                    'Hispanic',
                                    '</sup>'),titlefont=list(size=8)), 
          yaxis = yAxis, xaxis=xAxis,
          hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCAP,
                      font = list(size = 10), showarrow = FALSE, yref = 'paper', y = -0.4))


bBar <- f.SYARace %>%
  filter(Race == "Black, Not Hispanic") %>%
  ggplot() +
  geom_bar(aes(x = Age, y = Population), fill = "green", stat = "identity", color="black") +
  scale_x_continuous(limits= c(0,85), breaks=seq(0,85, by=5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Age',
       y = 'Population') +
  theme(axis.text=element_text(size=8, color="grey50"),
        axis.text.x = element_text(hjust = 1, size = 8),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(color="grey90"), panel.grid.minor = element_blank(),
        legend.position = "none")

ggSYABARB <- ggplotly(bBar) %>%
     layout( title=list(text = paste0(grTitle,
                                    '<br>',
                                    '<sup>',
                                    'Black, Not Hispanic',
                                    '</sup>'),titlefont=list(size=10)), 
          yaxis = yAxis, xaxis=xAxis,
          hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCAP,
                      font = list(size = 10), showarrow = FALSE, yref = 'paper', y = -0.4))

aBar <- f.SYARace %>%
  filter(Race == "Asian/Pacific Islander, Not Hispanic") %>%
  ggplot() +
  geom_bar(aes(x = Age, y = Population), fill = "red", stat = "identity", color="black") +
  scale_x_continuous(limits= c(0,85), breaks=seq(0,85, by=5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Age',
       y = 'Population') +
  theme(axis.text=element_text(size=8, color="grey50"),
        axis.text.x = element_text(hjust = 1, size = 8),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(color="grey90"), panel.grid.minor = element_blank(),
        legend.position = "none")


ggSYABARAS <- ggplotly(aBar) %>%
     layout( title=list(text = paste0(grTitle,
                                    '<br>',
                                    '<sup>',
                                    'Asian/Pacific Islander, Not Hispanic',
                                    '</sup>'),titlefont=list(size=10)), 
          yaxis = yAxis, xaxis=xAxis,
          hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCAP,
                      font = list(size = 10), showarrow = FALSE, yref = 'paper', y = -0.4))


AMBar <- f.SYARace %>%
  filter(Race == "American Indian, Not Hispanic") %>%
  ggplot() +
  geom_bar(aes(x = Age, y = Population), fill = "purple", stat = "identity", color="black") +
  scale_x_continuous(limits= c(0,85), breaks=seq(0,85, by=5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Age',
       y = 'Population') +
  theme(axis.text=element_text(size=8, color="grey50"),
        axis.text.x = element_text(hjust = 1, size = 8),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(color="grey90"), panel.grid.minor = element_blank(),
        legend.position = "none")

ggSYABARAM <- ggplotly(AMBar) %>%
     layout( title=list(text = paste0(grTitle,
                                    '<br>',
                                    '<sup>',
                                    'American Indian, Not Hispanic',
                                    '</sup>'),titlefont=list(size=10)), 
          yaxis = yAxis, xaxis=xAxis,
          hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCAP,
                      font = list(size = 10), showarrow = FALSE, yref = 'paper', y = -0.4))


#Restructuring data

f.outData <- spread(f.SYARaceDL[,1:3],Race,Population)

outlist <- list("LINE" = ggSYALINE, "WHITE" = ggSYABARW, "HISP" = ggSYABARH, "BLACK" = ggSYABARB,
                "ASIAN" = ggSYABARAS, "AMIND" = ggSYABARAM, "CHDATA" = f.outData)
return(outlist)
}


