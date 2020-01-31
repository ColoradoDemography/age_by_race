# SYA by Race Chart Dashboard
# Adam Bickford January 2020
# 
library(plotly)
library(shiny)

source("setup.R")

  ctylist  <- popPlace(DOLAPool)
  yearList <- YrSelect(DOLAPool)


function(req) {
  htmlTemplate("index.html",
                county=selectInput("county","Select a county:", choices= ctylist[,2]),  # Build this from data set
                year=selectInput("year","Select a year:",choices= yearList), # Build this from data set
                goBtn = actionButton("goButton","Generate Charts"),
                line_chart = plotlyOutput("LINE"),
                white_chart = plotlyOutput("WHITE"),
                hispanic_chart = plotlyOutput("HISP"),
                black_chart = plotlyOutput("BLACK"),
                asian_chart = plotlyOutput("ASIAN"),
                am_chart = plotlyOutput("AMIND"),
                dlBtn = downloadButton("CHDATA","Download Data (CSV)"))
 }



 
