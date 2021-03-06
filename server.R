source("setup.R")

function(input, output, session) {

observeEvent( input$goButton,{
    cty <- popPlace(DOLAPool)
    selcty <- input$county
    selyr<- input$year
    OutPlot <- GenPlot(DBPool=DOLAPool,ctyfips=cty, ctyname= selcty, datyear= selyr)

#OutData <- genData(DBPool=DOLAPool,ctyfips=ctylist, ctyname= input$county, datyear= year)

output$LINE <-  renderPlotly({OutPlot[["LINE"]]})
output$WHITE <- renderPlotly({OutPlot[["WHITE"]]})
output$HISP <-  renderPlotly({OutPlot[["HISP"]]})
output$BLACK <- renderPlotly({OutPlot[["BLACK"]]})
output$ASIAN <- renderPlotly({OutPlot[["ASIAN"]]})
output$AMIND <- renderPlotly({OutPlot[["AMIND"]]})

output$CHDATA=downloadHandler(
    filename= function(){
      paste0("Age_by_Race_",selcty,"_",selyr,".csv")
    },
    content= function(file){
      write.csv(OutPlot[["CHDATA"]], file, row.names=FALSE)
    }
  )

})
}
