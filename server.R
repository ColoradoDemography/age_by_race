source("setup.R")

function(input, output, session) {

observeEvent( input$goButton,{
  browser()
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

})
}
