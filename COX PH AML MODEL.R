library(shiny)
library(shinydashboard)
library(shinythemes)
library(survival)
library(data.table)
# model Building#
attach(aml)
coxmodel<-coxph(Surv(time,status)~x)
saveRDS(coxmodel,"coxmodel.rds")
coxmodel<-readRDS("coxmodel.rds")
#shiny stucture
ui<-fluidPage(theme = shinytheme(theme = "journal"),
              titlePanel("COX PROPORTIONAL HAZARD MODEL"),
              sidebarLayout(
                sidebarPanel(
                  numericInput("time",
                               label = "Time",
                               value = 5,
                               min = 0,
                               max = 162,
                               ),
                  selectInput("status",
                              label = "Censoring Status",
                              choices = c(1,0),
                              selected = 1
                              ),
                  selectInput("x",
                              label = 	"Maintenance chemotherapy",
                              choices = list("Maintained","Nonmaintained"),
                              selected = "Maintained"),
                  actionButton("submitbutton", "Go", class = "btn btn-primary")
                            
                    
                  ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata'), # Prediction results table
                  plotOutput("kaplan_meier"), 
                 fluid = TRUE ))
                )

server<-function(output,input,session){
  # input data
  inputdataset<-reactive({
    df<-data.frame(
      Survival<-c("time","status","x"),
      Value<-as.character(c(input$time,input$status,input$x)),
      stringsAsFactors = FALSE
    )
    #chemotherapy<-0
    #df<-rbind(df,chemotherapy)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test$x<-factor(test$x,levels=c("Maintained","Nonmaintained"))
    predicted_life=round(predict(coxmodel,test,type="survival"), 4)
    Output <- data.frame(predicted_life=round(predict(coxmodel,test,type="survival"), 4),treatment_status=ifelse(predicted_life>0.50,"Treatment is effective","There is no hope"))
   
    print(Output)
  })
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  # Prediction results table
  output$tabledata <- renderTable({
    if(input$submitbutton>0) { 
      isolate(inputdataset()) 
    } 
  })
  output$kaplan_meier<-renderPlot({
    km.model<-survfit(Surv(aml$time,aml$status)~aml$x)
    plot(km.model,col=rainbow(3))
    #points(output$predicted_life,pch=11)
  })
  
}
shinyApp(ui = ui, server = server)

