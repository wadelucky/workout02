#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Analysis in investment"),

         fluidRow(
        column(4,sliderInput("amount",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     step = 1000,
                     value = 1000)),
        column(4,sliderInput("contrib",
                     "Annual Contribution",
                     min=0,
                     max=50000,
                     step=500,
                     value=2000)),
         column(4,sliderInput("rate",
                     "Return Rate(in %)",
                     min=0,
                     max=20,
                     step=0.1,
                     value=5)),
         column(4,sliderInput("growth",
                     "Growth Rate(in %)",
                     min=0,
                     max=20,
                     step=0.1,
                     value=2)),
         column(4,sliderInput("year",
                     "Years",
                     min=0,
                     max=50,
                     step=1,
                     value=20)),
         column(4,selectInput("facet",
                     "Facet?",
                     choices=c("No","Yes"),
                     selected = "No"))
         ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         h4("Timeline"),
         plotOutput("distPlot"),
         
         h4("Balances"),
         verbatimTextOutput("view")
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   modalities<-reactive({
     future_value<-function(amount,rate,years){
       FV=amount#initialization
       if(years==0){return(FV)}
       for (i in 1:years){
         FV=FV*(1+rate)
       }
       return(FV)
     }
     annuity<-function(contrib,rate,years){
       plus<-1#initialization
       if(years==0){return(0)}
       for(i in 1:years){
         plus<-plus*(1+rate)
       }
       FVA=contrib*(plus-1)/rate
       return(FVA)
     }
     growing_annuity<-function(contrib,rate,growth,years){
       grow<-1  
       plus<-1#initialization
       if(years==0){return(0)}
       for(i in 1:years){
         plus<-plus*(1+rate)
         grow<-grow*(1+growth)
       }
       FVGA=contrib*(plus-grow)/(rate-growth)
       return(FVGA)
     }
     #initialization
     no_contrib<-rep(input$amount,input$year+1)
     fixed_contrib<-rep(input$amount,input$year+1)
     growing_contrib<-rep(input$amount,input$year+1)
     for(i in 1:input$year+1){
       no_contrib[i]<-future_value(amount=input$amount,rate=input$rate/100,years=i-1)
       fixed_contrib[i]<-future_value(amount=input$amount,rate=input$rate/100,years=i-1)+annuity(contrib=input$contrib,rate=input$rate/100,years=i-1)
       growing_contrib[i]<-future_value(amount=input$amount,rate=input$rate/100,years=i-1)+growing_annuity(contrib=input$contrib,rate=input$rate/100,growth=input$growth/100,years=i-1)
     }
     year<-c(0:input$year)
     #type<-c(rep("1: no_contrib",input$year+1),rep("2: fixed_contrib",input$year+1),rep("3: growing_contrib",input$year+1))
     modalities<-data.frame(year,no_contrib,fixed_contrib,growing_contrib)
   })
   # backup<-reactive({
   #   no_contribution<-rep(0,input$48)
   #   fixed_contribution<-rep(0,48)
   #   growing_contribution<-rep(0,48)
   #   for(j in 1:3){
   #     for(i in 1:16){
   #       no_contribution[i+16*j-16]<-future_value(amount=10000,rate=rat[j],years=i-1)
   #       fixed_contribution[i+16*j-16]<-future_value(amount=10000,rate=rat[j],years=i-1)+annuity(contrib=2000,rate=rat[j],years=i-1)
   #       growing_contribution[i+16*j-16]<-future_value(amount=10000,rate=rat[j],years=i-1)+growing_annuity(contrib=2000,rate=rat[j],growth=0.04,years=i-1)
   #     }
   #   }
   #   year<-rep(c(0:15),3)
   #   type<-c(rep("1: regular savings",16),rep("2: high-yield savings",16),rep("3: index fund",16))
   #   triple_modalties<-data.frame(type,year,no_contribution,fixed_contribution,growing_contribution)
   #   triple_modalties
   # })
   output$distPlot <- renderPlot({
     if(input$facet=="No"){
       gg<-ggplot(data = modalities())+
       geom_point(aes(x = year, y = fixed_contrib,color = "2: fixed_contrib")) +
       geom_line(aes(x = year, y = fixed_contrib,color = "2: fixed_contrib")) +
       geom_point(aes(x = year, y = no_contrib, color = "1: no_contrib")) + 
       geom_line(aes(x = year, y = no_contrib, color = "1: no_contrib")) + 
       geom_point(aes(x = year, y = growing_contrib, color = "3: growing_contrib")) +
       geom_line(aes(x = year, y = growing_contrib, color = "3: growing_contrib")) +
       xlab("year")+ylab("value")+
       ggtitle("Three modes of investing")+
       labs(color="variable")
     }
     if(input$facet=="Yes"){
       temp<-melt(modalities(),id.vars="year")
      gg <- ggplot(data = temp,aes(x=year,y=value))+geom_line(aes(color=variable))+
        geom_area(aes(fill=variable),alpha=0.5)+
        geom_point(aes(color=variable))+facet_grid(~variable)+ggtitle("Three modes of investing")
       }
     return(gg)
   })
   output$view <-renderPrint({
     #print(melt(modalities(),id.vars="year"))
     print(modalities())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

