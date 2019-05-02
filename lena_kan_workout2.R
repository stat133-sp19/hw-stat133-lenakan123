#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws 
ui <- fluidPage(

   # Application title
   titlePanel("Different investing Scenarios with Data from Warmup6"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     #Input: The initial amount 
      column(4,
             sliderInput(inputId = "initial",
                         label = "Initial Amount",
                         min = 0,
                         max = 100000,
                         step = 500, 
                         value = 1000,
                         pre= "$")),
      
      # Input: The return rate (in%)
      column(width =4,
             sliderInput(inputId = "rate",
                         label = "Return Rate (in %)",
                         min = 0, 
                         max = 20, 
                         value = 5, 
                         step = 0.1)),
      
      
      # Input: The year (in%)
      column(width = 4, 
             sliderInput(inputId = "years", 
                         label = "Years",
                         min = 0, max = 50,
                         value = 20, step = 1)),
      
      # Input: The annual contribution (in%)
      column(width = 4,
             sliderInput(inputId = "contribution",
                         label = "Annual Contribution",
                         min = 0, max = 50000,
                         value = 2000, 
                         step = 500,
                         pre = "$")),
      
      
      
      # Input: The growth rate (in%)
      column(width = 4,
             sliderInput(inputId = "growth", 
                         label = "Growth Rate (in %)",
                         min = 0, max = 20,
                         value = 2, step = 0.1)),
      
     
      
      # Input: The return rate (in%)
      column(width = 4, 
             selectInput(inputId = "facet", 
                         label = "Facet?",
                         choices = c("Yes", "No")))
      
      ), 
   
      # Show a plot of the generated distribution
      mainPanel(
        h4('Timelines'),
         plotOutput("Timelines"),
        h4("Balances"),
         tableOutput("Balances")
         )
)

##Got the inputs correct, now proceed to make the outputs 
# Define server logic required to draw a histogram
server <- function(input, output){

  savings <- reactive({
    
    future_value <- function(i=input$initial, r=input$rate, y=input$years){
      if (y <0){
        stop("time cannot be a negative number")
      }
      else if (i <0){
        stop("initial investment cannot be negative")
      }
      else{
        return(i*(1+r/100)^y)
      }
    }
    
    annuity <- function(c=input$contribution, r=input$rate, y=input$years){
      if (y <0){
        stop("years cannot be negative")
      }
      else if (c <0){
        stop("contribution cannot be negative")
      }
      else{
        return(c*((1+r/100)^y -1)/(r/100))
      }
    }
    
    growing_annuity <- function(c=input$contribution, r=input$rate, g=input$growth, y=input$years){
      if (y <0){
        stop("years cannot be negative")
      }
      else if (c <0){
        stop("contribution cannot be negative")
      }
      else{
        return(c*((1+r/100)^y -(1+g/100)^y)/(r/100-g/100))
      }
    }
    
    no_contribution <- c()
    for (z in 0:input$years){
      value <- future_value(y=z)
      no_contribution[z+1] <- value
    }
    
    fixed <- c()
    for (z in 0:input$years){
      value<- annuity(y=z) + future_value(y=z)
      fixed[z+1] <-  value
    }
    
    growth <- c()
    for (z in 0:input$years){
      value <- growing_annuity(y=z) + future_value(y=z)
      growth[z+1] <- value
    }
    
    savings <- data.frame(year = c(0:input$years), no_contrib = no_contribution, fixed_contrib = fixed, growing_contrib = growth)
    return(savings)
    
  })
  
  output$Balances <- renderTable({savings()

  })
  
  savings_2 <- reactive({
    savings <- savings() 
    savings_2 <- data.frame(year = rep(c(0:input$years), 3), 
                                    savings = c(savings$no_contrib, savings$fixed_contrib, savings$growing_contrib), 
                                    variable = c(rep("no_contrib", length(savings$year)), 
                                                        rep("fixed_contrib", length(savings$year)), 
                                                        rep("growing_contrib", length(savings$year))))
    savings_2$variable <- factor(savings_2$variable, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
    return(savings_2)
  })
  
  output$Timelines <- renderPlot({
    if(input$facet == "No"){
      ggplot(savings())+ geom_line(aes(x=year, y=no_contrib, colour="no_contrib"))+
        geom_point(aes(x=year, y=no_contrib, colour="no_contrib"))+
        geom_line(aes(x=year, y=fixed_contrib, colour="fixed_contrib"))+
        geom_point(aes(x=year, y=fixed_contrib, colour="fixed_contrib"))+
        geom_line(aes(x=year, y=growing_contrib, colour="growing_contrib"))+
        geom_point(aes(x=year, y=growing_contrib, colour="growing_contrib"))+
        labs(title="Three modes of investing", x="year", y="value")+
        scale_colour_manual(name="variable",
                            breaks=c("no_contrib", "fixed_contrib", "growing_contrib"), 
                            values=c("no_contrib" = "red","fixed_contrib" = "blue", "growing_contrib" = "green"))
      
    }
    
    else{
      ggplot(savings_2(), aes(x=year, y=savings, color = variable))+
        geom_point()+
        geom_line()+
        geom_area(aes(fill = variable), alpha=0.4)+
        facet_wrap(~variable)
      
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

