# code from Professor Lecy

## USER INTERFACE 

my.ui <- fluidPage(

  
  titlePanel("Stock Value"),

  
  sidebarLayout(
  
  
    sidebarPanel(
    

                  
         checkboxGroupInput("companies", "Select Which Companies to Display:",
                      
                      c("Company 1" = "org1",
                        "Company 2" = "org2",
                        "Company 3" = "org3"),
                        
                        selected=c("org1","org2")
                        
                        )
 
 
    ), # end of sidebarPanel
    
    
    
    mainPanel( 
    
                 plotOutput("distPlot"),
                 
                 verbatimTextOutput("line1"), 
                 verbatimTextOutput("line2"),
                 verbatimTextOutput("line3"),
                 verbatimTextOutput("line4")                 
              )
    
    
    
  ) # end of sidebarLayout
  
  
  
) # end of fluidPage



## SERVER

# data steps

org1 <- cumsum( rnorm(1000) )
org2 <- cumsum( rnorm(1000) )
org3 <- cumsum( rnorm(1000) )



max.y <- max( c(org1,org2,org3) )
min.y <- min( c(org1,org2,org3) )



my.server <- function(input, output) 
{

  
  output$distPlot <- renderPlot({
  
  
          plot( org1, ylim=c(min.y,max.y), type="n", xlab="Time",ylab="Performance")
          


          if( "org1" %in% input$companies )
          {
            lines( org1, col="blue" )
          }
          if( "org2" %in% input$companies )
          {
             lines( org2, col="green" )
          }          
          if( "org3" %in% input$companies )
          {
            lines( org3, col="darkred" )
          }
    
    
    
  }) # end of renderPlot
  

 
  output$line1 <- renderPrint({ "The value of input$companies:" }) 
  
  output$line2 <- renderPrint({ input$companies })
  
  output$line3 <- renderPrint({ paste("Class of input$companies:", class(input$companies) ) })
  
  output$line4 <- renderPrint({ paste("'org1' %in% input$companies:", 'org1' %in% input$companies ) })

} # end of server






# LAUNCH THE APP !

shinyApp( ui = my.ui, server = my.server )



