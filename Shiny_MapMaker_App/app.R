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
# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Wedding Map Maker"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        p(strong("Upload your zip code file. App expects one column with header 'Zips'.")),  
        fileInput(inputId = "datafile", label = "Upload a csv zipcode file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
        p(strong("Customize your image")),
        textInput(inputId="toptext", label="Top Text", value = "Bride & Groom"),
        textInput(inputId="bottext", label="Bottom Text", value = "June 7th 2018"),
        textInput(inputId="wedzip", label="Zip Code for Wedding", value = "66212"),
        #p(strong("Specify colors...")), 
        tags$a(href="https://www.google.com/search?q=color+picker", "Use this link for selecting colors "),
        textInput(inputId="var_backgroundcolor", label="Background Color", value = "#021c44"),
        textInput(inputId="var_heartcolor", label="Heart Color", value = '#ce0202'),
        textInput(inputId="var_statefillcolor", label="State Fill Color", value = "#6b6c6d"),
        textInput(inputId="var_statelinecolor", label="State Line Color", value = "#c0c2c4"),
        textInput(inputId="var_textcolor", label="Text Color", value = "#FFFFFF"),
        textInput(inputId="watermark", label="Remove Watermark Password", value = "***")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("mapPlot", width = "100%",height = "500px"),
         #plotOutput("test"),
         br(),
         br(),
         p("(If any) The input file included the following which could not be mapped as US Zip Codes:"), 
         tableOutput("contents")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Load Zipcode reference file
  ZipLatLong <- read.csv("ZipLatLongCurated.csv", sep=",",colClasses=c("Zip"="character")) 
  
  #load invite zipcodes
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      #load input file to InviteZips
      inFile <- input$datafile
      if (is.null(inFile))
        return(NULL)
      InviteZips <-read.csv(inFile$datapath, header=TRUE,colClasses=('character')) #input chosen file first column as character
      
      #head(InviteZips)
      
      #dedup invite zips to uzips
      #uzips <- as.data.frame(unique(InviteZips$Zip))
      uzips <- as.data.frame(unique(InviteZips[, 1]))
      #head(uzips)
      colnames(uzips) <- "ZIP" #name column "ZIP"
      
      subset(uzips, !(ZIP %in% ZipLatLong$Zip)) # Use this code to see if there are any zip codes that aren't in the lookup list
      })
    
    
    #######THIS WORKS  
   #output$test <- renderPlot({
   #    hist(rnorm(100), main = input$toptext)
   #   plot(ZipLatLong$Latitude,ZipLatLong$Longitude, main = input$toptext) 
  #     })
    #############
    
   output$mapPlot <- renderPlot({
     
     inFile <- input$datafile
     if (is.null(inFile))
       return(NULL)
     InviteZips <-read.csv(inFile$datapath, header=TRUE,colClasses=('character')) #input chosen file first column as character
     
     #unique zips
     uzips <- as.data.frame(unique(InviteZips[, 1]))
    
     colnames(uzips) <- "ZIP" #name column "ZIP"
     
      #merge uzips and lookups into one dataframe
     places_loc <-merge(uzips, ZipLatLong, by.x="ZIP", by.y="Zip") 
      #build dataframe for wedding heart
     WedZip <- as.data.frame(input$wedzip) #######AS INPUT FROM APP 
     colnames(WedZip) <- "ZIP" #name column "Zip"
     WedZip <-merge(WedZip, ZipLatLong, by.x="ZIP", by.y="Zip")
      
      ##load base shape file from ggplot2 ##http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
     us <- map_data("state")
      
     plot(ZipLatLong$Latitude,ZipLatLong$Longitude, main = input$toptext) 
      #######BUILD PLOT USING DECLARED VARIABLES ABOVE#######
      
      ##First ggplot outputs results to file with scaled margins  
     finalprintplot<-
        
      ggplot()+ 
        geom_polygon(data = us, #makes the state outline
                     aes(x = long, y = lat, group=group),
                    color =input$var_statelinecolor , #"#f4f4f4" -Line color ############3
                     fill = input$var_statefillcolor) + #fill color
        coord_fixed(1.3) + #sets the lat,long scale constant
        geom_point(aes(x=Longitude, y=Latitude), #plots the points for guests
                   data = places_loc,  
                   alpha = 0.5,  
                   size = 15, #15 --have to make really big to save as large image
                   color = "white")+ 
        geom_point(aes(x=Longitude, y=Latitude), #plots the point for wedding
                data = WedZip, #use wedding location data frame 
                   alpha = 1,  
                   size = 10, #10
                   color = input$var_heartcolor,
                  shape = "❤", #3 "❤", ◎",             
                   stroke = 1)+ #2
        theme_void() + #removes all the chart stuff
      #theme(plot.margin = unit(c(2.55,1.1,3,1.1), "cm"),plot.background = element_rect(fill = input$var_backgroundcolor )) + #trial and error with the plot margins to get it to fit 8x10 dimensions
       #theme(aspect.ratio=8.5/10.5,plot.background = element_rect(fill = input$var_backgroundcolor )) + #trial and error with the plot margins to get it to fit 8x10 dimensions
       annotate("text", x = -97, y = 54, colour = input$var_textcolor, size=18,family="URW Chancery L",fontface="italic", label = input$toptext) + # try to center a pain
       annotate("text", x = -97, y = 40, colour = "white", size=17, label = ifelse(input$watermark=="pTNKpQTC284yHP","", "WATERMARKED"),alpha = .75) + # WATERMARK
       annotate("text", x = -98, y = 23.5, colour = input$var_textcolor, size=13,family="URW Chancery L",fontface="italic", label = input$bottext) # try to center a pain
     
      #finalprintplot + theme(plot.margin = unit(c(.125,.06,.25,.125), "cm"),plot.background = element_rect(fill = input$var_backgroundcolor))
      finalprintplot + theme(plot.margin = unit(c(1.5,.06,1,.125), "cm"),plot.background = element_rect(fill = input$var_backgroundcolor))
      
     #finalprintplot+ theme(plot.margin = unit(c(2.55,1.1,3,1.1), "cm"),plot.background = element_rect(fill = input$var_backgroundcolor )) #trial and error with the plot margins to get it to fit 8x10 dimensions

   })   
    
    
    
    }


# Run the application 
shinyApp(ui = ui, server = server)

