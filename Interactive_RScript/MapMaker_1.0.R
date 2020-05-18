###Script to generate wedding maps. Designed to run fully locally and avoid dependencies on things changing...

###CHANGELOG
#1.0 original Couples names at top, date at bottom


#######LOAD PACKAGES############
#if it breaks someday, might be able to force install known working version of ggplot2 2.2.1
#require(devtools)
#install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")

library(ggplot2)

#######DECLARE YOUR VARIABLES#######
#Common colors. Original Navy = '#021c44', White = '#FFFFFF', Red = '#f50602' 
# Zip codes for testing = very North ND ="58793" , KCMO= "64112", Very South TX = 78526

var_backgroundcolor <-'#021c44'
var_heartcolor <- '#ce0202' 
var_statefillcolor <- "#6b6c6d"
var_statelinecolor <- "#c0c2c4"
var_textcolor <- '#FFFFFF'
var_wedzip <- "64112" 
var_top_text <-"~Alexi & John~"
var_bot_text <-"July 18th 2020"
var_customerfilename <- "~/Documents/WeddingMapMaker/testcustomer.jpg"


#######LOAD AND PREP DATA#######
#Load Zipcode reference file
ZipLatLong <- read.csv("~/Documents/WeddingMapMaker/ZipLatLongCurated.csv", sep=",",colClasses=c("Zip"="character")) 

#Note force Zip column to be character to preserve leading zeros
#View(ZipLatLong)

#load invite zipcodes
InviteZips <- read.csv("~/Documents/WeddingMapMaker/InviteZips.csv", sep=";",colClasses=c("Zip"="character")) 
#Again note force Zip column to character to preserve leading zeros
#View(InviteZips)

#Prep Base Data
uzips <- as.data.frame(unique(InviteZips$Zip)) #deduplicate zip codes and coerce to dataframe
colnames(uzips) <- "ZIP" #name column "ZIP"
#nrow(uzips) #Number of rows
#View(uzips) #View

subset(uzips, !(ZIP %in% ZipLatLong$Zip)) # Use this code to see if there are any zip codes that aren't in the lookup list

#merge into one dataframe
places_loc <-merge(uzips, ZipLatLong, by.x="ZIP", by.y="Zip")
#View(places_loc)


#build dataframe for wedding heart
WedZip <- as.data.frame(var_wedzip)
colnames(WedZip) <- "ZIP" #name column "Zip"
WedZip <-merge(WedZip, ZipLatLong, by.x="ZIP", by.y="Zip")

##load base shape file from ggplot2 ##http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
us <- map_data("state")



#######BUILD PLOT USING DECLARED VARIABLES ABOVE#######

##First ggplot outputs results to file with scaled margins  
finalprintplot<-
  ggplot()+ 
  geom_polygon(data = us, #makes the state outline
               aes(x = long, y = lat, group=group),
               color = var_statelinecolor, #"#f4f4f4" -Line color
               fill = var_statefillcolor) + #fill color
  coord_fixed(1.3) + #sets the lat,long scale constant
  geom_point(aes(x=Longitude, y=Latitude), #plots the points for guests
             data = places_loc,  
             alpha = 0.5,  
             size = 40, #15 --have to make really big to save as large image
             color = "white")+ 
  geom_point(aes(x=Longitude, y=Latitude), #plots the point for wedding
             data = WedZip, #use wedding location data frame 
             alpha = 1,  
             size = 20, #10
             color = var_heartcolor,
             shape = "❤", #3 "❤", ◎",             
             stroke = 1)+ #2
  theme_void() + #removes all the chart stuff
  #theme(plot.margin = unit(c(5,1,1,1), "cm"))+
  theme(plot.margin = unit(c(2.55,1.1,3,1.1), "cm"),plot.background = element_rect(fill = var_backgroundcolor )) + #trial and error with the plot margins to get it to fit 8x10 dimensions
  #annotate("text", x = -113, y = 27, colour = var_textcolor, size=18,family="URW Chancery L",fontface="italic", label = var_text) # left pocket #v1.0
  annotate("text", x = -98, y = 53, colour = var_textcolor, size=25,family="URW Chancery L",fontface="italic", label = var_top_text) + # try to center a pain
  annotate("text", x = -98, y = 21, colour = var_textcolor, size=18,family="URW Chancery L",fontface="italic", label = var_bot_text) # try to center a pain

#######SAVE PLOT AS JPG FOR 8x10 - 16x20 PRINTING AND PREVIEW#######
#save plot as hi res jpeg
ggsave(var_customerfilename, plot=finalprintplot, width=20,height=16, dpi=300)

##Second ggplot uses default margins for preview 
finalprintplot + theme(plot.margin = unit(c(2,1,4,1), "cm"),plot.background = element_rect(fill = var_backgroundcolor )) 









