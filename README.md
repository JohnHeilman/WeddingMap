# WeddingMap
This repository has some code used to generate data visualization we used for our wedding "guest book." Based on our invitees addresses, we plot the zip codes on a map using ggplot.

There are two flavors of the code. First is a normal interactive R Script (in folder Interactive_RScript.) To play around with Shiny, I also converted the basic approach into a shiny app where you can make changes to the plot characteristics through the shiny web gui (app files in Shiny_MapMaker_App.)

For either approach, you'll need a single CSV file with a list of zip codes to map. Don't worry about de-duping the list as the script will handle that for you. See the file named InviteZips.csv for an example.
