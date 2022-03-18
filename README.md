# Us_Rates-_Monitor
#parseData.r parse data from US Treasury using Rvest.

#calculations.r calculates the forward curve, breakeven rates and other indices used in the app.

#importData.r is the main script that binds together the parts for always displaying the latest data in the app. The script imports the CSV-file from Amazon S3 and if the CSV-file is not up to date, it will run the parseData script parsing the latest data followed by calculations. It will then save as the new data table as an updated CSV-file. Besides avoiding unnecessary scraping, this approach also limits the number of calculations used in the Shiny app which makes it faster to run.

#app.r the Shiny app that includes both the ui and the server. A bit messy but gets the job done.
