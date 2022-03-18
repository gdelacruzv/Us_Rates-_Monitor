#####################################################################################################
### Import and tidy date
#####################################################################################################

# Logic:
# Import saved csv-file. If date < sys.date -> parse current years data, run calculations and parse.


# ------------------------------------------------------------------------------- 
# Connect to Amazon, and get csv-file
# ------------------------------------------------------------------------------- 



  select(-1)

#hej <- tableForShiny

##Get table
tableForShiny <- read.csv("tableUsRates.csv")

tableForShiny$date <- as.Date(tableForShiny$date)
tableForShiny$rate <- as.numeric(tableForShiny$rate)
tableForShiny$maturity <- as.numeric(tableForShiny$maturity)
tableForShiny$info <- as.character(tableForShiny$info)
tableForShiny$des <- as.character(tableForShiny$des)
tableForShiny <- arrange(tableForShiny, date)

tableUsRates$date <- as.Date(tableUsRates$date) 


# ------------------------------------------------------------------------------- 
# Logic script. If dates not up to date, parse data.
# ------------------------------------------------------------------------------- 

if (max(tableForShiny$date) < Sys.Date()) {
  source("parseData.r")
  
  ###only keep dates after max date in tableusrates
  
  
  
  tableUsRates$date <- as.Date(tableUsRates$date)
  tableUsRates$rate <- as.numeric(tableUsRates$rate)
  tableUsRates$maturity <- as.numeric(tableUsRates$maturity)
  tableUsRates$info <- as.character(tableUsRates$info)
  tableUsRates$des <- as.character(tableUsRates$des)
  
  tableUsRates <- tableUsRates %>%
    filter(date > max(tableForShiny$date))
  
  
  source("calculations.r")
  
  
  
  
  ##Merge
  #tableUsRates2 <- bind_rows(tableUsRates, df_table)
  tableForShiny <- bind_rows(mutate_all(tableForShiny, as.character), mutate_all(tableUsRates, as.character)) 
  
  
  # ------------------------------------------------------------------------------- 
  # Save new csv
  # ------------------------------------------------------------------------------- 
  
  write.csv(tableForShiny, file.path(tempdir(), "tableForShiny.csv"))
  
 
  
  
} 


write.csv(tableForShiny, "tableUsRates.csv", sep=",")
