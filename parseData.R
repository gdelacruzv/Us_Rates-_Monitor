
#####################################################################################################
### Script for parsing rates
#####################################################################################################
library(shiny)
library(shinydashboard)
library(slider)
library(shinyWidgets)
library(rvest, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(gridExtra)
library(rstudioapi)


#####################################################################################################
### Script for parsing rates
#####################################################################################################

### Info
#Data have been parsed once and for each time it is parsed, it is saved in a csv-file. Look over how it think around year turn.


# ------------------------------------------------------------------------------- 
# Pages, years to parse, create empty df_realyield
# ------------------------------------------------------------------------------- 

pages = c("https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=realyieldYear&year=")

base_url = "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=realyieldYear&year="

#years = year(Sys.Date())

years = c("2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009","2008")

des_name = sub('.*data=', '', base_url)
des_name = sub('&year*.', '', des_name)

tableRealRates <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(tableRealRates) <- c( 'date', 'term', 'rate', 'des')

# ------------------------------------------------------------------------------- 
# Parsing function
# ------------------------------------------------------------------------------- 
parse_us_rates <- function(year) {
  url = paste(base_url, year, sep="")
  parse_rates <- read_html(url)
  
  df_realyield <- parse_rates %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  df_realyield <- df_realyield[[2]]
  return(df_realyield)
  
}
# ------------------------------------------------------------------------------- 
# Parse all pages and dates
# ------------------------------------------------------------------------------- 
for (base_url in pages) 
  
  des_name = sub('.*data=', '', base_url)
des_name = sub('&year*.', '', des_name)


for (year in years) {
  Sys.sleep(sample(1:3, 1, replace=T))
  df_realyield <- parse_us_rates(year)
  
  
  #rename first column to date
  df_realyield <- rename(df_realyield, "date" = 1)
  df_realyield$date <- as.Date(df_realyield$date, format = "%m/%d/%y") ##convert dates
  
  ###Create df_realyield with dates, from first each year to last, or to sys.date during current
  
  first_date = paste(year,"-01-01",sep="")
  if (paste(year,"-12-31",sep="") < Sys.Date()) {
    last_date = paste(year,"-12-31",sep="")
  } else {
    last_date = Sys.Date()
  }
  
  dates <- data.frame(
    date = seq(as.Date(first_date), as.Date(last_date), by = 'days')
  )
  df_realyield <- merge(dates, df_realyield, by ="date", all.x = TRUE)
  
  df_realyield <- df_realyield %>%
    fill(names(df_realyield), .direction = "down") %>%
    fill(names(df_realyield), .direction = "up")
  
  
  
  ###dplyr table
  df_realyield <- df_realyield %>%
    pivot_longer(
      !date,
      names_to = "term",
      values_to ="rate", values_transform = list(rate = as.character)
    ) %>%
    mutate(des = des_name) %>%
    mutate(info = "") 
  
  tableRealRates <- bind_rows(mutate_all(tableRealRates, as.character), mutate_all(df_realyield, as.character)) 
  
  #Clean
  
  tableRealRates$rate <- as.numeric(tableRealRates$rate)
  #df_realyield$date <- as.Date(df_realyield$date, format = "%m/%d/%y") ##convert dates
  
}     

# ------------------------------------------------------------------------------- 
# Tidying
# ------------------------------------------------------------------------------- 
tableRealRates <- tableRealRates %>%
  mutate(t1 = as.numeric(gsub("([0-9]+).*$", "\\1", term)))%>%
  mutate(maturity = case_when(
    grepl(" yr", term, fixed = TRUE) ~ (t1 *365 / 365),
    grepl(" YR", term, fixed = TRUE) ~ (t1 *365 / 365),
    
    #bills
    grepl("1 mo", term, fixed= TRUE) ~ (4 * 7 /365), ##365 days because bond equivalent
    grepl("2 mo", term, fixed= TRUE) ~ (8 * 7 /365), ##365 days because bond equivalent
    grepl("3 mo", term, fixed= TRUE) ~ (13 * 7 /365), ##365 days because bond equivalent
    grepl("6 mo", term, fixed= TRUE) ~ (26 * 7 /365) ##365 days because bond equivalent
    
  ))%>%
  
  ##Fix term column so names are the same
  mutate(term = case_when(
    grepl(" YR", term, fixed = TRUE) ~ paste(t1, "y", sep=""),
    grepl(" yr", term, fixed = TRUE) ~ paste(t1, "y", sep=""),
    grepl(" m", term, fixed = TRUE) ~ paste(t1, "m", sep="")
  ))%>%
  select(-t1)


#####################################################################################################
### Script for parsing nominal rates
#####################################################################################################

### Info
#Data have been parsed once and for each time it is parsed, it is saved in a csv-file. Look over how it think around year turn.


# ------------------------------------------------------------------------------- 
# Pages, years to parse, create empty df_yields
# ------------------------------------------------------------------------------- 

pages = c("https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=")

base_url = "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year="

#years = year(Sys.Date())

years = c("2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009","2008")

des_name = sub('.*data=', '', base_url)
des_name = sub('&year*.', '', des_name)

tableRates <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(tableRates) <- c( 'date', 'term', 'rate', 'des')

# ------------------------------------------------------------------------------- 
# Parsing function
# ------------------------------------------------------------------------------- 
parse_us_rates <- function(year) {
  url = paste(base_url, year, sep="")
  parse_rates <- read_html(url)
  
  df_yields <- parse_rates %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  df_yields <- df_yields[[2]]
  return(df_yields)
  
}
# ------------------------------------------------------------------------------- 
# Parse all pages and dates
# ------------------------------------------------------------------------------- 
for (base_url in pages) 
  
  des_name = sub('.*data=', '', base_url)
des_name = sub('&year*.', '', des_name)


for (year in years) {
  Sys.sleep(sample(1:3, 1, replace=T))
  df_yields <- parse_us_rates(year)
  
  
  #rename first column to date
  df_yields <- rename(df_yields, "date" = 1)
  df_yields$date <- as.Date(df_yields$date, format = "%m/%d/%y") ##convert dates
  
  ###Create df_yields with dates, from first each year to last, or to sys.date during current
  
  first_date = paste(year,"-01-01",sep="")
  if (paste(year,"-12-31",sep="") < Sys.Date()) {
    last_date = paste(year,"-12-31",sep="")
  } else {
    last_date = Sys.Date()
  }
  
  dates <- data.frame(
    date = seq(as.Date(first_date), as.Date(last_date), by = 'days')
  )
  df_yields <- merge(dates, df_yields, by ="date", all.x = TRUE)
  
  df_yields <- df_yields %>%
    fill(names(df_yields), .direction = "down") %>%
    fill(names(df_yields), .direction = "up")
  
  
  
  ###dplyr table
  df_yields <- df_yields %>%
    pivot_longer(
      !date,
      names_to = "term",
      values_to ="rate", values_transform = list(rate = as.character)
    ) %>%
    mutate(des = des_name) %>%
    mutate(info = "") 
  
  tableRates <- bind_rows(mutate_all(tableRates, as.character), mutate_all(df_yields, as.character)) 
  
  #Clean
  
  tableRates$rate <- as.numeric(tableRates$rate)
  #df_yields$date <- as.Date(df_yields$date, format = "%m/%d/%y") ##convert dates
  
  
  
  
}     


# ------------------------------------------------------------------------------- 
# Tidying
# ------------------------------------------------------------------------------- 
tableRates <- tableRates %>%
  mutate(t1 = as.numeric(gsub("([0-9]+).*$", "\\1", term)))%>%
  mutate(maturity = case_when(
    grepl(" yr", term, fixed = TRUE) ~ (t1 *365 / 365),
    grepl(" YR", term, fixed = TRUE) ~ (t1 *365 / 365),
    
    #bills
    grepl("1 mo", term, fixed= TRUE) ~ (4 * 7 /365), ##365 days because bond equivalent
    grepl("2 mo", term, fixed= TRUE) ~ (8 * 7 /365), ##365 days because bond equivalent
    grepl("3 mo", term, fixed= TRUE) ~ (13 * 7 /365), ##365 days because bond equivalent
    grepl("6 mo", term, fixed= TRUE) ~ (26 * 7 /365) ##365 days because bond equivalent
    
  ))%>%
  
  ##Fix term column so names are the same
  mutate(term = case_when(
    grepl(" YR", term, fixed = TRUE) ~ paste(t1, "y", sep=""),
    grepl(" yr", term, fixed = TRUE) ~ paste(t1, "y", sep=""),
    grepl(" m", term, fixed = TRUE) ~ paste(t1, "m", sep="")
  ))%>%
  select(-t1)

#####################################################################################################
### Script for parsing bill rates
#####################################################################################################

### Info
#Data have been parsed once and for each time it is parsed, it is saved in a csv-file. Look over how it think around year turn.


# ------------------------------------------------------------------------------- 
# Pages, years to parse, create empty df_bills
# ------------------------------------------------------------------------------- 

pages = c("https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=billRatesYear&year=")

base_url = "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year="

#years = year(Sys.Date())

years = c("2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009","2008")

des_name = sub('.*data=', '', base_url)
des_name = sub('&year*.', '', des_name)

TableBillRates <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(TableBillRates) <- c( 'date', 'term', 'rate', 'des')

# ------------------------------------------------------------------------------- 
# Parsing function
# ------------------------------------------------------------------------------- 
parse_us_rates <- function(year) {
  url = paste(base_url, year, sep="")
  parse_rates <- read_html(url)
  
  df_bills <- parse_rates %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  df_bills <- df_bills[[2]]
  return(df_bills)
  
}
# ------------------------------------------------------------------------------- 
# Parse all pages and dates
# ------------------------------------------------------------------------------- 
for (base_url in pages) 
  
  des_name = sub('.*data=', '', base_url)
des_name = sub('&year*.', '', des_name)


for (year in years) 
  Sys.sleep(sample(1:3, 1, replace=T))
df_bills <- parse_us_rates(year)

df_bills<-df_bills[-c(3,5,7,9,10,11)]



#rename first column to date
df_bills <- rename(df_bills, "date" = 1)
df_bills$date <- as.Date(df_bills$date, format = "%m/%d/%y") ##convert dates

###Create df_bills with dates, from first each year to last, or to sys.date during current

first_date = paste(year,"-01-01",sep="")
if (paste(year,"-12-31",sep="") < Sys.Date()) {
  last_date = paste(year,"-12-31",sep="")
} else {
  last_date = Sys.Date()
}

dates <- data.frame(
  date = seq(as.Date(first_date), as.Date(last_date), by = 'days')
)
df_bills <- merge(dates, df_bills, by ="date", all.x = TRUE)

df_bills <- df_bills %>%
  fill(names(df_bills), .direction = "down") %>%
  fill(names(df_bills), .direction = "up")



###dplyr table
df_bills <- df_bills %>%
  pivot_longer(
    !date,
    names_to = "term",
    values_to ="rate", values_transform = list(rate = as.character)
  ) %>%
  mutate(des = des_name) %>%
  mutate(info = "") 

TableBillRates <- bind_rows(mutate_all(TableBillRates, as.character), mutate_all(df_bills, as.character)) 

#Clean

TableBillRates$rate <- as.numeric(TableBillRates$rate)
#df_bills$date <- as.Date(df_bills$date, format = "%m/%d/%y") ##convert dates







# ------------------------------------------------------------------------------- 
# Tidying
# ------------------------------------------------------------------------------- 
TableBillRates <- TableBillRates %>%
  mutate(maturity = as.numeric(gsub("([0-9]+).*$", "\\1", term)))
TableBillRates[TableBillRates == "4 WEEKS"] <- "1 Mo"
TableBillRates[TableBillRates == "8 WEEKS"] <- "2 Mo"
TableBillRates[TableBillRates == "13 WEEKS"] <- "3 Mo"
TableBillRates[TableBillRates == "26 WEEKS"] <- "6 Mo"

BE<- function(x) x*7/365  
TableBillRates<-cbind(TableBillRates[1:5], lapply(TableBillRates[6], BE) )


tableUsRates<- rbind(tableRealRates,TableBillRates,tableRates)
write.csv(tableUsRates,"tableUsRates.csv", row.names = FALSE)
cat("The data frame is exported", "\n")

