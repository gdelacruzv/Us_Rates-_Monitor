#####################################################################################################
### Function that calculates breakeven, forward rates etc
#####################################################################################################


# ------------------------------------------------------------------------------- 
# Function for tidying date before calculating forward rates
# ------------------------------------------------------------------------------- 


calculate_frw_step_1 <- function(df) {
  
  ####Create dataframe  with dates by quarter. end of month, and each day last month
  
  ##Months
  dates <- data.frame(
    date = seq(as.Date('2007-01-01'), Sys.Date(), by = 'month')-1
  ) 
  #each day last month
  dates2 <- data.frame(
    date = seq(Sys.Date()-30, Sys.Date(), by = 'days')
  )
  
  dates <- merge(dates, dates2, by ="date", all = TRUE)
  
  df$date <- as.Date(df$date)
  df <- merge(dates, df, by ="date", all.x = TRUE)# %>%
  #  na.omit()
  
  
  
  df <- df%>% 
    group_by(date)%>%
    mutate(dummy = 1L) %>% 
    inner_join(., ., by = "dummy", suffix=c("_short", "_long")) %>% 
    select(-dummy) %>% 
    #filter(bond_short < bond_long) %>%
    filter(date_short == date_long) %>%
    mutate(ttm_new_bond = (maturity_long  - maturity_short))#%>%
  
  
  return(df)
  
}


# ------------------------------------------------------------------------------- 
# Function for calculating forward rates
# ------------------------------------------------------------------------------- 
day_count <- 360

##Function for calculating forward rates

calculate_forward_rate <- function(maturity_short, yield_short, maturity_long, yield_long, day_count){
  
  short_bond <- (1+yield_short/100)^(maturity_short/day_count)
  long_bond <- (1+yield_long/100)^(maturity_long/day_count)
  days_between <- (maturity_long - maturity_short)
  forward_rate <- ((long_bond/short_bond)^(day_count/days_between)-1)*100
  return(round(forward_rate, digits=2))  
}



#####################################################################################################
### Calculate breakeven rates and forward breakeven rates
#####################################################################################################


tableUsRates$date <- as.Date(tableUsRates$date)
tableUsRates$rate <- as.numeric(tableUsRates$rate)
tableUsRates$maturity <- as.numeric(tableUsRates$maturity)


df <- tableUsRates %>%
  filter(des %in% c("yieldYear", "realyieldYear")) %>%
  filter(term %in% c("5y", "7y", "10y", "20y", "30y"))%>%
  distinct(date, maturity, term, des, .keep_all = TRUE) %>%
  pivot_wider(c("date", "maturity", "term"), ###Help column due to "rate are nog uniquely identified.
              names_from = des,
              values_from = rate) %>%
  filter(realyieldYear != "NA") %>%
  filter(realyieldYear != 0) %>% ##exclude when it is zero
  filter(yieldYear != "NA") %>%
  filter(yieldYear != 0) %>% ##exclude when it is zero
  mutate(breakeven = yieldYear - realyieldYear) %>%
  pivot_longer(!c("date", "maturity", "term"),
               names_to = "des",
               values_to = "rate")

# ------------------------------------------------------------------------------- 
# Create df with breakevens (non-forward)
# ------------------------------------------------------------------------------- 

##Add to tableUSRates
df_breakeven_non_frw  <- df %>%
  filter(des == "breakeven") %>%
  filter(rate != "NA")
df_breakeven_non_frw$date <- as.Date(df_breakeven_non_frw$date)


# ------------------------------------------------------------------------------- 
# Calculate Forward breakeven (5y5y and 10y10y)
# ------------------------------------------------------------------------------- 

#only 5y and 10y for 5y5y
df <- df %>%
  filter(term %in% c("5y", "10y", "20y"))

df <- calculate_frw_step_1(df)


##Calculate 1 y and 5y forward rate
df <- df %>%
  filter(term_short %in%  c("5y", "10y"))%>%
  filter(des_short %in%  c("breakeven"))%>%
  filter(des_long %in%  c("breakeven"))%>%
  filter(term_long %in%  c("10y", "20y")) %>%
  filter(term_short != term_long)


### Calling the forward function

df <- df %>%
  mutate(forward_rate = calculate_forward_rate(
    maturity_short,
    rate_short,
    maturity_long,
    rate_long,
    day_count)) %>%
  mutate(frw_term = paste(term_short, term_long, sep=""))%>%
  mutate(frw_des = "frw_breakeven") %>%
  filter(ttm_new_bond != "15") ##ta bort 5y20y

df_breakeven <- df ##Create df


#####################################################################################################
### Calculate Bills forward curve. 1m forward curve
#####################################################################################################

#####Calculate bill forward

df <- tableUsRates %>%
  filter(des %in% c("yieldYear"))%>%
  filter(maturity <= 2)%>%
  filter(term != "2m")


df <- calculate_frw_step_1(df)

df <- df %>%
  filter(term_short %in%  c("1m"))%>%
  filter(maturity_short <= maturity_long)

df <- df %>%
  mutate(forward_rate = calculate_forward_rate(
    maturity_short,
    rate_short,
    maturity_long,
    rate_long,
    day_count))%>%
  mutate(frw_term = paste(term_short, term_long, sep=""))%>%
  mutate(frw_des = "frw_short_curve")



##Change frw rate for the first bond to actual 
df <- df %>%
  mutate(forward_rate = case_when(
    ttm_new_bond == 0 ~ rate_short,
    ttm_new_bond != 0 ~ forward_rate
  ))

df_short_curve <- df #create dataset



#####################################################################################################
### Calculate Forward curve. 1y horizon forward curve
#####################################################################################################


df <- tableUsRates %>%
  filter(des %in% c("yieldYear"))%>%
  filter(maturity >= 1)

#calling function
df <- calculate_frw_step_1(df)

df <- df %>%
  filter(term_short %in%  c("1y", "5y"))%>%
  filter(maturity_short <= maturity_long)

df <- df %>%
  mutate(forward_rate = calculate_forward_rate(
    maturity_short,
    rate_short,
    maturity_long,
    rate_long,
    day_count))%>%
  mutate(frw_term = paste(term_short, term_long, sep=""))%>%
  mutate(frw_des = "frw_curve")



##Change frw rate for the first bond to actual 
df <- df %>%
  mutate(forward_rate = case_when(
    ttm_new_bond == 0 ~ rate_short,
    ttm_new_bond != 0 ~ forward_rate
  ))

df_frw_curve <- df %>%
  select(date_short, term_short,ttm_new_bond, forward_rate,frw_term, frw_des)


#####################################################################################################
### Calculate spreads (slope)
#####################################################################################################


#######Calculate Spreads, not expressed in spreads here

df <- tableUsRates %>%
  filter(des == "yieldYear") %>%
  filter(term %in% c("3m", "10y", "2y", "5y", "30y")) %>%
  filter(rate != "NA")%>%
  mutate(term = paste("yield", term, sep=""))%>%#fix name
  pivot_wider(c("date", "des"),
              names_from = term,
              values_from = rate)%>%
  mutate('3m10y' = round((yield10y - yield3m)*1,4))%>%
  mutate('2y10y' = round((yield10y - yield2y)*1,4)) %>%
  mutate("5y30y" = round((yield30y - yield5y)*1,4)) %>%
  mutate('2y5y10y' = round((2 * yield5y - yield2y - yield5y)*1,4))%>%
  select(date, des, '3m10y', '2y10y', '5y30y', '2y5y10y') %>%
  pivot_longer(!c("date", "des"),
               names_to  = "term",
               values_to = "rate")%>%
  mutate(des = "yieldYear") %>%
  mutate(info = "spread, percent")


df_spreads <- df




#####################################################################################################
### Merge datasets
#####################################################################################################


df<- bind_rows(mutate_all(df_frw_curve, as.character), mutate_all(df_short_curve, as.character),mutate_all(df_breakeven, as.character))

df <- df %>% #rename to fit
  select(date_short, term_short,ttm_new_bond, forward_rate,frw_term, frw_des)%>%
  rename(date = date_short,
         term = term_short,
         maturity = ttm_new_bond,
         rate = forward_rate,
         des= frw_des,
         info = frw_term)%>%
  ungroup()



##Merge with df spreads

tableUsRates <- bind_rows(mutate_all(tableUsRates, as.character), mutate_all(df_breakeven_non_frw, as.character),mutate_all(df, as.character), mutate_all(df_spreads, as.character))

