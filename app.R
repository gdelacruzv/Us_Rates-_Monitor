## app.R ##
library(shiny)
library(shinydashboard)
library(slider)
library(shinyWidgets)
#####################################################################################################
### Libraries
#####################################################################################################

library(rvest, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(gridExtra)
library(rstudioapi)



#####################################################################################################
### Import and tidy date
#####################################################################################################

# Logic:
# Import saved csv-file. If date < sys.date -> parse current years data, run calculations and parse.


#install.packages("rstudioapi")

#When its run locally
setwd(dirname(getActiveDocumentContext()$path))  #This is used when running it local

#source("importData.r")

###Fix formats. needs to be done throughout the app. weird. 

tableForShiny <- arrange(tableForShiny, date)
tableForShiny$term <- as.character(tableForShiny$term)

tableForShiny$des <- as.character(tableForShiny$des)
tableForShiny$maturity <- as.numeric(tableForShiny$maturity)
tableForShiny$info <- as.character(tableForShiny$info)
tableForShiny$date <- as.Date(tableForShiny$date)
tableForShiny$rate <- as.numeric(tableForShiny$rate)



# ------------------------------------------------------------------------------- 
# Create lists for sidebar page Line plots
# ------------------------------------------------------------------------------- 


line_plot_nominal_rates = list("UST 3m" = "yieldYear_3m",
                               "UST 2y" = "yieldYear_2y",
                               "UST 5y" = "yieldYear_5y", 
                               "UST 10y" = "yieldYear_10y",
                               "UST 30y" = "yieldYear_30y"
                               
)

line_plot_real_rates = list("TIPS 2y" = "realyieldYear_2y",
                            "TIPS 5y" = "realyieldYear_5y",
                            "TIPS 10y" = "realyieldYear_10y",
                            "TIPS 30y" = "realyieldYear_30y"
                            
)

line_plot_inflation_rates = list("Breakeven 5y" = 	"breakeven_5y",
                                 "Breakeven 10y" = "breakeven_10y",
                                 "Breakeven 5y5y" = "frw_breakeven_5y",
                                 "Breakeven 10y10y" = "frw_breakeven_10y"
)

line_plot_spreads = list("Spread 3m10y" = "yieldYear_3m10y",
                         "Spread 2y10y" = "yieldYear_2y10y",
                         "Spread 5y30y" = "yieldYear_5y30y",
                         "Spread 2y5y10y" = "yieldYear_2y5y10y"
)

line_plot_checkbox <- c(line_plot_spreads, line_plot_real_rates, line_plot_inflation_rates,line_plot_nominal_rates)




# ------------------------------------------------------------------------------- 
# Information for FAQs
# ------------------------------------------------------------------------------- 

info_page <- function(){
  fluidRow(
    h3( "Information about the app" ),
    tags$p("All data is from US Treasury and is scraped daily the first time the app is used. The data is then saved to Amazon S3 where it will be extracted when the app is used multiple times a day. This framework reduces unnecessary scraping and makes the app load faster."),
    
    h3( "Rates and methodology" ),
    strong("Treasury Yields (levels)"),
    p("Also knowned as Constant Maturity Treasury Rates. Real yields on  are interpolated by US Treasury from the daily yield curve. Yields to maturity is the total return anticipated on a bond if the bond is held until it  matures. "),
    
    strong("Treasury Real Yields"),
    p("Also knowned as Real Constant Maturity Treasury Rates or Treasury Inflation Protected Securities (TIPS). Real yields on Treasury Inflation Protected Securities (TIPS) are interpolated by US Treasury from the daily yield curve. "),
    
    strong("Forward Curve"),
    p("Forward Curve is the calculated expectation of the yield curve in the future. Forward curve should be calculated using the Spot Curve, however, in this app it is calculated using the Par Curve (Constant Maturity Treasury Rates). 1 year horizon is used, which states where the market is expected the yield curve to be in one year. For Bills, 1 month horizon is used. Forward curve is best suited to use as an indication for shorter maturies."),
    
    strong("Breakeven Inflation Rates"),
    p("The breakeven inflation rate represents a measure of expected inflation derived from Treasury Constant Maturity Securities and Treasury Inflation-Indexed Constant Maturity Securities for each term. The latest value implies what market participants expect inflation to be in the next x years, on average."),
    p("Breakeven 5y shows where the market expects the inflation will be on average for the next 5 year. A breakeven 5y5y shows where the market expects the inflation will be, on average, over the 5 year period that begins in 5 years."),
    
    
    h3( "Indices" ),
    
    strong("Spreads 3m10, 2m10y, 5y30y (slope)"),
    p("Spreads are calculated from the Constant Maturity Treasury Rates and shows the difference between different maturities. Some spreads are of more interest of others. 3m10y are for example of one Feds favourites. 2y10y and 5y30y are heavily followed on the financial markets."),
    
    strong("Butterfly spreads, 2y5y10y (curvature)"),
    p("Butterfly spreads are calculated from the Constant Maturity Treasury Rates and shows the curvature of the yield curve. A butterfly is a non-parallel shift of the yield curve when long and short rates shifts more (or less) then medium-term rates.") ,
    
    
    br()
  )
}


#####################################################################################################
### Create User Interface
#####################################################################################################


ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "US Rates Monitor"),
                    ## Sidebar content
                    dashboardSidebar(
                      tags$style(
                        "#sidebarItemExpanded {
            overflow: auto;
            height: calc(100vh - 50px) !important;
        }"
                      ),
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Curve plots", tabName = "curve_plots", icon = icon("chart-line", lib = "font-awesome")),
                        menuItem("Line plots", tabName = "line_plots", icon = icon("chart-line", lib = "font-awesome")),
                        ## 5th tab Data source, definition , i.e., help ---------------
                        menuItem( "FAQs", tabName = 'help', icon = icon('question-circle') ),
                        conditionalPanel("input.sidebarmenu === 'curve_plots'")
                        
                      )
                    ),
                    
                    
                    ## Body content
                    dashboardBody(
                      tabItems(
                        
                        # ------------------------------------------------------------------------------- 
                        # Tab Panel Dashboard
                        # ------------------------------------------------------------------------------- 
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  shinydashboard::valueBoxOutput("initials"),
                                  
                                  box(title="Select input parameters",
                                      status="danger", solidHeader = TRUE,width=12,
                                      
                                      box(  # Copy the line below to make a date range selector
                                        sliderInput(
                                          "slider",
                                          "Date range",
                                          min = as.Date(min(tableForShiny$date)),
                                          max = as.Date(max(tableForShiny$date)),
                                          value =  c(as.Date(max(tableForShiny$date)) - 365, as.Date(max(tableForShiny$date)))),
                                        width=12)
                                  ),
                                  box(plotOutput("plot_dash_4")), #level
                                  box(plotOutput("plot_dash_2")), #slope
                                  box(plotOutput("plot_dash_curvature")), #curvature
                                  
                                  
                                  #    box(plotOutput("plot_dash_bill_curve")), #bill curve
                                  box(plotOutput("plot_dash_3")), #par curve
                                  # hr(style = "border-color: #cbcbcb;"),
                                  # h2(paste0("Goods")),
                                  #  box(title="Breakeven Inflation Rate",
                                  #     status="danger", solidHeader = TRUE,width=12),
                                  box(plotOutput("plot_dash_breakeven_5y")), #breakeven,
                                  box(plotOutput("plot_dash_breakeven_10y")), #breakeven,
                                  
                                  
                                  
                                  # box(title="Forward curves",
                                  #  status="danger", solidHeader = TRUE,width=12),
                                  ###Forward curve
                                  #      box(plotOutput("plot_dash_frw_short_curve")), #short forward curve
                                  box(plotOutput("plot_dash_frw_curve")) #forward curve
                                  
                                  
                                  
                                )
                        ),
                        
                        # ------------------------------------------------------------------------------- 
                        # Tab Panel US Curve
                        # ------------------------------------------------------------------------------- 
                        tabItem(tabName = "curve_plots",
                                fluidRow(
                                  shinydashboard::valueBoxOutput("initial"),
                                  box(title="Select input parameters",
                                      status="danger", solidHeader = TRUE,width=12,
                                      
                                      box(#collapsible = TRUE,
                                        radioButtons("curve_type", "Select Curve",
                                                     c("Treasury Yield Curve (par curve)" = "treasury_curve",
                                                       "Treasury Real Yield Curve" = "real_curve",
                                                       "Treasury Breakeven Curve" = "breakeven_curve",
                                                       "Treasury Bill Curve" = "bill_curve")),width=6),
                                      
                                      
                                      box(#collapsible = TRUE,
                                        radioButtons("curve_length", "Curve length",
                                                     c("Only up to 10 years" = "short_curve",
                                                       "Full Curve" = "full_curve")),width=6),
                                      
                                      box(  # Copy the line below to make a date range selector
                                        sliderInput(
                                          "slider_curve",
                                          "Date range",
                                          min = as.Date(min(tableForShiny$date)),
                                          max = as.Date(max(tableForShiny$date)),
                                          value =  c(as.Date(max(tableForShiny$date)) - 365, as.Date(max(tableForShiny$date))),
                                          format = "yyyy-mm-dd"),
                                        width=12)
                                  ),
                                  
                                  
                                  box(plotOutput("plot1"),height=500, width=12),
                                  
                                )
                        ),
                        
                        # ------------------------------------------------------------------------------- 
                        # Tab Panel Line plots
                        # ------------------------------------------------------------------------------- 
                        tabItem(tabName = "line_plots",
                                fluidRow(
                                  shinydashboard::valueBoxOutput("initial2"),
                                  box(title="Select input parameters",
                                      status="danger", solidHeader = TRUE,width=12,
                                      
                                      
                                      #Nominal
                                      box(
                                        #    h4("Select Rates or Indices"),
                                        checkboxGroupButtons(
                                          inputId = "line_plot_nominal_rates",
                                          label = "Nominal Rates",
                                          choices = line_plot_nominal_rates,
                                          individual = TRUE,
                                          checkIcon = list(
                                            yes = tags$i(class = "fa fa-circle", 
                                                         style = "color: red"),
                                            no = tags$i(class = "fa fa-circle-o", 
                                                        style = "color: red")),
                                          selected = "yieldYear_3m"
                                        ),
                                        
                                        #real
                                        checkboxGroupButtons(
                                          inputId = "line_plot_real_rates",
                                          label = "Real Rates",
                                          choices = line_plot_real_rates,
                                          individual = TRUE,
                                          checkIcon = list(
                                            yes = tags$i(class = "fa fa-circle", 
                                                         style = "color: red"),
                                            no = tags$i(class = "fa fa-circle-o", 
                                                        style = "color: red")),
                                          selected = "yieldYear_3m"
                                        ),
                                        
                                        #Inflation
                                        checkboxGroupButtons(
                                          inputId = "line_plot_inflation_rates",
                                          label = "Breakeven Inflation rates",
                                          choices = line_plot_inflation_rates,
                                          individual = TRUE,
                                          checkIcon = list(
                                            yes = tags$i(class = "fa fa-circle", 
                                                         style = "color: red"),
                                            no = tags$i(class = "fa fa-circle-o", 
                                                        style = "color: red")),
                                          selected = ""
                                        ),
                                        
                                        #Spreads
                                        checkboxGroupButtons(
                                          inputId = "line_plot_spreads",
                                          label = "Spreads",
                                          choices = line_plot_spreads,
                                          individual = TRUE,
                                          checkIcon = list(
                                            yes = tags$i(class = "fa fa-circle", 
                                                         style = "color: red"),
                                            no = tags$i(class = "fa fa-circle-o", 
                                                        style = "color: red")),
                                          selected = ""
                                        ),width=12),
                                      
                                      
                                      box(  # Copy the line below to make a date range selector
                                        sliderInput(
                                          "slider_line",
                                          "Date range",
                                          min = as.Date(min(tableForShiny$date)),
                                          max = as.Date(max(tableForShiny$date)),
                                          value =  c(as.Date(max(tableForShiny$date)) - 365, as.Date(max(tableForShiny$date)))),
                                        width=12),
                                      
                                      
                                      box(
                                        checkboxGroupButtons(
                                          inputId = "extra_line",
                                          label = "Add extra information",
                                          choices = list("Zero intercept (solid)" = "0", "Fed Inflation Target (dashed)" = "2"),
                                          individual = TRUE,
                                          checkIcon = list(
                                            yes = tags$i(class = "fa fa-circle", 
                                                         style = "color: red"),
                                            no = tags$i(class = "fa fa-circle-o", 
                                                        style = "color: red")),
                                          selected = ""), 
                                        width = 12
                                      ),
                                      
                                      
                                  ),
                                  
                                  
                                  box(plotOutput("plot_line"),height=500, width=12),
                                  
                                )
                        ),
                        
                        
                        # ------------------------------------------------------------------------------- 
                        # Tab Panel FAQs page
                        # -------------------------------------------------------------------------------                             
                        
                        tabItem( tabName = 'help',
                                 ## 3.5.1 Data sources ---------------
                                 box(title="Information and terminology",
                                     status="danger", solidHeader = TRUE,width=12,
                                     
                                     box(id = 'info_page',
                                         info_page(), width=12 )),
                        )
                        
                        
                        
                        
                      ),
                      
                      # Footer -------------------------------
                      hr(style = "border-color: #cbcbcb;"),
                      fluidRow(
                        column(9,
                               p('All of the data used to generate this app were obtained from ', tags$a(href = "https://home.treasury.gov/", 'US Treasury', target = '_blank'), '.', style = "font-size: 85%"),
                               p("App created by ", 'Gil De La Cruz Vazquez', target = '_blank'), " in 2021", HTML("&bull;"),
                        
                               p("Have a question? Spot an error? Send an email ", tags$a(href = "mailto:gildelacruzvazquez@gmail.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%"),
                               # p(tags$em("Last updated: November 2020"), style = 'font-size:75%')
                        )
                      )
                    )
)

#####################################################################################################
### Server
#####################################################################################################


server <- function(input, output,session) {
  
  
  #current
  curve_type = c("yieldYear")
  title_text = paste("Treasury Yield Curve (par curve)", sep="")
  start_date = as.Date(max(tableForShiny$date)) - 365
  end_date = as.Date(max(tableForShiny$date))
  
  
  tableForShiny$date <- as.Date(tableForShiny$date)
  tableForShiny$rate <- as.numeric(tableForShiny$rate)
  tableForShiny$maturity <- as.numeric(tableForShiny$maturity)
  
  # ------------------------------------------------------------------------------- 
  # Tab panel Dashboard  
  # ------------------------------------------------------------------------------- 
  
  #--------------
  #----- Reactive function and Plot for UST yields Levels
  #-------------
  ###Level, Slope, Curvature
  
  
  ##################################################
  ## Plot yields LEVELS
  ##
  
  datasetInput_dashboard_4 <- reactive({
    
    #2,5,10,30 line plot
    
    start_date = as.Date(input$slider[1])
    
    end_date = as.Date(input$slider[2])
    
    
    curve_type = c("yieldYear")
    term_length = 30
    # curve_length = 10
    
    df <- tableForShiny %>%
      filter(des %in% curve_type)%>%
      #  filter(des %in% c("realyieldYear"))%>%
      filter(term %in% c("2y", "5y", "10y", "30y"))%>%
      # filter(maturity <= term_length) %>%
      filter(date >= start_date) %>%
      filter(date <= end_date) 
    
    
    df
    
  })
  
  output$plot_dash_4 <- renderPlot({
    
    
    df <- datasetInput_dashboard_4()
    
    title_text = paste("Treasury Yields Levels", sep="")
    subtitle_text = paste("Treasury Yields", sep="")
    
    
    df$date <- as.Date(df$date)
    df$rate <- as.numeric(df$rate)
    
    ggplot(data = df, aes(x = date, y = rate, group = term, color = term)) + 
      geom_line(size = 1)+
      geom_hline(yintercept = 0)+
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      labs(x="",y="Percent",
           title=title_text,
           subtitle=subtitle_text,
           caption="Source: US Treasury, own calculations.")
    
    
  })
  
  ##
  ##
  ###################################################
  
  
  
  
  
  ## PLOT Yield Curve SLOPES
  
  #Interactive control
  
  
  
  })
  
  # Plot
  output$plot_dash_2 <- renderPlot({
    
    
    title_text = paste("Treasury Yields Slope", sep="")
    subtitle_text = paste("Interest rate spreads", sep="")
    
    df <- datasetInput_dashboard_2()
    
    df$date <- as.Date(df$date)
    df$rate <- as.numeric(df$rate)
    
    
    ###Show in basispoints
    
    ggplot(df, aes(x = date, y = rate, group = term, color = term)) + 
      geom_line(size = 1)+
      #  geom_point()+
      # geom_hline(yintercept = 2, linetype = "dashed")+
      geom_hline(yintercept = 0)+
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      labs(x="",y="Basispoints",
           title=title_text,
           subtitle=subtitle_text,
           caption="Source: US Treasury, own calculations.")
    
    
    
  })
  
  
  
  
  
  ## PLOT Yield  Curvature
  
  #Interactive control
  
  datasetInput_dash_curvature <- reactive({
    
    #Curve, line plot
    
    
    tableForShiny$date <- as.Date(tableForShiny$date)
    tableForShiny$rate <- as.numeric(tableForShiny$rate)
    tableForShiny$maturity <- as.numeric(tableForShiny$maturity)
    
    
    start_date = as.Date(input$slider[1])
    end_date = as.Date(input$slider[2])
    
    
    curve_type = c("yieldYear")
    term_length = 30
    curve_length = 10
    
    
    
    df <- tableForShiny %>%
      filter(date >= start_date) %>%
      filter(date <= end_date) %>%
      filter(des %in% c(curve_type)) %>%
      filter(rate != "NA")%>%
      filter(term %in% c('2y5y10y'))%>%
      mutate(rate = rate * 100) ##Show with as bp
    
    
    df
  })
  
  # Plot
  output$plot_dash_curvature <- renderPlot({
    
    
    title_text = paste("Treasury Yields Curvature", sep="")
    subtitle_text = paste("Butterfly 2s5s10s", sep="")
    
    df <- datasetInput_dash_curvature()
    
    df$date <- as.Date(df$date)
    df$rate <- as.numeric(df$rate)
    
    
    
    ggplot(df, aes(x = date, y = rate, group = term, colour = term)) + 
      geom_line(size = 1)+
      #  geom_point()+
      geom_hline(yintercept = 0)+
      
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      labs(x="",y="Basispoints",
           title=title_text,
           subtitle=subtitle_text,
           caption="Source: US Treasury, own calculations.")
    
    
    
  })
  
  
  
  
  
  
  ##################################################
  ## Plot Yield Curve
  ##
  
  datasetInput_dashboard_3 <- reactive({
    
    #Curve plot
    
    tableForShiny$date <- as.Date(tableForShiny$date)
    tableForShiny$rate <- as.numeric(tableForShiny$rate)
    
    start_date = as.Date(input$slider[1])
    end_date = as.Date(input$slider[2])
    
    curve_type = c("yieldYear")
    term_length = 30
    curve_length = 10
    
    df <- tableForShiny %>%
      filter(des %in% curve_type)%>%
      #  filter(des %in% c("realyieldYear"))%>%
      filter(term != "2m")%>%
      filter(maturity <= term_length)%>%
      filter(date %in% c(start_date, end_date)) %>%
      arrange(date)%>%
      group_by(term)%>%
      mutate(row_n = paste("date_",1:n(),sep=""))%>%
      pivot_wider(c("des", "maturity", "term"),
                  names_from = row_n,
                  values_from = rate) %>%
      mutate(change  = round((date_2 - date_1)*100,0))
    
    df$colour <- ifelse(df$change < 0, "negative","positive")
    
    df <- df %>%
      filter(maturity <= curve_length)
    
    df
  })
  
  
  output$plot_dash_3 <- renderPlot({
    
    ##Curve plot
    
    df <- datasetInput_dashboard_3()
    
    start_date = as.Date(input$slider[1])
    end_date = as.Date(input$slider[2])
    title_text = paste("Treasury Yield Curve, ", sep="")
    subtitle_text = paste("Black line: ", end_date,", dashed line: ",start_date,".", sep="")
    
    
    scale_breaks = c(0,1,2,3,5,7,10)
    
    p1 <- ggplot(df, aes(x = maturity, y = date_2)) + 
      geom_line(size = 1)+
      geom_point()+
      geom_line(aes(y=date_1), linetype="dashed")+
      theme_minimal() +
      theme(legend.position="none",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      
      scale_x_continuous(breaks = scale_breaks)+
      theme(panel.grid.minor = element_line(colour = "white"))+
      
      labs(x="",y="Percent",
           title=title_text,
           subtitle=subtitle_text)
    
    
    p2 <- ggplot(data = df,
                 aes(x = maturity, y = change)) +
      #    geom_bar(stat = "identity", orientation = "x")+
      geom_bar(stat="identity",position="identity", orientation = "x", aes(fill = colour))+
      scale_fill_manual(values=c(positive="steelblue",negative="firebrick1"))+
      
      ylim(-max(abs(df$change)), max(abs(df$change)))+
      # geom_hline(yintercept = 0)+ ##intercept
      
      theme_minimal() +
      theme(legend.position="none",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      scale_x_continuous(breaks = scale_breaks)+
      theme(panel.grid.minor = element_line(colour = "white"))+
      
      labs(x="Time to maturity",y="Change, basispoints",
           caption="Source: US Treasury, own calculations.")
    
    p <- grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
    
    
  })
  
  
  
  ##################################################
  ## Plot Bill Curve
  ##
  
  datasetInput_dashboard_bill_curve <- reactive({
    
    #Curve plot
    
    tableForShiny$date <- as.Date(tableForShiny$date)
    tableForShiny$rate <- as.numeric(tableForShiny$rate)
    tableForShiny$maturity <- as.numeric(tableForShiny$maturity)
    
    start_date = as.Date(input$slider[1])
    end_date = as.Date(input$slider[2])
    
    curve_type = c("yieldYear")
    term_length = 1
    curve_length = 2
    
    df <- tableForShiny %>%
      filter(des %in% curve_type)%>%
      #  filter(des %in% c("realyieldYear"))%>%
      filter(term != "2m")%>%
      filter(maturity <= term_length)%>%
      filter(date %in% c(start_date, end_date)) %>%
      arrange(date)%>%
      group_by(term)%>%
      mutate(row_n = paste("date_",1:n(),sep=""))%>%
      pivot_wider(c("des", "maturity", "term"),
                  names_from = row_n,
                  values_from = rate) %>%
      mutate(change  = round((date_2 - date_1)*100,0))
    
    df$colour <- ifelse(df$change < 0, "negative","positive")
    
    df <- df %>%
      filter(maturity <= curve_length)
    
    df
  })
  
  
  output$plot_dash_bill_curve <- renderPlot({
    
    ##Curve plot
    
    df <- datasetInput_dashboard_bill_curve()
    
    title_text = paste("Treasury Bill Curve, ", sep="")
    subtitle_text = paste("Black line: ", end_date,", dashed line: ",start_date,".", sep="")
    
    
    p1 <- ggplot(df, aes(x = maturity, y = date_2)) + 
      geom_line(size = 1)+
      geom_point()+
      geom_line(aes(y=date_1), linetype="dashed")+
      theme_minimal() +
      theme(legend.position="none",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      labs(x="",y="Percent",
           title=title_text,
           subtitle=subtitle_text)
    
    
    p2 <- ggplot(data = df,
                 aes(x = maturity, y = change)) +
      #    geom_bar(stat = "identity", orientation = "x")+
      geom_bar(stat="identity",position="identity", orientation = "x", aes(fill = colour))+
      scale_fill_manual(values=c(positive="steelblue",negative="firebrick1"))+
      
      ylim(-max(abs(df$change)), max(abs(df$change)))+
      
      theme_minimal() +
      theme(legend.position="none",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      labs(x="Time to maturity",y="Change, basispoints",
           caption="Source: US Treasury, own calculations.")
    
    p <- grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
    
    
  })
  
  
  ##
  ##
  ###################################################
  
  
  
  
  ## PLOT Breakeven 5y
  
  #Interactive control
  datasetInput_dashboard_1 <- reactive({
    
    start_date = as.Date(input$slider[1])
    end_date = as.Date(input$slider[2])
    
    df <- tableForShiny %>%
      filter(des %in% c("breakeven", "frw_breakeven")) %>%
      filter(term %in% c("5y"))%>%
      filter(date >= start_date) %>%
      filter(date <= end_date) %>%
      mutate(des2 = case_when(
        des == "breakeven" ~ term,
        des == "frw_breakeven" ~ paste(term, term, sep="")
      ))
    
    
    df
  })
  
  #Plot  
  output$plot_dash_breakeven_5y <- renderPlot({
    
    
    df <- datasetInput_dashboard_1()
    
    title_text = paste("Breakeven Inflation Rate, 5y", sep="")
    subtitle_text = paste("Breakeven Inflation Rates (dashed: FOMC Inflation target)", sep="")
    
    
    #start_date = "2020-01-01"
    #end_date = "2021-01-01"
    
    tableForShiny$date <- as.Date(tableForShiny$date)
    tableForShiny$rate <- as.numeric(tableForShiny$rate)
    
    ggplot(data = df, aes(x = date, y = rate, color = des2)) + 
      geom_line(size = 1)+
      geom_hline(yintercept = 2, linetype = "dashed")+
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      labs(x="",y="Percent",
           title=title_text,
           subtitle=subtitle_text,
           caption="Source: US Treasury, own calculations.")
    
  })
  
  
  
  
  ## PLOT Breakeven 10y and 10y10y
  
  #Interactive control
  datasetInput_dashboard_breakeven_10 <- reactive({
    
    start_date = as.Date(input$slider[1])
    end_date = as.Date(input$slider[2])
    
    df <- tableForShiny %>%
      filter(des %in% c("breakeven", "frw_breakeven")) %>%
      filter(term %in% c("10y"))%>%
      filter(date >= start_date) %>%
      filter(date <= end_date) %>%
      mutate(des2 = case_when(
        des == "breakeven" ~ term,
        des == "frw_breakeven" ~ paste(term, term, sep="")
      ))
    
    
    df
  })
  
  #Plot  
  output$plot_dash_breakeven_10y <- renderPlot({
    
    
    df <- datasetInput_dashboard_breakeven_10()
    
    title_text = paste("Breakeven Inflation Rates, 10y", sep="")
    subtitle_text = paste("Breakeven Inflation Rates (dashed: FOMC Inflation target)", sep="")  
    
    #start_date = "2020-01-01"
    #end_date = "2021-01-01"
    
    tableForShiny$date <- as.Date(tableForShiny$date)
    tableForShiny$rate <- as.numeric(tableForShiny$rate)
    
    ggplot(data = df, aes(x = date, y = rate, color = des2)) + 
      geom_line(size = 1)+
      geom_hline(yintercept = 2, linetype = "dashed")+
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      labs(x="",y="Percent",
           title=title_text,
           subtitle=subtitle_text,
           caption="Source: US Treasury, own calculations.")
    
  })
  
  
  
  ##################################################
  ## Forward curves
  ##
  
  ####Get forward rate curve
  datasetInput_dashboard_frw_curve <- reactive({
    
    
    tableForShiny$date <- as.Date(tableForShiny$date)
    tableForShiny$rate <- as.numeric(tableForShiny$rate)
    tableForShiny$maturity <- as.numeric(tableForShiny$maturity)
    tableForShiny$des <- as.character(tableForShiny$des)
    tableForShiny$info <- as.character(tableForShiny$info)
    tableForShiny$term <- as.character(tableForShiny$term)
    #2,5,10,30 line plot
    
    start_date = as.Date(input$slider[1])
    
    end_date = as.Date(input$slider[2])
    
    ###Get previous month end
    if (end_date != max(tableForShiny$date)){
      end_date <- end_date - days(day(end_date))
    }
    
    start_date <- start_date - days(day(start_date))
    
    curve_type = c("frw_curve", "yieldYear")
    term_length = 30
    curve_length = 30
    
    
    df <- tableForShiny %>%
      filter(des %in% curve_type)%>%
      filter(maturity <= term_length)%>%
      filter(date == end_date)%>%
      filter(term != "2m")%>%
      filter(des == "frw_curve" & term != "5y" | des == "yieldYear")%>%
      #  filter(date %in% c(s_date, e_date)) %>%
      mutate(des2 = case_when(
        des == "frw_curve" ~ info,
        des == "yieldYear" ~ term,
      ))
    df <- df %>%
      filter(maturity <= curve_length)
    
    #  df$colour <- ifelse(df$slope < 0, "negative","positive")
    #  df <- df %>%
    #   filter(maturity <= curve_length)
    df
  })
  
  
  
  #Forward curve and par curve should be on the same on the dashboard
  output$plot_dash_frw_curve <- renderPlot({
    
    end_date = as.Date(input$slider[2])
    
    ##Curve plot
    title_text = paste("Par and Forward Curve", sep="")
    subtitle_text = paste(end_date, sep="")
    
    df <- datasetInput_dashboard_frw_curve()
    
    
    ###Fix names
    df <- df %>%
      mutate(des = case_when(
        des == "frw_curve" ~ "1y horizon Forward Curve",
        des == "yieldYear" ~ "Par Curve"
      ))
    
    
    scale_breaks = c(0,1,2,3,5,7,10,20,30)
    
    
    p1 <- ggplot(df, aes(x = maturity, y = rate, group = des, color = des)) + 
      geom_line(size = 1)+
      #geom_point()+
      # geom_line(aes(y=date_1), linetype="dashed")+
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      scale_x_continuous(breaks = scale_breaks)+
      theme(panel.grid.minor = element_line(colour = "white"))+
      
      labs(x="Time to maturity",y="Percent",
           title=title_text,
           subtitle=subtitle_text,
           caption="Source: US Treasury, own calculations.")
    p1
    
    
  })
  
  
  
  
  ##
  ##
  ###################################################
  
  
  # ------------------------------------------------------------------------------- 
  # Tab Panel US Curve
  # ------------------------------------------------------------------------------- 
  
  ### US Curve ###
  
  
  datasetInput <- reactive({
    
    
    #output$start_date
    
    #dates
    start_date = as.Date(input$slider_curve[1])
    end_date = as.Date(input$slider_curve[2])
    
    
    
    #length
    if (input$curve_length == "short_curve"){
      curve_length = 10
      scale_breaks = c(0,1,2,3,5,7,10)
    }
    if (input$curve_length == "full_curve"){
      curve_length = 30
      scale_breaks = c(0,1,2,3,5,7,10, 20, 30)
    }
    
    
    ##curve type
    if (input$curve_type == "treasury_curve"){
      curve_type = c("yieldYear")
      term_length = 30
    }
    if (input$curve_type == "real_curve"){
      curve_type = c("realyieldYear")
      term_length = 30
    }
    if (input$curve_type == "breakeven_curve"){
      curve_type = c("breakeven")
      term_length = 30
    }
    if (input$curve_type == "bill_curve"){
      curve_type = c("yieldYear")
      term_length = 1
      
    }
    
    
    
    
    df <- tableForShiny %>%
      filter(des %in% curve_type)%>%
      #  filter(des %in% c("realyieldYear"))%>%
      filter(term != "2m")%>%
      filter(maturity <= term_length)%>%
      filter(date %in% c(start_date, end_date)) %>%
      arrange(date)%>%
      group_by(term)%>%
      mutate(row_n = paste("date_",1:n(),sep=""))%>%
      pivot_wider(c("des", "maturity", "term"),
                  names_from = row_n,
                  values_from = rate) %>%
      mutate(change  = (date_2 - date_1)*100)
    
    df$colour <- ifelse(df$change < 0, "negative","positive")
    
    df <- df %>%
      filter(maturity <= curve_length)
    
    df
    #  data_jobless_claims2%>%
    # filter(rptdate==input$range_us)
  })
  
  # start_date = "2020-01-01"
  #  end_date = "2021-01-01"
  
  
  output$plot1 <- renderPlot({
    
    #length
    if (input$curve_length == "short_curve"){
      curve_length = 10
      scale_breaks = c(0,1,2,3,5,7,10)
    }
    if (input$curve_length == "full_curve"){
      curve_length = 30
      scale_breaks = c(0,1,2,3,5,7,10, 20, 30)
    }
    
    
    ##Plot text
    if (input$curve_type == "treasury_curve"){
      title_text = paste("Treasury Yield Curve (par curve), ",end_date, sep="")
    }
    if (input$curve_type == "real_curve"){
      title_text = paste("Treasury Real Yield Curve, ",end_date, sep="")
    }
    if (input$curve_type == "breakeven_curve"){
      title_text = paste("Treasury Breakeven Curve, ",end_date, sep="")
    }
    if (input$curve_type == "bill_curve"){
      title_text = paste("Treasury Bill Curve, ",end_date, sep="")
      scale_breaks = c(0,0.25,0.50,0.75,1)
    }
    
    
    
    df <- datasetInput()
    
    
    p1 <- ggplot(df, aes(x = maturity, y = date_2)) + 
      geom_line(size = 1)+
      geom_point()+
      geom_line(aes(y=date_1), linetype="dashed")+
      theme_minimal() +
      theme(legend.position="none",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      scale_x_continuous(breaks = scale_breaks)+
      theme(panel.grid.minor = element_line(colour = "white"))+
      
      labs(x="",y="Percent",
           title=title_text,
           subtitle=paste("US Par Curve ", end_date,". (Dashed ", start_date,")", sep=""))
    
    p2 <- ggplot(data = df,
                 aes(x = maturity, y = change)) +
      geom_bar(stat="identity",position="identity", orientation = "x", aes(fill = colour))+
      scale_fill_manual(values=c(positive="steelblue",negative="firebrick1"))+
      ylim(-max(abs(df$change)), max(abs(df$change)))+
      
      theme_minimal() +
      theme(legend.position="none",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      scale_x_continuous(breaks = scale_breaks)+
      theme(panel.grid.minor = element_line(colour = "white"))+
      
      labs(x="Time to maturity",y="Change, basispoints",
           caption="Source: US Treasury, own calculations.")
    
    
    p <- grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
    
    
    
  })
  
  
  
  
  # ------------------------------------------------------------------------------- 
  # Tab Panel Line Curve
  # ------------------------------------------------------------------------------- 
  
  ### US Curve ###
  
  
  datasetInput_line <- reactive({
    
    
    
    ### Add nicier names for list options which are then used in the plot and title
    line_plot_names <- as.data.frame(do.call(rbind, line_plot_checkbox)) %>%
      rename("line_choice" = 1) %>%
      rownames_to_column("full_name")
    
    # df <- as.data.frame(do.call(rbind, line_plot_checkbox_2)) %>%
    #      rename("line_choice" = 1) %>%
    #      rownames_to_column("full_name")
    
    # line_plot_names <- rbind(line_plot_names, df)
    
    
    
    #Input parameters
    start_date = as.Date(input$slider_line[1])
    end_date = as.Date(input$slider_line[2])
    
    
    group_nominal <- input$line_plot_nominal_rates
    group_real <- input$line_plot_real_rates
    group_inflation <- input$line_plot_inflation_rates
    group_spreads <- input$line_plot_spreads
    
    
    df <- tableForShiny %>%
      filter(date >= start_date) %>%
      filter(date <= end_date)%>%
      mutate(line_choice = paste(des, term, sep="_"))%>%
      filter(line_choice %in% c(group_nominal, group_real, group_inflation, group_spreads))
    
    df <- merge(df, line_plot_names, by = "line_choice", all.x = TRUE) ##Get better names
    
    
    df
    
  })
  
  
  #Forward curve and par curve should be on the same on the dashboard
  output$plot_line <- renderPlot({
    
    start_date = as.Date(input$slider_line[1])
    end_date = as.Date(input$slider_line[2])
    
    
    #Extra information
    zero_intercept = as.numeric(input$extra_line[1])
    fed_inflation_target = as.numeric(input$extra_line[2])
    
    
    df <- datasetInput_line()
    
    title_text = paste("Historical development", sep="")
    subtitle_text = paste("Between ", start_date, " and ", end_date, sep="")
    
    
    p1 <- ggplot(df, aes(x = date, y = rate, group = full_name, color = full_name)) + 
      geom_line(size = 1)+
      geom_hline(yintercept = zero_intercept, linetype = "solid")+
      geom_hline(yintercept = fed_inflation_target, linetype = "dashed")+
      
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            plot.caption=element_text(hjust=0),
            plot.subtitle=element_text(face="italic"),
            plot.title=element_text(size=16,face="bold"))+
      
      labs(x="",y="Percent",
           title=title_text,
           subtitle=subtitle_text,
           caption="Source: US Treasury, own calculations.")
    p1
    
    
  })
  
  
  
  
}

shinyApp(ui, server)