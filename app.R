##### DATA SETUP #############################################################

# load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(knitr)
library(plotly)

# import data
jailData <- read_csv("https://uofi.box.com/shared/static/b6kuknulot4bqyo7elc8gmm6qzhnk766.dat",
                     col_types = cols(booking_date = col_date(format = "%m/%d/%Y"), 
                                      booking_time = col_time(format = "%H:%M:%S"), 
                                      released_date = col_date(format = "%m/%d/%Y"), 
                                      released_time = col_time(format = "%H:%M:%S")))

# organize data
jailData <- jailData %>%
  # fix NA's if possible
  mutate(`super high level` = ifelse(is.na(`super high level`), 
                                     ifelse(crime == "AGGRAVATED BATTERY", "Violent",
                                            ifelse(crime == "ALL OTHER DISORDERLY CONDUCT: NOT DRUNK", "Public Order",
                                                   ifelse(crime == "DEFRAUDING DRUG & ALCOHOL SCREENING TEST", "DUI",
                                                          ifelse(crime == "ENDANGERING LIFE OR HEALTH OF CHILD", "Other",
                                                                 ifelse(crime == "FALSE POLICE REPORT", "Public Order",
                                                                        ifelse(crime == "MISCELLANEOUS OFFENSES", "Other",
                                                                               ifelse(crime == "VIOL OF CHILD MURDERER & VIOL OFF VS. YOUTH REG", "Other",
                                                                                      ifelse(crime == "OTHER TRAFFIC OFFENSES", "Traffic",
                                                                                             ifelse(crime == "RESISTING,OBSTRUCTING,OR DISARMING A POLICE OFC", "Interfering with Public Officers",
                                                                                                    ifelse(crime == "UNLAWFUL POSS OF FIREARMS & FIREARM AMMUNITION", "Public Order",
                                                                                                           ifelse(crime == "VIOL OF CHILD MURDERER & VIOL OFF VS. YOUTH REG", "Other",
                                                                                                                  ifelse(crime == "VIOLATION OF ORDERS OF PROTECTION", "Domestic Violence", 
                                                                                                                         crime)))))))))))), `super high level`)) %>%
  # drop columns
  select(booking_number, booking_date, booking_time, Last_name, First_name, jacket_number, released_date, 
         `age arrest`, `age releas`, `days jail`, `super high level`, crime, race, sex, 
         citizenship, maritial_status, employment_status, military, state) %>%
  # rename columns
  rename(age_arrest = `age arrest`, age_release = `age releas`, days_jail = `days jail`,
         marital_status = maritial_status, crime_category = `super high level`) %>%
  # remove expunged observations
  filter(Last_name != "EXPUNGED") %>%
  # fixing spelling, formatting
  mutate(marital_status = ifelse(marital_status == "Seperated", "Separated", marital_status),
         citizenship = ifelse(citizenship == "China (Republic of)", "CHINA",
                              ifelse(citizenship == "CONGO, DEMOCRATIC REPUBLIC OF THE", "DEMOCRATIC REPUBLIC OF THE CONGO",
                                     ifelse(citizenship == "CONGO (BRAZZAVILLE)", "CONGO",
                                            ifelse(citizenship == "US", "UNITED STATES", str_to_upper(citizenship))))),
         military = str_remove_all(military, "United States "),
         crime_category = ifelse(crime_category == "DUI", "DUI", str_to_title(crime_category)),
         crime_category = ifelse(crime_category == "Interfering With Public Officers", "Interfering w/Public Officers", crime_category),
         citizenship = str_to_title(citizenship)) %>%
  # remove names
  select(-c(Last_name, First_name))

# make new dataset with recidivist/non-recidivist dummy variable
jailData2 <- jailData %>%
  select(booking_number, booking_date, jacket_number) %>%
  mutate(newid = paste(booking_number,jacket_number)) %>%
  distinct(newid, .keep_all = TRUE) %>%
  count(jacket_number) %>%
  filter(n>1)

jailData3 <- jailData %>%
  mutate(newid = paste(booking_number,jacket_number)) %>%
  distinct(newid, .keep_all = TRUE) %>%
  mutate(recidivism = ifelse(jacket_number %in% jailData2$jacket_number,
                             "Recidivist", "Non-Recidivist"))

# only keep first booking for recidivists
recidivismData <- jailData3 %>%
  arrange(jacket_number, booking_date, booking_time) %>%
  group_by(jacket_number) %>%
  filter(booking_date == min(booking_date)) %>%
  group_by(jacket_number) %>%
  filter(booking_time == min(booking_time)) %>%
  select(-c(newid))





##### USER INTERFACE #########################################################

ui <- fluidPage(
  # css for user interface
  tags$head(
    tags$style(
      HTML("
        body{
          font-family: 'Trebuchet MS', sans-serif;
        }
        h4{
          font-weight: bold;
        }
        .shiny-notification{
          font-size: 20px;
          font-weight: bold;
        }
        .shiny-text-output{
          background-color: #e8e8e8;
        }
        #trait{
          background-color: #f6f6f5;
        }
        #category{
          background-color: #f6f6f5;
        }
        #sidebar{
          background-color: #d7d8d2;
        }
        #update{
          background-color: #f6f6f5;
          font-weight: bold;
        }
        #caption{
          color: dark-grey;
          font-size: 12px;
          font-style: italic;
          background-color: #d7d8d2;
        }
      ")
    )
  ),
  
  tabsetPanel(
    # first panel: recidivism by trait
    tabPanel(
      "Recidivism by Different Traits",
      sidebarLayout(
        # sidebar with options
        sidebarPanel(
          id="sidebar",
          # choose trait
          pickerInput(
            "trait", "Select a trait to view differences in:",
            choices = c("Race","Sex","First Offense Type","Marital Status",
                        "Employment Status","Military Status"),
            selected = c("Race"),
            multiple = FALSE
          ),
          # choose categories of trait
          pickerInput(
            "category", "Select which sub-categories of the trait to view:",
            choices = sort(unique(na.omit(recidivismData$race))), 
            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection."),
            selected = unique(na.omit(recidivismData$race)),
            multiple = TRUE
          ),
          # choose counts or proportions
          radioButtons(
            "type", "Select what type of plot and table to view:",
            choices = c("Counts", "Proportions"),
            selected = c("Counts")
          ),
          # update button
          actionButton(
            "update", "Update"
          ),
          tags$i("Press the button to see changes.", style="font-size: 13px;"),
          br(),
          br(),
          # summary of plot results
          verbatimTextOutput("summary_table"),
          br(),
          textOutput("caption")
        ),
        # main panel with plot
        mainPanel(
          plotlyOutput("bar_plot", width = "100%", height = "700px")
        )
      )
    ),
    
    # second panel: raw data
    tabPanel(
      "Data",
      numericInput("maxrows", "Number of rows", 20),
      verbatimTextOutput("raw_table"),
      "The data table shown above shows raw data from the",
      tags$a(href="http://www.co.champaign.il.us/sheriff", 
             "Champaign County Sheriff Office and Jail (CCSO)"),
      "that was edited only for formatting and maintaining anonymity of the individuals."
    ),
    
    # third panel: description of app
    tabPanel(
      "About this app",
      tags$div(
        tags$h4("Description of data"), 
        "The data used in this Shiny app was derived from a dataset from the",
        tags$a(href="http://www.co.champaign.il.us/sheriff", 
               "Champaign County Sheriff Office and Jail (CCSO),"), "and was downloaded from the following ",
        tags$a(href="https://uofi.box.com/shared/static/b6kuknulot4bqyo7elc8gmm6qzhnk766.dat", 
               "link."), tags$br(),
        "According to the description of the dataset provided by Professor Christopher Kinson of the Univerisity of Illinois at 
        Urbana-Champaign Statistics Department, 'The dataset contains 11082 observations and 40 columns for individuals who were 
        booked into jail in Champaign County Jail. The individuals are identifiable based on personal identity, and they are given 
        unique jacket numbers. The demographics of the people, reasons for being booked, and crime codes are also given in the data.'",
        tags$br(), tags$br(),
        
        tags$h4("Motivation of app"),
        "The motivation for this app (i.e. the main tab of 'Recidivism by Different Traits')
        came from the question, 'Do certain traits and/or demographics affect how likely someone is to commit multiple crimes?'
        By observing both the counts and proportions of recidivists and non-recidivists by different traits, differences in
        both the rate of conviction and recidivism can be visualized for categories of different traits.",
        tags$br(), tags$br(),
        
        tags$h4("Key points of data wrangling and analysis"),
        "Since recidivists are people who get convicted of multiple crimes over time, it is more than possible that 
        their traits/demographics change across each observation. Consequently, for simplicity in this app, the data shown
        for the recidivists are that of the individual's very first conviction and booking. In this case, this was determined
        to be the observation with the earliest booking date and time.",
        tags$br(),
        "Furthermore, there were some NA values present for the `super high level` column of the initial dataset that were 
        able to be determined based on the `crime` column of the same dataset. This was because the `super high level` column 
        (`crime_category` in the dataset shown in the 'Data' tab) was a broader category of the observations in the `crime` column, 
        so finding other observations with the same `crime` value allowed for NA values to be replaced with the corresponding 
        `super high level` column."
      )
    )
  )
)



##### SERVER #################################################################

server <- function(input, output, session) {
  # prompt user to use full-screen
  observe({
    showNotification(
      "For the best user experience, please open the app to be full-screen.",
      duration = 10, type ="error"
    )
  }) 
  
  # wait to press update button
  trait <- eventReactive(input$update, {input$trait})
  category <- eventReactive(input$update, {input$category})
  type <- eventReactive(input$update, {input$type})
  
  # new dataset for respective inputs
  plotData <- reactive({
    if (trait() == "Sex") {
      plotData <- recidivismData %>%
        group_by(recidivism) %>%
        count(sex) %>%
        filter(sex %in% category()) %>%
        mutate(abbrev = ifelse(sex == "Female", "F.", "M."))
    }
    
    if (trait() == "First Offense Type") {
      plotData <- recidivismData %>%
        group_by(recidivism) %>%
        count(crime_category) %>%
        filter(crime_category %in% category()) %>%
        mutate(abbrev = ifelse(crime_category == "City Ordinance Violation", "CityO.V.",
                               ifelse(crime_category == "Domestic Violence", "Dom.V.",
                                      ifelse(crime_category == "Interfering w/Public Officers", "Int.P.O.",
                                             ifelse(crime_category == "Other Criminal Offenses", "Oth.C.O.",
                                                    ifelse(crime_category == "Public Order", "Pub.Or.",
                                                           ifelse(crime_category == "DUI", "D.U.I.", crime_category)))))))
    }
    
    if (trait() == "Marital Status") {
      plotData <- recidivismData %>%
        group_by(recidivism) %>%
        count(marital_status) %>%
        filter(marital_status %in% category()) %>%
        mutate(abbrev = ifelse(marital_status == "Divorced", "Divor.",
                               ifelse(marital_status == "Separated", "Separ.",
                                      ifelse(marital_status == "Significant Other", "S.O.", marital_status))))
    }
    
    if (trait() == "Employment Status") {
      plotData <- recidivismData %>%
        group_by(recidivism) %>%
        count(employment_status) %>%
        filter(employment_status %in% category()) %>%
        mutate(abbrev = ifelse(employment_status == "Employed - Full Time", "Em.F.T.",
                               ifelse(employment_status == "Employed - Part Time", "Em.P.T.",
                                      ifelse(employment_status == "Laid Off", "L.O.",
                                             ifelse(employment_status == "Self Employed", "SelfEm.",
                                                    ifelse(employment_status == "Unemployed", "Unem.", employment_status))))))
    }
    
    if (trait() == "Military Status") {
      plotData <- recidivismData %>%
        group_by(recidivism) %>%
        count(military) %>%
        filter(military %in% category()) %>%
        mutate(abbrev = ifelse(military == "Air Force", "AirF.",
                               ifelse(military == "Coast Guard", "CoastG.",
                                      ifelse(military == "Marine Corps", "Marine",
                                             ifelse(military == "National Guard", "Nat.G.", military)))))
    }
    
    if (trait() == "Race") {
      plotData <- recidivismData %>%
        group_by(recidivism) %>%
        count(race) %>%
        filter(race %in% category()) %>%
        mutate(abbrev = ifelse(race == "Asian/Pacific Islander", "A./P.I.",
                               ifelse(race == "Native American", "Nativ.Am.", race)))
    }
    colnames(plotData) <- c("recidivism", "subcategory", "bookings", "abbrev")
    return(plotData)
  })
  
  # function for making bar plot
  trait_barplot <- function(plotData, chosen_trait=c("Race","Sex","First Offense Type","Marital Status",
                                                     "Employment Status","Military Status"), chosen_categories, chosen_type){
    
    g <- ggplot(plotData(), aes(fill=recidivism, x=reorder(abbrev, bookings), y=bookings,
                                text=paste0(subcategory, " - ", recidivism, ": ", bookings))) +
      geom_bar(position=ifelse(chosen_type == "Counts", "stack", "fill"), stat="identity") +
      coord_flip() + 
      ggtitle(paste0("Recidivism by ", chosen_trait)) +
      labs(y="Number of Bookings", fill="") + 
      scale_fill_manual(values=c("#8DB1DA", "#db5e73")) +
      theme(text=element_text(family="Trebuchet MS")) +
      theme(plot.margin=unit(c(1,0,1,0), "cm")) +
      theme(axis.title.y=element_blank()) +
      theme(axis.title.x=element_text(size=12, vjust=0.8)) +
      theme(plot.title=element_text(size=14, face="bold", hjust=0.5)) +
      theme(axis.ticks.y=element_blank()) +
      theme(panel.grid.major=element_blank()) +
      theme(panel.grid.minor=element_blank()) +
      theme(panel.background=element_rect(fill="#FEFEFE")) +
      theme(plot.background=element_rect(fill="#FEFEFE"))
    
    ggplotly(g, tooltip=c("text")) %>% 
      layout(legend=list(font=list(size=14), x=0.8, y=0.1))
  }
  
  
  # update trait category selections
  observeEvent(input$trait, {
    if (input$trait == "Sex") {
      updatePickerInput(
        session = session, inputId = "category", 
        choices = unique(na.omit(recidivismData$sex)), 
        selected = unique(na.omit(recidivismData$sex))
      )
    }
    
    if (input$trait == "First Offense Type") {
      updatePickerInput(
        session = session, inputId = "category", 
        choices = sort(unique(na.omit(recidivismData$crime_category))), 
        selected = unique(na.omit(recidivismData$crime_category))
      )
    }
    
    if (input$trait == "Marital Status") {
      updatePickerInput(
        session = session, inputId = "category", 
        choices = sort(unique(na.omit(recidivismData$marital_status))), 
        selected = unique(na.omit(recidivismData$marital_status))
      )
    }
    
    if (input$trait == "Employment Status") {
      updatePickerInput(
        session = session, inputId = "category", 
        choices = sort(unique(na.omit(recidivismData$employment_status))), 
        selected = unique(na.omit(recidivismData$employment_status))
      )
    }
    
    if (input$trait == "Military Status") {
      updatePickerInput(
        session = session, inputId = "category", 
        choices = sort(unique(na.omit(recidivismData$military))), 
        selected = unique(na.omit(recidivismData$military))
      )
    }
    
    if (input$trait == "Race") {
      updatePickerInput(
        session = session, inputId = "category", 
        choices = sort(unique(na.omit(recidivismData$race))), 
        selected = unique(na.omit(recidivismData$race))
      )
    }
  }, ignoreInit = TRUE)
  
  # caption text output
  caption <- eventReactive(input$update, {
    "Hover over the bars of the graph for the corresponding counts and unabbreviated category names."
  })
  output$caption <- renderText({
    caption()
  })
  
  # bar plot output
  output$bar_plot <- renderPlotly({
    trait_barplot(plotData(), chosen_trait=trait(), chosen_categories=category(), chosen_type = type())
  })
  
  # summary table output
  output$summary_table <- renderPrint({
    plotData2 <- plotData() %>%
      mutate(n_recidivist = ifelse(recidivism == "Recidivist", bookings, NA),
             n_nonrecidivist = ifelse(recidivism == "Non-Recidivist", bookings, NA)) %>%
      group_by(subcategory) %>%
      summarise(n_recidivist = sum(n_recidivist, na.rm=TRUE),
                n_nonrecidivist = sum(n_nonrecidivist, na.rm=TRUE)) %>%
      mutate(`# Recid : Non-Recid` = paste0(n_recidivist, " : ", n_nonrecidivist)) %>%
      mutate(`Percent Recidivism` = paste0(" ", signif((n_recidivist/(n_recidivist+n_nonrecidivist)*100), 3),"%")) %>%
      select(-c(n_recidivist, n_nonrecidivist)) %>%
      rename(Category = subcategory)
    
    if (type() == "Counts"){
      plotData3 <- plotData2 %>%
        select(c(Category, `# Recid : Non-Recid`))
    }
    if (type() == "Proportions"){
      plotData3 <- plotData2 %>%
        select(c(Category, `Percent Recidivism`))
    }
    
    kable(plotData3)
  })
  
  # organized raw data table output
  output$raw_table <- renderPrint({
    jailData_sub <- jailData %>% 
      select(-c(booking_number, citizenship, state)) %>%
      mutate(booking_date = as.character(booking_date),
             released_date = as.character(released_date))
    
    opt1 <- options(width = 1000)
    print(kable(tail(jailData_sub, input$maxrows)), row.names = FALSE)
    options(opt1)
  })
  
  
}

shinyApp(ui = ui, server = server)
