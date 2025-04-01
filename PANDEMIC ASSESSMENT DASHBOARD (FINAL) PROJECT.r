library(patchwork)
library(shiny)
library(purrr)
library(shinythemes)
library(plotly)
library(networkD3)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(data.table)
#DATA CLEANING
df <- read.csv("/Users/kingkorter/Downloads/Covid Data.csv")
head(df)
df <- df %>% rename(INTUBATED = INTUBED)
df <- df %>% rename(CLASSIFICATION_FINAL = CLASIFFICATION_FINAL)

df <- df %>%
  mutate(USMER = 
  case_when(USMER ==1 ~ "Primary lvl", 
  USMER ==2 ~ "Secondary lvl",
  USMER ==3 ~ "Tietrary lvl", 
  TRUE ~ as.character(USMER)
  ))
str(df$AGE)
 df <- df %>%
  mutate( SEX = 
  case_when(SEX ==1 ~ "Female", 
  SEX ==2 ~ "Male",
  TRUE ~ as.character(SEX)
  ))

 df <- df %>%
  mutate( PATIENT_TYPE = 
  case_when(PATIENT_TYPE ==1 ~ "Returned Home", 
  PATIENT_TYPE ==2 ~ "Hospitalized",
  TRUE ~ as.character(PATIENT_TYPE)
  ))
table(df$PATIENT_TYPE)
   df <- df %>%
  mutate( INTUBATED = 
  case_when(INTUBATED ==1 ~ "Yes", 
  INTUBATED %in% c(2,97,99) ~ "No",
  TRUE ~ as.character(INTUBATED)
  ))

     df <- df %>%
  mutate( PREGNANT = 
  case_when(PREGNANT ==1 ~ "Yes", 
  PREGNANT %in% c(2,97,99) ~ "No",
  TRUE ~ as.character(PREGNANT)
  ))
table(df$DATE_DIED)
str(df$DATE_DIED)
#      99      No     Yes 
#    7325 1007594   33656

   df <- df %>%
  mutate( CLASSIFICATION_FINAL = 
  case_when(CLASSIFICATION_FINAL %in% c(1) ~ "COVID-19 Positive Low",
  CLASSIFICATION_FINAL %in% c(2) ~ "COVID-19 Positive Mid",
  CLASSIFICATION_FINAL %in% c(3) ~ "COVID-19 Positive High", #might change this to show covid severity
  CLASSIFICATION_FINAL %in% c(4,5,6,7) ~ "COVID-19 Negative",
  TRUE ~ as.character(CLASSIFICATION_FINAL)
  ))

   df <- df %>%
  mutate( ICU = 
  case_when(ICU ==1 ~ "Yes", 
  ICU %in% c(2,97,99) ~ "No",
  TRUE ~ as.character(ICU)
  ))
# > table(df$ICU)

#      99      No     Yes 
#    7488 1024229   16858
unique(df$USMER)
df <- df %>%
mutate(across(-AGE, ~ ifelse(. %in% c(98,97, 99), NA, .)))#ifelse not structured to work across numerous columns 
df <- na.omit(df)
df <- df %>%
  mutate(across(-AGE, ~ ifelse(. %in% c("9999-99-99"), NA, .)))

df$DATE_DIED <- as.Date(df$DATE_DIED, format= "%d/%m/%Y")
unique(df$DATE_DIED)
# df <- df[df$CLASIFFICATION_FINAL == 1 | df$CLASIFFICATION_FINAL == 2 | df$CLASIFFICATION_FINAL == 3, ]
Condition_ <- c("DIABETES", "COPD", "ASTHMA", 
                                      "INMSUPR", "HIPERTENSION", "OTHER_DISEASE", 
                                      "CARDIOVASCULAR", "OBESITY", 
                                      "RENAL_CHRONIC", "TOBACCO", "PNEUMONIA") 
                              
print(Condition_)
unique(df$COPD)
# Condition_ <- names(df)[sapply(df[names(df) %in% c("DIABETES", "COPD", "ASTHMA", 
#                                                    "INMSUPR", "HIPERTENSION", "OTHER_DISEASE", 
#                                                    "CARDIOVASCULAR", "OBESITY", 
#                                                    "RENAL_CHRONIC", "TOBACCO", "PNEUMONIA")], 
#                               function(col) any(col == 1, na.rm = TRUE))]
# c("DIABETES" ,"COPD","ASTHMA","INMSUPR","HIPERTENSION",
#  "OTHER_DISEASE","CARDIOVASCULAR" ,"OBESITY","RENAL_CHRONIC","TOBACCO","PNEUMONIA") #setdiff(colnames(df), excluded_)
df <- df %>% mutate(dead=
case_when(is.na(DATE_DIED)~ NA_character_,
!is.na(DATE_DIED)~ "YES"))
unique(df$dead)

ui <- fluidPage(
    theme = shinytheme('cyborg'),
tags$style(HTML("
    .custom-title {
        text-align: center; 
        font-size: 32px; 
        font-weight: bold; 
        color: #2C3E50; 
        padding: 10px;
    }
")),
titlePanel(
    tags$div(
        icon("virus"),
        "COVID-19 Patient Analysis",
        class = "custom-title"
    )
),

    sidebarLayout(
        sidebarPanel(
            tags$h3("Please select filters"),
            selectInput("sex", label= HTML("<i class='fas fa-venus-mars'></i> Select Sex:"), choices = c(unique(df$SEX), "All"), selected = "All"),
            sliderInput("age", label= "Age", min= min(df$AGE), max= max(df$AGE), value = c(min(df$AGE), max(df$AGE))),

            checkboxGroupInput("classification", 
             label= HTML("<i class='fas fa-vial'></i> COVID-19 Test Outcome"), 
             choices = c(unique(df$CLASSIFICATION_FINAL)), 
             selected= df$CLASSIFICATION_FINAL),
            checkboxGroupInput("patient_type", 
             label= "Patient Status", 
             choices = c(unique(df$PATIENT_TYPE)),
             selected=df$PATIENT_TYPE),

            tags$h2("Select / Deselect all"),
            pickerInput("underlying_condition", 
             label= "Underlying Condition", 
             choices= Condition_, 
             multiple = TRUE,
             options = list(`actions-box` = TRUE),
             selected= NULL),

            selectInput("pregnant", label= HTML("<i class='fas fa-baby'></i> Pregnancy Status:"), choices = c(unique(df$PREGNANT)), selected= unique(df$PREGNANT), multiple= TRUE),
            selectInput("usmer", label= HTML("<i class='fas fa-hospital'></i> Medical Treatment Degree:"), choices = c(unique(df$USMER)), selected=unique(df$USMER),multiple= TRUE),
            selectInput("intubed", label= HTML("<i class='fas fa-head-side-mask'></i> Connected to a ventilator:"), choices = c(unique(df$INTUBATED)), selected= unique(df$INTUBATED), multiple= TRUE),
            selectInput("icu", label= HTML("<i class='fas fa-bed-pulse'></i> Admission into an ICU"), choices = c(unique(df$ICU)), selected= unique(df$ICU), multiple= TRUE),

            dateRangeInput("date", "Date of Death:", 
               start = min(df$DATE_DIED, na.rm = TRUE), 
               end = max(df$DATE_DIED, na.rm = TRUE),
               min = min(df$DATE_DIED, na.rm = TRUE),
               max = max(df$DATE_DIED, na.rm = TRUE)),
            checkboxInput("death_na", "Add People that didn't die", value = FALSE),
            checkboxInput("alive", "Select only People that didn't die", value = FALSE),
            textOutput("warning"),
            actionButton("btn", "FILTER")
            


        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Overview and Summary", verbatimTextOutput("text1"), plotOutput("graphs1"), verbatimTextOutput("text2")),
                tabPanel("Outcome Analysis", plotOutput("graphs2")),
                tabPanel("Underlying Condition Analysis", plotOutput("graphs3")),
                tabPanel("notNamedYet", plotOutput("graphs4"))
            )
        )
    )

)
server <- function(input, output) {
  filtered_data <- eventReactive(input$btn, {
    df1 <- df

    # Filter by sex
    if (input$sex != "All") {
        df1 <- df1 %>% filter(SEX == input$sex)
    }

    # Filter by age range
    df1 <- df1 %>% filter(AGE >= input$age[1] & AGE <= input$age[2])

    # Filter by classification
    if (!is.null(input$classification) && length(input$classification) > 0) {
        df1 <- df1 %>% filter(CLASSIFICATION_FINAL %in% input$classification)
    }

    # Filter by patient type
    if (!is.null(input$patient_type) && length(input$patient_type) > 0) {
        df1 <- df1 %>% filter(PATIENT_TYPE %in% input$patient_type)
    }

    # Fix filtering for underlying conditions
    if (!is.null(input$underlying_condition) && length(input$underlying_condition) > 0) {
        df1 <- reduce(input$underlying_condition, function(df, col) {df %>% filter(.data[[col]] == 1)}, .init = df1)


    }

    # Filter by pregnancy status
    if (!is.null(input$pregnant) && length(input$pregnant) > 0) {
        df1 <- df1 %>% filter(PREGNANT %in% input$pregnant)
    }

    # Filter by medical treatment degree
    if (!is.null(input$usmer) && length(input$usmer) > 0) {
        df1 <- df1 %>% filter(USMER %in% input$usmer)
    }

    # Filter by ventilator status
    if (!is.null(input$intubed) && length(input$intubed) > 0) {
        df1 <- df1 %>% filter(INTUBATED %in% input$intubed)
    }

    # Filter by ICU admission
    if (!is.null(input$icu) && length(input$icu) > 0) {
        df1 <- df1 %>% filter(ICU %in% input$icu)
    }

    # Fix death filtering logic
    if (input$alive) {
        df1 <- df1 %>% filter(is.na(DATE_DIED))
    } else if (input$death_na) {
        df1 <- df1 %>% filter(is.na(DATE_DIED) | (DATE_DIED >= input$date[1] & DATE_DIED <= input$date[2]))
    } else {
        df1 <- df1 %>% filter(DATE_DIED >= input$date[1] & DATE_DIED <= input$date[2])
    }

    # Add DEAD column
    df1 <- df1 %>% mutate(DEAD = ifelse(is.na(DATE_DIED), "NO", "YES"))
  
   
      if (input$alive == TRUE && input$death_na== TRUE){
  output$warning <- renderText({
    paste(
      "You shouldn't select both buttons at the same time"
    )
  })}
  return(df1)
})


  # Render text summary
  output$text1 <- renderText({
    req(filtered_data())
    df1 <- filtered_data()
    percentage <- (nrow(df1) / nrow(df)) * 100
    paste(
      "Total Number of cases from your filter is:", nrow(df1), "\n",
      "Total Number of recorded cases:", nrow(df), "\n",
      "Percentage of cases from your filter:", round(percentage, 2), "%"
    )
  })

  # Render filter information
  output$text2 <- renderPrint({
    req(filtered_data())
    df1 <- filtered_data()
    paste(
      "Filters applied: Sex", input$sex, 
      ", Age range:", paste(input$age, collapse = " - "), 
      ", COVID-19 Outcome:", paste(input$classification, collapse = ", "), 
      ", Patient Status:", paste(input$patient_type, collapse = ", "), 
      ", Underlying Condition:", paste(input$underlying_condition, collapse = ", "), 
      ", Pregnant:", paste(input$pregnant, collapse = ", "), 
      ", Medical Treatment Degree:", paste(input$usmer, collapse = ", "), 
      ", Connected to a ventilator:", paste(input$intubed, collapse = ", "), 
      ", Admission to an ICU:", paste(input$icu, collapse = ", "), 
      ", Death Date Range:", paste(input$date, collapse = " to "), 
      ", Selecting only people that died:", input$alive, 
      ", Including people that didn't die:", input$death_na
    )
  })

  # Render graphs in 2x2 layout
  output$graphs1 <- renderPlot({
    req(filtered_data())
    df1 <- filtered_data()
    df1 <- df1 %>% distinct()


    plot1 <- ggplot(df1, aes(x = SEX)) +
      geom_bar() +
      labs(title = "Bar chart of Sex", x = "Sex", y = "Count") +
      theme_minimal()

    plot2 <- ggplot(df1, aes(x = AGE)) +
      geom_density() +
      labs(title = "Density Plot of Age", x = "Age", y = "Density") +
      theme_minimal()

    plot3 <- ggplot(df1, aes(x = CLASSIFICATION_FINAL)) +
      geom_bar() +
      labs(title = "Bar chart of COVID-19 Outcome", x = "COVID-19 Outcome", y = "Count") +
      theme_minimal()

    plot4 <- ggplot(df1, aes(x = PATIENT_TYPE, y = AGE)) +
      geom_boxplot() +
      labs(title = "Box Plot of Patient Type and Age", x = "Patient Type", y = "Age") +
      theme_minimal()

    return( (plot1 | plot2) / (plot3 | plot4) )

  })

  output$graphs2 <- renderPlot({
    req(filtered_data())
    df1 <- filtered_data()
    df1 <- df1 %>% distinct()
    sex_dead <- df1 %>% count(SEX, DEAD)
    heat_map <- df1 %>% count(PATIENT_TYPE, DEAD)


    plot5 <- ggplot(heat_map, aes(x = PATIENT_TYPE,y=DEAD,fill=n)) +
      geom_tile(color="white") +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title =  "Heatmap of Patient Status vs. Death", y = "Death Status", x = "Patient Status",fill="count") +
      theme_minimal()

    plot6 <- ggplot(sex_dead, aes(x = SEX, y=n, fill = DEAD)) +
      geom_bar(position = "stack" ,stat="identity") +  
      labs(title = "Stacked Bar Chart of Gender vs Death Status",
           x = "Gender",
           y = "Count",
           fill = "Death Status") +
      theme_minimal()


    plot7 <- ggplot(df1, aes(x = CLASSIFICATION_FINAL, fill = DEAD)) +
      geom_bar(position = "stack") +  
      labs(title = "Stacked Bar Chart of Covid Severity vs Death Status",
           x = "Covid 19 Test Outcome",
           y = "Count",
           fill = "Death Status") +
      theme_minimal()

    plot8 <- ggplot(df1, aes(x = DEAD, y = AGE)) +
      geom_boxplot() +
      labs(title = "Box Plot of Final Outcome vs Age", x = "Final Outcome", y = "Age") +
      theme_minimal()

   return( (plot5 | plot6) / (plot7 | plot8) )
    
    
    

  })

  output$graphs3 <- renderPlot({
    
    req(filtered_data(), input$underlying_condition)  # Ensure data and input exist
    df1 <- filtered_data()

    # Ensure at least one condition is selected
    
        # Convert selected conditions to long format
    df_long <- df1 %>%
            select(CLASSIFICATION_FINAL, all_of(input$underlying_condition)) %>%
            pivot_longer(cols = all_of(input$underlying_condition), names_to = "Condition", values_to = "Has_Condition") %>%
            filter(Has_Condition == 1)  # Keep only rows where condition is present
    df_long2 <- df1 %>%
            select(DEAD, all_of(input$underlying_condition)) %>%
            pivot_longer(cols = all_of(input$underlying_condition), names_to = "Condition", values_to = "Has_Condition") %>% count(Condition, DEAD)
    
        # Plot the stacked bar chart
    plot9<-ggplot(df_long, aes(x = Condition, fill = CLASSIFICATION_FINAL)) +
            geom_bar(position = "stack") +
            labs(title = "Underlying Conditions Stacked by COVID Classification",
                 x = "Underlying Condition",
                 y = "Count",
                 fill = "COVID Classification") +
            theme_minimal() +
            scale_fill_brewer(palette = "Set2")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Aesthetic color palette
    



    plot10 <- ggplot(df_long2, aes(y = Condition,x=DEAD, fill=n)) +
      geom_tile(color="white") +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title =  "Heatmap of Conditions vs. Death", y = "Conditions", x = "Dead",fill="count") +
      theme_minimal()


   

   return(plot9 | plot10)
    
  })
  output$graphs4<- renderPlot({
     req(filtered_data())  # Ensure data and input exist
    df1 <- filtered_data()
    dfp <- df1 %>% count(PREGNANT, DEAD)
    dfu <- df1 %>% count(USMER, DEAD)
    dfi <- df1 %>% count(INTUBATED, DEAD)
    dfic <- df1 %>% count(ICU, DEAD)

    plot11 <- ggplot(dfp, aes(x = PREGNANT, y=n, fill = DEAD)) +
      geom_bar(position = "stack", stat ="identity") +  
      labs(title = "Stacked Bar Chart of pregnancy vs Death Status",
           x = "pregnancy",
           y = "Count",
           fill = "Death Status") +
      theme_minimal()

    plot12 <- ggplot(dfic, aes(x = ICU, y=n, fill = DEAD)) +
      geom_bar(position = "stack", stat ="identity") +  
      labs(title = "Stacked Bar Chart of icu vs Death Status",
           x = "icu",
           y = "Count",
           fill = "Death Status") +
      theme_minimal()

    plot13 <- ggplot(dfu, aes(x = USMER, y=n, fill = DEAD)) +
      geom_bar(position = "stack", stat ="identity") +  
      labs(title = "Stacked Bar Chart of usmer vs Death Status",
           x = "usmer",
           y = "Count",
           fill = "Death Status") +
      theme_minimal()

      plot14 <- ggplot(dfi, aes(x = INTUBATED, y=n, fill = DEAD)) +
      geom_bar(position = "stack", stat ="identity") +  
      labs(title = "Stacked Bar Chart of intubated vs Death Status",
           x = "intubated",
           y = "Count",
           fill = "Death Status") +
      theme_minimal()


  return( (plot11 | plot12) / (plot13 | plot14) )


  })
}

shinyApp(ui = ui, server = server )


