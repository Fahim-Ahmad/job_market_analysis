source("global.R")

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    shinyalert::useShinyalert(),
    tags$head(tags$style(HTML("
      .box {
        height: 170px;
        overflow-y: hidden;
      }
      .box .box-title {
        font-size: 12px;
        color: gray
      }
      .custom-box {
        height: 200px;
      }
    "))),
    uiOutput("value_boxes"),
    fluidRow(
      column(8,
             column(width = 12,
                           tabsetPanel(
                             id = "tabs",
                             type = 'tabs',
                             tabPanel("Technical Skills", value = "Technical Skills",
                                      box(title = "Top Technical Skills", width = NULL, status = "primary", solidHeader = FALSE,
                                          highchartOutput("technical_skills_graph", height = "120px"),
                                          class = "custom-box"
                                      )
                                      ),
                             tabPanel("Soft Skills", value = "Soft Skills",
                                      box(title = "Top Soft Skills", width = NULL, status = "primary", solidHeader = FALSE,
                                          highchartOutput("soft_skills_graph", height = "120px"),
                                          class = "custom-box"
                                      )
                             ),
                             tabPanel("Report", value = "Seniority Level",
                                      box(title = NULL, width = NULL, status = "primary", solidHeader = FALSE,
                                          "Placeholder to generate automatic report",
                                          class = "custom-box-report"
                                      )
                             )
                           )
             ),
             column(width = 4,
                    box(title = "Number of Jobs, by Seniority Level", width = NULL, status = "primary", solidHeader = FALSE,
                        highchartOutput("seniority_level_graph", height = "120px")
                    )
             ),
             column(width = 4,
                    box(title = "Number of Jobs, by Degree", width = NULL, status = "primary", solidHeader = FALSE,
                        highchartOutput("required_degree_graph", height = "120px")
                    )
             ),
             column(width = 4,
                    box(title = "Number of Jobs, by Location", width = NULL, status = "primary", solidHeader = FALSE,
                        highchartOutput("job_location_graph", height = "120px")
                    )
             ),
             column(width = 4,
                    box(title = "Number of Jobs, by Employment Type", width = NULL, status = "primary", solidHeader = FALSE,
                        highchartOutput("employment_type_graph", height = "120px")
                    )
             ),
             column(width = 4,
                    box(title = "Number of Jobs, by Gender", width = NULL, status = "primary", solidHeader = FALSE,
                        highchartOutput("gender_graph", height = "120px")
                    )
             ),
             column(width = 4,
                    box(title = "Average Salary", width = NULL, status = "primary", solidHeader = FALSE,
                        "Placeholder for Box-plot / Density / Histogram"
                    )
             )
             ),
      column(4, style='height:600px;overflow-y: scroll;',
             column(6, offset = 0, style='padding:0px;',
                    selectInput("indicator", label = "Filter Jobs By:", choices = indicators, selected = "", width = "100%")
                    ),
             column(6, offset = 0, style='padding:0px;',
                    selectInput("value", label = "-", choices = c("All"), selected = "All", width = "100%")
             ),
             DT::dataTableOutput("table")
      )
    ),
  )
)

server <- function(input, output, session){
  
  output$value_boxes <- renderUI({
    
    splitLayout(cellWidths = rep("20%", 5),
                valueBox(n_jobs, width = "100%", subtitle = "Total Jobs", color = "navy"),
                valueBox(n_jobs_fulltime, width = "100%", subtitle = "Full-Time Jobs", color = "blue"),
                valueBox(n_jobs_parttime, width = "100%", subtitle = "Part-Time Jobs", color = "light-blue"),
                valueBox(n_jobs_contract, width = "100%", subtitle = "Contract Jobs", color = "aqua"),
                valueBox(avg_salary, width = "100%", subtitle = "Average Salary", color = "teal"),
                # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    )
  })
  
  output$seniority_level_graph <- renderHighchart({highcart_bar(df, 'Seniority Level')})
  output$required_degree_graph <- renderHighchart({highcart_bar(df, 'Required Degree')})
  output$job_location_graph <- renderHighchart({highcart_bar(df, 'Job Location')})
  output$employment_type_graph <- renderHighchart({highcart_bar(df, 'Employment Type')})
  output$gender_graph <- renderHighchart({highcart_bar(df, 'Gender')})
  output$technical_skills_graph <- renderHighchart({highcart_bar(technical_skills, 'skills')})
  output$soft_skills_graph <- renderHighchart({highcart_bar(soft_skills, 'skills')})
  # output$test1_graph <- output$test2_graph <- output$test3_graph <- output$test4_graph <- renderHighchart({highcart_bar(df, 'Gender')})
  
  dt <- reactive({
    if (input$value != "All") {
      return(df %>% filter(!!sym(input$indicator) == input$value))
    } else {
      return(df)
    }
  })
  
  observeEvent(input$indicator, {
    selected_indicator <- input$indicator
    
    updateSelectInput(session, "value",
                      choices = c("All", unique(df[selected_indicator])),
                      selected = "All")
  })
  output$table <- DT::renderDataTable(
    dt() %>%
    # df %>%
      select(c(`Job Title`, `Company Name`, `Job Location`)),
    # options = list(pageLength = 10, lengthChange = FALSE, autoWidth = TRUE, searching = FALSE),
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      autoWidth = TRUE,
      searching = FALSE,
      rowCallback = JS(
        "function(row, data) {",
        "  $(row).on('click', function() {",
        "    var index = $(row).index();",
        "    Shiny.setInputValue('selected_row', index);",
        "  });",
        "}"
      )
    ),
    rownames= FALSE,
    selection = 'none'
  )
  
  observeEvent(input$selected_row, {
    req(input$selected_row)
    
    full_row_data <- df[input$selected_row + 1, ]
    
    company_name <- full_row_data$`Company Name`
    company_link <- full_row_data$`Company Link`
    technical_skills <- glue::glue(
      "{ifelse(is.na(full_row_data$`Technichal Skills 1`), '', full_row_data$`Technichal Skills 1`)},
      {ifelse(is.na(full_row_data$`Technichal Skills 2`), '', full_row_data$`Technichal Skills 2`)},
      {ifelse(is.na(full_row_data$`Technichal Skills 3`), '', full_row_data$`Technichal Skills 3`)}
      ",
    )
    
    soft_skills <- glue::glue(
      "{ifelse(is.na(full_row_data$`Soft Skills 1`), '', full_row_data$`Soft Skills 1`)},
      {ifelse(is.na(full_row_data$`Soft Skills 2`), '', full_row_data$`Soft Skills 2`)},
      {ifelse(is.na(full_row_data$`Soft Skills 3`), '', full_row_data$`Soft Skills 3`)}
      ",
    )
    
    language <- glue::glue(
      "{ifelse(is.na(full_row_data$`Linguistics Skills 1`), '', full_row_data$`Linguistics Skills 1`)},
      {ifelse(is.na(full_row_data$`Linguistics Skills 2`), '', full_row_data$`Linguistics Skills 2`)},
      {ifelse(is.na(full_row_data$`Linguistics Skills 3`), '', full_row_data$`Linguistics Skills 3`)}
      ",
    )
    
    required_degree <- glue::glue(
      "{full_row_data$`Required Degree`} ({ifelse(is.na(full_row_data$`Major 1`), '', full_row_data$`Major 1`)}, {ifelse(is.na(full_row_data$`Major 1`), '', full_row_data$`Major 2`)})"
    )
    
    seniority_level <- full_row_data$`Seniority Level`
    
    employment_type <- full_row_data$`Employment Type`
    gender <- ifelse(full_row_data$Gender == 'M', 'Male', 'Female')
    salary <- full_row_data$`Salary`
    
    location <- full_row_data$`Job Location`
    job_link <- full_row_data$`Job Link`
    
    age <- full_row_data$`Age`
    
    row_summary <- glue::glue(
      
      "<div style = 'text-align: left;'>
      <h4>About</h4>
      <ab>Company Name</b>: <a href='{company_link}'>{company_name}</a><br>
      <ab>location</b>: {location}<br>
      <ab>Employment Type and Salary</b>: {employment_type}<br>
      <ab>Salary</b>: {salary}<br>
      <br>
      
      <h4>Skills</h4>
      <ul>
        <li>Technical Skills: {technical_skills}</li>
        <li>Soft Skills: {soft_skills}</li>
        <li>Linguistics Skills: {language}<br></li>
      </ul>
      
      <h4>Requirements</h4>
      Degree: {required_degree}<br>
      Gender: {gender}<br>
      Age: {age}<br>
      </div>
      "
    )
    
    # Show the summary in a shinyalert popup
    shinyalert::shinyalert(
      title = full_row_data$`Job Title`,
      html = TRUE,
      showConfirmButton = FALSE,
      closeOnClickOutside = TRUE,
      text = row_summary,
      type = ""
    )
  })
  
}


# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     shinyalert::useShinyalert(),
#     fluidRow(
#       column(12, style='height:600px;overflow-y: scroll;',
#              DT::dataTableOutput("table")
#       )
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   output$table <- DT::renderDataTable(
#     df %>%
#       select(c(`Job Title`, `Company Name`, `Job Location`)),
#     options = list(
#       pageLength = 10,
#       lengthChange = FALSE,
#       autoWidth = TRUE,
#       searching = FALSE,
#       rowCallback = JS(
#         "function(row, data) {",
#         "  $(row).on('click', function() {",
#         "    var index = $(row).index();",
#         "    Shiny.setInputValue('selected_row', index);",
#         "  });",
#         "}"
#       )
#     ),
#     rownames = FALSE,
#     selection = 'none'
#   )
#   
#   observeEvent(input$selected_row, {
#     req(input$selected_row)
#     
#     full_row_data <- df[input$selected_row + 1, ]
#     
#     company_name <- full_row_data$`Company Name`
#     company_link <- full_row_data$`Company Link`
#     technical_skills <- glue::glue(
#       "{ifelse(is.na(full_row_data$`Technichal Skills 1`), '', full_row_data$`Technichal Skills 1`)},
#       {ifelse(is.na(full_row_data$`Technichal Skills 2`), '', full_row_data$`Technichal Skills 2`)},
#       {ifelse(is.na(full_row_data$`Technichal Skills 3`), '', full_row_data$`Technichal Skills 3`)}
#       ",
#     )
#     
#     soft_skills <- glue::glue(
#       "{ifelse(is.na(full_row_data$`Soft Skills 1`), '', full_row_data$`Soft Skills 1`)},
#       {ifelse(is.na(full_row_data$`Soft Skills 2`), '', full_row_data$`Soft Skills 2`)},
#       {ifelse(is.na(full_row_data$`Soft Skills 3`), '', full_row_data$`Soft Skills 3`)}
#       ",
#     )
#     
#     language <- glue::glue(
#       "{ifelse(is.na(full_row_data$`Linguistics Skills 1`), '', full_row_data$`Linguistics Skills 1`)},
#       {ifelse(is.na(full_row_data$`Linguistics Skills 2`), '', full_row_data$`Linguistics Skills 2`)},
#       {ifelse(is.na(full_row_data$`Linguistics Skills 3`), '', full_row_data$`Linguistics Skills 3`)}
#       ",
#     )
#     
#     required_degree <- glue::glue(
#       "{full_row_data$`Required Degree`} ({ifelse(is.na(full_row_data$`Major 1`), '', full_row_data$`Major 1`)}, {ifelse(is.na(full_row_data$`Major 1`), '', full_row_data$`Major 2`)})"
#     )
#     
#     seniority_level <- full_row_data$`Seniority Level`
#     
#     employment_type <- full_row_data$`Employment Type`
#     gender <- ifelse(full_row_data$Gender == 'M', 'Male', 'Female')
#     salary <- full_row_data$`Salary`
#     
#     location <- full_row_data$`Job Location`
#     job_link <- full_row_data$`Job Link`
#     
#     age <- full_row_data$`Age`
#     
#     row_summary <- glue::glue(
#       
#       "<div style = 'text-align: left;'>
#       <h4>About</h4>
#       <ab>Company Name</b>: <a href='{company_link}'>{company_name}</a><br>
#       <ab>location</b>: {location}<br>
#       <ab>Employment Type and Salary</b>: {employment_type} - {salary}<br>
#       <ab>Salary</b>: {salary}<br>
#       <br>
#       
#       <h4>Skills</h4>
#       <ul>
#         <li>Technical Skills: {technical_skills}</li>
#         <li>Soft Skills: {soft_skills}</li>
#         <li>Linguistics Skills: {language}<br></li>
#       </ul>
#       
#       <h4>Requirements</h4>
#       Degree: {required_degree}<br>
#       Gender: {gender}<br>
#       Age: {age}<br>
#       </div>
#       "
#     )
#     
#     shinyalert::shinyalert(
#       title = full_row_data$`Job Title`,
#       html = TRUE,
#       showConfirmButton = FALSE,
#       closeOnClickOutside = TRUE,
#       text = row_summary,
#       type = ""
#     )
#   })
# }

shinyApp(ui = ui, server = server)



