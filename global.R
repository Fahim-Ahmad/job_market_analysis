##### read data and data manipulation ---------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(highcharter)) install.packages("highcharter")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(shiny)) install.packages("shiny")
if(!require(shinyalert)) install.packages("shinyalert")

df <- readxl::read_excel("input/Last Header.xlsx")
n_jobs <- nrow(df)
n_jobs_fulltime <- df %>% filter(`Employment Type` == 'Full-Time') %>% nrow()
n_jobs_parttime <- df %>% filter(`Employment Type` == 'Part-time') %>% nrow()
n_jobs_contract <- df %>% filter(`Employment Type` == 'Contract') %>% nrow()
avg_salary <- round(mean(df$Salary))

indicators <- c("Employment Type", "Seniority Level", "Job Industry", "Required Degree", "Company Name", "Job Location", "Gender")

highcart_bar <- function(data, column) {
  tbl <- data %>%
    count(q = !!sym(column)) %>% 
    arrange(desc(n))
  
  tbl %>% 
    hchart('column', hcaes(x = q, y = n)) %>% 
    hc_xAxis(title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = "")) %>% 
    hc_exporting(enabled = FALSE, filename = "barchart")
}

# highcart_bar(df, 'Seniority Level')
# highcart_bar(df, 'Required Degree')
# highcart_bar(df, 'Job Location')
# highcart_bar(df, 'Employment Type')
# highcart_bar(df, 'Gender')

technical_skills <- df %>%
  select(c(`Technichal Skills 1`, `Technichal Skills 2`, `Technichal Skills 3`)) %>%
  pivot_longer(cols = everything(), names_to = 'col', values_to = 'skills') %>% 
  filter(!is.na(skills))

# highcart_bar(technical_skills, 'skills')

soft_skills <- df %>%
  select(c(`Soft Skills 1`, `Soft Skills 2`, `Soft Skills 3`)) %>%
  pivot_longer(cols = everything(), names_to = 'col', values_to = 'skills') %>% 
  filter(!is.na(skills))

# highcart_bar(soft_skills, 'skills')

language <- df %>%
  select(c(`Linguistics Skills 1`, `Linguistics Skills 2`, `Linguistics Skills 3`)) %>%
  pivot_longer(cols = everything(), names_to = 'col', values_to = 'language') %>% 
  filter(!is.na(language))

# highcart_bar(language, 'language')

# df %>% count(`Job Title`)
# df %>% count(`Job Industry`)
# df %>% count(`Economic Sector`)
# df %>% count(`Company Name`)
# df %>% count(`Company Industry`)
# df %>% count(Age)

