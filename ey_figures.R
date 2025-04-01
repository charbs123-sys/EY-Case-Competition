library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gridExtra)

data <- read_excel("data.xlsx", sheet = "Data")

df <- data %>%
  mutate(
    wages_2026 = `2025 wages` + (`2025 wages` - `2024 wages`),
    medical_2026 = `2025 medical costs` + (`2025 medical costs` - `2024 medical costs`),
    employees_2026 = `Number of employees 2025` * (sqrt(`Number of employees 2025`/`Number of employees 2024`)),
    claims_2026 = `2025 number of claims` * (sqrt(`2025 number of claims`/`2024 number of claims`))
  )


bool_mat <- as.data.frame(df > 0)
bool_vec <- bool_mat %>% rowwise() %>% summarise(product = prod(c(wages_2026,medical_2026,employees_2026,claims_2026)))
df <- df[bool_vec == 1,]

#observing that the Fishing and Agriculture industry has the most outliers
#we can remove these companies and consider them seperately 
outliers <- df %>% 
  group_by(Industry) %>% 
  mutate(
    Q1 = quantile(medical_2026, 0.25),
    Q3 = quantile(medical_2026, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    is_outlier = medical_2026 < lower_bound | medical_2026 > upper_bound
  ) %>% 
  filter(is_outlier) %>% 
  ungroup() %>% 
  select(`Company ID`, `Company name`, Industry, medical_2026, employees_2026)

outliers %>% arrange(desc(medical_2026))



#removing outliers - results do not change significantly - ignore for now,
#if we neglect outliers we reject Atlanti-safe paying a substantial premium
#df <- df %>% filter(!(`Company ID` %in% outliers$`Company ID`))


year <- c("2023","2024","2025","2026")

df <- df %>% 
  group_by(Industry) %>%
  summarise(
    wages2026 = sum(wages_2026),
    wages2025 = sum(`2025 wages`),
    wages2024 = sum(`2024 wages`),
    wages2023 = sum(`2023 wages`),
    
    medical2026 = sum(medical_2026),
    medical2025 = sum(`2025 medical costs`),
    medical2024 = sum(`2024 medical costs`),
    medical2023 = sum(`2023 medical costs`),
    
    employees2026 = sum(employees_2026),
    employees_2025 = sum(`Number of employees 2025`),
    employees_2024 = sum(`Number of employees 2024`),
    employees_2023 = sum(`Number of employees 2023`),
    
    claims2026 = sum(claims_2026),
    claims_2025 = sum(`2025 number of claims`),
    claims_2024 = sum(`2024 number of claims`),
    claims_2023 = sum(`2023 number of claims`)
  ) 

describe_df <- df %>% select(wages2026, medical2026, employees2026, claims2026)
summary(describe_df)

summary(df)

#average cost per claim for each industry (injuries from fishing is greater -> labour = higher medical expenses -> higher wages -> 
#greater premiums)
df %>% summarise(Industry, avg_cost_per_claim = medical2026/claims2026)




plot_feature <- function(df, df_feature, name) {
  df_pivot <- pivot_longer(df_feature, cols = everything(), names_to = "Year", values_to = "Wage")
  print(df_pivot)
  empt <- c()
  for (i in 1:6) {
    empt <- c(empt, rep(c(df %>% select(Industry))$Industry[i],4))
  }
  df_industry <- data.frame(empt)
  
  df_pivot$Industry <- empt
  
  if (name == "Wage") {
    title_name <- "Total Wage per industry"
  } else if (name == "Employee Count") {
    title_name <- "Number of Employees per Industry"
  } else if (name == "Medical Cost") {
    title_name <- "Medical Cost per Industry" 
  } else if (name == "Claim Count"){
    title_name <- "Claim Count per Industry"
  } else {
    title_name <- "Average Employee Wage per Industry"
  }
df_pivot %>%
    ggplot(aes(x = Year, y = Wage, group = Industry, color = Industry)) + 
    geom_line(linewidth = 1.5) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#2E2E38", color = NA),
      plot.background = element_rect(fill = "#2E2E38", color = NA),
      text = element_text(color = "#FFE600"),
      axis.text = element_text(color = "#FFE600"),
      axis.title = element_text(color = "#FFE600"),
      # Center the title and increase its font size
      plot.title = element_text(color = "#FFE600", size = 20, hjust = 0.5),
      legend.text = element_text(color = "#FFE600"),
      legend.title = element_text(color = "#FFE600"),
      panel.grid.major = element_line(color = "#FFE600", size = 0.5),
      panel.grid.minor = element_line(color = "#FFE600", size = 0.25),
      # Increase font size for x and y axis titles
      axis.title.x = element_text(color = "#FFE600", size = 16),
      axis.title.y = element_text(color = "#FFE600", size = 16)
    ) +
    xlab("Year") +
    ylab(name) +
    labs(title = title_name)
}

df_wages <- df %>% select(wages2023, wages2024, wages2025, wages2026)
df_employees <- df %>% select(employees_2023, employees_2024, employees_2025, employees2026)
df_medical <- df %>% select(medical2023, medical2024, medical2025, medical2026)
df_claims <- df %>% select(claims_2023, claims_2024, claims_2025, claims2026)

year <- c("2023","2024","2025","2026")
names(df_wages) <- year
names(df_employees) <- year
names(df_medical) <- year
names(df_claims) <- year

wage_per_employee <- data.frame(df_wages/df_employees)
names(wage_per_employee) <- year

plot_feature(df, df_wages, "Wage")
plot_feature(df, df_employees, "Employee Count")
plot_feature(df, df_medical, "Medical Cost")
plot_feature(df, df_claims, "Claim Count")
plot_feature(df, wage_per_employee, "Employee Wage")


#adding industry specific profit loading
loading <- df %>% group_by(Industry) %>% summarise(sum(medical2026))
loading[,2]/sum(loading[,2])
#fishing industry makes up more than 70% of medical expenses
#for layman only do 10% profit loading and for fishing do 15% loading
