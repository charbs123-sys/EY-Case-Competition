install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
library(tidyr)
set.seed(1234)
data <- read_excel("2025-Case-Comp-Data-CLEAN.xlsx", sheet = "Data")
sorted_data <- data %>% arrange(Industry)
summary_2025 <- data %>%
  group_by(Industry) %>%
  summarise(
    Total_Employees_2025 = sum(`Number of employees 2025`, na.rm = T),
    Total_Wages_2025 = sum(`2025 wages`, na.rm = T),
    Total_Claims_2025 = sum(`2025 number of claims`, na.rm = T),
    Total_Medical_Costs_2025 = sum(`2025 medical costs`, na.rm = T)
  )

data <- data %>%
  mutate(
    Growth_Employees = `Number of employees 2025` / `Number of employees 2023`,
    Growth_Wages = `2025 wages` / `2023 wages`,
    Growth_Claims = `2025 number of claims` / `2023 number of claims`,
    Growth_Medical_Costs = `2025 medical costs` / `2023 medical costs`
  )

growth_summary <- data %>%
  summarise(
    Avg_Growth_Employees = mean(Growth_Employees, na.rm = TRUE),
    Avg_Growth_Wages = mean(Growth_Wages, na.rm = TRUE),
    Avg_Growth_Claims = mean(Growth_Claims, na.rm = TRUE),
    Avg_Growth_Medical_Costs = mean(Growth_Medical_Costs, na.rm = TRUE)
  )
print(growth_summary)

lm_growth_employees <- lm(`Number of Employees 2025` ~ `Number of employees 2023` + 
                             `Number of employees 2024` + `Number of employees 2025`, data = data)
lm_growth_wages <- lm(`2025 wages` ~ `2023 wages` + 
                         `2024 wages` + `2025 wages`, data = data)
lm_growth_claims <- lm(`2025 number of claims` ~ `2023 number of claims` + 
                          `2024 number of claims` + `2025 number of claims`, data = data)
lm_growth_medical_costs <- lm(`2025 medical costs` ~ `2023 medical costs` + 
                                 `2024 medical costs` + `2025 medical costs`, data = data)

data <- data %>%
  group_by(Industry) %>%
  mutate(
    Predicted_Growth_Employees = predict(lm_growth_employees, newdata = cur_data()),
    Predicted_Growth_Wages = predict(lm_growth_wages, newdata = cur_data()),
    Predicted_Growth_Claims = predict(lm_growth_claims, newdata = cur_data()),
    Predicted_Growth_Medical_Costs = predict(lm_growth_medical_costs, newdata = cur_data())
  )

predicted_2026_industry <- data %>%
  group_by(Industry) %>%
  summarise(
    Predicted_Employees_2026 = sum(`Number of employees 2023` * Predicted_Growth_Employees, na.rm = TRUE),
    Predicted_Wages_2026 = sum(`2023 wages` * Predicted_Growth_Wages, na.rm = TRUE),
    Predicted_Claims_2026 = sum(`2023 number of claims` * Predicted_Growth_Claims, na.rm = TRUE),
    Predicted_Medical_Costs_2026 = sum(`2023 medical costs` * Predicted_Growth_Medical_Costs, na.rm = TRUE)
  )
predicted_2026_industry <- predicted_2026_industry %>%
  mutate(
    Claims_Per_Employee_2026 = Predicted_Claims_2026 / Predicted_Employees_2026
  )
print(predicted_2026_industry)

std_dev_2025 <- data %>%
  group_by(Industry) %>%
  summarise(
    SD_Medical_Costs_2025 = sd(`2025 medical costs`, na.rm = TRUE)
  )

# Parameters (replace with actual values for the industry)
n_claims <- ceiling(predicted_2026_industry$Predicted_Claims_2026[1]) # Example number of claims
mean_medical_cost <- ceiling(predicted_2026_industry$Predicted_Medical_Costs_2026[1]) # Example average medical cost per claim
sd_medical_cost <- std_dev_2025$SD_Medical_Costs_2025[1] # Example standard deviation for medical claims
v <- 0.95  # Discount factor

# Generate random samples for M_i (medical costs)
medical_claims <- rnorm(n_claims, mean = mean_medical_cost, sd = sd_medical_cost)

# Generate random times L_i (uniformly distributed between 0 and 1)
L_i <- runif(n_claims, 0, 1)

# Apply the discount factor
discounted_medical_claims <- medical_claims * v^L_i

# Calculate total present value of medical claims
total_medical_claims_pv <- sum(discounted_medical_claims)

# Assume P_medical is the premium for medical expenses
P_medical <- total_medical_claims_pv / v  # Using equivalence principle to solve for premium

P_medical









library(tidyr)

# Pivot to long format
long_data <- data %>%
  pivot_longer(
    cols = -c(`Company ID`, `Company name`, Industry),
    names_to = "Metric_Year",
    values_to = "Value"
  ) %>%
  separate(Metric_Year, into = c("Metric", "Year"), sep = " ") %>%
  mutate(Year = as.numeric(Year))

growth_data <- long_data %>%
  group_by(`Company ID`, Metric) %>%
  arrange(Year) %>%
  mutate(
    Growth = Value / lag(Value) - 1  # YoY growth rate
  ) %>%
  ungroup()

industry_growth <- growth_data %>%
  group_by(Industry, Metric, Year) %>%
  summarise(
    Avg_Growth = mean(Growth, na.rm = TRUE),
    .groups = "drop"
  )

# Example: Fit linear trend for Education industry's employee growth
education_employee <- industry_growth %>%
  filter(Industry == "Education" & Metric == "Number of employees")

model <- lm(Avg_Growth ~ Year, data = education_employee)

new_data <- data.frame(Year = 2026)
predicted_growth_2026 <- predict(model, new_data)

# Apply growth to 2025 values to get 2026 projections
industry_projections <- industry_growth %>%
  group_by(Industry, Metric) %>%
  mutate(
    Projected_2026 = last(Value) * (1 + predicted_growth_2026)
  )

# Aggregate employees to industry level
industry_employees <- long_data %>%
  filter(Metric == "Number of employees") %>%
  group_by(Industry, Year) %>%
  summarise(Total_Employees = sum(Value, na.rm = TRUE))

# Fit linear trend for Fishing and Agriculture
fish_agri_model <- lm(Total_Employees ~ Year, data = filter(industry_employees, Industry == "Fishing and Agriculture"))

# Predict 2026 employees
new_data <- data.frame(Year = 2026)
fish_agri_2026 <- predict(fish_agri_model, new_data)