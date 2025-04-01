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

lm_employees <- lm(`Number of employees 2026` ~ `Number of employees 2023` + 
                            `Number of employees 2024` + `Number of employees 2025`, data = data)
lm_wages <- lm(`2026 wages` ~ `2023 wages` + 
                        `2024 wages` + `2025 wages`, data = data)
lm_claims <- lm(`2026 number of claims` ~ `2023 number of claims` + 
                         `2024 number of claims` + `2025 number of claims`, data = data)
lm_medical_costs <- lm(`2026 medical costs` ~ `2023 medical costs` + 
                                `2024 medical costs` + `2025 medical costs`, data = data)

data <- data %>%
  group_by(Industry) %>%
  mutate(
    Predicted_Employees_2026 = predict(lm_employees, newdata = data),
    Predicted_Wages_2026 = predict(lm_wages, newdata = data),
    Predicted_Claims_2026 = predict(lm_claims, newdata = data),
    Predicted_Medical_Costs_2026 = predict(lm_medical_costs, newdata = data)
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