library(readxl)
library(dplyr)
library(tidyr)
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

lm_growth_employees <- glm(Growth_Employees ~ `Number of employees 2023`, data = data)
lm_growth_wages <- glm(Growth_Wages ~ `2023 wages`, data = data)
lm_growth_claims <- glm(Growth_Claims ~ `2023 number of claims`, data = data)
lm_growth_medical_costs <- glm(Growth_Medical_Costs ~ `2023 medical costs`, data = data)

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

mean_expected_claims <- mean(predicted_2026_industry$Predicted_Claims_2026)
print(paste("Mean Expected Claims across Industries: ", mean_expected_claims))

# Set seed for reproducibility
set.seed(123)

# Parameters (replace with actual values for the industry)
n_claims <- ceiling(predicted_2026_industry$Predicted_Claims_2026[1])  # Example number of claims
mean_medical_cost <- ceiling(predicted_2026_industry$Predicted_Medical_Costs_2026[1])  # Example average medical cost per claim
sd_medical_cost <- sd(predicted_2026_industry$Predicted_Medical_Costs_2026[1]) # Example standard deviation for medical claims
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
