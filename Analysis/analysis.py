import pandas as pd
import matplotlib.pyplot as plt

data = pd.read_excel("data.xlsx")
medical = data.groupby("Industry").sum(numeric_only = True)

#medical_year = pd.concat([medical["Number of employees 2023"], medical["Number of employees 2024"], medical["Number of employees 2025"]], axis = 1)

#shows theres not really any percentage change in number of claims of number of employees increase
#this means that for each industry we can assume the percentage number of claims will be uniform to previous years
medical_year = pd.concat([medical["2023 number of claims"]/medical["Number of employees 2023"], medical["2024 number of claims"]/medical["Number of employees 2024"], 
                        medical["2025 number of claims"]/medical["Number of employees 2025"]], axis = 1)

#this shows that the actual medical costs have substantially increased (percentage of number of claims has been uniform so we must consider an external factor)
#this is only for fishing, other medical costs are uniform as well
medical_year = pd.concat([medical["2023 medical costs"], medical["2024 medical costs"], medical["2025 medical costs"]], axis = 1)


#this is for how wages have change over time, we want to project this to 2024, the obvious trend is upwards and this can be used to calculate inflation
medical_year = pd.concat([medical["2023 wages"], medical["2024 wages"], medical["2025 wages"]], axis = 1)

#need to project medical expenses, wages, number of medical claims
#Then find PV to find total costs paid by the insurance company

#can downscale to find the amount paid by each company in insurance to employees 

medical_year = medical_year.T
#medical_year = medical_year.drop(columns = medical_year.columns[2])
# Plot the 2023 medical costs
plt.figure(figsize=(10, 6))

for industry in medical_year.columns:
    plt.plot(medical_year.index, medical_year[industry], marker = "o", linestyle = "-", label = industry)

# Customize the plot
plt.xlabel("Year")
plt.ylabel("Medical Costs ($)")
plt.title("Medical Costs by Industry Over Time")
plt.xticks(medical_year.index)  # Ensure years appear as x-axis labels
plt.legend(title="Industry", bbox_to_anchor=(1.05, 1), loc="upper left")  # Place legend outside
plt.grid(True, linestyle="--", alpha=0.6)

# Show the plot
plt.tight_layout()
plt.show()

#looking at the 