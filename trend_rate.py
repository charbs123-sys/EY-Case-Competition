import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import math

data = pd.read_excel("data.xlsx")
#medical = data.groupby("Industry")

#print(np.sqrt(data["Number of employees 2025"]/data["Number of employees 2024"]) - 1)
#print(np.sqrt(data["2025 wages"]/data["2024 wages"]) - 1)
#print(np.sqrt(data["2025 number of claims"]/data["2024 number of claims"]) - 1)
#print(np.sqrt(data["2025 medical costs"]/data["2024 medical costs"]) - 1)

trend_wages = np.sqrt(data["2025 wages"]/data["2024 wages"]) - 1
project_wages = data["2025 wages"] * (1 + trend_wages)


trend_medical_costs = np.sqrt(data["2025 medical costs"]/data["2024 medical costs"]) - 1
#what we expect each company will pay in medical expenses per year
project_medical = data["2025 medical costs"] * (1 + trend_medical_costs)

trend_employees = np.sqrt(data["Number of employees 2025"]/data["Number of employees 2024"]) - 1
#number of employees for each company
n = data["Number of employees 2025"] * (1 + trend_employees)

trend_number_claims = np.sqrt(data["2025 number of claims"]/data["2024 number of claims"]) - 1
p = (data["2025 number of claims"] * (1 + trend_number_claims))/n

C = n * p
C *= 0.7

#use australian one-year gov bond yield + 1.5% for taking on more risk
i = 0.03817 + 0.015
v = 1/(1 + i)

L_i = []

for i in range(len(C)):
    L_i.append(np.random.uniform(0, 1, math.ceil(C[i])))

#this is the total medical expenses expected to be paid for each company
P_medical = []
for company_index in range(len(L_i)):
    arr = np.array(L_i[company_index])
    discounts = v ** arr
    medical = project_medical[company_index]
    P_medical.append(sum(medical/len(L_i[company_index]) * discounts) / v)




#consider the average age in the work force as x = 40
#assume injury means 4 times more likely to incur mortality (studies show 3x but 4x for island nation context)
#use UDD approximation
#for now ignore the effect of L_i on the annuity
factor = 51/104 * (1 - (v ** 4) * (1 - 0.00937) * (1-0.001014) * (1-0.001104) * (1-0.001208) * (1-0.001327))
term_annuity = 1 + v * (1 - 0.000937) + (v ** 2) * (1 - 0.000937) * (1 - 0.001014) + (v ** 3) * (1 - 0.000937) * (1 - 0.001014) * (1-0.001104)
annuity_approx = term_annuity - factor

#need to fix projections based on output below, the salary jump it too high
P_PI = []
for company_index in range(len(L_i)):
    P_PI.append(0.75 * annuity_approx * project_wages[company_index]/n[company_index] * len(L_i[company_index]) * 1/v)



P_operational = 2400000 * v * data["2025 medical costs"]/sum(data["2025 medical costs"])

#so the total premium paid for each company is
P_total = P_operational + np.array(P_PI) + np.array(P_medical)

new_df = data["Industry"]
P_industry = pd.concat([new_df, P_total], axis = 1)
grouped1 = P_industry.groupby("Industry").sum("2025 medical costs")


P_temp = pd.DataFrame(P_medical)
P_temp = pd.concat([new_df, P_temp], axis = 1)
group_medical = P_temp.groupby("Industry").sum("0")

P_temp = pd.DataFrame(P_PI)
P_temp = pd.concat([new_df, P_temp], axis = 1)
group_PI = P_temp.groupby("Industry").sum("0")

P_temp = pd.concat([new_df, P_operational], axis = 1)
group_operational = P_temp.groupby("Industry").sum("2025 medical costs")

#account for external factors

#1 - lower premium initially by about 10%, corporate taxes will reduce overtime and we can increase this again
P_total *= (1 - 0.1)

#2- investment risk free rate
P_total *= (1 - 0.0442)

""" #3 - nature of work
P_total *= (1 + 0.01) """

#4-Ageing population
P_total *= (1+0.03)


P_industry = pd.concat([new_df, P_total], axis = 1)
grouped = P_industry.groupby("Industry").sum("2025 medical costs")

#standard to have ~ 10-20% profit and contingency loading -> stay at 10% -> decrease for other companies and increase for fishing
grouped.iloc[2,0] *= 1.15
grouped.iloc[[1,3,4,5],0] *= 1.1
print(grouped1)
print(grouped)
print((grouped1 - grouped)/grouped1)