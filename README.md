# 📊 Econometric Analysis of Technological Innovation Determinants in China

This project applies econometric models to analyze the relationship between technological innovation and economic performance in China. It includes statistical modeling, hypothesis testing, and forecasting techniques implemented in **R**.

It was developed as part of the **Econometrics** course at the **Faculty of Economic Cybernetics, Statistics and Informatics, Bucharest University of Economic Studies (ASE)**. 

## 📌 Project Overview
This project investigates the factors influencing **technological innovation** in China using econometric techniques. It examines the relationship between the number of **patents** and key economic indicators such as **GDP per capita, industry share, education level, and policy strength**. The study utilizes **panel data regression models** to evaluate how these determinants shape innovation trends.

## 📂 Description of the Data Used
The dataset consists of **panel data covering 141 cities in China over a period of 5 years (2019-2023)**. It captures both **temporal variations** and **regional differences**, making it suitable for econometric analysis. The main data sources include:

- **PatYee Database** – Number of patents licensed per region.
- **China City Statistical Yearbook** – Economic and demographic indicators.
- **Economic Performance Statistics (EPS) Platform** – GDP per capita and industry-specific data.
- **Legal Star Database** – Policy intensity related to innovation.

### **Key Variables**
- **Dependent Variable:**
  - **lnpat**: Natural logarithm of the number of patents filed per region.

- **Independent Variables:**
  - **lngdp**: Natural logarithm of Gross Domestic Product per capita.
  - **ind**: Industry share as a percentage of GDP.
  - **hum**: Higher education enrollment rate.
  - **inn**: Number of innovations in smart construction technologies.
  - **qua**: Strength of policy incentives.
  - **mea**: Number of pilot projects in smart construction.

## ⚙ Key Features
✅ **Single and Multiple Regression Models**  
✅ **Panel Data Analysis (Fixed Effects, Random Effects, Pooled OLS)**  
✅ **Hypothesis Testing & Model Validation**  
✅ **Forecasting Innovation Trends**  
✅ **Data Visualization & Correlation Analysis**  

## 📂 Project Structure
```
📁 econometric-analysis
│── 📜 aplicatia_1.R                  # Simple regression model
│── 📜 aplicatia_1_corelograma.R      # Correlation analysis
│── 📜 aplicatia_1_descriere_variabile.R  # Descriptive statistics
│── 📜 aplicatia_1_prognoza.R         # Forecasting model
│── 📜 aplicatia_2_multipla_inc.R     # Multiple regression model
│── 📜 aplicatia_2_multipla_ipoteze.R # Hypothesis testing
│── 📜 aplicatia_2_prognoza.R         # Forecasting for multiple regression
│── 📜 aplicatia_3_cod.R              # Panel data models (FE, RE, Pooled OLS)
```

## 📸 Screenshots
<p align="center">
  <img src="https://github.com/user-attachments/assets/6a5f7b25-6366-4339-aebe-020101b0b6b5" width="60%">
</p>
<p align="center">
  <img src="https://github.com/user-attachments/assets/4b2ec064-3ac4-4abe-94e3-173c5b007f7a" width="60%">
</p>
<p align="center">
  <img src="https://github.com/user-attachments/assets/4efd1187-076b-4912-b4d8-f58ee2e28010" width="60%">
</p>
