**Red_Blue Project**

## **About the Project**

This project visualizes the correlation between various state-level economic outcomes during the COVID-19 pandemic and their voting patterns in the 2020 U.S. Presidential Election. It examines several economic indicators - the changes in the employment population ratio, credit card spending, and personal consumption expenditures, among others - against the proportion of votes received by Joe Biden in each state in 2020.

The analysis is carried out via two types of regression models: monthly regressions and regressions with year dummies and interaction terms. The final output visualizes these correlations by depicting the coefficients on the Biden vote share for each economic variable over time. These visualizations are rendered using R Shiny.

## **Project Structure**

The raw data, obtained from various sources, are stored in the **`data-raw`** folder.

### **Data Cleaning and Compilation**

The data cleaning process is conducted in Stata using **`code/data_clean.do`**. Each section of this script pulls from the **`data-raw`** folder, with references to original sources included in the comments. The cleaned, intermediate datasets are stored in the **`data`** folder, while the final merged dataset, **`merged_data.dta`**, is stored in the **`output`** folder.

### **Data Analysis**

Data analysis is performed in R with the script **`code/analysis.R`**, using functions defined in **`code/functions.R`**. The **`code/analysis.R`** file reads in **`merged_data.dta`** defines regression variables, runs regressions, and stores the output in data sets in the **`charts`** folder. The output of these regressions, namely the change (or percent change) in a CPS/BEA/Opportunity Insights/etc. variable, is stored in two formats:

1.  **`charts/coefficients_monthly_regressions.rds`**: Contains the results of monthly regressions

2.  **`charts/coefficients_year_interactions.rds`**: Contains the results of regressions with year dummies and interaction terms

### **Data Visualization**

The visual representation of the data is done through an R Shiny app using **`charts/app_monthly.R`**, **`charts/app_year_interaction.R`**, and **`charts/app_by_category.R`**.

-   **`app_monthly.R`** : Plots monthly coefficients on the Biden share for each outcome variable

-   **`app_year_interaction.R`** : Plots coefficients of the pooled sample with year dummies and year interaction terms with the Biden share for each outcome variable

-   **`app_by_category.R`** : Plots the same information as **`app_year_interaction.R`**, but the outcome variables are grouped by source

## **Workflow**

The workflow of the code can be summarized as follows: **`data_clean.do`** -\> **`analysis.R`** (utilizing **`functions.R`**) -\> **`app_type.R`** where **`type`** can be one of {monthly, year_interaction, by_category}.

## **Contact**

Don't hesitate to email me if you have any questions!
