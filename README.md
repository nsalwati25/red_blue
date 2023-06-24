# red_blue

The raw data are stored in the *data-raw* folder by source.

The data are cleaned and compiled in Stata using *code*/**data_clean.do**. Each section of this file pulls from *data-raw* (and contains a link to the original source). The intermediate datasets are stored in the *data* folder. These files are merged to create **merged_data.dta** in the *output* folder

The data are pulled into R using *code*/**analysis.R**. Regression variables are defined and the regressions are run in this script using functions defined in the *code*/**functions.R** script. Two types of regressions are run: monthly regressions, which are stored in *charts*/**coefficients_monthly_regressions.rds** and regressions with year dummies and interaction terms, which are stored in *charts*/**coefficients_year_interactions.rds**

The coefficients are visualized in the R Shiny app using *charts*/**app_monthly.R**, *charts*/**app_year_interaction.R**, and *charts*/**app_by_category.R.**

-   **app_monthly.R** plots monthly coefficients for each variable.

-   **app_year_interaction.R** plots pooled coefficients with year dummies and year interaction terms for each variable.

-   **app_by_category** plots the same information as **app_year_interaction.R** but are visualized by source type.

The order of the code files is thus data_clean.do \> analysis.R \> app\_`type`.R where `type` = {monthly, year_interaction, by_category}

Don't hesitate to email me if you have any questions!
