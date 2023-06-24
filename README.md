# red_blue

The data are cleaned and compiled in stata using *code*/**data_clean.do**, creating **merged_data.dta** in the *output* folder

The data are pulled into R using *code*/**analysis.R**. Regression variables are defined and the regressions are run in this script using functions defined in the *code*/**functions.R** script. Two types of regressions are run: monthly regressions, which are stored in *charts*/**coefficients_monthly_regressions.rds** and regressions with year dummies and interaction terms, which are stored in *charts*/**coefficients_year_interactions.rds**

The coefficients are visualized in the R Shiny app using *charts*/**app_monthly.R**, *charts*/**app_year_interaction.R**, and *charts*/**app_by_category.R.**

The order of the code files is thus data_clean.do \> analysis.R \> app\_`type`.R where `type` = {monthly, year_interaction, by_category}

Don't hesitate to email me if you have any questions!
