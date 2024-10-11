## Inspiration

Computational implementation of semester project in Linear Models ( 654. Γραμμικά Μοντέλα) in National and Kapodistrian University of Athens (2023-2024).

## Assignment

  We were given a subset of the data of [World Bank's WDI database](https://databank.worldbank.org/reports.aspx?source/world-development-indicators) (data_tidy.csv) consisting of 27 variables for 120 countries, in the year 2018.

  The assignment comprised of 3 parts.

### Section 1: Descriptive Statistics

1.    Create 2 new categorical Variables (FertCat, AdolFertRate), based on different values of the variables "AdolFertRate" and "Inflation" respectively.
2. Study and comment on the descriptive statistical measures for the Variables CorControl, PolStab, VoiceAcc, FertCat, InflCat.
3. Use package "psych" and the provided function "describeBy" in order to calculate basic descriptors (sample mean and standard deviation) for the variables CorControl, PolStab, VoiceAcc by FertCat.

### Section 2: Descriptive Statistics

Choose 1 of CorControl, PolStab, VoiceAcc and develop a multiple regression model estimating the variable through the other indicators. The other 2 should not be used in the models.

1. Create the full multiple linear model.
2. Follow a Backward Stepwise procedure with AIC as a criterion to arrive at the "optimal" model.
3. Calculate the $R^2$ and $R_{adj}^2$ for the resulting model and comment on them.
4. Interpret coefficients of the independent variables of the model
5. Select two of the independent variables in the resulting model and perform the statistical significance test at the 5% level: $H_0:b_i=0 vs H_1:b_i\neq 0$
6. Calculate the 99% confidence intervals for the coefficients of the resulting model.
7. Are they satisfied? If not, try a transformation on the variables.
8. The data_new.csv file includes 8 more countries for which the estimates of Corruption Control (CorControl), Political Stability (PolStab), and Voice and Accountability (VoiceAcc) are not available. Based on your model,
 i. Give a point forecast for the dependent variable for each of the 8 countries.
 ii. Construct mean and individual prediction intervals for the dependent variable of each of the 8 countries.
                
### Section 3: ANOVA

Perform a variance analysis with the independent variables "FertCat" and "InflCat" and the variable selected in Section 2 as the dependent variable.

1. State the assumptions of the ANOVA model and fit such a model that studies the effect of the dependent variables on the response variable. Comment on the model.
2. Test the interaction and main effects of the factors on the response variable at the 5% level of statistical significance.
3. Test the assumptions of the model by checking the residuals.
4. Investigate whether there is an effect of each level of factors on the response variable.

## My Work

I selected CorControl as the dependent variable of the multiple linear model, excluding "VoiceAcc", "PolStab", "Country".

The optimal multiple linear model produced by application of Backward Stepwise procedure with AIC as a criterion was: 

| **Independent Variable** | **Estimation** | **Standard Deviation** | **t value** | **Pr(T>t**   |
| ------------------------ | -------------- | ---------------------- | ----------- | ------------ |
| (Intercept)              | -8.472e-01     | 6.708e-01              | -1.263      | 0.20933      |
| ElectrAccess             | -1.114e-02     | 3.619e-03              | -3.079      | 0.00263 **   |
| AgriLand                 | -3.633e-03     | 2.227e-03              | -1.631      | 0.10569      |
| CO2                      | -2.564e-02     | 1.354e-02              | -1.893      | 0.06097 .    |
| Internet                 | 1.305e-02      | 4.690e-03              | 2.783       | 0.00634 **   |
| PopGrowth                | -1.236e-01     | 6.079e-02              | -2.034      | 0.04440 *    |
| AdolFertRate             | -1.346e-02     | 4.175e-03              | -3.224      | 0.00167 **   |
| GDPdollars               | 2.933e-05      | 3.586e-06              | 8.179       | 5.74e-13 *** |
| PopPerc14                | 2.624e-02      | 1.349e-02              | 1.945       | 0.05440      |
| WomBusiness              | 6.641e-03      | 3.267e-03              | 2.033       | 0.04449      |
| FertCat                  | 2.630e-01      | 1.896e-01              | 1.387       | 0.16836      |

The analysis.R contains all the necessary code to complete the assignment. The report.pdf is a full report describing solutions to every section of the assignment
