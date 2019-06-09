# Code for "An Analysis of The Gender Wage Gap In Australia"

## Data notes

Data source is Taxation Statistics 2016-17, available for public access at data.gov.au. 

This analysis is intended as an update to my earlier analysis on 2013-2014 statistics on the gender gap, available at [GitHub](https://github.com/philip-khor/tidytuesday-pk/tree/master/2018/w4_2018). My prior analysis used taxable income, which I judged as too noisy. Differences between genders could be due to differences in deductibles between genders and other aspects of tax policy not related to the gender pay gap. 

The code for the analysis is available at https://github.com/the-ambitious-economist/wages-and-salaries.

### Data cleaning procedures

Bottom 5% of occupations by number of individuals have been removed. Differences in wages for job categories with few observations may be skewed by the small number of individuals. If records for one gender is removed by this procedure but not the other, the remaining record is removed. Occupations not otherwise classified have been removed. Records for occupation left blank are not considered.  
