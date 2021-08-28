### Application of Discrete Choice Model in the American Community Survey (ACS)

**Data:**           
The ACS is a large-scale survey conducted by the United States Census every year that covers a 1-in-100 sample of the American population. Respondents in the survey provide information about a number of economic and demographic topics. More information about the ACS can be found at https://www.census.gov/programs-surveys/acs

I use an individual-level version of the ACS sample that is provided through the USA IPUMS program housed at the Minnesota Population Center in this project. IPUMS now o ers microdata related to other US-based surveys, as well as census microdata from over 100 other countries. More information about IPUMS can be found at the https://www.ipums.org/

**Purpose:**           
The purpose of this project is to study migration, specifically examine the relationship between income and migration. Florida is the targeted states in this project.

**Structure:**            
The structure of the report is somewhat looser, but still in the spirit of the logistic regression method.Three analysis parts are included in the report.  

1. Move or Stay:
- Examine how income variable is related to the binary decision of whether to stay in the same housing unit, or to move to a new housing unit, regardless of the migration distance;
- Discuss mediators or correlated variables that would bias the effects.
- Estimate a model that includes the control variables and produce a figure showing the predicted probabilities from the simple model.

2. Move Distance:   
- Examine how income variable relates to the decision to move across various geographic boundaries (e.g., stay, move within a state, move between states, etc.).
-  Estimate an multinomial logistic regression model that shows the first order relationship between independent variables and multinomial outcome.

3. Move Destination:
- Examine how income relates to the decision of which state to move to. This applies only to those who make moves between states. 
- An additional data file, ``States.rds`` that contains some key attributes is used for this problem.



