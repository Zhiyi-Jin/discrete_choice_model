library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(stargazer)
library(modelr)
library(nnet)
library(survival)
library(kableExtra)

## 1.Move or Stay
#load data
Florida <- readRDS("Florida.rds")
States <- readRDS("States.rds")

#transfer Y 
Florida_1 <- Florida %>% 
  mutate(migrate1 = as.factor(migrate1),
         migrat = fct_recode(migrate1,
                             "0" = "1",
                             "1" = "2",
                             "1" = "3")) %>% 
  mutate(migrat = as.numeric(as.character(migrat))) %>% #0-stay; 1-move
  mutate(income = inctot/1000)                          #thousands of dollars

#X: income
N_0 <- prettyNum(nrow(Florida_1),big.mark = ",")
ggplot(Florida_1, aes(x = income)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Figure 1.  Histogram of Income", 
       x = "Income(thousands dollars)",
       y = "Count", 
       caption = paste0("Source: American Community Survey, N=",N_0))
       
#log-transforming of income
Florida_1$income_log <- log(Florida_1$income)

#replace NaNs and Inf with NA
Florida_1$income_log[which(is.infinite(Florida_1$income_log))] <- NA
Florida_1$income_log[which(is.na(Florida_1$income_log))] <- NA

#relationship between x and y
N <- prettyNum(nrow(filter(Florida_1,!is.na(income_log) & !is.na(migrat))),big.mark = ",")

ggplot(Florida_1, aes(x = income_log, y = migrat)) +
  geom_smooth(method = lm, formula = y ~ x) +
  geom_quasirandom(groupOnX = F, width = 0.2, alpha = 0.3) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  labs(title = "Figure 2. Residential Choice by Income",
       x = "Log(Income)",
       y = "Move or Stay",
       caption = paste0("Source: American Community Survey, N=",N))
 
#recode control variables
Florida_1 <- Florida_1 %>% 
  mutate(race = as.factor(race)) %>% 
  mutate(race = fct_recode(race, 
                           "White" = "1",
                           "Black" = "2",
                           "Asian" = "4",
                           "Asian" = "5",
                           "Asian" = "6",
                           "Other" = "7",
                           "Other" = "3",
                           "Other" = "8",
                           "Other" = "9"))
Florida_1 <- Florida_1 %>% 
  mutate(marst = as.factor(marst)) %>% 
  mutate(marriage = fct_recode(marst,
                                "Married" = "1",
                                "Married" = "2",
                                "Not married" = "3", # including seperation
                                "Not married" = "4",
                                "Not married" = "5",
                                "Not married" = "6"))

Florida_1 <- Florida_1 %>% 
  mutate(ownership = factor(ownershp, 
                           labels = c("Owned", "Rented")))

#fit models
m1 <- glm(migrat ~ income_log, family="binomial", Florida_1)
m2 <- glm(migrat ~ income_log + age, family = "binomial", Florida_1)
m3 <- glm(migrat ~ income_log + age + marriage, family = "binomial", Florida_1)
m4 <- glm(migrat ~ income_log + age + marriage + race, family = "binomial", Florida_1)
m5 <- glm(migrat ~ income_log + age + marriage + race + trantime, family = "binomial", Florida_1)
m6 <- glm(migrat ~ income_log + age + marriage + race + trantime + ownership, family = "binomial", Florida_1) 
 
stargazer(m1, m2, m3, m4, m5, m6, type="html",
          title = "<b>Table 1. Logistic Regression Models of Residential Mobility<b>",
          notes = "Data from American Community Survey",
          dep.var.caption = "", dep.var.labels.include = F,
          covariate.labels = c("log(income)",
                               "age",
                               "not married(ref=married)",
                               "race: black(ref=white)",
                               "race: other",
                               "race: asian",
                               "travel time for wotk",
                               "rented(ref=owned)"))
 
Florida_1 %>% 
  filter(!is.na(income_log) & !is.na(ownership)) %>% 
  ggplot(aes(x=income_log,..count..,fill=ownership)) +
  geom_density(position = position_fill(), size=0, alpha=0.6) +
  labs(title = "Figure 3. Income Distribution by House Ownership",
       x = "Log(income)",
       y = "Count",
       caption = paste0("Source: American Community Survey, N=",N))
  
#using model 1 & 5, making prediction
pred <- Florida_1 %>% 
  summarize(income_log = quantile(income_log, probs = seq(0, 1, 0.1), na.rm=T),
            age = mean(age),
            race = c("White"),
            trantime = mean(trantime),
            marriage = c("Married")) %>%
  expand_grid(ownership = c("Owned", "Rented")) %>% 
  gather_predictions(m1, m6, type = "response")

#make a plot
ggplot(pred, aes(x = income_log, y = pred, group = ownership, color = ownership)) +
  geom_line(aes(x = income_log, y = pred), size = 2) +
  facet_grid(cols = vars(model)) +
  labs(title = "Figure 4. Predicted Probability of Residential Move",
       x = "Log(income)",
       y = "Prob. of Residential Move",
       caption = "Source: American Community Survey") 
 
## 2.Move Distance
#Visualize: income_log & migrate1
Florida_1 <- Florida_1 %>% 
  mutate(migrate1 = factor(migrate1, 
                           labels = c("No move", "Move within state", "Moved between states")))

#boxplot
N_1 <- prettyNum(nrow(filter(Florida_1,!is.na(income_log) & !is.na(migrate1))),big.mark = ",")
Florida_1 %>% 
  filter(!is.na(income_log) & !is.na(migrate1)) %>%
  ggplot(aes(x = migrate1, y = income_log)) +
  geom_boxplot() +
  labs(title = "Figure 5. Income and Multinomial Migration Outcome",
       x = "Migration Choice",
       y = "Log(income)",
       caption = paste0("Source: American Community Survey, N=",N_1))

#1) job-related variable: employment status
Florida_1 <- Florida_1 %>% 
  filter(labforce != 0) %>% 
  mutate(labforce = factor(labforce, 
                           labels = c("No", "Yes"))) %>% 
  mutate(labforce = fct_relevel(labforce,"No"))

t1 <- with(Florida_1, table(migrate1, labforce))
t1 %>% kbl(caption = "Table 2. Migration Choices by Labour Force", 
           booktabs = T, align = "c") %>% 
  kable_classic() %>% 
  kable_styling(full_width = T, position = "center")

#2) education
Florida_1 <- Florida_1 %>% 
  filter(educ !=0) %>% 
  mutate(educ = as.factor(educ)) %>% 
  mutate(educ = fct_recode(educ,
                           "<=Secondary Education" = "1", 
                           "<=Secondary Education" = "2",
                           "<=Secondary Education" = "3",
                           "<=Secondary Education" = "4",
                           "<=Secondary Education" = "5",
                           "<=Secondary Education" = "6",
                           "Higher Education" = "7",
                           "Higher Education" = "8",
                           "Higher Education" = "10",
                           "Higher Education" = "11"))

t2 <- with(Florida_1, table(migrate1, educ))
t2 %>% kbl(caption = "Table 3. Migration Choices by Education", 
           booktabs = T, align = "c") %>% 
  kable_classic() %>% 
  kable_styling(full_width = T, position = "center")

#3) ownership
t3 <- with(Florida_1, table(migrate1, ownership))
t3 %>% kbl(caption = "Table 4. Migration Choices by House Ownership", 
           booktabs = T, align = "c") %>% 
  kable_classic() %>% 
  kable_styling(full_width = T, position = "center")

#fit models
m.a <- multinom(relevel(migrate1, ref = "No move") ~ income_log, data = Florida_1)
m.b <- multinom(relevel(migrate1, ref = "No move") ~ income_log + labforce, data = Florida_1)
m.c <- multinom(relevel(migrate1, ref = "No move") ~ income_log + labforce + educ, data = Florida_1)
m.d <- multinom(relevel(migrate1, ref = "No move") ~ income_log + labforce + educ + ownership, data = Florida_1)

N_2 <- prettyNum(nrow(filter(Florida_1,!is.na(income_log) & !is.na(migrate1) & educ !=0 & labforce != 0)),big.mark = ",")

stargazer(m.a, m.b, m.c, m.d,
          header = F, type = "html",
          title = "<b>Table 5. Multinomial Logistic Regression Models of Residential Mobility<b>",
          dep.var.caption = "", dep.var.labels.include = F,
          covariate.labels = c("log(income)",
                               "labforceYes(ref:No)",
                               "higher education(ref:<=secondary education)",
                               "rented(ref:owned)"),
          notes = paste0("Source: American Community Survey, N=",N_2))

#make a data frame for prediction
pred2 <- Florida_1 %>% 
  summarize(income_log = quantile(income_log, probs = seq(0, 1, 0.1), na.rm=T),
            labforce = "Yes",
            educ = "Higher Education") %>%
  expand_grid(ownership = c("Rented"))

#make prediction and combine with the data frame
pred2.1 <- predict(m.a, newdata = pred2, "probs") %>% 
  as_tibble() %>% 
  bind_cols(pred2[,1])

pred2.2 <- predict(m.b, newdata = pred2, "probs") %>% 
  as_tibble() %>% 
  bind_cols(pred2)

#transfer into long format
pred2.1_long <- pred2.1 %>% 
  pivot_longer(cols = `No move`:`Moved between states`,
               names_to = "mobility",
               values_to = "prediction")

pred2.2_long <- pred2.2 %>% 
  pivot_longer(cols = `No move`:`Moved between states`,
               names_to = "mobility",
               values_to = "prediction")

#make a plot
ggplot(pred2.1_long, aes(income_log, prediction, color = mobility)) +
  geom_line(size = 1.5) +
  labs(title = "Figure 6. Predicted Probability by Simple Model",
       x = "Log(income)",
       y = "Predicted proability",
       caption = "Source: American Community Survey")

ggplot(pred2.2_long, aes(income_log, prediction, color = mobility)) +
  geom_line(size = 1.5) +
  labs(title = "Figure 7. Predicted Probability by Complex Model",
       x = "Log(income)",
       y = "Predicted proability",
       caption = "Source: American Community Survey")

## 3. Move Destination
#select those who are moved between states and select columns
Florida_2 <- Florida_1 %>%
  filter(migrate1 == "Moved between states") %>% 
  select(serial, statefip, income_log,)
  

#data preparation of -states-
States_1 <- States %>% 
  mutate(UncomMonth = FreezingMonths + HotMonths) %>% 
  mutate(Hhincom_log = log(HhincPtile50)) %>% 
  mutate(MinorityShare = 1-RaceHispShareWhite) %>% 
  select(statefip, MinorityShare, Hhincom_log, Sunlight, UncomMonth)
  
#merge
Florida_new <- full_join(Florida_2, States_1, by = character())

Florida_new$choice <- ifelse(Florida_new$statefip.x == Florida_new$statefip.y, TRUE, FALSE)
Florida_new_2 <- select(Florida_new, -statefip.x)
Florida_new_2 <- Florida_new_2[!is.na(Florida_new_2$income_log),]
Florida_new_2 <- Florida_new_2[!is.na(Florida_new_2$Hhincom_log),]
Florida_new_2 <- Florida_new_2[!is.na(Florida_new_2$Sunlight),]
Florida_new_2 <- Florida_new_2[!is.na(Florida_new_2$UncomMonth),]

#fit models
n1 <- clogit(choice ~ MinorityShare + Hhincom_log + Sunlight + UncomMonth + strata(serial), data = Florida_new_2)
n2 <- clogit(choice ~  MinorityShare + Hhincom_log + Sunlight + UncomMonth +
               MinorityShare:income_log + Hhincom_log:income_log + 
               Sunlight:income_log + UncomMonth:income_log + strata(serial), data = Florida_new_2)

stargazer(n1, n2, header = F, type = "html",
          title = "<b>Table 6. Multinomial logistic regression models of states preferences<b>",
          dep.var.caption = "", dep.var.labels.include = F,
          covariate.labels = c("Minority Share",
                               "Log(Household Income)",
                               "Sunlight",
                               "Uncomfortable Months",
                               "Minority Share:Log(Income)",
                               "Log(Household Income):Log(Income)",
                               "Sunlight:Log(Income)",
                               "Uncomfortable Months:Log(Income)"),
          notes = "Data from American Community Survey")

 
