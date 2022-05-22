# COVID-19 Project
## Figure 3
### Mortality % as a function of Smoking Prevalence

ggplot (Smoking_Mortality_grouping, aes (x=smoking_prevalence,y=percent_mortality))+
  geom_point()+geom_smooth(method="lm")

## Removing Visually-Obvious Outliers After Graphing

Smoke_Mortality$outlier = Smoke_Mortality$new_confirmed < 0    
Smoke_Mortality = filter(Smoke_Mortality, outlier != TRUE)
Smoke_Mortality <- subset(Smoke_Mortality, , -c(outlier))

Smoke_Mortality$outlier = Smoke_Mortality$new_confirmed > 800000
Smoke_Mortality = filter(Smoke_Mortality, outlier != TRUE)
Smoke_Mortality <- subset(Smoke_Mortality, , -c(outlier))

Smoke_Mortality$outlier = Smoke_Mortality$new_deceased > 6000
Smoke_Mortality = filter(Smoke_Mortality, outlier != TRUE)
Smoke_Mortality <- subset(Smoke_Mortality, , -c(outlier))

Smoke_Mortality$outlier = Smoke_Mortality$new_deceased < 0
Smoke_Mortality = filter(Smoke_Mortality, outlier != TRUE)
Smoke_Mortality <- subset(Smoke_Mortality, , -c(outlier))

## Creating new columns to see if smoking prevalence has a relationship with infection rates

Smoke_Mortality$new_deceased <- (Smoke_Mortality$new_deceased +1) ## to get rid of odd zeroes
Smoke_Mortality$new_confirmed <- (Smoke_Mortality$new_confirmed +1) ## adding another '1' constant to data 

Smoke_Mortality$outlier = Smoke_Mortality$percent_mortality > 100   ## A few values ended up above 100%
Smoke_Mortality = filter(Smoke_Mortality, outlier != TRUE)
Smoke_Mortality <- subset(Smoke_Mortality, , -c(outlier))

##Winsorizing data as many non-sense data points still exist

percent_mortality_win <-  Winsorize(Smoke_Mortality$percent_mortality, minval = NULL, maxval = NULL, probs = c(0.01, 0.99), na.rm = FALSE, type =1)
Smoke_Mortality$percent_mortality_win <- percent_mortality_win ## adding winsorized information into the data frame

Smoke_Mortality$percent_mortality <- (Smoke_Mortality$new_deceased/Smoke_Mortality$new_confirmed)*100

# Extracting month/year data

Smoke_Mortality_month <- Smoke_Mortality
Smoke_Mortality_month$year_month <- floor_date(Smoke_Mortality_month$date, "month")

##Grouping By smoking prevalence % 

Smoking_Mortality_grouping <- Smoke_Mortality_month %>% ## Aggregating cleaned data
  group_by(smoking_prevalence) %>% 
  dplyr::summarize(percent_mortality = mean(percent_mortality_win)) %>% 
  as.data.frame()

## Linear Regression Analysis

[Smoking.Mortality.Regression <- lm(percent_mortality ~ smoking_prevalence, data= Smoking_Mortality_grouping)
summary(Smoking.Mortality.Regression) ## Reveals significant (p=0.0175, f= 5.909, r2=0.03884,n=120) negative relationship between smoking prevalence and percent daily mortality

Fig3_Mortality <- ggplot (Smoking_Mortality_grouping, aes (x=smoking_prevalence,y=percent_mortality,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Smoking Prevalence (%)",y="Daily Mortality (%)",caption="*Data acquired from 145 countries across America, Europe, Africa, and Asia. n= 69699 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=5))







