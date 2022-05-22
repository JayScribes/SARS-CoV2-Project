# COVID-19 Project
## Figure 4
### Mortality % as a function of Diabetes Prevalence

ggplot (Diabetes_Mortality_grouping, aes (x=diabetes_prevalence,y=percent_mortality))+
  geom_point()+geom_smooth(method="lm")

## Removing Visually-Obvious Outliers After Graphing

Diabetes_Mortality$outlier = Diabetes_Mortality$new_confirmed < 0   
Diabetes_Mortality = filter(Diabetes_Mortality, outlier != TRUE)
Diabetes_Mortality<- subset(Diabetes_Mortality, , -c(outlier))

Diabetes_Mortality$outlier = Diabetes_Mortality$new_deceased < 0   
Diabetes_Mortality = filter(Diabetes_Mortality, outlier != TRUE)
Diabetes_Mortality<- subset(Diabetes_Mortality, , -c(outlier))

Diabetes_Mortality$outlier = Diabetes_Mortality$new_deceased > 6000   
Diabetes_Mortality = filter(Diabetes_Mortality, outlier != TRUE)
Diabetes_Mortality<- subset(Diabetes_Mortality, , -c(outlier))

## Creating new columns to see if diabetes prevalence has a relationship with  mortality rates

Diabetes_Mortality$new_tested <- (Diabetes_Mortality$new_deceased +1) ## to get rid of odd zeroes
Diabetes_Mortality$new_confirmed <- (Diabetes_Mortality$new_confirmed +1) ## adding another '1' constant to data 
Diabetes_Mortality$percent_mortality <- (Diabetes_Mortality$new_deceased/Diabetes_Mortality$new_confirmed)*100

## Cleaning calculated values

Diabetes_Mortality$outlier = Diabetes_Mortality$percent_mortality >100   
Diabetes_Mortality = filter(Diabetes_Mortality, outlier != TRUE)
Diabetes_Mortality<- subset(Diabetes_Mortality, , -c(outlier))

percent_mortality_win <-  Winsorize(Diabetes_Mortality$percent_mortality, minval = NULL, maxval = NULL, probs = c(0.015, 0.985), na.rm = FALSE, type =1)
Diabetes_Mortality$percent_mortality_win <- percent_mortality_win ## adding winsorized information into the data frame

Diabetes_Mortality_grouping <- Diabetes_Mortality %>%
  group_by(diabetes_prevalence) %>% 
  dplyr::summarize(percent_mortality = mean(percent_mortality_win)) %>% 
  as.data.frame()

## Linear Regression Analysis

Diabetes.Mortality.Regression <- lm(percent_mortality ~ diabetes_prevalence, data= Diabetes_Mortality_grouping)
summary(Diabetes.Mortality.Regression)

Fig4_Mortality <- ggplot (Diabetes_Mortality_grouping, aes (x=diabetes_prevalence,y=percent_mortality,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Diabetes Prevalence (%)",y="Daily Mortality (%)",caption="*Data acquired from 202 countries across America, Europe, Africa, and Asia. n= 83246 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=6))