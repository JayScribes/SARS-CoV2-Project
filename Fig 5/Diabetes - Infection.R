# COVID-19 Project
## Figure 4
### Infection % as a function of Diabetes Prevalence

ggplot (Diabetes_Infection_grouping, aes (x=diabetes_prevalence,y=percent_infection))+
  geom_point()+geom_smooth(method="lm")

## Removing Visually-Obvious Outliers After Graphing

Diabetes_Infection$outlier = Diabetes_Infection$new_confirmed < 0   
Diabetes_Infection = filter(Diabetes_Infection, outlier != TRUE)
Diabetes_Infection<- subset(Diabetes_Infection, , -c(outlier))

Diabetes_Infection$outlier = Diabetes_Infection$new_tested < 0   
Diabetes_Infection = filter(Diabetes_Infection, outlier != TRUE)
Diabetes_Infection<- subset(Diabetes_Infection, , -c(outlier))

Diabetes_Infection$outlier = Diabetes_Infection$new_tested > 2000000   
Diabetes_Infection = filter(Diabetes_Infection, outlier != TRUE)
Diabetes_Infection<- subset(Diabetes_Infection, , -c(outlier))

## Creating new columns to see if diabetes prevalence has a relationship with infection rates

Diabetes_Infection$new_tested <- (Diabetes_Infection$new_tested +1) ## to get rid of odd zeroes
Diabetes_Infection$new_confirmed <- (Diabetes_Infection$new_confirmed +1) ## adding another '1' constant to data 
Diabetes_Infection$percent_infection <- (Diabetes_Infection$new_confirmed/Diabetes_Infection$new_tested)*100

## Cleaning calculated values

Diabetes_Infection$outlier = Diabetes_Infection$percent_infection > 100   
Diabetes_Infection = filter(Diabetes_Infection, outlier != TRUE)
Diabetes_Infection<- subset(Diabetes_Infection, , -c(outlier))

percent_infection_win <-  Winsorize(Diabetes_Infection$percent_infection, minval = NULL, maxval = NULL, probs = c(0.015, 0.985), na.rm = FALSE, type =1)
Diabetes_Infection$percent_infection_win <- percent_infection_win ## adding winsorized information into the data frame

##Grouping By diabetes prevalence % 

Diabetes_Infection_grouping <- Diabetes_Infection %>%
  group_by(diabetes_prevalence) %>% 
  dplyr::summarize(percent_infection = mean(percent_infection_win)) %>% 
  as.data.frame()

## Linear Regression Analysis

Diabetes.Infection.Regression <- lm(percent_infection ~ diabetes_prevalence, data= Diabetes_Infection_grouping)
summary(Diabetes.Infection.Regression)

Fig4_Infection <- ggplot (Diabetes_Infection_grouping, aes (x=diabetes_prevalence,y=percent_infection,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Diabetes Prevalence (%)",y="Daily Infection (%)",caption="*Data acquired from 142 countries across America, Europe, Africa, and Asia. n= 66764 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=6))