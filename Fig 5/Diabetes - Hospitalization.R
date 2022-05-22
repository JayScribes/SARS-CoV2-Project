# COVID-19 Project
## Figure 4
### Hospitalization % as a function of Diabetes Prevalence

ggplot (Diabetes_Hosp_grouping, aes (x=diabetes_prevalence,y=percent_Hosp))+
  geom_point()+geom_smooth(method="lm")

## Removing Visually-Obvious Outliers After Graphing

Diabetes_Hospitalization$outlier = Diabetes_Hospitalization$new_hospitalized_patients < 0   
Diabetes_Hospitalization = filter(Diabetes_Hospitalization, outlier != TRUE)
Diabetes_Hospitalization<- subset(Diabetes_Hospitalization, , -c(outlier))


Diabetes_Hospitalization$outlier = Diabetes_Hospitalization$new_confirmed < 0   
Diabetes_Hospitalization = filter(Diabetes_Hospitalization, outlier != TRUE)
Diabetes_Hospitalization<- subset(Diabetes_Hospitalization, , -c(outlier))

## Creating new columns to see if diabetes prevalence has a relationship with Hospitalization rates

Diabetes_Hospitalization$new_hospitalized_patients <- (Diabetes_Hospitalization$new_hospitalized_patients +1) ## to get rid of odd zeroes
Diabetes_Hospitalization$new_confirmed <- (Diabetes_Hospitalization$new_confirmed +1) ## adding another '1' constant to data 
Diabetes_Hospitalization$Percent_Hosp <- (Diabetes_Hospitalization$new_hospitalized_patients/Diabetes_Hospitalization$new_confirmed)*100

##Cleaning Calculated Values

Diabetes_Hospitalization$outlier = Diabetes_Hospitalization$Percent_Hosp > 100   
Diabetes_Hospitalization = filter(Diabetes_Hospitalization, outlier != TRUE)
Diabetes_Hospitalization<- subset(Diabetes_Hospitalization, , -c(outlier))

percent_hosp_win <-  Winsorize(Diabetes_Hospitalization$Percent_Hosp, minval = NULL, maxval = NULL, probs = c(0.04, 0.96), na.rm = FALSE, type =1)
Diabetes_Hospitalization$percent_hosp_win <- percent_hosp_win ## adding winsorized information into the data frame

Diabetes_Hosp_grouping <- Diabetes_Hospitalization %>%
  group_by(diabetes_prevalence) %>% 
  dplyr::summarize(percent_Hosp = mean(percent_hosp_win)) %>% 
  as.data.frame()

## Linear Regression Analysis

Diabetes.Hosp.Regression <- lm(percent_Hosp ~ diabetes_prevalence, data= Diabetes_Hosp_grouping)
summary(Diabetes.Hosp.Regression)

Fig4_Hospitalization <- ggplot (Diabetes_Hosp_grouping, aes (x=diabetes_prevalence,y=percent_Hosp,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Diabetes Prevalence (%)",y="Daily Hospitalization (%)",caption="*Data acquired from AR, ES, TH, HK, PH, BR, CH, MX, LI, US, BE, Fr, UK, ML. n = 8018 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=6))