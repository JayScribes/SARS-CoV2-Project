# COVID-19 Project
## Figure 3
### Hospitalization % as a function of Smoking Prevalence

ggplot (Smoking_Hospitalization_grouping, aes (x=smoking_prevalence,y=percent_hospitalization))+
  geom_point()+geom_smooth(method="lm")

## Removing values below 0

Smoke_Hospitalization$outlier = Smoke_Hospitalization$new_hospitalized_patients < 0    
Smoke_Hospitalization = filter(Smoke_Hospitalization, outlier != TRUE)
Smoke_Hospitalization <- subset(Smoke_Hospitalization, , -c(outlier))

## Creating calculated fields for percent hospitalization

Smoke_Hospitalization$percent_hospitalization <- (Smoke_Hospitalization$new_hospitalized_patients/Smoke_Hospitalization$new_confirmed)*100

## Cleaning Data, removing extreme values and winsorizing

Smoke_Hospitalization$outlier = Smoke_Hospitalization$percent_hospitalization < 0    
Smoke_Hospitalization = filter(Smoke_Hospitalization, outlier != TRUE)
Smoke_Hospitalization <- subset(Smoke_Hospitalization, , -c(outlier))

Smoke_Hospitalization$outlier = Smoke_Hospitalization$percent_hospitalization > 100  
Smoke_Hospitalization = filter(Smoke_Hospitalization, outlier != TRUE)
Smoke_Hospitalization <- subset(Smoke_Hospitalization, , -c(outlier))

percent_hospitalization_win <-  Winsorize(Smoke_Hospitalization$percent_hospitalization, minval = NULL, maxval = NULL, probs = c(0.04, 0.96), na.rm = FALSE, type =1)
Smoke_Hospitalization$percent_hospitalization_win <- percent_hospitalization_win ## adding winsorized information into the data frame

Smoke_Hospitalization$outlier = Smoke_Hospitalization$percent_hospitalization_win > 63  
Smoke_Hospitalization = filter(Smoke_Hospitalization, outlier != TRUE)
Smoke_Hospitalization <- subset(Smoke_Hospitalization, , -c(outlier))

## Grouping by month

Smoke_Hospitalization_month <- Smoke_Hospitalization
Smoke_Hospitalization_month$year_month <- floor_date(Smoke_Hospitalization_month$date, "month")

##Grouping By smoking prevalence % 

Smoking_Hospitalization_grouping <- Smoke_Hospitalization_month %>% 
  group_by(smoking_prevalence) %>% 
  dplyr::summarize(percent_hospitalization = mean(percent_hospitalization_win)) %>% 
  as.data.frame()

Smoking.Hospitalization.Regression <- lm(percent_hospitalization ~ smoking_prevalence, data= Smoking_Hospitalization_grouping)
summary(Smoking.Hospitalization.Regression)


Fig3_Hospitalization <- ggplot (Smoking_Hospitalization_grouping, aes (x=smoking_prevalence,y=percent_hospitalization,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Smoking Prevalence (%)",y="Daily Hospitalization (%)",caption="*Data acquired from ES, AR TH, BR, US, PH, CH, MX, BE, FR, UK, ML, JP. n= 7814 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=5))