# COVID-19 Project
## Figure 3
### ICU % as a function of Smoking Prevalence

ggplot (Smoking_ICU_for_calc, aes (x=smoking_prevalence,y=percent_ICU))+
  geom_point()+geom_smooth(method="lm")

#Data seems clean to the eye, no need to clean it yet

# Extracting month/year data

Smoke_ICU <- Smoke_ICU
Smoke_ICU$year_month <- floor_date(Smoke_ICU$date, "month")

Smoking_ICU_grouping <- Smoke_ICU %>% ## Aggregating cleaned data
  group_by(smoking_prevalence) %>% 
  dplyr::summarize(confirme_dby_month = mean(new_confirmed)) %>% 
  as.data.frame()

Smoking_ICU_grouping2 <- Smoke_ICU %>% ## Aggregating cleaned data
  group_by(smoking_prevalence) %>% 
  dplyr::summarize(ICU_by_month = mean(new_intensive_care_patients)) %>% 
  as.data.frame()

Smoking_ICU_for_calc <- Smoking_ICU_grouping
Smoking_ICU_for_calc$icu <- Smoking_ICU_grouping2$ICU_by_month

Smoking_ICU_for_calc$percent_ICU <- (Smoking_ICU_for_calc$icu/Smoking_ICU_for_calc$confirmedby_month)*100

Smoking.ICU.Regression <- lm(percent_ICU ~ smoking_prevalence, data= Smoking_ICU_for_calc)
summary(Smoking.ICU.Regression)

Fig3_ICU <- ggplot (Smoking_ICU_for_calc, aes (x=smoking_prevalence,y=percent_ICU,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Smoking Prevalence (%)",y="Daily ICU (%)",caption="*Data acquired from ES, AR, BR, US, MX, SW, FR. n= 5185 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=5))