# COVID-19 Project
## Figure 4
### ICU % as a function of Diabetes Prevalence

ggplot (Diabetes_ICU_grouping, aes (x=diabetes_prevalence,y=percent_ICU))+
  geom_point()+geom_smooth(method="lm")

## Removing Visually-Obvious Outliers After Graphing

Diabetes_ICU$outlier = Diabetes_ICU$new_confirmed < 0   
Diabetes_ICU = filter(Diabetes_ICU, outlier != TRUE)
Diabetes_ICU<- subset(Diabetes_ICU, , -c(outlier))

Diabetes_ICU$outlier = Diabetes_ICU$new_ICU < 0   
Diabetes_ICU = filter(Diabetes_ICU, outlier != TRUE)
Diabetes_ICU<- subset(Diabetes_ICU, , -c(outlier))

## Creating new columns to see if diabetes prevalence has a relationship with ICU rates

Diabetes_ICU$new_ICU <- (Diabetes_ICU$new_ICU +1) ## to get rid of odd zeroes
Diabetes_ICU$new_confirmed <- (Diabetes_ICU$new_confirmed +1) ## adding another '1' constant to data 
Diabetes_ICU$Percent_ICU <- (Diabetes_ICU$new_ICU/Diabetes_ICU$new_confirmed)*100

## Cleaning calculated values

Diabetes_ICU$outlier = Diabetes_ICU$Percent_ICU > 100   
Diabetes_ICU = filter(Diabetes_ICU, outlier != TRUE)
Diabetes_ICU<- subset(Diabetes_ICU, , -c(outlier))

percent_ICU_win <-  Winsorize(Diabetes_ICU$Percent_ICU, minval = NULL, maxval = NULL, probs = c(0.02, 0.98), na.rm = FALSE, type =1)
Diabetes_ICU$percent_ICU_win <- percent_ICU_win ## adding winsorized information into the data frame

Diabetes_ICU_grouping <- Diabetes_ICU %>%
  group_by(diabetes_prevalence) %>% 
  dplyr::summarize(percent_ICU = mean(percent_ICU_win)) %>% 
  as.data.frame()

## Linear Regression Analysis

Diabetes.ICU.Regression <- lm(percent_ICU ~ diabetes_prevalence, data= Diabetes_ICU_grouping)
summary(Diabetes.ICU.Regression)

Fig4_ICU <- ggplot (Diabetes_ICU_grouping, aes (x=diabetes_prevalence,y=percent_ICU,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Diabetes Prevalence (%)",y="Daily ICU (%)",caption="*Data acquired from BR, ES, AR, MX, SW, Fr, US. n = 4520 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=6))