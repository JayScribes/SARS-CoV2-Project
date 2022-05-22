# COVID-19 Project
## Figure 3
### Infection % as a function of Smoking Prevalence

ggplot (Smoking_Infection_grouping, aes (x=smoking_prevalence,y=Percent_Infection))+
  geom_point()+geom_smooth(method="lm")

## Removing Visually-Obvious Outliers After Graphing

Smoke_Infection$outlier = Smoke_Infection$new_tested > 10000000    
Smoke_Infection = filter(Smoke_Infection, outlier != TRUE)
Smoke_Infection <- subset(Smoke_Infection, , -c(outlier))

Smoke_Infection$outlier = Smoke_Infection$new_tested < 0    
Smoke_Infection = filter(Smoke_Infection, outlier != TRUE)
Smoke_Infection <- subset(Smoke_Infection, , -c(outlier))

Smoke_Infection$outlier = Smoke_Infection$new_confirmed < 0   
Smoke_Infection = filter(Smoke_Infection, outlier != TRUE)
Smoke_Infection <- subset(Smoke_Infection, , -c(outlier))

## Creating new columns to see if smoking prevalence has a relationship with infection rates

Smoke_Infection$new_tested <- (Smoke_Infection$new_tested +1) ## to get rid of odd zeroes
Smoke_Infection$new_confirmed <- (Smoke_Infection$new_confirmed +1) ## adding another '1' constant to data 
Smoke_Infection$Percent_Infection <- (Smoke_Infection$new_confirmed/Smoke_Infection$new_tested)*100

Smoke_Infection$outlier = Smoke_Infection$Percent_Infection > 100   ## A few values ended up above 100%
Smoke_Infection = filter(Smoke_Infection, outlier != TRUE)
Smoke_Infection <- subset(Smoke_Infection, , -c(outlier))

##Winsorizing data as many non-sense data points still exist

percent_infection_win <-  Winsorize(Smoke_Infection$Percent_Infection, minval = NULL, maxval = NULL, probs = c(0.01, 0.99), na.rm = FALSE, type =1)
Smoke_Infection$Percent_Infection_win <- percent_infection_win ## adding winsorized information into the data frame

# Extracting month/year data

Smoke_Infection_month <- Smoke_Infection
Smoke_Infection_month$year_month <- floor_date(Smoke_Infection_month$date, "month")

##Grouping By smoking prevalence % 

Smoking_Infection_grouping <- Smoke_Infection_month %>%
  group_by(smoking_prevalence) %>% 
  dplyr::summarize(Percent_Infection = mean(Percent_Infection_win)) %>% 
  as.data.frame()

## Linear Regression Analysis

Smoking.Infection.Regression <- lm(Percent_Infection ~ smoking_prevalence, data= Smoking_Infection_grouping)
summary(Smoking.Infection.Regression) # reveals no significant predictive values of smoking prevalence on daily infection rates in 112 countries

Fig3_Infection <- ggplot (smoke_infection_graph_data, aes (x=smoking_prevalence,y=Percent_Infection,))+
  geom_point(color="turquoise4")+geom_smooth(method="lm",color="turquoise4",fill="azure4")+ theme_classic()+
  labs(x="Smoking Prevalence (%)",y="Daily Infection (%)",caption="*Data acquired from 111 countries across America, Europe, Africa, and Asia. n= 58861 ")+
  theme(plot.caption=element_text(hjust=0))+theme(plot.caption=element_text(size=6))