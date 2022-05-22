# COVID-19 Project
## Figure 1
### Hospitalization % After Testing by Age Groups

## Blindly cleaning data of Basic nonsense prior to making calculated fields
## Plotting prior to cleaning would take too long with this large dataset and many columns

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_0 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_1 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_2 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_3 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_4 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_5 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_6 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_7 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_8 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_confirmed_age_9 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

## The above removed some 200,000 data points, mostly due to N/A rows at the beginning of 2020

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_0 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_1 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_2 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_3 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_4 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_5 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_6 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_7 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_8 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

Age_Hospitalization$outlier = Age_Hospitalization$new_hospitalized_patients_age_9 < 0    
Age_Hospitalization = filter(Age_Hospitalization, outlier != TRUE)
Age_Hospitalization <- subset(Age_Hospitalization, , -c(outlier))

## The Above removed an additional roughly 200,000 data points for the same reason
## This leaves us with about 278 000 data points

## Calculated Fields

Age_Hosp <- cbind.data.frame(Age_Hospitalization$date, Age_Hospitalization$country)

Age_Hosp$percent_hosp_0 <- (Age_Hospitalization$new_hospitalized_patients_age_0/Age_Hospitalization$new_confirmed_age_0)*100
Age_Hosp$percent_hosp_1 <- (Age_Hospitalization$new_hospitalized_patients_age_1/Age_Hospitalization$new_confirmed_age_1)*100
Age_Hosp$percent_hosp_2 <- (Age_Hospitalization$new_hospitalized_patients_age_2/Age_Hospitalization$new_confirmed_age_2)*100
Age_Hosp$percent_hosp_3 <- (Age_Hospitalization$new_hospitalized_patients_age_3/Age_Hospitalization$new_confirmed_age_3)*100
Age_Hosp$percent_hosp_4 <- (Age_Hospitalization$new_hospitalized_patients_age_4/Age_Hospitalization$new_confirmed_age_4)*100
Age_Hosp$percent_hosp_5 <- (Age_Hospitalization$new_hospitalized_patients_age_5/Age_Hospitalization$new_confirmed_age_5)*100
Age_Hosp$percent_hosp_6 <- (Age_Hospitalization$new_hospitalized_patients_age_6/Age_Hospitalization$new_confirmed_age_6)*100
Age_Hosp$percent_hosp_7 <- (Age_Hospitalization$new_hospitalized_patients_age_7/Age_Hospitalization$new_confirmed_age_7)*100
Age_Hosp$percent_hosp_8 <- (Age_Hospitalization$new_hospitalized_patients_age_8/Age_Hospitalization$new_confirmed_age_8)*100
Age_Hosp$percent_hosp_9 <- (Age_Hospitalization$new_hospitalized_patients_age_9/Age_Hospitalization$new_confirmed_age_9)*100

## Further removal of nonsense percentages above 100 or below 0

Age_Hosp$outlier = Age_Hosp$percent_hosp_0 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_1 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_2 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_3 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_4 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_5 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_6 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_7 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_8 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

Age_Hosp$outlier = Age_Hosp$percent_hosp_9 > 100    
Age_Hosp = filter(Age_Hosp, outlier != TRUE)
Age_Hosp <- subset(Age_Hosp, , -c(outlier))

percent_hospitalized_0_winsorized <-  Winsorize(Age_Hosp$percent_hosp_0, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_0_win<- percent_hospitalized_0_winsorized ## adding winsorized information into the data frame

percent_hospitalized_1_winsorized <-  Winsorize(Age_Hosp$percent_hosp_1, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_1_win<- percent_hospitalized_1_winsorized ## adding winsorized information into the data frame

percent_hospitalized_2_winsorized <-  Winsorize(Age_Hosp$percent_hosp_2, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_2_win<- percent_hospitalized_2_winsorized ## adding winsorized information into the data frame

percent_hospitalized_3_winsorized <-  Winsorize(Age_Hosp$percent_hosp_3, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_3_win<- percent_hospitalized_3_winsorized ## adding winsorized information into the data frame

percent_hospitalized_4_winsorized <-  Winsorize(Age_Hosp$percent_hosp_4, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_4_win<- percent_hospitalized_4_winsorized ## adding winsorized information into the data frame

percent_hospitalized_5_winsorized <-  Winsorize(Age_Hosp$percent_hosp_5, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_5_win<- percent_hospitalized_5_winsorized ## adding winsorized information into the data frame

percent_hospitalized_6_winsorized <-  Winsorize(Age_Hosp$percent_hosp_6, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_6_win<- percent_hospitalized_6_winsorized ## adding winsorized information into the data frame

percent_hospitalized_7_winsorized <-  Winsorize(Age_Hosp$percent_hosp_7, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_7_win<- percent_hospitalized_7_winsorized ## adding winsorized information into the data frame

percent_hospitalized_8_winsorized <-  Winsorize(Age_Hosp$percent_hosp_8, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_8_win<- percent_hospitalized_8_winsorized ## adding winsorized information into the data frame

percent_hospitalized_9_winsorized <-  Winsorize(Age_Hosp$percent_hosp_9, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Hosp$percent_hosp_9_win<- percent_hospitalized_9_winsorized ## adding winsorized information into the data frame

## Aggregating Data by Week

Age_Hosp <- edit(Age_Hosp)
Age_Hosp$date <- as.Date(Age_Hosp$date) ## changing year/month data to correct format

Age_Hosp_month <- Age_Hosp
Age_Hosp_month$year_week <- floor_date(Age_Hosp_month$date, "week")

Age_hosp_0 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_0 = mean(percent_hosp_0_win)) %>% 
  as.data.frame()

Age_hosp_1 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_1 = mean(percent_hosp_1_win)) %>% 
  as.data.frame()

Age_hosp_2 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_2 = mean(percent_hosp_2_win)) %>% 
  as.data.frame()

Age_hosp_3 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_3 = mean(percent_hosp_3_win)) %>% 
  as.data.frame()

Age_hosp_4 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_4 = mean(percent_hosp_4_win)) %>% 
  as.data.frame()

Age_hosp_5 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_5 = mean(percent_hosp_5_win)) %>% 
  as.data.frame()

Age_hosp_6 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_6 = mean(percent_hosp_6_win)) %>% 
  as.data.frame()

Age_hosp_7 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_7 = mean(percent_hosp_7_win)) %>% 
  as.data.frame()

Age_hosp_8 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_8 = mean(percent_hosp_8_win)) %>% 
  as.data.frame()

Age_hosp_9 <- Age_Hosp_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(hosp_9 = mean(percent_hosp_9_win)) %>% 
  as.data.frame()

## Making a new Data frame for plotting

Age_hosp_combined <- cbind.data.frame(Age_hosp_0, Age_hosp_1$hosp_1,Age_hosp_2$hosp_2,Age_hosp_3$hosp_3,Age_hosp_4$hosp_4,Age_hosp_5$hosp_5,Age_hosp_6$hosp_6,
                                      Age_hosp_7$hosp_7,Age_hosp_8$hosp_8,Age_hosp_9$hosp_9)

Age_hosp_combined <- edit(Age_hosp_combined) ## fixing headers

Age_hosp_combined$year_week <- as.Date(Age_hosp_combined$year_week) ## changing year/month data to correct format

## Aggregate Ages Based on American CDC Information

Age_hosp_combined$hosp_sub50 <- (Age_hosp_combined$hosp_0 + Age_hosp_combined$hosp_1 + Age_hosp_combined$hosp_2 + Age_hosp_combined$hosp_3 + Age_hosp_combined$hosp_4)/5

Age_hosp_combined$hosp_5069 <-(Age_hosp_combined$hosp_5 + Age_hosp_combined$hosp_6)/2

Age_hosp_combined$hosp_7089 <-(Age_hosp_combined$hosp_7 + Age_hosp_combined$hosp_8)/2



## rest of the 10-year age ranges will be kept

## Plotting

Fig1_Hospitalization <- ggplot(Age_hosp_combined, aes(year_week)) + 
  geom_line(aes(y = hosp_sub50, colour = ">50")) +
  geom_point(aes(y = hosp_sub50, colour = ">50")) +
  geom_line(aes(y = hosp_5069, colour = "50-69")) +
  geom_point(aes(y = hosp_5069, colour = "50-69")) +
  geom_line(aes(y = hosp_7089, colour = "70-89")) +
  geom_point(aes(y = hosp_7089, colour = "70-89")) +
  geom_line(aes(y = hosp_9, colour = "90+")) +
  geom_point(aes(y = hosp_9, colour = "90+")) +
  theme_classic() +
  labs(x="Date",y="Average Daily Hospitalization Percentage"
       ,caption="*Data from BR, AR. n=276365") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=7))+theme(plot.caption=element_text(size=5))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=6))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position="none") +
  scale_y_continuous(name="Daily Hospitalized Percentage", limits=c(0, 100))