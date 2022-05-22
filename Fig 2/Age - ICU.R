# COVID-19 Project
## Figure 1
### ICU % After Testing by Age Groups


## Blindly cleaning data of Basic nonsense prior to making calculated fields
## Plotting prior to cleaning would take too long with this large dataset and many columns

Age_ICU$outlier = Age_ICU$new_confirmed_age_0 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_1 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_2 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_3 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_4 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_5 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_6 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_7 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_8 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_confirmed_age_9 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

## The above removed some 200,000 data points, mostly due to N/A rows at the beginning of 2020

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_0 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_1 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_2 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_3 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_4 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_5 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_6 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_7 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_8 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

Age_ICU$outlier = Age_ICU$new_intensive_care_patients_age_9 < 0    
Age_ICU = filter(Age_ICU, outlier != TRUE)
Age_ICU <- subset(Age_ICU, , -c(outlier))

## The Above removed an additional roughly 200,000 data points for the same reason
## This leaves us with about 50 000 data points

## Calculated Fields

Age_ICU2 <- cbind.data.frame(Age_ICU$date, Age_ICU$country)

Age_ICU2$percent_ICU_0 <- (Age_ICU$new_intensive_care_patients_age_0/Age_ICU$new_confirmed_age_0)*100
Age_ICU2$percent_ICU_1 <- (Age_ICU$new_intensive_care_patients_age_1/Age_ICU$new_confirmed_age_1)*100
Age_ICU2$percent_ICU_2 <- (Age_ICU$new_intensive_care_patients_age_2/Age_ICU$new_confirmed_age_2)*100
Age_ICU2$percent_ICU_3 <- (Age_ICU$new_intensive_care_patients_age_3/Age_ICU$new_confirmed_age_3)*100
Age_ICU2$percent_ICU_4 <- (Age_ICU$new_intensive_care_patients_age_4/Age_ICU$new_confirmed_age_4)*100
Age_ICU2$percent_ICU_5 <- (Age_ICU$new_intensive_care_patients_age_5/Age_ICU$new_confirmed_age_5)*100
Age_ICU2$percent_ICU_6 <- (Age_ICU$new_intensive_care_patients_age_6/Age_ICU$new_confirmed_age_6)*100
Age_ICU2$percent_ICU_7 <- (Age_ICU$new_intensive_care_patients_age_7/Age_ICU$new_confirmed_age_7)*100
Age_ICU2$percent_ICU_8 <- (Age_ICU$new_intensive_care_patients_age_8/Age_ICU$new_confirmed_age_8)*100
Age_ICU2$percent_ICU_9 <- (Age_ICU$new_intensive_care_patients_age_9/Age_ICU$new_confirmed_age_9)*100

## Further removal of nonsense percentages above 100 or below 0

Age_ICU2$outlier = Age_ICU2$percent_ICU_0 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_1 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_2 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_3 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_4 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_5 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_6 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_7 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_8 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

Age_ICU2$outlier = Age_ICU2$percent_ICU_9 > 100    
Age_ICU2 = filter(Age_ICU2, outlier != TRUE)
Age_ICU2 <- subset(Age_ICU2, , -c(outlier))

## Winsorizing data

percent_ICU_0_winsorized <-  Winsorize(Age_ICU2$percent_ICU_0, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_0_win<- percent_ICU_0_winsorized ## adding winsorized information into the data frame

percent_ICU_1_winsorized <-  Winsorize(Age_ICU2$percent_ICU_1, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_1_win<- percent_ICU_1_winsorized ## adding winsorized information into the data frame

percent_ICU_2_winsorized <-  Winsorize(Age_ICU2$percent_ICU_2, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_2_win<- percent_ICU_2_winsorized ## adding winsorized information into the data frame

percent_ICU_3_winsorized <-  Winsorize(Age_ICU2$percent_ICU_3, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_3_win<- percent_ICU_3_winsorized ## adding winsorized information into the data frame

percent_ICU_4_winsorized <-  Winsorize(Age_ICU2$percent_ICU_4, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_4_win<- percent_ICU_4_winsorized ## adding winsorized information into the data frame

percent_ICU_5_winsorized <-  Winsorize(Age_ICU2$percent_ICU_5, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_5_win<- percent_ICU_5_winsorized ## adding winsorized information into the data frame

percent_ICU_6_winsorized <-  Winsorize(Age_ICU2$percent_ICU_6, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_6_win<- percent_ICU_6_winsorized ## adding winsorized information into the data frame

percent_ICU_7_winsorized <-  Winsorize(Age_ICU2$percent_ICU_7, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_7_win<- percent_ICU_7_winsorized ## adding winsorized information into the data frame

percent_ICU_8_winsorized <-  Winsorize(Age_ICU2$percent_ICU_8, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_8_win<- percent_ICU_8_winsorized ## adding winsorized information into the data frame

percent_ICU_9_winsorized <-  Winsorize(Age_ICU2$percent_ICU_9, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_ICU2$percent_ICU_9_win<- percent_ICU_9_winsorized ## adding winsorized information into the data frame

## Aggregating Data by Week

Age_ICU2 <- edit(Age_ICU2) ## Fixing headers
Age_ICU2$date <- as.Date(Age_ICU2$date) ## changing year/month data to correct format



Age_ICU_week <- Age_ICU2
Age_ICU_week$year_week <- floor_date(Age_ICU_week$date, "week")



Age_ICU_0 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_0 = mean(percent_ICU_0_win)) %>% 
  as.data.frame()

Age_ICU_1 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_1 = mean(percent_ICU_1_win)) %>% 
  as.data.frame()

Age_ICU_2 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_2 = mean(percent_ICU_2_win)) %>% 
  as.data.frame()

Age_ICU_3 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_3 = mean(percent_ICU_3_win)) %>% 
  as.data.frame()

Age_ICU_4 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_4 = mean(percent_ICU_4_win)) %>% 
  as.data.frame()

Age_ICU_5 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_5 = mean(percent_ICU_5_win)) %>% 
  as.data.frame()

Age_ICU_6 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_6 = mean(percent_ICU_6_win)) %>% 
  as.data.frame()

Age_ICU_7 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_7 = mean(percent_ICU_7_win)) %>% 
  as.data.frame()

Age_ICU_8 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_8 = mean(percent_ICU_8_win)) %>% 
  as.data.frame()

Age_ICU_9 <- Age_ICU_week %>%
  group_by(year_week) %>% 
  dplyr::summarize(ICU_9 = mean(percent_ICU_9_win)) %>% 
  as.data.frame()

## Making a new Data frame for plotting

Age_ICU_combined <- cbind.data.frame(Age_ICU_0,Age_ICU_1$ICU_1,Age_ICU_2$ICU_2,Age_ICU_3$ICU_3,Age_ICU_4$ICU_4,Age_ICU_5$ICU_5,Age_ICU_6$ICU_6,Age_ICU_7$ICU_7,
                                     Age_ICU_8$ICU_8,Age_ICU_9$ICU_9)

Age_ICU_combined <- edit(Age_ICU_combined) ## fixing headers

Age_ICU_combined$year_week <- as.Date(Age_ICU_combined$year_week) ## changing year/month data to correct format

## Aggregate Ages Based on American CDC Information

Age_ICU_combined$icu_sub50 <- (Age_ICU_combined$ICU_0 + Age_ICU_combined$ICU_1 + Age_ICU_combined$ICU_2 + Age_ICU_combined$ICU_3 + Age_ICU_combined$ICU_4)/5

Age_ICU_combined$ICU_5069 <-(Age_ICU_combined$ICU_5 + Age_ICU_combined$ICU_6)/2

Age_ICU_combined$ICU_7089 <-(Age_ICU_combined$ICU_7 + Age_ICU_combined$ICU_8)/2

## rest of the 10-year age ranges will be kept

## Plotting

Fig1_ICU <- ggplot(Age_ICU_combined, aes(year_week)) + 
  geom_line(aes(y = icu_sub50, colour = ">50")) +
  geom_point(aes(y = icu_sub50, colour = ">50")) +
  geom_line(aes(y = ICU_5069, colour = "50-69")) +
  geom_point(aes(y = ICU_5069, colour = "50-69")) +
  geom_line(aes(y = ICU_7089, colour = "70-89")) +
  geom_point(aes(y = ICU_7089, colour = "70-89")) +
  geom_line(aes(y = ICU_9, colour = "90+")) +
  geom_point(aes(y = ICU_9, colour = "90+")) +
  theme_classic() +
  labs(x="Date",y="Average Daily ICU Percentage"
       ,caption="*Data from BR, AR. n=24387") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=7))+theme(plot.caption=element_text(size=5))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=6))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position="none")+
  scale_y_continuous(name="Daily ICU Percentage", limits=c(0, 100))