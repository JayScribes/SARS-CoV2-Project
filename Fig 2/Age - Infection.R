# COVID-19 Project
## Figure 1
### Infection % After Testing by Age Groups

ggplot (Age_Infect_month, aes (x=date,y=percent_infection_0_win))+
  geom_point()

## Blindly cleaning data of Basic nonsense prior to making calculated fields
## Plotting prior to cleaning would take too long with this large dataset and many columns

Age_Infection$outlier = Age_Infection$new_confirmed_age_0 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_1 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_2 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_3 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_4 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_5 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_6 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_7 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_8 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_confirmed_age_9 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

## The above removed some 200,000 data points, mostly due to N/A rows at the beginning of 2020

Age_Infection$outlier = Age_Infection$new_tested_age_0 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_1 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_2 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_3 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_4 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_5 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_6 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_7 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_8 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

Age_Infection$outlier = Age_Infection$new_tested_age_9 < 0    
Age_Infection = filter(Age_Infection, outlier != TRUE)
Age_Infection <- subset(Age_Infection, , -c(outlier))

## The Above removed an additional roughly 200,000 data points for the same reason
## This leaves us with about 370 000 data points


## Calculated Fields

Age_Infect <- cbind.data.frame(Age_Infection$date, Age_Infection$country)

Age_Infect$Percent_Infection_0 <- (Age_Infection$new_confirmed_age_0/Age_Infection$new_tested_age_0)*100
Age_Infect$Percent_Infection_1 <- (Age_Infection$new_confirmed_age_1/Age_Infection$new_tested_age_1)*100
Age_Infect$Percent_Infection_2 <- (Age_Infection$new_confirmed_age_2/Age_Infection$new_tested_age_2)*100
Age_Infect$Percent_Infection_3 <- (Age_Infection$new_confirmed_age_3/Age_Infection$new_tested_age_3)*100
Age_Infect$Percent_Infection_4 <- (Age_Infection$new_confirmed_age_4/Age_Infection$new_tested_age_4)*100
Age_Infect$Percent_Infection_5 <- (Age_Infection$new_confirmed_age_5/Age_Infection$new_tested_age_5)*100
Age_Infect$Percent_Infection_6 <- (Age_Infection$new_confirmed_age_6/Age_Infection$new_tested_age_6)*100
Age_Infect$Percent_Infection_7 <- (Age_Infection$new_confirmed_age_7/Age_Infection$new_tested_age_7)*100
Age_Infect$Percent_Infection_8 <- (Age_Infection$new_confirmed_age_8/Age_Infection$new_tested_age_8)*100
Age_Infect$Percent_Infection_9 <- (Age_Infection$new_confirmed_age_9/Age_Infection$new_tested_age_9)*100

## More data cleaning
Age_Infect <- edit(Age_Infect) ## cleaning headers

Age_Infect$date <- as.Date(Age_Infect$date) ## changing year/month data to correct format

## Further removal of nonsense percentages above 100 or below 0

Age_Infect_month <- Age_Infect
Age_Infect_month$year_week <- floor_date(Age_Infect_month$date, "week")

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_0 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_1 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_2 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_3 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_4 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_5 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_6 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_7 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_8 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

Age_Infect_month$outlier = Age_Infect_month$Percent_Infection_9 > 100    
Age_Infect_month = filter(Age_Infect_month, outlier != TRUE)
Age_Infect_month <- subset(Age_Infect_month, , -c(outlier))

## Winsorizing data

percent_infection_0_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_0, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_0_win <- percent_infection_0_winsorized ## adding winsorized information into the data frame

percent_infection_1_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_1, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_1_win <- percent_infection_1_winsorized ## adding winsorized information into the data frame

percent_infection_2_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_2, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_2_win <- percent_infection_2_winsorized ## adding winsorized information into the data frame

percent_infection_3_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_3, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_3_win <- percent_infection_3_winsorized ## adding winsorized information into the data frame

percent_infection_4_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_4, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_4_win <- percent_infection_4_winsorized ## adding winsorized information into the data frame

percent_infection_5_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_5, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_5_win <- percent_infection_5_winsorized ## adding winsorized information into the data frame

percent_infection_6_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_6, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_6_win <- percent_infection_6_winsorized ## adding winsorized information into the data frame

percent_infection_7_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_7, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_7_win <- percent_infection_7_winsorized ## adding winsorized information into the data frame

percent_infection_8_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_8, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_8_win <- percent_infection_8_winsorized ## adding winsorized information into the data frame

percent_infection_9_winsorized <-  Winsorize(Age_Infect_month$Percent_Infection_9, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Age_Infect_month$percent_infection_9_win <- percent_infection_9_winsorized ## adding winsorized information into the data frame

## Aggregating Data by Week

Age_inf_0 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_0 = mean(percent_infection_0_win)) %>% 
  as.data.frame()

Age_inf_1 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_1 = mean(percent_infection_1_win)) %>% 
  as.data.frame()

Age_inf_2 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_2 = mean(percent_infection_2_win)) %>% 
  as.data.frame()

Age_inf_3 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_3 = mean(percent_infection_3_win)) %>% 
  as.data.frame()

Age_inf_4 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_4 = mean(percent_infection_4_win)) %>% 
  as.data.frame()

Age_inf_5 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_5 = mean(percent_infection_5_win)) %>% 
  as.data.frame()

Age_inf_6 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_6 = mean(percent_infection_6_win)) %>% 
  as.data.frame()

Age_inf_7 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_7 = mean(percent_infection_7_win)) %>% 
  as.data.frame()

Age_inf_8 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_8 = mean(percent_infection_8_win)) %>% 
  as.data.frame()

Age_inf_9 <- Age_Infect_month %>%
  group_by(year_week) %>% 
  dplyr::summarize(inf_9 = mean(percent_infection_9_win)) %>% 
  as.data.frame()

## Making a new Data frame for plotting

Age_infect_combined <- cbind.data.frame(Age_inf_0, Age_inf_1$inf_1, Age_inf_2$inf_2, Age_inf_3$inf_3, Age_inf_4$inf_4, Age_inf_5$inf_5, Age_inf_6$inf_6,
                                        Age_inf_7$inf_7, Age_inf_8$inf_8, Age_inf_9$inf_9)

Age_infect_combined <- edit(Age_infect_combined) ## fixing headers

Age_infect_combined$year_week <- as.Date(Age_infect_combined$year_week) ## changing year/month data to correct format

## Aggregate Ages 

Age_infect_combined$inf_sub50 <- (Age_infect_combined$inf_0 + Age_infect_combined$inf_1 + Age_infect_combined$inf_2 + Age_infect_combined$inf_3
                                  + Age_infect_combined$inf_4)/5

Age_infect_combined$inf_5069 <- (Age_infect_combined$inf_5 + Age_infect_combined$inf_6)/2

Age_infect_combined$inf_7089 <- (Age_infect_combined$inf_7 + Age_infect_combined$inf_8)/2

## rest of the 10-year age ranges will be kept

## Line and dot plot?


Fig1_Infection <- ggplot(Age_infect_combined, aes(year_week)) + 
  geom_line(aes(y = inf_sub50, colour = "<50")) +
  geom_point(aes(y = inf_sub50, colour = "<50")) +
  geom_line(aes(y = inf_5069, colour = "50-69")) +
  geom_point(aes(y = inf_5069, colour = "50-69")) +
  geom_line(aes(y = inf_7089, colour = "70-89")) +
  geom_point(aes(y = inf_7089, colour = "70-89")) +
  geom_line(aes(y = inf_9, colour = "90+")) +
  geom_point(aes(y = inf_9, colour = "90+")) +
  theme_classic() +
  labs(x="Date",y="Average Daily Infected Percentage"
       ,caption="*Data from BR, AR. n=141377") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=7))+theme(plot.caption=element_text(size=5))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=6))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position="none") +
  scale_y_continuous(name="Daily Infected Percentage", limits=c(0, 100))



