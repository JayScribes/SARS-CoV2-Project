# COVID-19 Project
## Figure 2
### ICU % After Confirmed Infection in Males and Females

ggplot (Sex_ICU_MFP, aes (x=year_month,y=avg_new_percent_ICU_female))+
  geom_point()

# After plotting all the data, new_confirmed_male has visually-obvious outliers and needs to be cleaned

Sex_ICU$outlier = Sex_ICU$new_confirmed_male > 10000    # A group of outliers was present, this removes them.
Sex_ICU = filter(Sex_ICU, outlier != TRUE)
Sex_ICU <- subset(Sex_ICU, , -c(outlier))

## Aggregating Data by unique week to smooth out variability and make it clearer


Sex_ICU$percent_ICU_male <- (Sex_ICU$new_intensive_care_patients_male/Sex_ICU$new_confirmed_male)*100
Sex_ICU$percent_ICU_female <- (Sex_ICU$new_intensive_care_patients_female/Sex_ICU$new_confirmed_female)*100

## Getting means and Standard deviations by month
Sex_ICU_month <- Sex_ICU
Sex_ICU_month$year_month <- floor_date(Sex_ICU_month$date, "week")


## Weird values in July to September 2021 and February 2020 more ICU than confirmed - Cleaning through WInsorizing

percent_ICU_male_winsorized <-  Winsorize(Sex_ICU_month$percent_ICU_male, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Sex_ICU_month$percent_ICU_male_win <- percent_ICU_male_winsorized ## adding winsorized information into the data frame

percent_ICU_female_winsorized <-  Winsorize(Sex_ICU_month$percent_ICU_female, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Sex_ICU_month$percent_ICU_female_win <- percent_ICU_female_winsorized ## adding winsorized information into the data frame

## Getting descriptive stats out of the Winsorized data and aggregating by month

Sex_ICU_MFP <- Sex_ICU_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_percent_ICU_female = mean(percent_ICU_female_win)) %>% 
  as.data.frame()

Sex_ICU_MMP <- Sex_ICU_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_percent_ICU_male = mean(percent_ICU_male_win)) %>% 
  as.data.frame()

Sex_ICU_SDFP <- Sex_ICU_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_percent_ICU_female = sd(percent_ICU_female_win)) %>% 
  as.data.frame()

Sex_ICU_SDMP <- Sex_ICU_month %>%  
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_percent_ICU_male = sd(percent_ICU_male_win)) %>% 
  as.data.frame()

remove(Sex_ICU_MFP, Sex_ICU_MMP, Sex_ICU_SDFP, Sex_ICU_SDMP, Sex_ICU_month) ## to clean environment

## Creating a data frame with all relevant information for graphing
Sex_ICU_month_combined <- cbind.data.frame(Sex_ICU_MFP$year_month, Sex_ICU_MFP$avg_new_percent_ICU_female, Sex_ICU_MMP$avg_new_percent_ICU_male,
  Sex_ICU_SDFP$sd_new_percent_ICU_female,Sex_ICU_SDMP$sd_new_percent_ICU_male)

Sex_ICU_month_combined <- edit(Sex_ICU_month_combined) ## to change header names

Sex_ICU_month_combined$year_month <- as.Date(Sex_ICU_month_combined$year_month) ## changing year/month data to correct format

## Plotting the data

Fig2_ICU<- ggplot(Sex_ICU_month_combined, aes(year_month)) + 
  geom_line(aes(y = avg_new_percent_ICU_male, colour = "Male")) +
  geom_point(aes(y = avg_new_percent_ICU_male, colour = "Male")) +
  geom_line(aes(y = avg_new_percent_ICU_female, colour = "Female")) +
  geom_point(aes(y = avg_new_percent_ICU_female, colour = "Female")) +
  theme_classic() +
  labs(x="Date",y="Average Daily ICU Percentage"
       ,caption="*Data from BR, AR, ES.m=80206") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=7))+theme(plot.caption=element_text(size=5))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=6))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.text=element_blank())+theme(legend.position="none")+
  scale_y_continuous(name="Daily ICU Percentage", limits=c(0, 100))


  
