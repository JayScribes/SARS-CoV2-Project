# COVID-19 Project
## Figure 2
### Hospitalization % After Confirmed Infection in Males and Females

ggplot (Sex_Hosp_MFP, aes (x=year_month,y=avg_new_percent_hosp_female))+
  geom_point() ## used throughout to plot data to see effects of manipulations

Sex_Hospitalization$outlier = Sex_Hospitalization$new_confirmed_male > 20000    # A group of outliers was present, this removes them.
Sex_Hospitalization = filter(Sex_Hospitalization, outlier != TRUE)
Sex_Hospitalization <- subset(Sex_Hospitalization, , -c(outlier))

## Aggregating Data by unique week to smooth out variability and make it clearer


Sex_Hospitalization$percent_hosp_male <- (Sex_Hospitalization$new_hospitalized_patients_male/Sex_Hospitalization$new_confirmed_male)*100
Sex_Hospitalization$percent_hosp_female <- (Sex_Hospitalization$new_hospitalized_patients_female/Sex_Hospitalization$new_confirmed_female)*100

Sex_Hosp_month <- Sex_Hospitalization
Sex_Hosp_month$year_month <- floor_date(Sex_Hosp_month$date, "week")

## Many non-sense points are coming up due to higher hospitalization rates than confirmed rates. Will Winsorize data.

percent_hosp_male_winsorized <-  Winsorize(Sex_Hosp_month$percent_hosp_male, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Sex_Hosp_month$percent_hosp_male_win <- percent_hosp_male_winsorized ## adding winsorized information into the data frame

percent_hosp_female_winsorized <-  Winsorize(Sex_Hosp_month$percent_hosp_female, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Sex_Hosp_month$percent_hosp_female_win <- percent_hosp_female_winsorized ## adding winsorized information into the data frame

## Getting descriptive stats out of the Winsorized data and aggregating by month

## A lot of odd data at the beginning (towards January 2020), rest seem normal

Sex_Hosp_MFP <- Sex_Hosp_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_percent_hosp_female = mean(percent_hosp_female_win)) %>% 
  as.data.frame()

Sex_Hosp_MMP <- Sex_Hosp_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_percent_hosp_male = mean(percent_hosp_male_win)) %>% 
  as.data.frame()

Sex_Hosp_SDFP <- Sex_Hosp_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_percent_hosp_female = sd(percent_hosp_female_win)) %>% 
  as.data.frame()

Sex_Hosp_SDMP <- Sex_Hosp_month %>%  
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_percent_hosp_male = sd(percent_hosp_male_win)) %>% 
  as.data.frame()

remove(Sex_Hosp_MFP, Sex_Hosp_MMP, Sex_Hosp_SDFP, Sex_Hosp_SDMP, Sex_Hosp_month) ## to clean environment

## Creating a data frame with all relevant information for graphing
Sex_Hosp_month_combined <- cbind.data.frame(Sex_Hosp_MFP$year_month, Sex_Hosp_MFP$avg_new_percent_hosp_female, Sex_Hosp_MMP$avg_new_percent_hosp_male,
                                           Sex_Hosp_SDFP$sd_new_percent_hosp_female,Sex_Hosp_SDMP$sd_new_percent_hosp_male)

Sex_Hosp_month_combined <- edit(Sex_Hosp_month_combined) ## to change header names

Sex_Hosp_month_combined$year_month <- as.Date(Sex_Hosp_month_combined$year_month) ## changing year/month data to correct format

## Plotting the data

Fig2_Hospitalization <- ggplot(Sex_Hosp_month_combined, aes(year_month)) + 
  geom_line(aes(y = avg_new_percent_hosp_male, colour = "Male")) +
  geom_point(aes(y = avg_new_percent_hosp_male, colour = "Male")) +
  geom_line(aes(y = avg_new_percent_hosp_female, colour = "Female")) +
  geom_point(aes(y = avg_new_percent_hosp_female, colour = "Female")) +
  theme_classic() +
  labs(x="Date",y="Average Daily Hospitalization Percentage"
       ,caption="*Data from BR, AR, ES, PH, MX, IN.n=307362") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=7))+theme(plot.caption=element_text(size=5))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=6))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position="none")+
  scale_y_continuous(name="Daily Hospitalized  Percentage", limits=c(0, 100))