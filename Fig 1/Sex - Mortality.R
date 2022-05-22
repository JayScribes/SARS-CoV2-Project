# COVID-19 Project
## Figure 2
### Mortality % After Confirmed Infection in Males and Females

ggplot (Sex_Mort_MMP, aes (x=year_month,y=avg_new_mortality_male))+
  geom_point()

Sex_Mortality$outlier = Sex_Mortality$new_confirmed_male > 20000    # A group of outliers was present, this removes them.
Sex_Mortality = filter(Sex_Mortality, outlier != TRUE)
Sex_Mortality <- subset(Sex_Mortality, , -c(outlier))

Sex_Mortality$outlier = Sex_Mortality$new_confirmed_male < 0   # A group of outliers was present, this removes them.
Sex_Mortality = filter(Sex_Mortality, outlier != TRUE)
Sex_Mortality <- subset(Sex_Mortality, , -c(outlier))

Sex_Mortality$outlier = Sex_Mortality$new_deceased_female < 0    # A group of outliers was present, this removes them.
Sex_Mortality = filter(Sex_Mortality, outlier != TRUE)
Sex_Mortality <- subset(Sex_Mortality, , -c(outlier))

Sex_Mortality$outlier = Sex_Mortality$new_deceased_male < 0    # A group of outliers was present, this removes them.
Sex_Mortality = filter(Sex_Mortality, outlier != TRUE)
Sex_Mortality <- subset(Sex_Mortality, , -c(outlier))

## Aggregating Data by unique week to smooth out variability and make it clearer


Sex_Mortality$percent_mortality_male <- (Sex_Mortality$new_deceased_male/Sex_Mortality$new_confirmed_male)*100
Sex_Mortality$percent_mortality_female <- (Sex_Mortality$new_deceased_female/Sex_Mortality$new_confirmed_female)*100

Sex_Mort_month <- Sex_Mortality
Sex_Mort_month$year_month <- floor_date(Sex_Mort_month$date, "week")

## Many non-sense points are coming up due to higher hospitalization rates than confirmed rates. Will Winsorize data.

percent_mortality_male_winsorized <-  Winsorize(Sex_Mort_month$percent_mortality_male, minval = NULL, maxval = NULL, probs = c(0.00,0.98), na.rm = FALSE, type =1)
Sex_Mort_month$percent_mortality_male_win <- percent_mortality_male_winsorized ## adding winsorized information into the data frame

percent_mortality_female_winsorized <-  Winsorize(Sex_Mort_month$percent_mortality_female, minval = NULL, maxval = NULL, probs = c(0.00,0.98), na.rm = FALSE, type =1)
Sex_Mort_month$percent_mortality_female_win <- percent_mortality_female_winsorized ## adding winsorized information into the data frame

## Getting descriptive stats out of the Winsorized data and aggregating by month

Sex_Mort_MFP <- Sex_Mort_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_mortality_female = mean(percent_mortality_female_win)) %>% 
  as.data.frame()

Sex_Mort_MMP <- Sex_Mort_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_mortality_male = mean(percent_mortality_male_win)) %>% 
  as.data.frame()

Sex_Mort_SDFP <- Sex_Mort_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_mortality_female = sd(percent_mortality_female_win)) %>% 
  as.data.frame()

Sex_Mort_SDMP <- Sex_Mort_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_mortality_male = sd(percent_mortality_male_win)) %>% 
  as.data.frame()

remove(Sex_Infect_MFP, Sex_Infect_MMP, Sex_Infect_SDFP, Sex_Infect_SDMP, Sex_Infect_month) ## to clean environment

## Creating a data frame with all relevant information for graphing
Sex_Mort_month_combined <- cbind.data.frame(Sex_Mort_MFP$year_month, Sex_Mort_MFP$avg_new_mortality_female, Sex_Mort_MMP$avg_new_mortality_male,
                                              Sex_Mort_SDFP$sd_new_mortality_female,Sex_Mort_SDMP$sd_new_mortality_male)

Sex_Mort_month_combined <- edit(Sex_Mort_month_combined) ## to change header names

Sex_Mort_month_combined$year_month <- as.Date(Sex_Mort_month_combined$year_month) ## changing year/month data to correct format

## Plotting the data

Fig2_Mortality <- ggplot(Sex_Mort_month_combined, aes(year_month)) + 
  geom_line(aes(y = avg_new_mortality_male, colour = "Male")) +
  geom_point(aes(y = avg_new_mortality_male, colour = "Male")) +
  geom_line(aes(y = avg_new_mortality_female, colour = "Female")) +
  geom_point(aes(y = avg_new_mortality_female, colour = "Female")) +
  theme_classic() +
  labs(x="Date",y="Average Daily Mortality Rate"
       ,caption="*Data from DE, ES, PH,IN, BR, MX, PE, MY, CZ, AR, CO, US.n=236683") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=7))+theme(plot.caption=element_text(size=5))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=8))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position = c(.9,.75))+
  scale_y_continuous(name="Daily Deceased Percentage", limits=c(0, 100))