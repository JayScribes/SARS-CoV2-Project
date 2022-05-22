# COVID-19 Project
## Figure 2
### Infection % After Testing in Males and Females

ggplot (Sex_Infect_MFP, aes (x=year_month,y=avg_new_percent_infected_female))+
  geom_point()

Sex_Infection$outlier = Sex_Infection$new_tested_male > 20000    # A group of outliers was present, this removes them.
Sex_Infection = filter(Sex_Infection, outlier != TRUE)
Sex_Infection <- subset(Sex_Infection, , -c(outlier))

Sex_Infection$outlier = Sex_Infection$new_confirmed_male > 20000    # A group of outliers was present, this removes them.
Sex_Infection = filter(Sex_Infection, outlier != TRUE)
Sex_Infection <- subset(Sex_Infection, , -c(outlier))

## Aggregating Data by unique week to smooth out variability and make it clearer


Sex_Infection$percent_infected_male <- (Sex_Infection$new_confirmed_male/Sex_Infection$new_tested_male)*100
Sex_Infection$percent_infected_female <- (Sex_Infection$new_confirmed_female/Sex_Infection$new_tested_female)*100


Sex_Infect_month <- Sex_Infection
Sex_Infect_month$year_month <- floor_date(Sex_Infect_month$date, "week")

## Many non-sense points are coming up due to higher hospitalization rates than confirmed rates. Will Winsorize data.

percent_infected_male_winsorized <-  Winsorize(Sex_Infect_month$percent_infected_male, minval = NULL, maxval = NULL, probs = c(0.02,0.98), na.rm = FALSE, type =1)
Sex_Infect_month$percent_infected_male_win <- percent_infected_male_winsorized ## adding winsorized information into the data frame

percent_infected_female_winsorized <-  Winsorize(Sex_Infect_month$percent_infected_female, minval = NULL, maxval = NULL, probs = c(0.02,0.98), na.rm = FALSE, type =1)
Sex_Infect_month$percent_infected_female_win <- percent_infected_female_winsorized ## adding winsorized information into the data frame

## Getting descriptive stats out of the Winsorized data and aggregating by month


Sex_Infect_MFP <- Sex_Infect_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_percent_infected_female = mean(percent_infected_female_win)) %>% 
  as.data.frame()

Sex_Infect_MMP <- Sex_Infect_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(avg_new_percent_infected_male = mean(percent_infected_male_win)) %>% 
  as.data.frame()

Sex_Infect_SDFP <- Sex_Infect_month %>% 
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_percent_infected_female = sd(percent_infected_female_win)) %>% 
  as.data.frame()

Sex_Infect_SDMP <- Sex_Infect_month %>%  
  group_by(year_month) %>% 
  dplyr::summarize(sd_new_percent_infected_male = sd(percent_infected_male_win)) %>% 
  as.data.frame()

remove(Sex_Infect_MFP, Sex_Infect_MMP, Sex_Infect_SDFP, Sex_Infect_SDMP, Sex_Infect_month) ## to clean environment

## Creating a data frame with all relevant information for graphing
Sex_infect_month_combined <- cbind.data.frame(Sex_Infect_MFP$year_month, Sex_Infect_MFP$avg_new_percent_infected_female, Sex_Infect_MMP$avg_new_percent_infected_male,
                                            Sex_Infect_SDFP$sd_new_percent_infected_female,Sex_Infect_SDMP$sd_new_percent_infected_male)

Sex_infect_month_combined <- edit(Sex_infect_month_combined) ## to change header names

Sex_infect_month_combined$year_month <- as.Date(Sex_infect_month_combined$year_month) ## changing year/month data to correct format

## Plotting the data

Fig2_Infection <- ggplot(Sex_infect_month_combined, aes(year_month)) + 
  geom_line(aes(y = avg_new_percent_infected_male, colour = "Male")) +
  geom_point(aes(y = avg_new_percent_infected_male, colour = "Male")) +
  geom_line(aes(y = avg_new_percent_infected_female, colour = "Female")) +
  geom_point(aes(y = avg_new_percent_infected_female, colour = "Female")) +
  theme_classic() +
  labs(x="Date",y="Average Daily Infected Percentage"
       ,caption="*Data from BR, AR, EE, US.n=962275") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=7))+theme(plot.caption=element_text(size=5))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=6))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position="none") +
  scale_y_continuous(name="Daily Infected Percentage", limits=c(0, 100))