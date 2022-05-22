# COVID-19 Project
## Figure 5
### Hospitalization % As a Function of Vaccinations %

ggplot (Vax_Hospitalization, aes (x=date,y=percent_hosp_win))+
  geom_point()

Vax_Hospitalization$outlier = Vax_Hospitalization$new_confirmed < 0    
Vax_Hospitalization = filter(Vax_Hospitalization, outlier != TRUE)
Vax_Hospitalization <- subset(Vax_Hospitalization, , -c(outlier))

Vax_Hospitalization$outlier = Vax_Hospitalization$new_hospitalized_patients < 0    
Vax_Hospitalization = filter(Vax_Hospitalization, outlier != TRUE)
Vax_Hospitalization <- subset(Vax_Hospitalization, , -c(outlier))


Vax_Hospitalization$percent_hospitalized <- (Vax_Hospitalization$new_hospitalized_patients/Vax_Hospitalization$new_confirmed)*100
Vax_Hospitalization$percent_vaccinated <- (Vax_Hospitalization$cumulative_persons_fully_vaccinated/Vax_Hospitalization$population)*100

Vax_Hospitalization$outlier = Vax_Hospitalization$percent_hospitalized > 100    
Vax_Hospitalization = filter(Vax_Hospitalization, outlier != TRUE)
Vax_Hospitalization <- subset(Vax_Hospitalization, , -c(outlier))

Vax_Hospitalization$outlier = Vax_Hospitalization$percent_vaccinated > 100    
Vax_Hospitalization = filter(Vax_Hospitalization, outlier != TRUE)
Vax_Hospitalization <- subset(Vax_Hospitalization, , -c(outlier))

percent_vaccinated_winsorized <-  Winsorize(Vax_Hospitalization$percent_vaccinated, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Vax_Hospitalization$percent_vax_win <- percent_vaccinated_winsorized ## adding winsorized information into the data frame

percent_hospitalized_winsorized <-  Winsorize(Vax_Hospitalization$percent_hospitalized, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Vax_Hospitalization$percent_hosp_win <- percent_hospitalized_winsorized ## adding winsorized information into the data frame

Vax_Hospitalization_week <- Vax_Hospitalization
Vax_Hospitalization_week$year_week <- floor_date(Vax_Hospitalization_week$date, "week")

## Aggregating by month to make data that's not too choppy when plotting

Vax_Hosp_Mean <- Vax_Hospitalization_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(percent_hospitalized = mean(percent_hosp_win)) %>% 
  as.data.frame()

Vax_Hosp_Mean2 <- Vax_Hospitalization_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(percent_vaccinated = mean(percent_vax_win)) %>% 
  as.data.frame()

Vax_Hosp_SD <- Vax_Hospitalization_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(hospitalized_sd = sd(percent_hosp_win)) %>% 
  as.data.frame()

Vax_Hosp_SD2 <- Vax_Hospitalization_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(vaccinated_sd = sd(percent_vax_win)) %>% 
  as.data.frame()

Vax_Hosp_Comb <- cbind.data.frame(Vax_Hosp_Mean, Vax_Hosp_SD$hospitalized_sd,Vax_Hosp_Mean2$percent_vaccinated,Vax_Hosp_SD2$vaccinated_sd)

Vax_Hosp_Comb <- edit(Vax_Hosp_Comb) ## To fix headers

Vax_Hosp_Comb$year_week <- as.Date(Vax_Hosp_Comb$year_week) ## changing year/month data to correct format

Vax_Hosp_Comb$vax_SD <- (Vax_Hosp_Comb$vaccinated_sd + Vax_Hosp_Comb$percent_vaccinated)
Vax_Hosp_Comb$Hosp_SD <- (Vax_Hosp_Comb$percent_hospitalized - Vax_Hosp_Comb$hospitalized_sd)

Fig5_Hospitalization <- ggplot(Vax_Hosp_Comb, aes(year_week)) + 
  geom_line(aes(y = percent_vaccinated, colour = "Total Vaccinated")) +
  geom_point(aes(y = percent_vaccinated, colour = "Total Vaccinated")) +
  geom_errorbar(ymin=Vax_Hosp_Comb$vax_SD,ymax=Vax_Hosp_Comb$percent_vaccinated,color="turquoise4")+
  geom_line(aes(y = percent_hospitalized, colour = "Daily Hospitalized")) +
  geom_point(aes(y = percent_hospitalized, colour = "Daily Hospitalized")) +
  geom_errorbar(ymax=Vax_Hosp_Comb$percent_hospitalized,ymin=Vax_Hosp_Comb$Hosp_SD,color="coral")+
  theme_classic()+
  labs(x="Date",y="Percentage"
       ,caption="*Data from BR, US, CH, LI, FR, BE, AR, ES, UK, MX, ML, HK, JP, TH, PH. n=231746") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=10))+theme(plot.caption=element_text(size=6))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.45, 'cm'))+theme(legend.position = c(.25,.9)) +
  scale_y_continuous(name="Percentage", limits=c(0, 80))

## Spearman's R

Vax_Hosp_Comb$outlier = Vax_Hosp_Comb$percent_vaccinated < 0.1   ## To start at December 27th 2020 
Vax_Hosp_Comb_Corr = filter(Vax_Hosp_Comb, outlier != TRUE)
Vax_Hosp_Comb_Corr <- subset(Vax_Hosp_Comb_Corr, , -c(outlier))

cor.test(Vax_Hosp_Comb_Corr$percent_vaccinated, Vax_Hosp_Comb_Corr$percent_hospitalized, method = "spearman")

# Not significant p = 0.3 Rho = -0.122