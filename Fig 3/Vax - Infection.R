# COVID-19 Project
## Figure 5
### Infection % As a Function of Vaccinations %

ggplot (Vax_Infect_Week2, aes (x=year_month,y=avg_new_percent_infected))+
  geom_point()

##Removing Visually Obvious Outliers

Vax_Infection$outlier = Vax_Infection$new_confirmed < 0    
Vax_Infection = filter(Vax_Infection, outlier != TRUE)
Vax_Infection <- subset(Vax_Infection, , -c(outlier))

Vax_Infection$outlier = Vax_Infection$new_tested < 0    
Vax_Infection = filter(Vax_Infection, outlier != TRUE)
Vax_Infection <- subset(Vax_Infection, , -c(outlier))


Vax_Infection$outlier = Vax_Infection$new_tested >20000000    
Vax_Infection = filter(Vax_Infection, outlier != TRUE)
Vax_Infection <- subset(Vax_Infection, , -c(outlier))

## Making Calculated Fields relevant to the question at hand

Vax_Infection$percent_infection <- (Vax_Infection$new_confirmed/Vax_Infection$new_tested)*100
Vax_Infection$percent_vaccinated <- (Vax_Infection$cumulative_persons_fully_vaccinated/Vax_Infection$population)*100

Vax_Infection$outlier = Vax_Infection$percent_infection > 100    
Vax_Infection = filter(Vax_Infection, outlier != TRUE)
Vax_Infection <- subset(Vax_Infection, , -c(outlier))

Vax_Infection$outlier = Vax_Infection$percent_vaccinated > 100    
Vax_Infection = filter(Vax_Infection, outlier != TRUE)
Vax_Infection <- subset(Vax_Infection, , -c(outlier))

percent_infected_winsorized <-  Winsorize(Vax_Infection$percent_infection, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Vax_Infection$percent_infected_win <- percent_infected_winsorized ## adding winsorized information into the data frame


Vax_Infect_week <- Vax_Infection
Vax_Infect_month$year_month <- floor_date(Vax_Infect_month$date, "week")

## Aggregating by month to make data that's not too choppy when plotting

Vax_Infect_Mean <- Vax_Infect_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(percent_inf = mean(percent_infected_win)) %>% 
  as.data.frame()

Vax_Infect_Mean2 <- Vax_Infect_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(percent_vax = mean(percent_vaccinated)) %>% 
  as.data.frame()

Vax_Infect_SD <- Vax_Infect_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(SD_inf = sd(percent_infected_win)) %>% 
  as.data.frame()

Vax_Infect_SD2 <- Vax_Infect_month %>%
  group_by(year_month) %>% 
  dplyr::summarize(SD_vax = sd(percent_vaccinated)) %>% 
  as.data.frame()


Vax_Infect_Comb <- cbind.data.frame(Vax_Infect_Mean, Vax_Infect_Mean2$percent_vax, Vax_Infect_SD$SD_inf,Vax_Infect_SD2$SD_vax)

Vax_Infect_Comb <- edit(Vax_Infect_Comb) ## to fix headers

Vax_Infect_Comb$year_month <- as.Date(Vax_Infect_Comb$year_month) ## changing year/month data to correct format

Vax_Infect_Comb$infect_SU <- (Vax_Infect_Comb$percent_inf+Vax_Infect_Comb$SD_inf)
Vax_Infect_Comb$infect_SDo <- (Vax_Infect_Comb$percent_inf-Vax_Infect_Comb$SD_inf)

Vax_Infect_Comb$vax_SU <- (Vax_Infect_Comb$percent_vax+Vax_Infect_Comb$SD_vax)
Vax_Infect_Comb$vax_SDo <- (Vax_Infect_Comb$percent_vax-Vax_Infect_Comb$SD_vax)


Fig5_Infection <- ggplot(Vax_Infect_Comb, aes(year_month)) + 
  geom_line(aes(y = percent_vax, colour = "Total Vaccinated")) +
  geom_point(aes(y = percent_vax, colour = "Total Vaccinated")) +
  geom_line(aes(y = percent_inf, colour = "Daily Infected")) +
  geom_point(aes(y = percent_inf, colour = "Daily Infected")) +
  geom_errorbar(ymax=Vax_Infect_Comb$percent_inf,ymin=Vax_Infect_Comb$infect_SDo,color="coral")+
  geom_errorbar(ymax=Vax_Infect_Comb$vax_SU,ymin=Vax_Infect_Comb$percent_vax,color="turquoise4")+
  theme_classic()+
  labs(x="Date",y="Percentage"
       ,caption="*Data from 133 countries across America, Europe, Africa, and Asia. n=500555") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=10))+theme(plot.caption=element_text(size=6))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position = c(.2,.9))

## Spearman's R

Vax_Infect_Comb$outlier = Vax_Infect_Comb$percent_vax < 0.0001  ## To get on December 20th 2020  
Vax_Infect_Comb_Corr = filter(Vax_Infect_Comb, outlier != TRUE)
Vax_Infect_Comb_Corr <- subset(Vax_Infect_Comb_Corr, , -c(outlier))

cor.test(Vax_Infect_Comb_Corr$percent_vax, Vax_Infect_Comb_Corr$percent_inf, method = "spearman")

## Not significant p = 0.18 Rho = -0.159
