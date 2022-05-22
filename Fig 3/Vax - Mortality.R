# COVID-19 Project
## Figure 5
### Mortality % As a Function of Vaccinations %

ggplot (Vax_Mort_Comb, aes (x=year_week,y=percent_vaccinated))+
  geom_point()

Vax_Mortality$outlier = Vax_Mortality$new_confirmed < 0    
Vax_Mortality = filter(Vax_Mortality, outlier != TRUE)
Vax_Mortality <- subset(Vax_Mortality, , -c(outlier))

Vax_Mortality$outlier = Vax_Mortality$new_deceased < 0    
Vax_Mortality = filter(Vax_Mortality, outlier != TRUE)
Vax_Mortality <- subset(Vax_Mortality, , -c(outlier))

## Making Calculated Fields relevant to the question at hand

Vax_Mortality$cumulative_persons_fully_vaccinated <- (Vax_Mortality$cumulative_persons_fully_vaccinated +1)

Vax_Mortality$percent_deceased <- (Vax_Mortality$new_deceased/Vax_Mortality$new_confirmed)*100
Vax_Mortality$percent_vaccinated <- (Vax_Mortality$cumulative_persons_fully_vaccinated/Vax_Mortality$population)*100

Vax_Mortality$outlier = Vax_Mortality$percent_deceased > 100  
Vax_Mortality = filter(Vax_Mortality, outlier != TRUE)
Vax_Mortality <- subset(Vax_Mortality, , -c(outlier))

Vax_Mortality$outlier = Vax_Mortality$percent_vaccinated > 100  
Vax_Mortality = filter(Vax_Mortality, outlier != TRUE)
Vax_Mortality <- subset(Vax_Mortality, , -c(outlier))

percent_vaccinated_winsorized <-  Winsorize(Vax_Mortality$percent_vaccinated, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Vax_Mortality$percent_vax_win <- percent_vaccinated_winsorized ## adding winsorized information into the data frame

percent_deceased_winsorized <-  Winsorize(Vax_Mortality$percent_deceased, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Vax_Mortality$percent_dead_win <- percent_deceased_winsorized ## adding winsorized information into the data frame

Vax_Mortality_week <- Vax_Mortality
Vax_Mortality_week$year_week <- floor_date(Vax_Mortality_week$date, "week")

## Aggregating by month to make data that's not too choppy when plotting

Vax_Mort_Mean <- Vax_Mortality_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(percent_deceased = mean(percent_dead_win)) %>% 
  as.data.frame()

Vax_Mort_SD <- Vax_Mortality_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(SD_deceased = sd(percent_dead_win)) %>% 
  as.data.frame()

Vax_Mort_Mean2 <- Vax_Mortality_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(percent_vaccinated = mean(percent_vax_win)) %>% 
  as.data.frame()

Vax_Mort_SD2 <- Vax_Mortality_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(SD_vax = sd(percent_vax_win)) %>% 
  as.data.frame()

Vax_Mort_Comb <- cbind.data.frame(Vax_Mort_Mean, Vax_Mort_SD$SD_deceased,Vax_Mort_Mean2$percent_vaccinated,Vax_Mort_SD2$SD_vax)

Vax_Mort_Comb <- edit(Vax_Mort_Comb) ## to fix headers

Vax_Mort_Comb$year_week <- as.Date(Vax_Mort_Comb$year_week) ## changing year/month data to correct format

Vax_Mort_Comb$vax_SD <- (Vax_Mort_Comb$SD_vax + Vax_Mort_Comb$percent_vaccinated)
Vax_Mort_Comb$Dead_SD <- (Vax_Mort_Comb$percent_deceased - Vax_Mort_Comb$SD_deceased)

Fig5_Mortality <- ggplot(Vax_Mort_Comb, aes(year_week)) + 
  geom_line(aes(y = percent_vaccinated, colour = "Total Vaccinated")) +
  geom_point(aes(y = percent_vaccinated, colour = "Total Vaccinated")) +
  geom_errorbar(ymin=Vax_Mort_Comb$vax_SD,ymax=Vax_Mort_Comb$percent_vaccinated,color="turquoise4")+
  geom_line(aes(y = percent_deceased, colour = "Daily Deceased")) +
  geom_point(aes(y = percent_deceased, colour = "Daily Deceased")) +
  geom_errorbar(ymax=Vax_Mort_Comb$percent_deceased,ymin=Vax_Mort_Comb$Dead_SD,color="coral")+
  theme_classic()+
  labs(x="Date",y="Percentage"
       ,caption="*Data from 218 countries across America, Europe, Africa, and Asia. n=1208582") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=10))+theme(plot.caption=element_text(size=6))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position = c(.2,.9)) +
  scale_y_continuous(name="Percentage", limits=c(0, 80))

Vax_Mort_Comb$outlier = Vax_Mort_Comb$percent_vaccinated < 0.1    ## To get on December 27th 2020
Vax_Mort_Comb_Corr = filter(Vax_Mort_Comb, outlier != TRUE)
Vax_Mort_Comb_Corr <- subset(Vax_Mort_Comb_Corr, , -c(outlier))

cor.test(Vax_Mort_Comb_Corr$percent_vaccinated, Vax_Mort_Comb_Corr$percent_deceased, method = "spearman")

## Significant <0.001 w/ -0.419 Rho (moderate-weak)