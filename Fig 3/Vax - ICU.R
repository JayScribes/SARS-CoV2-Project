# COVID-19 Project
## Figure 5
### MICU % As a Function of Vaccinations %

ggplot (Vax_ICU, aes (x=date,y=percent_ICU_win))+
  geom_point()

Vax_ICU$outlier = Vax_ICU$new_confirmed < 0    
Vax_ICU = filter(Vax_ICU, outlier != TRUE)
Vax_ICU <- subset(Vax_ICU, , -c(outlier))

Vax_ICU$outlier = Vax_ICU$new_ICU < 0    
Vax_ICU = filter(Vax_ICU, outlier != TRUE)
Vax_ICU <- subset(Vax_ICU, , -c(outlier))

## Calculated Fields

Vax_ICU$percent_ICU <- (Vax_ICU$new_ICU/Vax_ICU$new_confirmed)*100
Vax_ICU$percent_vaccinated <- (Vax_ICU$cumulative_persons_fully_vaccinated/Vax_ICU$population)*100

Vax_ICU$outlier = Vax_ICU$percent_ICU > 100    
Vax_ICU = filter(Vax_ICU, outlier != TRUE)
Vax_ICU <- subset(Vax_ICU, , -c(outlier))

Vax_ICU$outlier = Vax_ICU$percent_vaccinated > 100    
Vax_ICU = filter(Vax_ICU, outlier != TRUE)
Vax_ICU <- subset(Vax_ICU, , -c(outlier))

percent_vaccinated_winsorized <-  Winsorize(Vax_ICU$percent_vaccinated, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Vax_ICU$percent_vax_win <- percent_vaccinated_winsorized ## adding winsorized information into the data frame

percent_ICU_winsorized <-  Winsorize(Vax_ICU$percent_ICU, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
Vax_ICU$percent_ICU_win <- percent_ICU_winsorized ## adding winsorized information into the data frame

Vax_ICU_week <- Vax_ICU
Vax_ICU_week$year_week <- floor_date(Vax_ICU_week$date, "week")

## Aggregating by month to make data that's not too choppy when plotting

Vax_ICU_Mean <- Vax_ICU_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(percent_ICU = mean(percent_ICU_win)) %>% 
  as.data.frame()

Vax_ICU_Mean2 <- Vax_ICU_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(percent_vaccinated = mean(percent_vax_win)) %>% 
  as.data.frame()

Vax_ICU_SD <- Vax_ICU_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(SD_ICU = SD(percent_ICU_win)) %>% 
  as.data.frame()

Vax_ICU_SD2 <- Vax_ICU_week %>% 
  group_by(year_week) %>% 
  dplyr::summarize(SD_vax = SD(percent_vax_win)) %>% 
  as.data.frame()

Vax_ICU_Comb <- cbind.data.frame(Vax_ICU_Mean, Vax_ICU_SD$SD_ICU,Vax_ICU_Mean2$percent_vaccinated,Vax_ICU_SD2$SD_vax)

Vax_ICU_Comb <- edit(Vax_ICU_Comb) ## to fix headers

Vax_ICU_Comb$year_week <- as.Date(Vax_ICU_Comb$year_week) ## changing year/month data to correct format

Vax_ICU_Comb$SD_vaxg <- (Vax_ICU_Comb$SD_vax + Vax_ICU_Comb$percent_vaccinated)
Vax_ICU_Comb$SD_ICUg <- (Vax_ICU_Comb$percent_ICU - Vax_ICU_Comb$SD_ICU)

Fig5_ICU <- ggplot(Vax_ICU_Comb, aes(year_week)) + 
  geom_line(aes(y = percent_vaccinated, colour = "Total Vaccinated")) +
  geom_point(aes(y = percent_vaccinated, colour = "Total Vaccinated")) +
  geom_errorbar(ymin=Vax_ICU_Comb$SD_vaxg,ymax=Vax_ICU_Comb$percent_vaccinated,color="turquoise4")+
  geom_line(aes(y = percent_ICU, colour = "Daily ICU")) +
  geom_point(aes(y = percent_ICU, colour = "Daily ICU")) +
  geom_errorbar(ymax=Vax_ICU_Comb$percent_ICU,ymin=Vax_ICU_Comb$SD_ICUg,color="coral")+
  theme_classic()+
  labs(x="Date",y="Percentage"
       ,caption="*Data from BR, US, CH, FR, AR, ES, MX, SW. n=195026") +
  theme(plot.title=element_text(size=12))+theme(axis.title=element_text(size=10))+theme(plot.caption=element_text(size=6))+
  theme(legend.title=element_blank()) + theme(plot.caption=element_text(hjust=0)) + theme(plot.title=element_text(hjust=-0.1))+
  theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, 'cm'))+theme(legend.position = c(.2,.9)) +
  scale_y_continuous(name="Percentage", limits=c(0, 80))

## Spearman's R

Vax_ICU_Comb$outlier = Vax_ICU_Comb$percent_vaccinated < 0.1  ## To start at December 27th 2020  
Vax_ICU_Comb_Corr = filter(Vax_ICU_Comb, outlier != TRUE)
Vax_ICU_Comb_Corr <- subset(Vax_ICU_Comb_Corr, , -c(outlier))

cor.test(Vax_ICU_Comb_Corr$percent_vaccinated, Vax_ICU_Comb_Corr$percent_ICU, method = "spearman")

# Significant p < 0.001 Rho = -0.61 (moderate-strong)