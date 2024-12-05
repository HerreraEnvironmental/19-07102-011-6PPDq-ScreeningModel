source('ScreeningModel.R')
source('plottingArguments.R')
#summarize Screening model scores output graphically and with tables
kc_roads_score %>%
  st_drop_geometry() %>%
  group_by(KC_FCC) %>%
  summarise(n=length(which(!is.na(TotalScore))),
            Score_Mean=mean(TotalScore,na.rm=T),
            Score_Median=median(TotalScore,na.rm=T),
            NoSW_n=n(),
            NoSWScore_Mean=mean(Imputed_SW_Score),
            NoSWScore_Median=median(Imputed_SW_Score))

example_score_segment_id<-c('16TH-11076','208TH-15051','FALL CITY-CARNATION-40224','PRESTON-FALL CITY-61575',
                            'UNION HILL-58754','I-5-41872','SR 18-81753','RAMP-62710')
kc_roads_score %>% 
  filter(RoadSegmentID %in% example_score_segment_id)

plot_ecdf_Scores<-kc_roads_score %>%
  ggplot(aes(TotalScore,col=KC_FCC,group=KC_FCC))+
  stat_ecdf()+
  theme_bw()
ggsave('plots/plot_ecdf_Scores.png',plot_ecdf_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores<-kc_roads_score %>%
  violin_plot_arguments_by_fcc(TotalScore,'')+
  scale_y_continuous('Screening Model Score',limits=c(0,5))
ggsave('plots/plot_jitter_Scores.png',plot_jitter_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores_antiLog<-kc_roads_score %>%
  mutate(TotalScore_AntiLog=10^TotalScore) %>%
  violin_plot_arguments_by_fcc(TotalScore_AntiLog,'')+
  scale_y_continuous('Screening Model Score (antilog)',
                     labels=scales::label_log(),
                     limits=c(0,10^5))
ggsave('plots/plot_jitter_Scores_antiLog.png',plot_jitter_Scores_antiLog,scale=0.8,width=10,height=4.5)

#noSW includes the areas without coverage (e.g., Yarrow Point), with imputed conveyance values
plot_ecdf_noSW_Scores<-kc_roads_score %>%
  ggplot(aes(Imputed_SW_Score,col=KC_FCC,group=KC_FCC))+
  stat_ecdf()+
  theme_bw()
ggsave('plots/plot_ecdf_noSW_Scores.png',plot_ecdf_noSW_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_noSW_Scores<-kc_roads_score %>%
  violin_plot_arguments_by_fcc(Imputed_SW_Score,'')+
  scale_y_continuous('Screening Model Score\n(Imputed Conveyance)',limits=c(0,5))
ggsave('plots/plot_jitter_noSW_Scores.png',plot_jitter_noSW_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_noSW_Scores_antiLog<-kc_roads_score %>%
  mutate(ImputedSW_Score_AntiLog=10^Imputed_SW_Score) %>%
  violin_plot_arguments_by_fcc(ImputedSW_Score_AntiLog,'')+
  scale_y_continuous('Screening Model Score (antilog)\n(Imputed Conveyance)',
                     labels=scales::label_log(),
                     limits=c(0,10^5))
ggsave('plots/plot_jitter_noSW_Scores_antiLog.png',plot_jitter_noSW_Scores_antiLog,scale=0.8,width=10,height=4.5)


#top 10s
top10_scores<-arrange(kc_roads_score,desc(TotalScore)) %>% st_drop_geometry()%>%slice(1:10) %>%
  mutate(RoadMiles=round(RoadMiles,1),
         across(c(TrafficIntensity:MediumVehicleCount,RoadwaySkirtImperviousness,PctConveyed),round)) %>%
  select(Juris,FULLNAME,RoadSegmentID,KC_FCC,RoadMiles,TrafficIntensity,HeavyVehicleCount,
         MediumVehicleCount,GenScore,RoadwaySkirtImperviousness,RoadwayDrainage,PctConveyed,StreamWaterCrossing,
         RoadwayConnectednessScore,TotalScore) 
top10_scores%>%
  write.csv('outputs/top10_roads.csv',row.names = F)

top10_scores_KC<-kc_roads_score %>%
  filter(Juris=='King County') %>%
  arrange(desc(TotalScore)) %>% 
  st_drop_geometry()%>%
  slice(1:10) %>%
  mutate(RoadMiles=round(RoadMiles,1),
         across(c(TrafficIntensity:MediumVehicleCount,RoadwaySkirtImperviousness,PctConveyed),round)) %>%
  select(Juris,FULLNAME,RoadSegmentID,KC_FCC,RoadMiles,TrafficIntensity,HeavyVehicleCount,
         MediumVehicleCount,GenScore,RoadwaySkirtImperviousness,RoadwayDrainage,PctConveyed,StreamWaterCrossing,
         RoadwayConnectednessScore,TotalScore) 
top10_scores_KC%>%
  write.csv('outputs/top10_roads_kc.csv',row.names = F)

