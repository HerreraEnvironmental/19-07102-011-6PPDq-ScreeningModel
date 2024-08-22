library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(cowplot)

# emme_links<-st_read('inputs/emme_links_with_hvy_med/emme_links.shp') 
# st_crs(emme_links)<-2926
# 
# # st_write(emme_links,'outputs/emme_links_proj.shp')
# # 
# # zip('outputs/emme_links_project.zip',list.files('outputs/','emme',full.names = T))
# 
# st_address<-st_read(paste0("https://gisdata.kingcounty.gov/arcgis/rest/services/",
#                            "OpenDataPortal/transportation__st_address_line/MapServer/108/query?outFields=*&where=1%3D1&f=geojson"))
# 
# st_address_ID<-st_address%>%
#   group_by(ST_NAME) %>%
#   mutate(RoadSegmentID=paste0(ST_NAME,'-',1:n()))
# NOTE OBJECT IDs are different between st_address and the KC_RAODS
# st_address %>%
#   slice(1:10000)  %>%
#   select(OBJECTID:DIRSUFFIX,RoadSegmentID) %>%
#   arrange(RoadSegmentID)
# 
# jurisdictions<-st_read(paste0("https://gisdata.kingcounty.gov/arcgis/rest/",
#                           "services/OpenDataPortal/admin___base/MapServer/",
#                           "446/query?outFields=*&where=1%3D1&f=geojson"))
# st_address_juris<-st_address %>%
#   select(FRADDL:FULLNAME,Shape_Length) %>%
#   st_join(jurisdictions %>% select(CITYNAME)) %>%
#   mutate(JurisFROMJOIN=ifelse(grepl('FWY|SR|RAMP',FULLNAME),'WSDOT',CITYNAME)) %>%
#   filter(!grepl('.',row.names(.),fixed = T))

hec_gdb<-'\\\\herrera.local\\hecnet\\gis-k\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE6ppdq_20240717.gdb'

st_layers(hec_gdb)

hec_gis_export<-
#read_excel('inputs/KC_Street_Address_SummaryAttributes_20240223_v2.xlsx') %>%
#read_excel('O:\\proj\\Y2019\\19-07102-011\\GIS\\KC_Street_Address_SummaryAttributes_20240718_v2.xlsx')
 st_read(hec_gdb,
         layer = 'King_County_St_Addresses_6PPDQ_Metrics_20240718')
   
kc_roads_SM<-
  hec_gis_export %>%  
  st_transform(4326) %>%
 # left_join(ditch_estimates) %>%
 # mutate(DitchLen_FT=as.numeric(DitchLength)) %>%
  transmute(FRADDL,FRADDR,TOADDL,TOADDR,FULLNAME,
            RoadSegmentID,
            KC_FCC=factor(KC_FCC,levels=c('L','C','M','P','F'),
                       labels=c('Local','Collector','Minor','Primary','Freeway')),
            Juris=Jurisdiction,
            TAZ,
            emme_linkID,
            ADT_PSRC=ADT_tveh_PSRC,
            MediumVehicle=MedVehicle_PSRC,
            HeavyVehicle=HeavyVehicle_PSRC,
            CurbLen_FT=ifelse(is.na(CurbLen_FT),0,CurbLen_FT),
            PctCurbed=CurbLen_FT/Shape_Leng*100,
            GutterLen_FT =ifelse(is.na(GutterLen_FT ),0,GutterLen_FT ),
            PctGuttered=GutterLen_FT/Shape_Leng*100,
            RdSkirtPctImp=RdSkirtPctImpervious*100,
    Grade,
            RdSkirtPctOverwater=100*as.numeric(ifelse(is.na(RdSkirtPctOverwater),0,RdSkirtPctOverwater)),
            StreamWaterCrossing=ifelse(is.na(StreamWaterCrossing),0,StreamWaterCrossing),
    isRamp=ifelse(is.na(isRamp),0,isRamp),
    isOverpass=ifelse(is.na(isOverpass),0,isOverpass),
    isUnderpass=ifelse(is.na(isUnderpass),0,isUnderpass),
             OpenConvey=Ditch|Swales|Streams,
             ClosedConvey=Pipe,
             Convey=OpenConvey|ClosedConvey,
            across(PipeLen_FT:StreamLen_FT ,function(x) ifelse(is.na(x),0,as.numeric(x))),
             ConveyType=case_when(DitchLen_FT>0~'Ditched',
                                  PipeLen_FT>0~'Piped',
                                  SwaleLen_FT>0~'Swale',
                                  StreamLen_FT>0~'Stream',
                                  T~'None') %>% factor(levels=c('None','Ditched','Swale','Stream','Piped')),
            ConveyLen_FT=(PipeLen_FT +DitchLen_FT+SwaleLen_FT+StreamLen_FT),
            PctConveyed=ConveyLen_FT/Shape_Leng*100,
            PctConveyed=ifelse(PctConveyed>100,100,PctConveyed),
    #remove conveyance for non-evalueated stormwater juris
            PctConveyed=ifelse(Juris %in% c("King County",'Seattle','WSDOT'),PctConveyed,NA), 
    ConveyType=if_else(Juris %in% c("King County",'Seattle','WSDOT'),ConveyType,NA), 
            BusTraffic =ifelse(is.na(BusTraffic ),0,BusTraffic ),
            Shape_Length=Shape_Leng,
             RoadMiles=Shape_Leng/5280
         ) #%>%
  #right_join(st_address_juris %>% select(FRADDL:FULLNAME,Shape_Length,JurisFROMJOIN),.) %>%
  #mutate(JurisFinal=ifelse(Juris=='WSDOT',Juris,JurisFROMJOIN)) #%>%
 # filter(JurisFinal %in% c('WSDOT','King County') ) %>%
 # arrange(OBJECTID)
  
nrow(kc_roads_SM) #89309
#nrow(st_address) #89458

kc_roads_SM %>%
  st_drop_geometry() %>%
  group_by(KC_FCC) %>%
  summarise(n=n(),
            AveRoadLength=mean(RoadMiles),
            TotalRoadMiles=sum(RoadMiles),
            AveADT=mean(ADT_PSRC,na.rm=T),
          #  VehicleRoadMiles=sum(RoadMiles*ADT_PSRC,na.rm=T),
            AveMediumVehicles=mean(MediumVehicle,na.rm=T),
          #  HeavyMediumRoadMiles=sum(RoadMiles*MediumVehicle,na.rm=T),
            #HeavyVehicles=mean(FreightRank,na.rm=T),
            AveHeavyVehicles=mean(HeavyVehicle,na.rm=T),
           # HeavyVehicleRoadMiles=sum(RoadMiles*HeavyVehicle,na.rm=T),
         #   RoadMilesConvey=sum(RoadMiles*Convey,na.rm=T),
            PctConveyed=mean(PctConveyed,na.rm=T),
            PctOverwater=mean(RdSkirtPctOverwater,na.rm=T),
            RdSkirtPctImp=mean(RdSkirtPctImp,na.rm=T)) %>%
  write.csv('outputs/kc_wsdot_roads_summar.csv',row.names = F)

#length of each road segement
kc_roads_SM %>%
  ggplot(aes(RoadMiles,after_stat(count),fill=KC_FCC))+
  geom_density(alpha=0.5)+
  theme_bw()+
  scale_x_log10(label=scales::number,breaks=c(0.001,.01,0.1,0.5,1,5,10),
                minor_breaks=NULL)+
  scale_fill_viridis_d()


plot_dist_ADT_count<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC))+
  geom_density(alpha=.5)+
  geom_density(alpha=.5)+
  scale_x_log10('Average Daily Traffic',
                limits=c(10,200000),#breaks=c(100,500,1000,5000,10000,50000,100000,200000),
                minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()+
  scale_fill_viridis_d()#+
#  geom_vline(xintercept=c(500,5000,15000))+
 # annotate('label',x=c(100,1000,8000,25000),y=1.5,label=c('Low','Mod.','High','V. High'))

plot_ADT_FCC<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,fill=KC_FCC))+
  geom_density(alpha=.5)+
  geom_density(alpha=.5)+
  scale_x_log10('Average Daily Traffic',
                limits=c(10,200000),#breaks=c(100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()+
  scale_fill_viridis_d()#+
 # geom_vline(xintercept=c(500,5000,15000))+
  #annotate('label',x=c(100,1000,8000,25000),y=1.5,label=c('Low','Mod.','High','V. High'))
ggsave('plots/adt_by_fcc.png',plot_ADT_FCC,scale=0.8,width=10,height=4.5)

plot_ADT_FCC_no_FW_locals<-kc_roads_SM %>%
  filter(KC_FCC %in% c('Collector','Minor','Primary','Freeway')) %>%
  ggplot(aes(ADT_PSRC,fill=KC_FCC))+
  geom_density(alpha=.5)+
  scale_x_log10('Average Daily Traffic',
                limits=c(10,200000),#breaks=c(100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()+
  scale_fill_viridis_d()#+
 # geom_vline(xintercept=c(500,5000,15000))+
  #annotate('label',x=c(100,1000,8000,25000),y=1.5,label=c('Low','Mod.','High','V. High'))

kc_roads_SM %>%
  ggplot(aes(MediumVehicle))+
  geom_density(alpha=.5)+
  scale_x_log10('Medium Vehicle Count (per day)',
                limits=c(1,50000),#breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()+
  scale_fill_viridis_d() #+
#geom_vline(xintercept = c(5,50,1000))+
# annotate('label',x=c(1,30,500,5000),y=0.5,label=c('Low','Mod.','High','V. High'))

plot_dist_med_freight_count<-kc_roads_SM %>%
  ggplot(aes(MediumVehicle,fill=KC_FCC))+
  geom_density(alpha=.5)+
  scale_x_log10('Medium Vehicle Count (per day)',
                limits=c(1,50000),#breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()+
  scale_fill_viridis_d() #+
#geom_vline(xintercept = c(5,50,1000))+
# annotate('label',x=c(1,30,500,5000),y=0.7,label=c('Low','Mod.','High','V. High'))
ggsave('plots/freight_med_by_fcc.png',plot_dist_med_freight_count,scale=0.8,width=10,height=4.5)

kc_roads_SM %>%
  ggplot(aes(HeavyVehicle))+
  geom_density(alpha=.5)+
  scale_x_log10('Heavy Vehicle Count (per day)',
                limits=c(1,50000),#breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()+
  scale_fill_viridis_d() #+
  #geom_vline(xintercept = c(5,50,1000))+
 # annotate('label',x=c(1,30,500,5000),y=0.5,label=c('Low','Mod.','High','V. High'))

plot_dist_freight_count<-kc_roads_SM %>%
  ggplot(aes(HeavyVehicle,fill=KC_FCC))+
  geom_density(alpha=.5)+
  scale_x_log10('Heavy Vehicle Count (per day)',
                limits=c(1,50000),#breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw() +
  scale_fill_viridis_d()#+
  #geom_vline(xintercept = c(5,50,1000))+
 # annotate('label',x=c(1,30,500,5000),y=0.7,label=c('Low','Mod.','High','V. High'))
ggsave('plots/freight_by_fcc.png',plot_dist_freight_count,scale=0.8,width=10,height=4.5)


#with emme link output
# emme_links %>%
#   ggplot(aes(X.dyhtrk))+
#   geom_density(alpha=.5)+
#   scale_x_log10()+
#   theme_bw() +
#   geom_vline(xintercept = c(5,50,1000))

plot_ADT_med_Freight<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,MediumVehicle))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()+scale_y_log10()+
  geom_abline(slope=1)

plot_ADT_Freight<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,HeavyVehicle))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()+scale_y_log10()+
  geom_abline(slope=1)

plot_med_Freight<-kc_roads_SM %>%
  ggplot(aes(MediumVehicle,HeavyVehicle))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()+scale_y_log10()+
  geom_abline(slope=1)

plot_ADT_Imp<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,RdSkirtPctImp))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()

plot_Imp_FCC<-kc_roads_SM %>%
  ggplot(aes(RdSkirtPctImp,fill=KC_FCC))+
  geom_density(alpha=.5)+
  theme_bw()+
  xlab('Road Skirt Imperviousness (%)')+
  scale_fill_viridis_d()
#  geom_vline(xintercept = c(.25,0.5))+
  # ggtitle("Roadway Connectedness")+
  # annotate('label',y=6,x=c(.75,0.375,.125),label=c('High\n(>50% imp & Piped)',
  #                                       'Moderate\n(>25% Imp\nOR\n>50% Imp &\nConveyance Present)',
  #                                       'Low\n(<25% Imp)'))
ggsave('plots/imperviousness_by__FCC.png',plot_Imp_FCC,scale=0.8,width=10,height=4.5)

plot_convey_FCC<-kc_roads_SM %>%
  ggplot(aes(PctConveyed,fill=KC_FCC))+
  geom_density(alpha=.5)+
  theme_bw()+
  xlab('Conveyance (%)')+
  scale_fill_viridis_d()
ggsave('plots/conveyance_by_FCC.png',plot_convey_FCC,scale=0.8,width=10,height=4.5)

# plot_convey_FCC<-kc_roads_SM %>%
#   ggplot(aes(ConveyType))+
#   geom_bar(stat='count')+
#   scale_y_log10()+
#   facet_wrap(~KC_FCC)+
#   theme_bw()

plot_convey_imp<-kc_roads_SM %>%
  ggplot(aes(RdSkirtPctImp,fill=ConveyType))+
  geom_density(alpha=.5)+
  facet_wrap(~KC_FCC,scales = 'free_y')+
  theme_bw()+
  scale_fill_viridis_d()
ggsave('plots/imperviousness_by_conveyance_FCC.png',plot_convey_imp,scale=0.8,width=10,height=4.5)

plot_convey_type<-kc_roads_SM %>%
  filter(ConveyType!='Stream') %>%
  ggplot(aes(RdSkirtPctImp,fill=ConveyType))+
  geom_density(alpha=.5)+
  facet_wrap(~KC_FCC,scales = 'free_y')+
  theme_bw()+
  scale_x_continuous('Road Skirt Imperviousness (%)')+
  scale_fill_viridis_d()#+
  # scale_y_log10('count',limits=c(1,NA),
  #               labels=scales::label_number(scale_cut = scales::cut_short_scale()),
  #               minor_breaks=c(1:10,10*1:10,100*1:10,1000*1:10))
ggsave('plots/conveyance_by_type_FCC.png',plot_convey_type,scale=0.8,width=10,height=4.5)

plot_convey_type_impcat<-kc_roads_SM %>%
  mutate(ImpervCat=ifelse(is.na(RdSkirtPctImp)|RdSkirtPctImp<25,'<25%',
         ifelse(RdSkirtPctImp<50,'>=25 to 50%','>=50%'))) %>%
  filter(ConveyType!='Stream') %>%
  ggplot()+
  geom_bar(aes(x=ImpervCat,y=after_stat(count),fill=ConveyType,group=ConveyType))+
  facet_wrap(~KC_FCC,
             scales='free_y')+
  scale_x_discrete('Imperviousness Range')+
  scale_y_continuous('Count (# Segments)')+
  scale_fill_viridis_d('Conveyance Type')+
  theme_bw()
ggsave('plots/conveyance_by_type_FCC_impcat.png',plot_convey_type_impcat,scale=0.8,width=10,height=4.5)

# predict conveyance based on imperviousness and conveyance
kc_roads_SM %>%
  filter(!is.na(ConveyType)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=PctConveyed))+
  geom_point(aes(col=ConveyType))+
  facet_wrap(~KC_FCC)+
  geom_smooth(span=.9)+
  theme_bw()

library(tidyr);library(purrr)
model_conveyPct<-kc_roads_SM %>%
  filter(!is.na(ConveyType)) %>%
  group_by(KC_FCC) %>%
  nest() %>%
  mutate(loessModel=map(.x=data,.f=~loess(PctConveyed~RdSkirtPctImp,data=.x,span=.9)))

kc_roads_SM_predConvey<-kc_roads_SM %>%
  group_by(KC_FCC) %>%
  nest() %>%
  left_join(model_conveyPct %>% select(KC_FCC,loessModel)) %>%
  mutate(ConveyPct_Predicted=map2(.x=data,.y=loessModel,.f=~{
    predict(.y,data.frame(RdSkirtPctImp=.x$RdSkirtPctImp))
  })) %>%
  select(-loessModel) %>%
  unnest(cols = c(data, ConveyPct_Predicted)) %>%
  select(KC_FCC,RoadSegmentID,ConveyPct_Predicted) %>%
  ungroup()

kc_roads_SM %>%
  left_join(kc_roads_SM_predConvey) %>%
  ggplot(aes(x=RdSkirtPctImp,y=PctConveyed))+
  geom_point(col='grey')+
  facet_wrap(~KC_FCC)+
  geom_smooth()+
  geom_line(aes(y=ConveyPct_Predicted),col='red')+
  theme_bw()


library(leaflet)

#add in multipliers for emission factor rates
multiplier_PV_LT<-1.12
multiplier_mediumVehicle<-2.42
multiplier_heavyVehicle<-7.83

kc_roads_score_alternative<-kc_roads_SM %>%
  left_join(kc_roads_SM_predConvey) %>%
  transmute(RoadSegmentID,
            Juris,
            FULLNAME,RoadSegmentID,KC_FCC,
            RoadMiles,
            TrafficIntensity=ifelse(is.na(ADT_PSRC),1,ADT_PSRC),
            HeavyVehicleCount=ifelse(is.na(HeavyVehicle),0,HeavyVehicle),
            MediumVehicleCount=ifelse(is.na(MediumVehicle),0,MediumVehicle),
            PV_LT_Count=TrafficIntensity-HeavyVehicleCount-MediumVehicleCount,
            GenScore=round(log10(multiplier_PV_LT*PV_LT_Count+
                                   multiplier_mediumVehicle*MediumVehicleCount+
                                   multiplier_heavyVehicle*HeavyVehicleCount+
                                   1),2),
            GenScore=ifelse(is.na(GenScore),0,GenScore),
            RoadwaySkirtImperviousness=RdSkirtPctImp,
            RoadwaySkirtOverWater=RdSkirtPctOverwater,
            RoadwayDrainage=ConveyType,
            PctConveyed,
            PctConveyed_PREDICTED=ifelse(is.na(PctConveyed),ConveyPct_Predicted,PctConveyed),
            StreamWaterCrossing,
            ImpScore=log10((RoadwaySkirtImperviousness+1)/100), #note that imperviousness is inclusive of water surface
            ConveyScore=ifelse(StreamWaterCrossing==1,0,log10((PctConveyed+1)/100)),
            ConveyScore_PRED=ifelse(StreamWaterCrossing==1,0,log10((PctConveyed_PREDICTED+1)/100)),
            RoadwayConnectednessScore=round(ImpScore+ConveyScore,2),
            TotalScore=(GenScore+RoadwayConnectednessScore),
           TotalScore=ifelse(TotalScore<0,0,TotalScore),
           No_SW_RoadwayConnectednessScore=round(ImpScore+ConveyScore_PRED,2),
           No_SW_Score=GenScore+No_SW_RoadwayConnectednessScore,
           No_SW_Score=ifelse(No_SW_Score<0,0,No_SW_Score)
  ) %>%
  mutate(ScorePercentile=round(100*rank(TotalScore,ties.method='max',na.last='keep')/(length(which(!is.na(TotalScore)))),1),
         No_SW_ScorePercentile=round(100*rank(No_SW_Score,ties.method = 'max')/n(),1))

kc_roads_score_alternative %>%
  st_drop_geometry() %>%
  group_by(KC_FCC) %>%
  summarise(n=length(which(!is.na(TotalScore))),
            Score_Mean=mean(TotalScore,na.rm=T),
            Score_Median=median(TotalScore,na.rm=T),
            NoSW_n=n(),
            NoSWScore_Mean=mean(No_SW_Score),
            NoSWScore_Median=median(No_SW_Score))

example_score_segment_id<-c('16TH-11076','208TH-15051','FALL CITY-CARNATION-40224','PRESTON-FALL CITY-61575',
                            'UNION HILL-58754','I-5-41872','SR 18-81753','RAMP-62710')
kc_roads_score_alternative %>% 
  filter(RoadSegmentID %in% example_score_segment_id)

plot_ecdf_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(TotalScore,col=KC_FCC,group=KC_FCC))+
  stat_ecdf()+
  theme_bw()
ggsave('plots/plot_ecdf_Scores.png',plot_ecdf_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(x=KC_FCC,TotalScore,fill=KC_FCC,group=KC_FCC))+
  geom_jitter(aes(color=KC_FCC),height = 0,alpha=.2)+
  geom_boxplot(outlier.colour = NA,col='black',alpha=1,fill=NA)+
  scale_color_viridis_d()+
  theme_bw()+
  scale_y_continuous('Screening Model Score',limits=c(0,6))+
  xlab('Road Classification')+
  scale_fill_viridis_d()
ggsave('plots/plot_jitter_Scores.png',plot_jitter_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores_antiLog<-kc_roads_score_alternative %>%
  #filter(TotalScore>=7) %>%
  ggplot(aes(x=KC_FCC,10^TotalScore,fill=KC_FCC,group=KC_FCC))+
  geom_jitter(aes(color=KC_FCC),height = 0,alpha=.2)+
  geom_boxplot(outlier.colour = NA,col='black',alpha=1,fill=NA)+
  scale_color_viridis_d()+
  theme_bw()+
  scale_y_continuous('Screening Model Score (antilog)',
                     labels=scales::label_log())+
  xlab('Road Classification')+
  scale_fill_viridis_d()
ggsave('plots/plot_jitter_Scores_antiLog.png',plot_jitter_Scores_antiLog,scale=0.8,width=10,height=4.5)

#all KC (no stormwater data)
plot_ecdf_noSW_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(No_SW_Score,col=KC_FCC,group=KC_FCC))+
  stat_ecdf()+
  theme_bw()
ggsave('plots/plot_ecdf_noSW_Scores.png',plot_ecdf_noSW_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_noSW_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(x=KC_FCC,No_SW_Score,fill=KC_FCC,group=KC_FCC))+
  geom_jitter(aes(color=KC_FCC),height = 0,alpha=.2)+
  geom_boxplot(outlier.colour = NA,col='black',alpha=1,fill=NA)+
  scale_color_viridis_d()+
  theme_bw()+
  scale_y_continuous('Screening Model (No Conveyance) Score',limits=c(0,6))+
  xlab('Road Classification')+
  scale_fill_viridis_d()
ggsave('plots/plot_jitter_noSW_Scores.png',plot_jitter_noSW_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_noSW_Scores_antiLog<-kc_roads_score_alternative %>%
  #filter(TotalScore>=7) %>%
  ggplot(aes(x=KC_FCC,10^No_SW_Score,fill=KC_FCC,group=KC_FCC))+
  geom_jitter(aes(color=KC_FCC),height = 0,alpha=.2)+
  geom_boxplot(outlier.colour = NA,col='black',alpha=1,fill=NA)+
  scale_color_viridis_d()+
  theme_bw()+
  scale_y_continuous('Screening Model (No Conveyance) Score (antilog)',
                     labels=scales::label_log())+
  xlab('Road Classification')+
  scale_fill_viridis_d()
ggsave('plots/plot_jitter_noSW_Scores_antiLog.png',plot_jitter_noSW_Scores_antiLog,scale=0.8,width=10,height=4.5)


#top 10s
top10_scores<-arrange(kc_roads_score_alternative,desc(TotalScore)) %>% st_drop_geometry()%>%slice(1:10) %>%
  mutate(RoadMiles=round(RoadMiles,1),
         across(c(TrafficIntensity:MediumVehicleCount,RoadwaySkirtImperviousness,PctConveyed),round)) %>%
  select(Juris,FULLNAME,RoadSegmentID,KC_FCC,RoadMiles,TrafficIntensity,HeavyVehicleCount,
         MediumVehicleCount,GenScore,RoadwaySkirtImperviousness,RoadwayDrainage,PctConveyed,StreamWaterCrossing,
         RoadwayConnectednessScore,TotalScore) 
top10_scores%>%
  write.csv('outputs/top10_roads.csv',row.names = F)

top10_scores_KC<-kc_roads_score_alternative %>%
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

kc_roads_score_alternative %>%
  st_write('outputs/GIS/kc_roads_scored.geojson',append = F)
#zip('outputs/GIS/kc_roads_scored.zip',list.files('outputs','kc_roads_scored',full.names = T))

pal_alt<-colorNumeric('RdYlBu',0:6,reverse=T)
kc_roads_score_alternative %>% 
  filter(!st_is_empty(.)) %>%
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(color=~pal_alt(TotalScore),popup = ~paste(FULLNAME,RoadSegmentID,KC_FCC,
                                                         paste('Juris from LOG:',Juris),
                                                         #paste('Juris from Spatial Join:',JurisFinal),
                                                     paste('Total Score:',TotalScore),
                                                     paste('ADT:',round(TrafficIntensity)),
                                                     paste('Hvy Veh:',round(HeavyVehicleCount)),
                                                     paste('Med Veh:',round(MediumVehicleCount)),
                                                     paste('%Imp:',round(RoadwaySkirtImperviousness,0)),
                                                     paste('%Wtr:',round(RoadwaySkirtOverWater,0)),
                                                     paste('Drainage:',RoadwayDrainage),
                                                     paste('%Conveyed:',round(PctConveyed,0)),
                                                     paste('OverWater:',ifelse(StreamWaterCrossing==1,'Yes','No')),
                                                    # paste('Connectedness:',RoadwayConnectednessScore),
                                                     sep='<br>'),
               opacity = 1) %>%
  addLegend('bottomright',pal_alt,0:6,title='Screening Model\nScore')


#top 10000 map:
kc_roads_score_alternative %>% 
  arrange(desc(No_SW_Score)) %>%
  slice(1:20000)%>%
  filter(!st_is_empty(.)) %>%
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(color=~pal_alt(No_SW_Score),popup = ~paste(FULLNAME,RoadSegmentID,KC_FCC,
                                                         paste('Juris from LOG:',Juris),
                                                        # paste('Juris from Spatial Join:',JurisFinal),
                                                         paste('Total Score:',No_SW_Score),
                                                         paste('ADT:',round(TrafficIntensity)),
                                                         paste('Hvy Veh:',round(HeavyVehicleCount)),
                                                         paste('Med Veh:',round(MediumVehicleCount)),
                                                         paste('%Imp:',round(RoadwaySkirtImperviousness,0)),
                                                         paste('%Wtr:',round(RoadwaySkirtOverWater,0)),
                                                         paste('Drainage:',RoadwayDrainage),
                                                         paste('%Conveyed:',round(PctConveyed_PREDICTED,0)),
                                                         paste('OverWater:',ifelse(StreamWaterCrossing==1,'Yes','No')),
                                                         # paste('Connectedness:',RoadwayConnectednessScore),
                                                         sep='<br>'),
               opacity = 1) %>%
  addLegend('bottomright',pal_alt,0:6,title='Screening Model\nScore')

#Plots for Presentation
#ADT distribution by KC_FCC
#Breakpoints for ADT classes

#Vehicle weight distribution by KC_FF and ADT Classes
#breakpoints for VW classes

#Roadway Skirt Imperviousness by KC_FCC and ADT classes
#Roadway Skirt Imperviousness and Conveyance type

#Map of Scores
#zoom in too neighborhood
