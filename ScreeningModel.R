library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(cowplot)
library(leaflet)
library(tidyr)
library(purrr)

hec_gdb<-'\\\\herrera.local\\hecnet\\gis-k\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE6ppdq_20241007.gdb'

#st_layers(hec_gdb)

hec_gis_export<-
#read_excel('inputs/KC_Street_Address_SummaryAttributes_20240223_v2.xlsx') %>%
#read_excel('O:\\proj\\Y2019\\19-07102-011\\GIS\\KC_Street_Address_SummaryAttributes_20240718_v2.xlsx')
 st_read(hec_gdb,
         layer = 'King_County_St_Addresses_6PPDQ_Metrics_20241008_v2')

#read in juridstiction by areas (if a WSDOT road runs through the city, what city is it?)
geo_sw_juris<-read_excel('O:\\proj\\Y2019\\19-07102-011\\GIS\\KC_Street_Address_Juris_SWJuris_20240924.xlsx') %>%
  distinct()

#missing jurisdictions from the aggregrated stormwater dataset
missing_sw_juris<-c('Carnation','Medina','Yarrow Point','Skykomish')

kc_roads_SM<-
  hec_gis_export %>%  
  st_transform(4326) %>%
  left_join(geo_sw_juris) %>%
 # left_join(ditch_estimates) %>%
 # mutate(DitchLen_FT=as.numeric(DitchLength)) %>%
  transmute(FRADDL,FRADDR,TOADDL,TOADDR,FULLNAME,
            RoadSegmentID,
            KC_FCC=factor(KC_FCC,levels=c('L','C','M','P','F'),
                       labels=c('Local','Collector','Minor','Primary','Freeway')),
           # Juris,
            SW_Juris,
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
            #Where is stormwater data available?
            SW_DATA_Available=(Juris=='WSDOT'&ConveyLen_FT>0)|SW_Juris %in% c('King County','Seattle'),
            Aggr_SW_DATA_Available=!(SW_Juris %in% missing_sw_juris),
            #calculate percent conveyed
            PctConveyed=ConveyLen_FT/Shape_Leng*100,
            PctConveyed=ifelse(PctConveyed>100,100,PctConveyed),
            PctConveyed=ifelse(SW_DATA_Available,PctConveyed,NA),
            #assume road drains in both directions and need conveyance on both sides
            PctConveyed_Half=0.5*ConveyLen_FT/Shape_Leng*100,
            PctConveyed_Half=ifelse(PctConveyed_Half>100,100,PctConveyed_Half),
            PctConveyed_Half=ifelse(SW_DATA_Available,PctConveyed_Half,NA),
            #aggregated stormwater data,
            AggregatedSWLen_KC=ifelse(is.na(AggregatedSWLen_KC)&Aggr_SW_DATA_Available,0,AggregatedSWLen_KC),
           #use seattle, KC, and WSDOT data if there are data available
            AggregatedSWLen_KC=ifelse(SW_DATA_Available,ConveyLen_FT,AggregatedSWLen_KC),
            Aggr_PctConveyed=AggregatedSWLen_KC/Shape_Leng*100,
            Aggr_PctConveyed=ifelse(Aggr_PctConveyed >100,100,Aggr_PctConveyed),
            Aggr_PctConveyed=ifelse(Aggr_SW_DATA_Available,Aggr_PctConveyed,NA),
            #aggregate - assume road drains in both directions and need conveyance on both sides
            Aggr_PctConveyed_Half=0.5*AggregatedSWLen_KC/Shape_Leng*100,
            Aggr_PctConveyed_Half=ifelse(Aggr_PctConveyed_Half>100,100,Aggr_PctConveyed_Half),
            Aggr_PctConveyed_Half=ifelse(Aggr_SW_DATA_Available,Aggr_PctConveyed_Half,NA),
            #remove conveyance for WSDOT within other jurisdictions
            ConveyType=if_else(SW_DATA_Available,ConveyType,NA), 
            BusTraffic =ifelse(is.na(BusTraffic ),0,BusTraffic ),
            Shape_Length=Shape_Leng,
            RoadMiles=Shape_Leng/5280
     )
  
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
         SegmentsWithSWData=length(which(SW_DATA_Available)),
            PctConveyed=mean(PctConveyed,na.rm=T),
         PctConveyed_Half=mean(PctConveyed_Half,na.rm=T),
         SegmentsWithAggrSWData=length(which(Aggr_SW_DATA_Available)),
         Aggr_PctConveyed=mean(Aggr_PctConveyed,na.rm=T),
         Aggr_PctConveyed_Half=mean(Aggr_PctConveyed_Half,na.rm=T),
            PctOverwater=mean(RdSkirtPctOverwater,na.rm=T),
            RdSkirtPctImp=mean(RdSkirtPctImp,na.rm=T)) %>%
  write.csv('outputs/kc_wsdot_roads_summar.csv',row.names = F)


#Need to impute conveyance for jurisdictions outside of compiled dataset
#several issues with WSDOT Ramps in highly impervious areas lacking conveyance
#this is due to issues in generating a two-dimension roadway surface and 
#6-m skirt due to overlay issue with the main road itself
model_conveyPct<-kc_roads_SM %>%
  filter(Aggr_SW_DATA_Available) %>%
  group_by(KC_FCC) %>%
  nest() %>%
  mutate(loessModel=map(.x=data,.f=~loess(Aggr_PctConveyed~RdSkirtPctImp,data=.x,span=.9)))

#use Primary Arterial for Freeways inside of Jurisdictions
kc_roads_SM_predConvey<-kc_roads_SM %>%
  st_drop_geometry() %>%
  #impute primary arterial values for both primary and freeways without conveyance data and where
  #>75% impervious and <50% conveyed
  filter(!Aggr_SW_DATA_Available|
           (KC_FCC %in% c('Primary','Freeway')&RdSkirtPctImp>75&Aggr_PctConveyed<50)) %>%
  group_by(KC_FCC) %>%
  nest() %>%
  # left_join(model_conveyPct %>% select(KC_FCC,loessModel)) %>%
  mutate(ConveyPct_Predicted=map(.x=data,.f=~{
    if (KC_FCC %in% c('Primary','Freeway')){
      predict(model_conveyPct$loessModel[model_conveyPct$KC_FCC=='Primary'][[1]],
              data.frame(RdSkirtPctImp=.x$RdSkirtPctImp))
    }else{
      predict(model_conveyPct$loessModel[model_conveyPct$KC_FCC==KC_FCC][[1]],
              data.frame(RdSkirtPctImp=.x$RdSkirtPctImp))
    }
  })) %>%
  # select(-loessModel) %>%
  unnest(cols = c(data, ConveyPct_Predicted)) %>%
  ungroup() %>%
  select(KC_FCC,RoadSegmentID,ConveyPct_Predicted)

#plotting arguments
density_plot_arguments_by_fcc<-function(data,x_var,x_name,LOG=F){
  x_var=ensym(x_var)
  ggplot(data,aes(!!x_var,fill=KC_FCC))+
    geom_density(alpha=.5)+
    theme_bw()+
    scale_fill_viridis_d()+
    scale_y_continuous('Density')+
    {
      if(!LOG){
        scale_x_continuous(x_name)
      } else {
        scale_x_log10(x_name,
                      limits=c(.1,NA),
                      breaks=10^(0:4),
                      minor_breaks=c(.1*1:10,1:10,10*1:10,100*1:10,1000*1:10,10^4*1:10,10^5*1:10),
                      labels=scales::label_number(scale_cut = scales::cut_short_scale()))
      }
    }
}
violin_plot_arguments_by_fcc<-function(data,y_var,y_name,LOG=F){
  y_var=ensym(y_var)
  ggplot(data,aes(KC_FCC,!!y_var))+
    geom_jitter(alpha=0.4,height = 0,aes(fill=KC_FCC),shape=21)+
    geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
    geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
    theme_bw()+
    scale_fill_viridis_d()+
    {
      if(!LOG){
        scale_y_continuous(y_name)
        } else {
      scale_y_log10(y_name,
        limits=c(.1,NA),
        breaks=10^(0:4),
        minor_breaks=c(.1*1:10,1:10,10*1:10,100*1:10,1000*1:10,10^4*1:10,10^5*1:10),
        labels=scales::label_number(scale_cut = scales::cut_short_scale()))
        }
    }
}

#length of each road segement
kc_roads_SM %>%
  density_plot_arguments_by_fcc(.,RoadMiles,'Road Miles',F)

plot_ADT_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(.,ADT_PSRC,'Average Daily Trafic',T)
ggsave('plots/adt_by_fcc.png',plot_ADT_FCC,scale=0.8,width=10,height=4.5)

 plot_ADT_FCC_violin<-kc_roads_SM %>%
   violin_plot_arguments_by_fcc(ADT_PSRC,'Average Daily Traffic',LOG=T)
 ggsave('plots/adt_by_fcc_violin.png',plot_ADT_FCC_violin,scale=0.8,width=10,height=4.5)
 

plot_dist_med_freight_count<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(MediumVehicle,'Medium Vehicle Count (per day)',T)
ggsave('plots/freight_med_by_fcc.png',plot_dist_med_freight_count,scale=0.8,width=10,height=4.5)

plot_dist_med_freight_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(MediumVehicle,'Medium Vehicle Count (per day)',LOG=T)
  ggsave('plots/freight_med_by_fcc_violin.png',plot_dist_med_freight_violin,scale=0.8,width=10,height=4.5)

plot_dist_freight_count<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(HeavyVehicle,'Heavy Vehicle Count (per day)',T)
ggsave('plots/freight_by_fcc.png',plot_dist_freight_count,scale=0.8,width=10,height=4.5)

plot_dist_heavy_freight_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(HeavyVehicle,'Heavy Vehicle Count (per day)',LOG=T)
ggsave('plots/freight_heavy_by_fcc_violin.png',plot_dist_heavy_freight_violin,scale=0.8,width=10,height=4.5)


plot_Imp_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(RdSkirtPctImp,'Road Skirt Imperviousness (%)')
ggsave('plots/imperviousness_by__FCC.png',plot_Imp_FCC,scale=0.8,width=10,height=4.5)

plot_Imp_FCC_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(RdSkirtPctImp,'Road Skirt Imperviousness (%)')
ggsave('plots/imperviousness_by_FCC_violin.png',plot_Imp_FCC_violin,scale=0.8,width=10,height=4.5)

plot_convey_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_by_FCC.png',plot_convey_FCC,scale=0.8,width=10,height=4.5)

plot_convey_FCC_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_by_FCC_violin.png',plot_convey_FCC_violin,scale=0.8,width=10,height=4.5)

##Aggregated stormwater
plot_aggr_convey_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(Aggr_PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_aggr_by_FCC.png',plot_aggr_convey_FCC,scale=0.8,width=10,height=4.5)

plot_aggr_convey_FCC_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(Aggr_PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_aggr_by_FCC_violin.png',plot_aggr_convey_FCC_violin,scale=0.8,width=10,height=4.5)

plot_convey_imp<-kc_roads_SM %>%
  filter(SW_DATA_Available) %>%
  ggplot(aes(RdSkirtPctImp,fill=ConveyType))+
  geom_density(alpha=.5)+
  facet_wrap(~KC_FCC,scales = 'free_y')+
  theme_bw()+
  scale_fill_viridis_d()
ggsave('plots/imperviousness_by_conveyance_FCC.png',plot_convey_imp,scale=0.8,width=10,height=4.5)

plot_convey_imp_violin<-kc_roads_SM %>%
  filter(SW_DATA_Available) %>%
  ggplot(aes(ConveyType,y=RdSkirtPctImp))+
  geom_jitter(alpha=0.4,height = 0,aes(fill=ConveyType),shape=21)+
  geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
  geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
  facet_wrap(~KC_FCC,scales = 'free_y')+
  theme_bw()+
  scale_fill_viridis_d()+
  xlab('Conveyance Type')+
  ylab('Skirt Imperviousness (%)')
ggsave('plots/imperviousness_by_conveyance_violin.png',plot_convey_imp_violin,scale=0.8,width=10,height=4.5)

plot_convey_type_impcat<-kc_roads_SM %>%
  mutate(ImpervCat=ifelse(is.na(RdSkirtPctImp)|RdSkirtPctImp<25,'<25%',
         ifelse(RdSkirtPctImp<50,'>=25 to 50%','>=50%'))) %>%
  filter(ConveyType!='Stream'&SW_DATA_Available) %>%
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
conveyance_by_impervious_FCC<-kc_roads_SM %>%
  filter(SW_DATA_Available) %>%
#  filter(Juris %in% c('Seattle','King County')|(Juris=='WSDOT'&PctConveyed>0)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=PctConveyed))+
  geom_point(aes(col=ConveyType))+
  facet_wrap(~KC_FCC)+
  geom_smooth(span=.9)+
  theme_bw()
ggsave('plots/conveyance_by_impervious_FCC.png',conveyance_by_impervious_FCC,scale=0.8,width=10,height=4.5)

conveyance_aggr_by_impervious_FCC<-kc_roads_SM %>%
  filter(Aggr_SW_DATA_Available) %>%
  #  filter(Juris %in% c('Seattle','King County')|(Juris=='WSDOT'&PctConveyed>0)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=Aggr_PctConveyed))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  geom_smooth(span=.9)+
  theme_bw()
ggsave('plots/conveyance_aggr_by_impervious_FCC.png',conveyance_aggr_by_impervious_FCC,scale=0.8,width=10,height=4.5)

conveyance_aggr_by_impervious_FCC_KCSeattleVsOther<-kc_roads_SM %>%
  filter(Aggr_SW_DATA_Available) %>%
  #  filter(Juris %in% c('Seattle','King County')|(Juris=='WSDOT'&PctConveyed>0)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=Aggr_PctConveyed,
             col=ifelse(SW_DATA_Available,'King County, Seattle, WSDOT','All Others Compiled')))+
  geom_point(alpha=.2)+
  facet_wrap(~KC_FCC)+
  geom_smooth(span=.9,se=F)+
  theme_bw()+
  ylab('Conveyance (%)')+
  scale_color_discrete('Data Source')


conveyance_by_impervious_FCC_imputed<-kc_roads_SM %>%
  st_drop_geometry() %>%
  left_join(kc_roads_SM_predConvey) %>%
  mutate(PctConveyed_USE=ifelse(is.na(ConveyPct_Predicted),Aggr_PctConveyed,ConveyPct_Predicted)) %>%
  #  filter(Juris %in% c('Seattle','King County')|(Juris=='WSDOT'&PctConveyed>0)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=PctConveyed_USE))+
  geom_point(aes(col=Aggr_SW_DATA_Available))+
  facet_wrap(~KC_FCC)+
  #geom_smooth(span=.9)+
  theme_bw()
ggsave('plots/conveyance_by_impervious_FCC_imputed.png',conveyance_by_impervious_FCC_imputed,
       scale=0.8,width=10,height=4.5)


# Scoring the Roads -------------------------------------------------------

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
            SW_DATA_Available,
            PctConveyed=Aggr_PctConveyed,
            PctConveyed_PREDICTED=ifelse(is.na(ConveyPct_Predicted),PctConveyed,ConveyPct_Predicted),
            StreamWaterCrossing,
            ImpScore=log10((RoadwaySkirtImperviousness+1)/100), #note that imperviousness is inclusive of water surface
            ConveyScore=ifelse(StreamWaterCrossing==1,0,log10((PctConveyed+1)/100)),
            ConveyScore_PRED=ifelse(StreamWaterCrossing==1,0,log10((PctConveyed_PREDICTED+1)/100)),
            RoadwayConnectednessScore=round(ImpScore+ConveyScore,2),
            TotalScore=(GenScore+RoadwayConnectednessScore),
           TotalScore=ifelse(TotalScore<0,0,TotalScore),
           Imputed_SW_RoadwayConnectednessScore=round(ImpScore+ConveyScore_PRED,2),
           Imputed_SW_Score=GenScore+Imputed_SW_RoadwayConnectednessScore,
           Imputed_SW_Score=ifelse(Imputed_SW_Score<0,0,Imputed_SW_Score)
  ) %>%
  mutate(ScorePercentile=round(100*rank(TotalScore,ties.method='max',na.last='keep')/(length(which(!is.na(TotalScore)))),1),
         Imputed_SW_ScorePercentile=round(100*rank(Imputed_SW_Score,ties.method = 'max')/n(),1))

kc_roads_score_alternative %>%
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
kc_roads_score_alternative %>% 
  filter(RoadSegmentID %in% example_score_segment_id)

plot_ecdf_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(TotalScore,col=KC_FCC,group=KC_FCC))+
  stat_ecdf()+
  theme_bw()
ggsave('plots/plot_ecdf_Scores.png',plot_ecdf_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores<-kc_roads_score_alternative %>%
  violin_plot_arguments_by_fcc(TotalScore,'')+
  scale_y_continuous('Screening Model Score',limits=c(0,5))
ggsave('plots/plot_jitter_Scores.png',plot_jitter_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores_antiLog<-kc_roads_score_alternative %>%
  mutate(TotalScore_AntiLog=10^TotalScore) %>%
  violin_plot_arguments_by_fcc(TotalScore_AntiLog,'')+
   scale_y_continuous('Screening Model Score (antilog)',
                     labels=scales::label_log(),
                     limits=c(0,10^5))
ggsave('plots/plot_jitter_Scores_antiLog.png',plot_jitter_Scores_antiLog,scale=0.8,width=10,height=4.5)

#all KC (no stormwater data)
plot_ecdf_noSW_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(Imputed_SW_Score,col=KC_FCC,group=KC_FCC))+
  stat_ecdf()+
  theme_bw()
ggsave('plots/plot_ecdf_noSW_Scores.png',plot_ecdf_noSW_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_noSW_Scores<-kc_roads_score_alternative %>%
  violin_plot_arguments_by_fcc(Imputed_SW_Score,'')+
  scale_y_continuous('Screening Model (Imputed Conveyance) Score',limits=c(0,5))
ggsave('plots/plot_jitter_noSW_Scores.png',plot_jitter_noSW_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_noSW_Scores_antiLog<-kc_roads_score_alternative %>%
  mutate(ImputedSW_Score_AntiLog=10^Imputed_SW_Score) %>%
  violin_plot_arguments_by_fcc(ImputedSW_Score_AntiLog,'')+
  scale_y_continuous('Screening Model (Imputed Conveyance) Score (antilog)',
                     labels=scales::label_log(),
                     limits=c(0,10^5))
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

file.remove('outputs/GIS/kc_roads_scored.geojson')
kc_roads_score_alternative %>%
  st_write('outputs/GIS/kc_roads_scored.geojson',append = F)
#zip('outputs/GIS/kc_roads_scored.zip',list.files('outputs','kc_roads_scored',full.names = T))

pal_alt<-colorNumeric('RdYlBu',0:5,reverse=T)
kc_roads_score_alternative %>% 
  filter(!st_is_empty(.)&SW_DATA_Available) %>%
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(color=~pal_alt(TotalScore),popup = ~paste(FULLNAME,RoadSegmentID,KC_FCC,
                                                         paste('Juris:',Juris),
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
  addLegend('bottomright',pal_alt,0:5,title='Screening Model\nScore')


#top 10000 map:
kc_roads_score_alternative %>% 
  arrange(desc(Imputed_SW_Score)) %>%
  slice(1:20000)%>%
  filter(!st_is_empty(.)) %>%
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(color=~pal_alt(Imputed_SW_Score),popup = ~paste(FULLNAME,RoadSegmentID,KC_FCC,
                                                         paste('Juris:',Juris),
                                                        # paste('Juris from Spatial Join:',JurisFinal),
                                                         paste('Total Score:',Imputed_SW_Score),
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
  addLegend('bottomright',pal_alt,0:5,title='Screening Model\nScore')

#Plots for Presentation
#ADT distribution by KC_FCC
#Breakpoints for ADT classes

#Vehicle weight distribution by KC_FF and ADT Classes
#breakpoints for VW classes

#Roadway Skirt Imperviousness by KC_FCC and ADT classes
#Roadway Skirt Imperviousness and Conveyance type

#Map of Scores
#zoom in to neighborhood

# show score sensitiviy across multiple metrics
library(plotly)
dummy_data<-expand_grid(
  ADT=c(1,5,10,50,100,200,500,800,1000,2000,5000,8000,10000,c(2,5,8,10)*10^4),
  RoadwaySkirtImperviousness=c(0,25,50,75,100),
  PctConveyed=c(0,25,50,75,100)
) %>%
  mutate(
    GenScore=log10(ADT),
  ImpScore=log10((RoadwaySkirtImperviousness+1)/100), #note that imperviousness is inclusive of water surface
ConveyScore=log10((PctConveyed+1)/100),
RoadwayConnectednessScore=round(ImpScore+ConveyScore,2),
TotalScore=(GenScore+RoadwayConnectednessScore)
) %>%
  mutate(TotalScore=ifelse(TotalScore<0,0,TotalScore)) 

dummy_data %>%
  ggplot(aes(x=ADT,y=TotalScore,col=factor(PctConveyed)))+
  geom_line()+
  facet_wrap(~factor(paste0(RoadwaySkirtImperviousness,'% Impervious'),
                     levels=paste0(c(0,25,50,75,100),'% Impervious')))+
  theme_bw()+
 # scale_x_log10(minor_breaks=c(.1*1:10,1:10,10*1:10,100*1:10,1000*1:10,10^4*1:10,10^5*1:10),
 #                 labels=scales::label_number(scale_cut = scales::cut_short_scale()),
  #              sec.axis = sec_axis(~ . , name = "Conveyance (%)", breaks = NULL, labels = NULL))+
  scale_x_continuous('Average Daily Traffic (All Passenger Vehicles)',
                     #sec.axis = sec_axis(~ . , name = "Conveyance (%)", breaks = NULL, labels = NULL),
                     labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  scale_y_continuous('Score',
                     #sec.axis = sec_axis(~ . , name = "Skirt Imperviousness (%)", breaks = NULL, labels = NULL),
                     limits=c(0,5.5),breaks=0:5
                     )+
  scale_color_discrete('Conveyance(%)')+
  theme(legend.position = 'bottom')


kc_roads_score_alternative %>%
  ggplot(aes(x=RoadwaySkirtImperviousness,y=PctConveyed_PREDICTED))+
  geom_point(aes(col=TotalScore))+
#  geom_density_2d_filled(h=30,alpha=.5)+
  facet_wrap(~KC_FCC)+
  scale_color_viridis_c()

kc_roads_score_alternative %>%
  ggplot(aes(x=TrafficIntensity ,y=TotalScore))+
  geom_point(aes(col=RoadwaySkirtImperviousness))+
  #  geom_density_2d_filled(h=30,alpha=.5)+
  facet_wrap(~KC_FCC)+
  scale_color_viridis_c()

kc_roads_score_alternative %>%
  ggplot(aes(x=TrafficIntensity ,y=TotalScore))+
  geom_point(aes(col=PctConveyed))+
  #  geom_density_2d_filled(h=30,alpha=.5)+
  facet_wrap(~KC_FCC)+
  scale_color_viridis_c()
