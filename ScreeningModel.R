#this script takes the processed data from the harmonized spatial layers and applies the scoring model
# First a model is developed to impute conveyance data where no data exist
# Then the Scoring equation is run and the resultant dataset is saved as a geoJSON
#run script to import data
source('dataImport.R')

library(ggplot2)
library(cowplot)
library(leaflet)
library(tidyr)
library(purrr)
source('plottingArguments.R')
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
  group_by(KC_FCC,RoadSegmentID) %>%
  summarise(ConveyPct_Predicted=mean(ConveyPct_Predicted))

#plot imputed roads by conveyance
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

kc_roads_score<-kc_roads_SM %>%
  left_join(kc_roads_SM_predConvey) %>%
  transmute(RoadSegmentID,
            Juris,
            FULLNAME,RoadSegmentID,KC_FCC,
            RoadMiles,
            RoadArea_SQFT,
            TrafficIntensity=ifelse(is.na(ADT_PSRC),1,ADT_PSRC),
            HeavyVehicleCount=ifelse(is.na(HeavyVehicle),0,HeavyVehicle),
            MediumVehicleCount=ifelse(is.na(MediumVehicle),0,MediumVehicle),
            PV_LT_Count=TrafficIntensity-HeavyVehicleCount-MediumVehicleCount, #passenger vehicle count
            #Tire Wear Particle / 6PPDQ Generation Score
            GenScore=round(log10(multiplier_PV_LT*PV_LT_Count+ 
                                   multiplier_mediumVehicle*MediumVehicleCount+
                                   multiplier_heavyVehicle*HeavyVehicleCount+
                                   1),2),
            GenScore=ifelse(is.na(GenScore),0,GenScore),
            #Imperviousness Score
            RoadwaySkirtImperviousness=RdSkirtPctImp,
            RoadwaySkirtOverWater=RdSkirtPctOverwater,
            RoadwayDrainage=ConveyType,
            SW_DATA_Available,
            PctConveyed=Aggr_PctConveyed,
            PctConveyed_PREDICTED=ifelse(is.na(ConveyPct_Predicted),PctConveyed,ConveyPct_Predicted),
            StreamWaterCrossing,
            ImpScore=log10(ifelse(RoadwaySkirtImperviousness<=1,1,
                                  RoadwaySkirtImperviousness/100)), #note that imperviousness is inclusive of water surface
            #Conveyance Score
            ConveyScore=ifelse(StreamWaterCrossing==1,0,
                               log10(ifelse(PctConveyed<=1,1,PctConveyed/100))),
            ConveyScore_PRED=ifelse(StreamWaterCrossing==1,0,log10((PctConveyed_PREDICTED+1)/100)), #using imputed data
            #Connectedness Score
            RoadwayConnectednessScore=round(ImpScore+ConveyScore,2),
            Imputed_SW_RoadwayConnectednessScore=round(ImpScore+ConveyScore_PRED,2),
            #total Score (without and with imputation)
            TotalScore=(GenScore+RoadwayConnectednessScore),
             TotalScore=ifelse(TotalScore<0,0,
                               ifelse(TotalScore>5,5,TotalScore)),
           Imputed_SW_Score=GenScore+Imputed_SW_RoadwayConnectednessScore,
           Imputed_SW_Score=ifelse(Imputed_SW_Score<0,0,
                                   ifelse(Imputed_SW_Score>5,5,Imputed_SW_Score))
  ) %>%
  #calculate perceniltes
  mutate(ScorePercentile=round(100*rank(TotalScore,ties.method='max',na.last='keep')/(length(which(!is.na(TotalScore)))),1),
         Imputed_SW_ScorePercentile=round(100*rank(Imputed_SW_Score,ties.method = 'max')/n(),1))

file.remove('outputs/GIS/kc_roads_scored.geojson')
kc_roads_score %>%
  st_write('outputs/GIS/kc_roads_scored.geojson',append = F)

