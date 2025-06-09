# install.packages("pwr2")  # if you haven't already
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(purrr)
library(pwr2)
library(GGally)
##storm data
storm_data<-read_excel('~/GitHub/23-08026-000-6PPD-q-Characterization-and-Protocol-Dev-/inputs/6PPDqStorm.xlsx') %>%
  mutate(Date=as.Date(Date),
         BMP=factor(BMP,levels=c('SCTF-TB1','SCTF-TB2','SCTF-TB2.5','SCTF-TB4','STTC-TB1','STTC-G2')),)

bmp_files<-list.files(path='~/GitHub/23-08026-000-6PPD-q-Characterization-and-Protocol-Dev-/inputs',pattern='81563|81564|81565|81678|82371|82372|82373|82954|82955|82959|82960|L82961')

bmp_data<-purrr::map(paste0('~/GitHub/23-08026-000-6PPD-q-Characterization-and-Protocol-Dev-/inputs/',bmp_files),.f=~{
  read.csv(.x)
}) %>%
  bind_rows() %>%
  tidyr::separate(Locator,sep='-',into=c('Site','BMP','Flow')) %>%
  mutate(BMP=ifelse(BMP=='TB25','TB2.5',BMP))%>%
  mutate(BMP=factor(paste(Site,BMP,sep='-'),levels=c('SCTF-TB1','SCTF-TB2','SCTF-TB2.5','SCTF-TB4','STTC-TB1','STTC-G2')),
         Flow=ifelse(is.na(Flow),'IN',Flow),
         Date=as.Date(Collect.Date,'%m/%d/%Y'),
         DateTime=as_datetime(Collect.Date,'%m/%d/%Y %H:%M',tz='America/Los_Angeles')) %>%
  filter(Lab.ID!='L81563-13')  %>%#remove dupe
  #fix issue of missing 1 minute difference
  group_by(Date,BMP,floor_date(DateTime,'hour')) %>%
  mutate(DateTime=min(DateTime))

influent_6PPDQ<-bmp_data %>%
  filter(Flow=='IN') %>%
  pull(Result)*1000 

quantile(influent_6PPDQ)
sigma = sd(influent_6PPDQ)
mean_log<-mean(log10(influent_6PPDQ))
sigma_log<-sd(log10(influent_6PPDQ))

# Power analysis for a 1-way repeated measures ANOVA
pwr.1way(
  alpha = 0.05,       # Significance level
  # power = 0.80,       # Desired power
  k = 12,         # Control vs Treatment
  n = 6,   # Repeated storm events
  # delta = 0.25,           # Effect size (Cohen's f)
  f=0.6
)

power_out<-pwr.1way(
  alpha = 0.05,       # Significance level
  # power = 0.80,       # Desired power
  k = 12,         # Control vs Treatment
  n = 6,   # Repeated storm events
  sigma=sigma_log,
  delta=seq(0.1,1,by=.05)
)$power

ggplot()+
  geom_point(aes(x=10^seq(0.1,1,by=.05),
                 y=power_out))+
  geom_hline(yintercept=.8)+
  ylab('Power (1-beta)')+
  xlab('Factor')


pwr::pwr.t.test(n=6,d=(3.5-2.5)/sigma_log,sig.level=0.05,type='paired')

t_test_power<-pwr::pwr.t.test(n=6,d=seq(0.01,.8,by=.01)/sigma_log,sig.level=0.05,type='paired')$power

ggplot()+
  geom_line(aes(x=10^seq(0.01,.8,by=.01),
                y=t_test_power))+
  geom_hline(yintercept=.8)+
  scale_y_continuous('Paired t-test Power (1-beta)',limits=c(0,1),
                     breaks=seq(0,1,by=.2))+
  scale_x_continuous('Factor Effect (multiplicative)',limits=c(1,5))+
  theme_bw()



#simulate
#assume that the I5 influece source was a score of 5

station_info<-read.table(text=
'Site	Score	ADT	RdSkirtImp	Congestion	Canopy	Speed	Curb	Slope	Curve
NKC_AVONDALE_116TH	4.08	26657	38	Minor	No	40	Yes	Flat	Minor
NKC_AVONDALE_130TH	3.33	36281	5	Minor	Yes	40	Yes	Flat	Straight
NKC_133RD	3.24	8071	18.5	None	Yes	35	Yes	Flat	Straight
NKC_196TH	3.12	8626	13.2	None	No	35	Yes	Minor	Straight
NKC_195TH_NOVHILL	3.09	9156	11.5	Moderate	Yes	35	Yes	Minor	Minor
NKC_CEDARPARK	2	420	20.6	None	Yes	25	Yes	Minor	Straight
NKC_224TH	1.01	73	14.2	None	Yes	25	Yes	Flat	Straight
SKC_277TH	4.04	32317	23.4	Moderate	No	40	Yes	Minor	Straight
SKC_140TH_188TH	3.55	20510	29.1	Minor	Yes	40	Yes	Flat	Straight
SKC_PETRO	2.77	5000	40	Moderate	Yes	25	Yes	Minor	Minor
SKC_272ND	3.8	21926	18.9	None	No	45	No	Moderate	Straight
NEW_PETRO	4.29	26300	57.5	Moderate	No	40	Yes	Flat	Straight',
header=T) %>%
  mutate(Congestion=factor(Congestion,c('None','Minor','Moderate'),labels=c('None','Minor','Moderate')),
         Slope=factor(Slope,c('Flat','Minor','Moderate')))

station_info %>%
  ggplot(aes(x=ADT,y=RdSkirtImp,size=Speed,col=Curve,shape=Canopy))+
  geom_point()+
  facet_grid(Congestion~Slope)+
  theme_bw()+
  scale_y_continuous(sec.axis = sec_axis(~.,name='CONGESTION',breaks=NULL,labels = NULL))+
  scale_x_log10(sec.axis = sec_axis(~.,name='SLOPE',breaks=NULL,labels = NULL),
                minor_breaks=c(100*1:10,1000*1:10,10^4*1:10))+
  scale_size(range=c(3,6))

station_info %>% 
  mutate(#Speed=factor(Speed),
         Log10ADT=log10(ADT)) %>%
  select(-c(Site,ADT,Curb,Score)) %>%
  relocate(Log10ADT) %>%
  ggpairs(  upper = list(continuous = "cor", combo = "box_no_facet",discrete = "count"),
            lower = list(continuous = "points", combo = "dot_no_facet",discrete = "count")#,
          #  diag=list(continuous='blankDiag',discrete='blankDiag')
          )+
  theme_bw()

set.seed(1337)
simConcSites<-tibble(
  Site=rep(LETTERS[1:12],each=6),
  #Site=rep(Stations,each=6),
  Score=rep(seq(1,5,length.out=12),each=6),
  #Score=rep(Model_Scores,each=6),
  Sim_6PPDQ=10^(rnorm(n=6*12,mean=mean_log-(5-Score),sd=sigma_log))
)
ggplot(simConcSites,aes(x=Score,y=Sim_6PPDQ,col=Site))+
  geom_boxplot()+
  geom_jitter(height = 0,width=.1)+
  scale_y_log10('Simulated 6PPDQ (ng/L)',
                minor_breaks=c(0.01*1:10,.1*1:10,1:10,10*1:10,100*1:10,1000*1:10),
                labels=scales::comma)+
  geom_hline(yintercept=2,lty=2)+
  geom_hline(yintercept=10,lty=3)+
  theme_bw()+
  xlim(1,5)

aov(log(Sim_6PPDQ)~Site,data=simConcSites) %>% summary()
lm(log(Sim_6PPDQ)~Score,data=simConcSites) %>% summary()

pairwise.wilcox.test(simConcSites$Sim_6PPDQ,simConcSites$Site,
                     p.adjust.method = 'BH')


set.seed(1337)
power_ouput<-tibble(
  one_v_three_WILCOX=rep(NA_integer_,1000),
  oneHalf_v_two_WILCOX=rep(NA_integer_,1000),
  three_v_four_WILCOX=NA_integer_,
  three_v_five_WILCOX=NA_integer_,
  four_v_five_WILCOX=NA_integer_,
  fourHalf_v_five_WILCOX=NA_integer_
)
for(i in 1:1000){
  simConc<-tibble(
    Score=rep(c(1,1.5,2,3,4,4.5,5),each=6),
    StormEvent=rep(LETTERS[1:6],7),
    Sim_6PPDQ=10^(rnorm(n=6*length(unique(Score)),mean=mean_log-(5-Score),sd=sigma_log))
  )
  power_ouput$one_v_three_WILCOX[i]<-simConc %>% filter(Score %in% c(1,3)) %>% 
    tidyr::pivot_wider(id_cols = StormEvent,names_from = Score,values_from = Sim_6PPDQ) %>%
    rename(groupA=2,groupB=3) %>%
    wilcox.test(Pair(groupA,groupB)~1,data=.) %>% .$p.value
  power_ouput$oneHalf_v_two_WILCOX[i]<-simConc %>% filter(Score %in% c(1.5,2)) %>% 
    tidyr::pivot_wider(id_cols = StormEvent,names_from = Score,values_from = Sim_6PPDQ) %>%
    rename(groupA=2,groupB=3) %>%
    wilcox.test(Pair(groupA,groupB)~1,data=.) %>% .$p.value
  power_ouput$three_v_four_WILCOX[i]<-simConc %>% filter(Score %in% c(3,4)) %>% 
    tidyr::pivot_wider(id_cols = StormEvent,names_from = Score,values_from = Sim_6PPDQ) %>%
    rename(groupA=2,groupB=3) %>%
    wilcox.test(Pair(groupA,groupB)~1,data=.) %>% .$p.value
  power_ouput$three_v_five_WILCOX[i]<-simConc %>% filter(Score %in% c(3,5)) %>% 
    tidyr::pivot_wider(id_cols = StormEvent,names_from = Score,values_from = Sim_6PPDQ) %>%
    rename(groupA=2,groupB=3) %>%
    wilcox.test(Pair(groupA,groupB)~1,data=.) %>% .$p.value
  power_ouput$fourHalf_v_five_WILCOX[i]<-simConc %>% filter(Score %in% c(4.5,5)) %>% 
    tidyr::pivot_wider(id_cols = StormEvent,names_from = Score,values_from = Sim_6PPDQ) %>%
    rename(groupA=2,groupB=3) %>%
    wilcox.test(Pair(groupA,groupB)~1,data=.) %>% .$p.value
  power_ouput$four_v_five_WILCOX[i]<-simConc %>% filter(Score %in% c(4,5)) %>% 
    tidyr::pivot_wider(id_cols = StormEvent,names_from = Score,values_from = Sim_6PPDQ) %>%
    rename(groupA=2,groupB=3) %>%
    wilcox.test(Pair(groupA,groupB)~1,data=.) %>% .$p.value
}

power_ouput %>%
  summarise(across(ends_with('COX'),~length(which(.<=.05))))/1000

####comparing similar scores
pwr::pwr.t.test(n=6,d=seq(0.1,1,by=.1)/sigma_log)$power


#multivariate power test
library(nlme)
# lm_power_out<-tibble(
#   #ScoreSig=rep(NA,1000),
#   #SlopeSig=rep(NA,1000),
#  # CongestionSig=rep(NA,1000),
#   AntDrySig=rep(NA,1000),
#   IntenSig=rep(NA,1000),
#   Ant_DropIntenSig=rep(NA,1000),
# #  CongestDropDrySig=rep(NA,1000)
# )
power.out<-tibble(
  Quade_pValueSig=rep(NA,1000),
  Quade_postHoc_195_196_pValueSig=rep(NA,1000),
  Quade_postHoc_195_Avon116_pValueSig=rep(NA,1000),
  KW_pValueSig=rep(NA,1000),
  KW_postHoc_195_196_pValueSig=rep(NA,1000),
  KW_postHoc_195_Avon116_pValueSig=rep(NA,1000),
  fiveVariable_SlopeSig=rep(NA,1000),
  fourVariable_SlopeSig=rep(NA,1000),
  threeVariable_SlopeSig=rep(NA,1000)
)
set.seed(1337)
for(i in 1:1000){
  simConc<-tibble(
    Site=rep(station_info$Site,each=6),
    StormEvent=rep(LETTERS[1:6],nrow(station_info)),
    Ant_Dry=rep(c(3,4,2,3,5,10),times=nrow(station_info)),
    Intensity=rep(c(0.5,0.75,2,1,0.5,1.5),times=nrow(station_info))) %>%
    left_join(station_info,by='Site') %>%
    group_by(Site) %>%
    mutate(Sim_6PPDQ=10^(rnorm(n=n(),
                        mean=mean_log-(5-log10(ADT*RdSkirtImp/100))+
                          #Ant_Dry/100+
                          #Intensity/200+
                        #  ifelse(Congestion=='None',0,log10(3))+
                          ifelse(Slope=='Moderate',log10(2),ifelse(Slope=='Minor',log10(1.5),0))#+
                         # ifelse(Canopy=='Yes',log10(.9),0)+
                          #ifelse(Curve=='Minor',log10(2),0)#+
                        #  ifelse(Speed>=40,0.15,ifelse(Speed>=35,0.08,0))
                                 ,
                        sd=sigma_log))
  )
   
  #quade and post hoc quade
  power.out$Quade_pValueSig[i]<-with(simConc,quade.test(Sim_6PPDQ,groups=Site,blocks=StormEvent))$p.value
  power.out$Quade_postHoc_195_196_pValueSig[i]<-with(simConc,quadeAllPairsTest(Sim_6PPDQ,groups=Site,blocks=StormEvent,p.adjust.method='none'))$p.value[
    'NKC_196TH','NKC_195TH_NOVHILL'
  ]
  power.out$Quade_postHoc_195_Avon116_pValueSig[i]<-with(simConc,quadeAllPairsTest(Sim_6PPDQ,groups=Site,blocks=StormEvent,p.adjust.method='none'))$p.value[
    'NKC_AVONDALE_116TH','NKC_195TH_NOVHILL'
  ]
  power.out$KW_pValueSig[i]<-with(simConc,kruskalTest(x=Sim_6PPDQ,g=Site %>% factor()))$p.value
  power.out$KW_postHoc_195_196_pValueSig[i]<-with(simConc,kwAllPairsDunnTest(Sim_6PPDQ,g=Site %>%factor(),p.adjust.method='none'))$p.value[
    'NKC_196TH','NKC_195TH_NOVHILL'
  ]
  power.out$KW_postHoc_195_Avon116_pValueSig[i]<-with(simConc,kwAllPairsDunnTest(Sim_6PPDQ,g=Site %>%factor(),p.adjust.method='none'))$p.value[
    'NKC_AVONDALE_116TH','NKC_195TH_NOVHILL'
  ]
  
  simConc_summary<-simConc %>%
    group_by(Site,Score,ADT,RdSkirtImp,Congestion,Slope,Curve,Speed,Canopy) %>%
    summarise(Sim_6PPDQ=mean(log10(Sim_6PPDQ)),
              .groups = 'drop') %>%
    ungroup() %>%
    mutate(SpeedGroup=ifelse(Speed>=35,'High','Low'),
           SlopeGroup=ifelse(Slope=='Flat','No','Yes'),
           CongestionGroup=ifelse(Congestion=='None','No','Yes'),
           CurveGroup=ifelse(Curve=='Straight','No','Yes'),
           Ratio_6PPDQ_ADT=10^Sim_6PPDQ/ADT)
  
  power.out$fiveVariable_SlopeSig[i]<-summary(
      with(simConc_summary,lm(Sim_6PPDQ~log10(ADT)+RdSkirtImp+SlopeGroup+CongestionGroup+CurveGroup))
    )$coef[4,4]
  power.out$fourVariable_SlopeSig[i]<-summary(
      with(simConc_summary,lm(Sim_6PPDQ~log10(ADT)+RdSkirtImp+SlopeGroup+CongestionGroup))
    )$coef[4,4]
  power.out$threeVariable_SlopeSig[i]<-summary(
    with(simConc_summary,lm(Sim_6PPDQ~log10(ADT)+RdSkirtImp+SlopeGroup))
  )$coef[4,4]
  
}
power.out %>%
  summarise(across(ends_with('Sig'),~length(which(.<=0.05))))/nrow(power.out)

library(PMCMRplus)
#ancova and post
with(simConc,aov(Sim_6PPDQ~Site+StormEvent))
TukeyHSD(with(simConc,aov(Sim_6PPDQ~Site+StormEvent)))

#anova and post
with(simConc,aov(Sim_6PPDQ~Site))
TukeyHSD(with(simConc,aov(Sim_6PPDQ~Site)))

#quade and post hoc quade
with(simConc,quade.test(Sim_6PPDQ,groups=Site,blocks=StormEvent))
with(simConc,quadeAllPairsTest(Sim_6PPDQ,groups=Site,blocks=StormEvent,p.adjust.method='none'))


#kruskal wallis and post hoc dunn
with(simConc,kruskalTest(x=Sim_6PPDQ,g=Site %>% factor()))
with(simConc,kwAllPairsDunnTest(Sim_6PPDQ,g=Site %>% factor(),p.adjust.method='none'))

simConc %>%
  ggplot(aes(x=factor(Site),y=Sim_6PPDQ))+
  geom_boxplot()+
  scale_y_log10(minor_breaks=c(.1*1:10,1:10,10*1:10,100*1:10))

#SPEED AND ADT are highly correlated, the Speed parameter takes all the significance in the regression

simConc %>%
  group_by(Site,Score,ADT,RdSkirtImp,Congestion,Slope,Curve,Speed,Canopy) %>%
  summarise(Sim_6PPDQ=mean(log10(Sim_6PPDQ))) %>%
  ungroup() %>%
  mutate(SpeedGroup=ifelse(Speed>=35,'High','Low'),
         SlopeGroup=ifelse(Slope=='Flat','No','Yes'),
         CongestionGroup=ifelse(Congestion=='None','No','Yes'),
         CurveGroup=ifelse(Curve=='Straight','No','Yes'),
         Ratio_6PPDQ_ADT=10^Sim_6PPDQ/ADT) %>%
  with(.,lm(Sim_6PPDQ~log10(ADT)+log10(RdSkirtImp)+CongestionGroup+SlopeGroup+SpeedGroup)) %>%
 # with(.,lm(Ratio_6PPDQ_ADT~log10(RdSkirtImp)+CongestionGroup+SlopeGroup+SpeedGroup)) %>%
  summary()

simConc %>%
  group_by(Site) %>%
  summarise(Sim_6PPDQ=10^mean(log10(Sim_6PPDQ))) %>%
  left_join(station_info) %>%
  mutate(SpeedGroup=ifelse(Speed>=35,'High','Low'),
         SlopeGroup=ifelse(Slope=='Flat','No','Yes'),
         CongestionGroup=ifelse(Congestion=='None','No','Yes'),
         CurveGroup=ifelse(Curve=='Straight','No','Yes'))  %>%
  ggplot(aes(x=ADT,y=RdSkirtImp,shape=Curve,label=round(Sim_6PPDQ,1)))+
  geom_label()+
  facet_grid(CongestionGroup~SlopeGroup+SpeedGroup)+
  theme_bw()+
  scale_y_continuous(sec.axis = sec_axis(~.,name='CONGESTION',breaks=NULL,labels = NULL))+
  scale_x_log10(sec.axis = sec_axis(~.,name='SLOPE\nSPEED',breaks=NULL,labels = NULL),
                minor_breaks=c(100*1:10,1000*1:10,10^4*1:10))

simConc %>%
  ggplot(aes(x=RdSkirtImp,y=Sim_6PPDQ,col=RdSkirtImp))+
  geom_point()+
  scale_y_log10()#+
 # scale_x_log10()

simConc %>%
  ggplot(aes(x=RdSkirtImp,y=ADT,label=Site))+
  geom_label()


