#plotting arguments
density_plot_arguments_by_fcc<-function(data,x_var,x_name,LOG=F){
  x_var=ensym(x_var)
  ggplot(data,aes(!!x_var,fill=KC_FCC))+
    geom_density(alpha=.5)+
    theme_bw()+
    scale_fill_viridis_d('Road Class')+
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
    theme(legend.position = 'none')+
    xlab('Road Classification')+
    scale_fill_viridis_d('Road Class')+
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