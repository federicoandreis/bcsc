# a few useful functions
acc_foo <- function(x,y,thr) {
  
  rr <- ifelse(x>=thr,1,0)
  sum(diag(table(rr,y)))/sum(table(rr,y))
  
}

expit <- function(x) exp(x)/(1+exp(x))
logit <- function(x) log(x/(1+x))

'%notin%' <- function(x,y) !('%in%'(x,y))


# visualise patterns
#
# regular invitations are depicted using CIRCLES
# other invitations are depicted using DIAMONDS
# permanent exclusions are depicted using FILLED BISQUE SQUARES
#
# compliance with respect to an event is indicated by a FILLED symbol
#
# special events are GREEN
# temporary exclusions are BLUE
# reminders are RED
# 
# emigration events are indicated by a FILLED CORAL TRIANGLE
# death events are indicated by a PURPLE CROSS
#
timeline_plot <- function(id_list, # list of ids to visualise timeline for
                          dframe, # the screening dataset 'ss'
                          upper_x=11, # upper time limit (x axis)
                          sort_by_length=TRUE, # should the timelines be sorted?
                          h_labels=FALSE, # horizontal labels
                          suppress_ids=FALSE, # don't print ids (use when too many)
                          ########################################
                          diagnosis=TRUE, #whether to plot the time to diagnosis
                          ########################################
                          inner_margins=c(5,5,1,0),
                          outer_margins=c(0,2,0,0),...) {
  
  sys_mar <- par()$mar
  sys_oma <- par()$oma
  
  par(mar=inner_margins,oma=outer_margins)
  n_women <- length(id_list)
  
  plot(0,type='n',xlab='Years',ylab='',
       xlim=c(0,upper_x),ylim=c(1,n_women),axes=FALSE,...)
  axis(1,at=0:upper_x)
  
  abline(v=seq(2,10,by=2),lty=2,col='gray')
  
  mtext('id',side=2,outer=TRUE,las=2)
  
  par(mar=sys_mar,oma=sys_oma)
  
  w_data_frame <- dframe %>% 
    filter(id%in%id_list) %>%
    group_by(id) %>% 
    mutate(max_time=max(time_from_first)) %>% 
    ungroup()
  
  if(sort_by_length) {
    
    id_list <- w_data_frame %>% 
      arrange(max_time) %>% 
      dplyr::select(id) %>% 
      unique %>% unlist
    
  }
  
  axis(2,at=1:n_women,
       labels=if(suppress_ids) {rep('',n_women)} else {id_list},
       las=ifelse(h_labels,1,2))
  
  for (i in 1:n_women) {
    
    tmp_df <- w_data_frame %>% 
      filter(id==id_list[i])
    
    segments(0,i,max(tmp_df$time_from_first/365),i,col='gray')
    
    ####################################
    # plot regular invitations - circles
    tmp_regular <- tmp_df %>% 
      filter(regular=='Yes')
    
    ########################################
    #permanent exclusion is designated with square
    #regular invitations with circles 
    # if attended - filled, if not - blank
    points(tmp_regular$time_from_first/365,rep(i,nrow(tmp_regular)),
           pch=ifelse(tmp_regular$permanent=='Yes',15,
                      ifelse(tmp_regular$compliance=='Yes',19,1)),
           cex=1.5,
           col=ifelse(tmp_regular$permanent=='Yes','bisque4',1))
    
    # special - green
    points(tmp_regular$time_from_first[tmp_regular$special=='Yes']/365,
           rep(i,sum(tmp_regular$special=='Yes')),
           pch=ifelse(tmp_regular[tmp_regular$special=='Yes',]$compliance=='Yes',19,1),
           cex=1.5,
           col='green')
    # temporary exclusions - blue
    points(tmp_regular$time_from_first[tmp_regular$temporary=='Yes']/365,
           rep(i,sum(tmp_regular$temporary=='Yes')),
           pch=ifelse(tmp_regular[tmp_regular$temporary=='Yes',]$compliance=='Yes',19,1),
           cex=1.5,
           col='blue')
    
    ###################################
    # plot other invitations - diamonds
    tmp_other <- tmp_df %>% 
      filter(regular=='No')
    
    # special - green
    points(tmp_other$time_from_first[tmp_other$special=='Yes']/365,
           rep(i,sum(tmp_other$special=='Yes')),
           pch=ifelse(tmp_other[tmp_other$special=='Yes',]$compliance=='Yes',18,5),
           cex=ifelse(tmp_other[tmp_other$special=='Yes',]$compliance=='Yes',1.5,1),
           col='green')
    # permanent exclusions - bisque4
    points(tmp_other$time_from_first[tmp_other$permanent=='Yes']/365,
           rep(i,sum(tmp_other$permanent=='Yes')),
           pch=15,
           cex=1.5,col='bisque4')
    # temporary exclusions - blue
    points(tmp_other$time_from_first[tmp_other$temporary=='Yes']/365,
           rep(i,sum(tmp_other$temporary=='Yes')),
           pch=ifelse(tmp_other[tmp_other$temporary=='Yes',]$compliance=='Yes',18,5),
           cex=ifelse(tmp_other[tmp_other$temporary=='Yes',]$compliance=='Yes',1.5,1),
           col='blue')
    # reminders - red
    points(tmp_other$time_from_first[tmp_other$reminder=='Yes']/365,
           rep(i,sum(tmp_other$reminder=='Yes')),
           pch=ifelse(tmp_other[tmp_other$reminder=='Yes',]$compliance=='Yes',18,5),
           cex=ifelse(tmp_other[tmp_other$reminder=='Yes',]$compliance=='Yes',1.5,1),
           col='red')
    
    ####################
    # emigration - coral triangles
    time_emigration <- (tmp_df$data_em - tmp_df$data_invito)[1]/365
    points(time_emigration,i,
           pch=17,
           cex=1.5,
           col='coral')
    # death - purple crosses
    time_death <- (tmp_df$data_mor - tmp_df$data_invito)[1]/365
    points(time_death,i,
           pch=4,lwd=2,
           cex=1.5,
           col='purple')
    
    ########################################
    #### Diagnosis is designated with pink snowflake
    ############################################## 
    if (diagnosis) {
      time_diagnosis <- (tmp_df$incidenza - tmp_df$data_invito)[1]/365
      points(time_diagnosis,i,
             pch=8,lwd=1,
             cex=1.5,
             col='deeppink')
    }
    
    #####################################################
    # add green cicrle with plus sign in it for suspicious results from the examination
    # according to the variable IdScreeningPositive
    #########################################
    if(any(tmp_df$IdScreeningPositive>0)) {
      k <- unique(tmp_df$IdScreeningPositive[tmp_df$IdScreeningPositive>0])
      visits <- tmp_df[tmp_df$id_prestazione%in%k,]
      points(visits$time_from_first/365,
             rep(i,length(k)),
             pch=10,
             cex=1.5,
             col='green3'
      )  
    }
    
    
    
  }
  
}

