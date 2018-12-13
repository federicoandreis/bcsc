# a few useful functions
acc_foo <- function(x,y,thr) {
  
  rr <- ifelse(x>=thr,1,0)
  sum(diag(table(rr,y)))/sum(table(rr,y))
  
}

expit <- function(x) exp(x)/(1+exp(x))
logit <- function(x) log(x/(1+x))

'%notin%' <- function(x,y) !('%in%'(x,y))


# visualise patterns
viz_pat <- function(sid) {
  
  nnn <- length(sid)
  
  plot(0,type='n',xlab='Years',ylab='id',
       xlim=c(0,10.5),ylim=c(1,nnn),axes=FALSE)
  axis(1,at=0:10)
  
  axis(2,at=1:nnn,labels=sid)
  abline(v=seq(2,10,by=2),lty=2,col='gray')
  
  for (i in 1:nnn) {
    ttp <- oo %>% filter(id==sid[i])
    
    segments(0,i,max(ttp$new_time0),i,
             col='gray')
    
    points(ttp$new_time0,rep(i,nrow(ttp)),
           pch=ttp$comply*15+1,
           col=ifelse(as.numeric(ttp$sp)==2,'green',
                      ifelse(ttp$ed==1,'blue',
                             ifelse(ttp$et==1,'orange',ttp$soll+1))),
           cex=ifelse(ttp$pure==1,1,.75))
    
    points(ttp$time_em[1],i,pch=4,lwd=2,cex=.75)
    # add death
    
  }
  
}

viz_pat2 <- function(sid,oo,sort_by_length=FALSE,ux=10.5,
                     cexreg=1,cexnreg=.75,...) {
  
  nnn <- length(sid)
  
  plot(0,type='n',xlab='Years',ylab='id',
       xlim=c(0,ux),ylim=c(1,nnn),axes=FALSE,...)
  axis(1,at=0:10)
  
  
  abline(v=seq(2,10,by=2),lty=2,col='gray')
  oo <- oo %>% ungroup()
  
  if(sort_by_length) {
    
    tsid <- oo %>% filter(id%in%sid) %>% 
      group_by(id) %>% 
      mutate(aa=max(time_from_first)) %>% 
      ungroup() %>% 
      arrange(aa) %>% 
      dplyr::select(id)
    sid <- unique(tsid$id)
    
  }
  
  axis(2,at=1:nnn,labels=sid)
  
  for (i in 1:nnn) {
    ttp <- oo %>% filter(id==sid[i])
    
    segments(0,i,max(ttp$time_from_first/365),i,col='gray')
    
    col_co <- as.integer(as.factor(ttp$compliance))-1
    col_sp <- as.integer(as.factor(ttp$special))-1
    col_pe <- as.integer(as.factor(ttp$permanent))-1
    col_te <- as.integer(as.factor(ttp$temporary))-1
    col_rem <- as.integer(as.factor(ttp$reminder))-1
    col_reg <- as.integer(as.factor(ttp$regular))-1
    
    time_em <- (ttp$data_em - ttp$data_invito)/365
    time_de <- (ttp$data_mor - ttp$data_invito)/365
    
    points(ttp$time_from_first/365,rep(i,nrow(ttp)),
           pch=col_co*15+1,
           col=ifelse(col_sp==1,'green',
                      ifelse(col_pe==1,'blue',
                             ifelse(col_te==1,'orange',
                                    col_rem+1))),
           cex=ifelse(col_reg==1,cexreg,cexnreg))
    
    points(time_em[1],i,pch=17,lwd=2,cex=cexreg,col='coral')
    points(time_de[1],i,pch=4,lwd=2,cex=cexreg,col='purple')
    
    
  }
  
}

viz_pat3 <- function(sid,oo,sort_by_length=FALSE,ux=10.5,
                     cexreg=1,cexnreg=.75,h_labels=FALSE,...) {
  
  nnn <- length(sid)
  
  plot(0,type='n',xlab='Years',ylab='id',
       xlim=c(0,ux),ylim=c(1,nnn),axes=FALSE,...)
  axis(1,at=0:10)
  
  
  abline(v=seq(2,10,by=2),lty=2,col='gray')
  oo <- oo %>% ungroup()
  
  if(sort_by_length) {
    
    tsid <- oo %>% filter(id%in%sid) %>% 
      group_by(id) %>% 
      mutate(aa=max(time_from_first)) %>% 
      ungroup() %>% 
      arrange(aa) %>% 
      dplyr::select(id)
    sid <- unique(tsid$id)
    
  }
  
  axis(2,at=1:nnn,labels=sid,las=ifelse(h_labels,1,2))
  
  for (i in 1:nnn) {
    ttp <- oo %>% filter(id==sid[i])
    
    segments(0,i,max(ttp$time_from_first/365),i,col='gray')
    
    col_co <- as.integer(as.factor(ttp$compliance))-1
    col_sp <- as.integer(as.factor(ttp$special))-1
    col_pe <- as.integer(as.factor(ttp$permanent))-1
    col_te <- as.integer(as.factor(ttp$temporary))-1
    col_rem <- as.integer(as.factor(ttp$reminder))-1
    col_reg <- as.integer(as.factor(ttp$regular))-1
    
    time_em <- (ttp$data_em - ttp$data_invito)/365
    time_de <- (ttp$data_mor - ttp$data_invito)/365
    
    points(ttp$time_from_first/365,rep(i,nrow(ttp)),
           pch=col_co*15+1,
           col=ifelse(col_sp==1,'green',
                      ifelse(col_pe==1,'blue',
                             ifelse(col_te==1,'orange',
                                    col_rem+1))),
           cex=ifelse(col_reg==1,cexreg,cexnreg))
    
    points(time_em[1],i,pch=17,lwd=2,cex=cexreg,col='coral')
    points(time_de[1],i,pch=4,lwd=2,cex=cexreg,col='purple')
    
    
  }
  
}

### viz with symbols rather than colours
# TBC