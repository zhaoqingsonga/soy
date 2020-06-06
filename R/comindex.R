#gjhjkjkjkjkjk
zonghezhishu<-function(dd){
  dd$chanliang<-chanliang(dd,10000)
  com<-as.matrix(scale(cbind(dd[traits("juexuan")],zengchan=zengchan(dd)),center=TRUE,scale=TRUE))
  for(j in 1:ncol(com)){
      com[,j][is.na(com[,j])]<-0
  }
    dd1<-transform(com,
       comindex=miaoqipingjia*5+huaqipingjia*5+guliqipingjia*5+
              chengshuqipingjia*10+zilipingjia*5+zhizhupingjia*5+zengchan*65
              )
return(dd1$comindex)
}
#jhljlkjljjjlkjkljljl
nextstage<-function(dd){
   #
   if(canDo(dd)){
     dd$zonghezhishu<-zonghezhishu(dd)
     dd1<-aggregate(dd$zonghezhishu,by=list(code=dd$code),mean,na.rm=TRUE)
     step<-quantile(dd1$x,seq(.01,1,.01))
     #
     lab=substr(dd1$code[2],1,1)
     stage<-getStage(lab)
     dd$nextstage[dd1$x>=step["85%"]]<- stage["nstage"]
     dd$nextstage[(dd1$x>=step["70%"])&dd1$x<step["85%"]]<- stage["cstage"]
     dd$nextstage[(dd1$x>=step["55%"])&dd1$x<step["70%"]]<- stage["pstage"]
     dd$nextstage[dd1$x<step["55%"]]<- 100
     #
     dd$nextstage[(dd$is_ck==1|dd$rp>1)]<- 100
     return(dd$nextstage)
   }else{
     return("CanNotDo")
   }
}


