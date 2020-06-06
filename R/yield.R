
#
#dui zhongwen bu youhao.
# zhiyao shi tongyi shiyan bianke.
yieldAnalysis<-function(dd,area=667)
{
  dd$chanliang<-chanliang(dd,area)
  #dd$biaozhunchanliang<-biaozhunchanliang(dd)
  fit<-aov(chanliang~code,data=dd)
  #summary(fit)
  library("agricolae")
  dt<-duncan.test(fit,"code",alpha=0.05)
  fcfx<-cbind(dt$means[order(dt$means$chanliang,decreasing=TRUE),],dt$groups$M)
  fcfx$CV<-fcfx$std/fcfx$chanliang*100
  check=mean(dd$chanliang[dd$is_ck==1],na.rm=TRUE)
  fcfx$zc<-(fcfx$chanliang/check-1)*100
  #names(fcfx)
  fcfx$chanliang<-round(fcfx$chanliang,2)
  fcfx$std<-round(fcfx$std,2)
  fcfx$Min<-round(fcfx$Min,2)
  fcfx$Max<-round(fcfx$Max,2)
  fcfx$CV<-round(fcfx$CV,2)
  fcfx$zc<-round(fcfx$zc,2)
  fcfx$px<-(1:nrow(fcfx))
  names(fcfx)<-c("yield(kg)","std","rp","Mix(kg)","max(kg)","group(5%)","CV%","more(%)","order")
  return(fcfx)
}

#
#dui zhongwen bu youhao,zhongwen ban.
#
pingshu<-function(dd){
  dd$zengchan<-zengchan(dd)
  dd$biaozhunchanliang<-biaozhunchanliang(dd)
  dd1<-aggregate(dd,by=list(code=dd$code),mean,na.rm=TRUE)
  trtr<-NULL
  for (i in 1:nrow(dd1)){
    trtr[i]<-paste(
      dd1[i,]$code,
      "(", dd1[i,]$laiyuan,")",
      #think of hr
      ",pingjun chanliang ",round(dd1[i,]$biaozhunchanliang,2),"kg." ,
      "pingjun bi duizhao zengchan ",round(dd1[i,]$zengchan,2),"%." ,
      "shengyuqi ",round(dd1[i,]$shengyuqi,0),"tian." ,
      "danbaizhihanliang ",round(dd1[i,]$danbai,2),"%,",
      "zhifanghanliang ",round(dd1[i,]$zhifang,2),"%.",
      dd1[i,]$jiejiaxixing,"jiejiaxixing,",
      "zhugao ",round(dd1[i,]$zhugao,2),"cm," ,
      dd1[i,]$huase,"hua,", dd1[i,]$rongmaose,"mao,",
      "zhujing ",round(dd1[i,]$zhujingjieshu,2),"jie," ,
      "danzhuyouxiaofenzhi ",round(dd1[i,]$fenzhishu,2),"ge,",
      "danzhuyouxiaojia ",round(dd1[i,]$youxiaojia,2),"ge," ,
      "zili ",dd1[i,]$lixing,"xing,",dd1[i,]$qise,"qi,",
      "bailizhong ",round(dd1[i,]$bailizhong,2),"g." ,
      "daofujibie ",round(dd1[i,]$daofuxing,2),"ji.",
      "jing zonghe pingding,gai pingxi jinru ",dd1[i,]$nextstage,"zu.",sep="")
  }
  return(trtr)
}
#
#
#



