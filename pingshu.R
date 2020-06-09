#
#对中文不友好,不能打包
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
      "，平均亩产",round(dd1[i,]$biaozhunchanliang,2),"公斤。" ,
      "平均比对照增产",round(dd1[i,]$zengchan,2),"%。" ,
      "生育期",round(dd1[i,]$shengyuqi,0),"天。" ,
      "蛋白质含量",round(dd1[i,]$danbai,2),"%，",
      "脂肪含量",round(dd1[i,]$zhifang,2),"%。",
      dd1[i,]$jiejiaxixing,"结荚习性，",
      "株高",round(dd1[i,]$zhugao,2),"厘米，" ,
      dd1[i,]$huase,"花，", dd1[i,]$rongmaose,"毛，",
      "主茎",round(dd1[i,]$zhujingjieshu,2),"节，" ,
      "单株有效分枝数",round(dd1[i,]$fenzhishu,2),"个，",
      "单株有效荚",round(dd1[i,]$youxiaojia,2),"个，" ,
      "籽粒",dd1[i,]$lixing,"形,",dd1[i,]$qise,"脐,",
      "百粒重",round(dd1[i,]$bailizhong,2),"克。" ,
      "倒伏级别",round(dd1[i,]$daofuxing,2),"级。",
      "经综合评定，该品系进入",dd1[i,]$nextstage,"组。",sep="")
  }
  return(trtr)
}
