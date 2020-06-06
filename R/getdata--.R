#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#
library(memoise)
chanliang<-function(dd,area=667){
  kzzlz<-dd$kaozhongzhulizhong
  xqz<-dd$xiaoquzhong
  xqmj<-dd$xiaoqumianji
  kzzlz[is.na(kzzlz)]<-0
  if(all(is.na(xqmj==TRUE))){
    return("NoXiaoqumianji")
  }else{
    y=(xqz+kzzlz/1000)/xqmj*area...
    return(y)
  }
}

shengyuqi<-function(dd){
  b<-dd$chengshuqi
  a<-dd$bozhongqi
  b[is.na(b)]<-"2050-01-01"
  a[is.na(a)]<-"2000-01-01"
  syq<-as.Date(b)-as.Date(a)
  #
  syq[syq>200]<-NA
    return(syq)
}

#Main function
#Get data from csvv
#
getWeb<-function(local=FALSE){
  if(local==TRUE){
    web<-"http://localhost/soybreedingtest/uploads/image/"
  }else{
    web<-"http://115.28.85.237/soybreeding/uploads/image/"
  }
  return(web)
}

#利用缓存技术
getYearData <- memoise(function(org_id=1002,year=2016){
  web=getWeb()
  filename<-paste(web,org_id,"-",year,".csv",sep="")
  mydata<-read.csv(filename,stringsAsFactors = FALSE)
  mydata<-mydata[order(mydata$id),]
  if(nrow(mydata>0)){
     mydata<-dataFilter(mydata)
     return(mydata)
  }else{
    print("Can not download date from web!");
  }
})
#
#huode mouge shiyan de shujv.
#
getExData<-function(ex_id=16,org=1002,year=2016){
  dd<-getYearData(org,year)
  mydata<-dd[dd$ex_id==ex_id,]
  return(mydata)
}
#利用缓存技术
getPersonalData <- memoise(function(admin_id=2,year=2016){
  web=getWeb()
  filename<-paste(web,"personal",admin_id,"-",year,".csv",sep="")
  mydata<-read.csv(filename,stringsAsFactors = FALSE)
  mydata<-mydata[order(mydata$id),]
  if(nrow(mydata>0)){
    mydata<-dataFilter(mydata)
    return(mydata)
  }else{
    return(FALSE)
  }
})
#
#
#
clearcache<-function(){
   forget(getPersonalData)
   forget(getYearData)
}
#Main function
#Get data from csvv
#
getTableData <- function(table="soystage"){
  web=getWeb()
  filename<-paste(web,table,".csv",sep="")
  mydata<-read.csv(filename,stringsAsFactors = FALSE)
  mydata<-mydata[order(mydata$id),]
  if(nrow(mydata>0)){
    mydata<-dataFilter(mydata)
    return(mydata)
  }else{
    return(FALSE);
  }
}
#
#
#
getExplId<-function(Pex_id=0){
   if(Pex_id==0){
     return(FALSE)
   }else{
     dd1<-getTableData("soyshiyan_soyplace")
     dd2<-subset(dd1,dd1$id==Pex_id)
     if(nrow(dd2)==1){
      dd3<-NULL
      dd3["ex_id"]<-dd2$soyshiyan_id
      dd3["pl_id"]<-dd2$soyplace_id
      return(dd3)
     }else{
       return(FALSE)
     }
   }
}
#
#
#
getMyexData<-function(Pex_id=0,admin_id=2,year=2016){
  expl_id<-getExplId(Pex_id)
  if(!all(expl_id==FALSE)){
     personaldata<-getPersonalData(admin_id,year)
     if(all(personaldata==FALSE)){
       return(FALSE)
     }else{
       mydata<-subset(personaldata,personaldata$ex_id==expl_id["ex_id"]&personaldata$place_id==expl_id["pl_id"])
       if(nrow(mydata)>0){
         return(mydata)
       }else{
         return(FALSE)
       }
     }
  }else{
   return(FALSE)
  }
}

dataSplit<-function(dd){
  dd1<-aggregate(dd$ex_id,by=list(ex_id=dd$ex_id,place_id=dd$place_id),length)
  ff<-NULL
  for(i in 1:nrow(dd1)){
    ff=c(ff,rep(i,dd1$x[i]))
  }
  return(split(dd,ff))
}



getStage<-function(lab="J"){
   ddstage<-getTableData(table="soystage")
   dd1<-ddstage[ddstage$pre==lab,]
   dd2<-ddstage[(ddstage$s_next==dd1$id&ddstage$id<20),]
   dd3<-NULL
   dd3["cstage"]<-dd1$id
   dd3["pstage"]<-dd2$id
   dd3["nstage"]<-dd1$s_next
   return(dd3)
}


#Mianfuntion
#Choose traits
#
dataFilter<-function(data){
  dataname=names(data)
  #去除不进行过滤的字段
  dn=dataname[!dataname%in%c("is_ck","is_stage",
                             "is_copy","fn","pid",
                             "sid","is_do","miaoqipingjia","huaqipingjia",
                             "guliqipingjia","chengshuqipingjia","zhizhupingjia",
                             "zilipingjia")]
    for(j in 1:ncol(data)){
     if(dataname[j]%in%dn){
       data[,j][data[,j]==-9]<-NA
       data[,j][data[,j]==0]<-NA
     }
     if(dataname[j]=="org_id"){ data[,j]<-transorg_id(data[,j])}
   }
  return(data)
}

#校正产量step=10,py=0,data=t1,出现问题，原来只能是5或10，现在是9，？？要重复写
#根据CK1校正产量:当前位置的上下20个范围内的CK平均为当然位置的产量矫正标准
#当间隔不确定时要计算间隔：interal
#
#用于计算试验的间隔，只用于单个试验
#
interval<-function(dd){
if(canDo(dd)){
  dd$idid<-1:nrow(dd)
  dd1<-subset(dd,dd$is_ck==1)
  id<-dd1$idid
  difid<-diff(id)
  if(length(difid)==0){return(0)}else{return(max(difid))}
}else{return(0)}
}
#
#判断是否能做：同试验，同地点,花了很长时间处理重复问题，这里不再加重复限制，
#
canDo<-function(dd){
  if(length(table(dd$ex_id))==1&length(table(dd$place_id))==1){
    return(TRUE)
  }else(
    return(FALSE)
        )
}

#
#用于计算某个id的参照产量：考虑了重复，考虑了id不连续的问题
#
ckYield<-function(dd,id){
    dd$newid<-1:nrow(dd)
    nid<-dd[dd$id==id,]$newid
    #is_ck向量中有NA,要把NA转化为0,如果不转化有问题
    dd$is_ck[is.na(dd$is_ck)]<-0
    #从id中获得重复信息
    rp<-dd[dd$id==id,]$rp
    intv<-interval(dd)
    #print(intv)
    stid<- nid-intv
    enid<-nid+intv
    cklist<-dd[dd$rp==rp&(dd$newid>=stid&dd$newid<=enid)&dd$is_ck==1,]
    #print(cklist)
    cky<-mean(cklist$chanliang,na.rm=TRUE)
    return(cky)
  }

  #
  #增产列%
  #
  zengchan<-function(dd){
      if(canDo(dd)){
        dd$newid<-1:nrow(dd)
        for(i in dd$newid){
          id<-dd[dd$newid==i,]$id
          dd$zengchan[i]<-dd$chanliang[i]/ckYield(dd,id)*100-100
        }
        if(!all(is.na(dd$zengchan))==TRUE){
          return(dd$zengchan)
        }else{
          return("NoResult!")
        }
      }else{
        return("CanNotDo!")
      }
    }

#
#标准产量列
#
biaozhunchanliang<-function(dd){
    avck<-mean(subset(dd,dd$is_ck==1)$chanliang,na.rm=TRUE)
    if(!is.na(avck)){
      dd$biaozhunchanliang<-zengchan(dd)/100*avck+avck
      return(dd$biaozhunchanliang)
    }else{return("NoAvck")}
}





#?????????????????????????????????
#有一个问题在里面在？？？要用list,这个地方费了很长时间
#花了将近一天也没有解决，废弃！！！
#zengchanrp<-function(dd){
# a<-NULL
#b<-NULL
#if(canDo(dd)){
#  for(i in names(table(dd$rp))){
#   a[[i]]<-subset(dd,dd$rp==i)
#  b[[i]]<-zengchan(a[[i]])
#}
 # return(b)
#}else{
 # return("can not do!")
#}
#}



