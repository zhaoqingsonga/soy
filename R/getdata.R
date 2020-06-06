#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#
#
#
#install.packages("ggplot2")
#install.packages("agricolae")
#install.packages("gdata")
#install.packages("multcomp")
#install.packages("dplyr")
#install.packages("printr")




library(memoise)
chanliang<-function(dd,area=667){
  kzzlz<-dd$kaozhongzhulizhong
  xqz<-dd$xiaoquzhong
  xqmj<-dd$xiaoqumianji
  kzzlz[is.na(kzzlz)]<-0
  xqz[is.na(xqz)]<-0
  if(all(is.na(xqmj==TRUE))){
    return("NoXiaoqumianji")
  }else{
    y=(xqz+kzzlz/1000)/xqmj*area
    y[y==0]<-NA
    return(y)
  }
}
shengyuqi<-function(dd){
  b<-dd$chengshuqi
  a<-dd$bozhongqi
  #syq<-(b-a)/(24*3600)
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

    web<-"http://localhost/ddyz/uploads/image/"
  }else{
    web<-"http://www.soybreeding.com/ddyz/uploads/image/"
  }
  return(web)
}
#利用缓存技术
getYearData <- memoise(function(org_id=1002,year=2020){
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
##############
##choose my data according to exid and myexid
################
#################
#chooseMyData<-function(exid=NA,myexid=NA,org=1002,year=2016,admin_id=2){
#  lib<-NA
#  lib<-as.list(lib)
#  if(!is.na(exid)){
#    lib$data<-getExData(ex_id = exid, org = org, year = year)
#    lib$name<-getMyExperimentName(exid=exid)
#    return(lib);
#  }else if(!is.na(myexid))
#    lib$data<-getMyexData(Pex_id = myexid,admin_id = admin_id, year = year)
#  lib$name<-getMyExperimentName(myexid=myexid)
#  return(lib);
#}

#####################################################
#rewrite the function chooseMyData
#because we have found the important package RMySQL (get data from internet)
#####################################################
chooseMyData<-function(myex_id=282,others=FALSE){

    ex_pl<-getExplId(myex_id)
    if(others){
      myquery<-paste("select * from thinkoao_soycess where ex_id=",ex_pl[1],sep="")
    }else{
      myquery<-paste("select * from thinkoao_soycess where ex_id=",ex_pl[1]," and place_id=",ex_pl[2],sep="")
    }
    con<-connectSql()
    mydata<-dbGetQuery(con,myquery)
    mydata<-dataFilter(mydata)

     lib<-NA
     lib<-as.list(lib)
     lib$data<-mydata
     lib$name<-getMyExperimentName(myexid=myex_id)
     dbDisconnect(con)
    return(lib);
}


##
#huode mouge shiyan de shujv.
##
getExData<-function(ex_id=16,org=1002,year=2016){
  dd<-getYearData(org,year)
  mydata<-dd[dd$ex_id==ex_id,]
  mydata<-mydata[order(mydata$place_id,mydata$rp,mydata$sort),]
  #remove the NA records
  mydata<-mydata[!is.na(mydata$code),]
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
#Get data from csv (get data from localhost)
#
#getTableData <- function(table="soystage"){
#  web=getWeb()
#  filename<-paste(web,table,".csv",sep="")
#  mydata<-read.csv(filename,stringsAsFactors = FALSE)
#  mydata<-mydata[order(mydata$id),]
#  if(nrow(mydata>0)){
#    mydata<-dataFilter(mydata)
#    return(mydata)
#  }else{
#    return(FALSE);
#  }
#}



################################################
#rewrite getTableData  (get data from internet.)
##################################################
connectSql<-function(){
  library(DBI)
  library(RMySQL)
  con<-
  dbConnect(MySQL(),host='115.28.85.237',username='zqs',password='123234345',port=3306,dbname='soybreeding')
  #deal with the chiense character
  gbk<-dbSendQuery(con,"SET NAMES gbk")
  return(con)
}

getTableData <- function(mytable="soystage",fields="*"){
  con<-connectSql()
  #assemble the query
  myquery<-paste("select ",fields," from thinkoao_",mytable,sep="")
  mydata<-dbGetQuery(con,myquery)
  dbDisconnect(con)
  if(nrow(mydata>0)){
    mydata<-dataFilter(mydata)
    return(mydata)
  }else{
    return(FALSE);
  }
}

#
#input id,return ex_id and pl_id
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
#When you input personal experiment id,you will get the personal experiment data.
#very amazing!
getMyexData<-function(Pex_id=0,admin_id=2,year=2016){
  expl_id<-getExplId(Pex_id)
  if(!all(expl_id==FALSE)){
    personaldata<-getPersonalData(admin_id,year)
    if(all(personaldata==FALSE)){
      return(FALSE)
    }else{
      mydata<-subset(personaldata,personaldata$ex_id==expl_id["ex_id"]&personaldata$place_id==expl_id["pl_id"])
      if(nrow(mydata)>0){
        #
        mydata<-mydata[order(mydata$place_id,mydata$rp,mydata$sort),]
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
    #duiyu bushi zhenglie tihuan de huiyouweiti.
    #if(dataname[j]=="org_id"){ data[,j]<-transorg_id(data[,j])}
    # There is a problem in get my experient data if we run the next line.Because we get data rely on
    #place id not on place name.
    #if(dataname[j]=="place_id"){ data[,j]<-transplace_id(data[,j])}
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
#用之前一定要计算产量列
#表中没有重复则可能要报错
#This function return the
ckYield_old<-function(dd,id){
  #如果只有一个对照，则返回这个对照的
  aa<-subset(dd,dd$is_ck==1&dd$rp==1)
  if(nrow(aa)==1){
    return(aa$chanliang)
  }else
  {
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
    return(cky)}
}

#
#I want to make a funciton that can get the nearest ck.
#perfect!!!
ckYield<-function(dd,myid){
  if(nrow(dd)==0){
    return(FAlSE)
  }else{
    #All the conditions
    is1<-dd$org_id==dd[dd$id==myid,]$org_id
    uni<-subset(dd,is1)
    is2<-uni$place_id==uni[uni$id==myid,]$place_id
    uni<-subset(uni,is2)
    is3<-uni$ex_id==uni[uni$id==myid,]$ex_id
    uni<-subset(uni,is3)
    is4<-uni$rp==uni[uni$id==myid,]$rp
    uni<-subset(uni,is4)
    is5<-uni$quzu==uni[uni$id==myid,]$quzu
    uni<-subset(uni,is5)
    unitdata<-uni
    #logi<-is5==((is1==is2)==(is3==is4))
    #the unit data.
    #unitdata<-subset(dd,logi)
    ckloc<-findCloseCk(which(unitdata$id==myid),unitdata$is_ck)
    myck<-mean(unitdata[ckloc,]$chanliang)
    return(myck)
  }
}

#
#This function get a position of ck that is close to input id.
#I very admire my thouhgt.
findCloseCk<-function(loc=6,mydata=c(0,0,0,0,0,1,0,0,0,0,1)){
  isckloc<-which(mydata==1)
  inloc<-abs(isckloc-loc)
  closeid<-which(inloc==min(inloc))
  return(isckloc[closeid])
}



#
#增产列%
#
zengchan<-function(dd){
  dd$chanliang<-chanliang(dd)
  j<-1
  for(i in dd$id){
    if(is.numeric(dd$chanliang[i])){
      dd$zengchan[j]<-dd[which(dd$id==i),]$chanliang/ckYield(dd,i)*100-100
    }else{
      dd$zengchan[j]<-NA
    }
    j<-j+1
  }
  if(!all(is.na(dd$zengchan))==TRUE){
    return(dd$zengchan)
  }else{
    return("NoResult!")
  }
}


# it is not perfect,because it is very difficult to read and understand.
#It takes me two days for this function
#calute the ck
#This version is new(20161202)
#This verson though about place,reapt and the number of ck.
zengchan_old_two_version<-function(ee){
  #add colume zengchan
  ee$zengchan<-NA
  unitpl<-ee$place_id[!duplicated(ee$place_id)]
  for(plid in unitpl){
    subee<-ee[ee$place_id==plid,]
    unitrp<-subee$rp[!duplicated(subee$rp)]
    for(rp in unitrp){
      #ee[ee$place_id==plid&ee$rp==rp,]
      #ee[ee$place_id==plid&ee$rp==rp,] is the unit data
      #format the colum zengchan
      ee[ee$place_id==plid&ee$rp==rp,]$zengchan<-0
      #find ck
      if(length(ee[ee$place_id==plid&ee$rp==rp,]$is_ck[ee[ee$place_id==plid&ee$rp==rp,]$is_ck==1])==1){
        ee[ee$place_id==plid&ee$rp==rp,]$zengchan<-subset(ee[ee$place_id==plid&ee$rp==rp,],ee[ee$place_id==plid&ee$rp==rp,]$is_ck==1)$chanliang[1]

      }else if(length(ee[ee$place_id==plid&ee$rp==rp,]$is_ck[ee[ee$place_id==plid&ee$rp==rp,]$is_ck==1])>=2){
        span<-interval(ee[ee$place_id==plid&ee$rp==rp,])
        if(span==0){
          return(0)
        }else{

          ran1<-which(ee[ee$place_id==plid&ee$rp==rp,]$is_ck==1)
          ckck1<-ee[ee$place_id==plid&ee$rp==rp,]$chanliang[ran1]
          #cut the top and the bottom
          if(length(ran1>=3)){
            ran<-ran1[2:(length(ran1)-1)]
          }else{
            ran<-ran1
          }

          ckck<-ee[ee$place_id==plid&ee$rp==rp,]$chanliang[ran]
          count<-ceiling(span/2)
          #just when the rp number is great than 3,the program is run.
          if(length(ee[ee$place_id==plid&ee$rp==rp,]$is_ck[ee[ee$place_id==plid&ee$rp==rp,]$is_ck==1])>=3){

            for(i in 1:count){
              #clear first
              ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ran-i]<-0
              ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ran-i]<-ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ran-i]+ckck
              #clear first
              ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ran+i]<-0
              ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ran+i]<-ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ran+i]+ckck
            }
          }

          #finish the top and the bottom
          ee[ee$place_id==plid&ee$rp==rp,]$zengchan[1:(min(ran1)+count)]<-0
          ee[ee$place_id==plid&ee$rp==rp,]$zengchan[1:(min(ran1)+count)]<-ckck1[1]
          ee[ee$place_id==plid&ee$rp==rp,]$zengchan[(max(ran1)-count):nrow(ee[ee$place_id==plid&ee$rp==rp,])]<-0
          ee[ee$place_id==plid&ee$rp==rp,]$zengchan[(max(ran1)-count):nrow(ee[ee$place_id==plid&ee$rp==rp,])]<-ckck1[length(ckck1)]
          #
          #deal with the ck
          ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ee[ee$place_id==plid&ee$rp==rp,]$is_ck==1]<-0
          ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ee[ee$place_id==plid&ee$rp==rp,]$is_ck==1]<-ee[ee$place_id==plid&ee$rp==rp,]$zengchan[ran1]+ckck1
        }
      }
    }
  }
  #calculte the colume zengchan
  ee$zengchan<-ee$chanliang/ee$zengchan*100-100

  return(ee$zengchan)
}

#
#标准产量列
#
biaozhunchanliang<-function(dd){
  avck<-mean(subset(dd,dd$is_ck==1)$chanliang,na.rm=TRUE)
  if(!is.na(avck)){
    dd$biaozhunchanliang<-as.numeric(zengchan(dd))/100*avck+avck
    dd$biaozhunchanliang[dd$is_ck==1]<-avck
    return(dd$biaozhunchanliang)
  }else{return("NoAvck")}
}
#
#Get the experiment name. Please check these tables(soyshiyan_soyplace,soyshiyan) before using.
#
getMyExperimentName<-function(exid=NA,myexid=NA){
  shiyan_place<-getTableData("soyshiyan_soyplace")
  shiyan<-getTableData("soyshiyan")
  if(!is.na(myexid)){
    ex_id<-subset(shiyan_place,shiyan_place$id==myexid)$soyshiyan_id
    shiyanname<-subset(shiyan,shiyan$id==ex_id)$shiyan_name
    return(shiyanname)
  }else if(!is.na(exid)){
    shiyanname<-subset(shiyan,shiyan$id==exid)$shiyan_name
    return(shiyanname)
  }else{
    return(FALSE)
  }
}
#
#Get the information of place based on  place_id
#
getPlaceInfo<-function(plist=10001){
  placetable<-getTableData("soyplace")
  return(subset(placetable,placetable$id%in%plist))
}
xiaoquzongzhong<-function(dd){
  t1<-dd$kaozhongzhulizhong/1000
  t2<-dd$xiaoquzhong
  t1[is.na(t1)]<-0
  t2[is.na(t2)]<-0
  xiaoquzongzhong=t1+t2
  return(xiaoquzongzhong)
}
#
#This function is maked to get the ck'name of a experiments.
#
getCkName<-function(dd){
  myid<-dd$id[1]
  if(nrow(dd)==0){
    return(FAlSE)
  }else{
    #All the conditions
    # their may be a questioin, please refer to function ckYield.
    is1<-dd$org_id==dd[dd$id==myid,]$org_id
    is2<-dd$place_id==dd[dd$id==myid,]$place_id
    is3<-dd$ex_id==dd[dd$id==myid,]$ex_id
    is4<-dd$rp==dd[dd$id==myid,]$rp
    is5<-dd$quzu==dd[dd$id==myid,]$quzu
    logi<-is5==((is1==is2)==(is3==is4))
    #get the unit data.
    unitdata<-subset(dd,logi)
    myckname<-unitdata[unitdata$is_ck==1,]$code
    return(myckname)
  }
}

#
#The usage of the function below need to analyze.
#
getMyExList<-function(username="zhaoqingsong",nf=2018){
  #handle the key table
  soystage<-getTableData("soystage")
  soyorg<-getTableData("soyorg")
  soyplace<-getTableData("soyplace")
  soyshiyan<-getTableData("soyshiyan")
  soyshiyan_soyplace<-getTableData("soyshiyan_soyplace")
  soyshiyanplace_admin<-getTableData("soyshiyanplace_admin")
  admin<-getTableData("admin")
  #
  #get some parameters form one experiemnts,such as how many ck,how many caritivars is more productive than
  #ck,which one is the highest yield and what is the yield,how many products does it overpass the ck.
  #
  getUserId<-function(username="zhaoqingsong"){
    return(admin[admin$username==username,]$id[1])
  }
  userid=getUserId(username)
  myexlist<-soyshiyanplace_admin[(soyshiyanplace_admin$admin_id==userid&soyshiyanplace_admin$nf==nf),]
  merge1<-merge(myexlist,soyshiyan_soyplace,by.x="soyshiyanplace_id",by.y="id")
  merge2<-merge(merge1,soyshiyan,by.x="soyshiyan_id",by.y="id")
  merge3<-merge(merge2,soyplace,by.x="soyplace_id",by.y="id")
  return(merge3[c("org_id.x","soyshiyanplace_id","shiyan_name","place_name")])
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



