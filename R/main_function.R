#######################################################################
# get repeat that has no duplicated data among the same position number
# finished on 2020.5.11 githubtest
#######################################################################

get_rp3<-function(test1=1:15){
  if(any(duplicated(test1))|length(test1)<=2){
    return("your sequence is not suitable for the function!!! try again")
  }

  test2<-sample(test1)
  while (any(test1==test2)==TRUE) {
    test2<-sample(test1)
  }
  test3<-sample(test1)
  while (any(test1==test3)==TRUE|any(test2==test3)==TRUE) {
    test3<-sample(test1)
  }
  return(c(test1,test2,test3))
}

###############
#
##############
calculate_physical_site<-function(vc=c(10000,11000,12,15,14)){
    py0<-vc[1]
    py1<-vc[2]
    g0<-vc[3]
    g1<-vc[4]
    g<-vc[5]
    py<-py0+(py1-py0)*(g-g0)/(g1-g0)
    return(py)
}

###########
#return list
#
############
get_fragment<-function(lg="L",genetic_distnace=20){
  mylist<-list()
  #防止与数据中的值相同，但又不影响结果
  shift<-genetic_distnace+0.00000001
  #
  concensus_data<-subset(physical_genetic_distance,
                         physical_genetic_distance$not_consistant==0&physical_genetic_distance$outlier==0)

  lg_sub<-subset(concensus_data,concensus_data$map_name==lg)

    fi<-which(lg_sub$feature_start<=shift)
    la<-which(lg_sub$feature_start>=shift)
    if(length(fi)==0){
      inter<-head(lg_sub,2)
    }else if(length(la)==0){
      inter<-tail(lg_sub,2)
    }else{
      inter<-rbind(tail(lg_sub[fi,],1), head(lg_sub[la,],1))
    }
   myreturn <-c(inter$feature_start_g[1],inter$feature_start_g[2],
                   inter$feature_start[1],inter$feature_start[2],genetic_distnace)
   mylist[[1]]<-myreturn
   mylist[[2]]<-paste(inter$is_normal,sep="",collapse="")
    return(mylist)
}
##########################
#
##############################

##############
#
################
LG_to_Ch<-function(lg="L"){
  myreturn<-subset(Ch_LG$Chromosome_Number,Ch_LG$Linkage_Group==lg)
  if(length(myreturn)==0){
    return(NA)
  }else{
    return(myreturn)
  }
}
########
#se function,calculate standard this a test github
#aaaaaa
#gggggggggg
#######
se<-function(v=c(2,3,4,5,2,3,5)){
  mysd<-sd(v,na.rm=TRUE)
  v<-v[!is.na(v)]
  myn<-length(v)
  return(mysd/sqrt(myn))
}
##

gitfunction<-function(){
  return("Hello world----!")
}


