allequal <-
function(ntag,colorsname,nspecial=0, name1="Metal",name2="EMPTY",location1=1,location2=2,nspecial1=1,nspecial2=1){
if(nspecial>2){stop("Maximum of 2 special band allowed")}
colorsname<-as.vector(colorsname)
if(nspecial==0){amostra<-sample(x=colorsname,size=ntag,replace=TRUE)}else{
amostra<-rep(NA,ntag)
if(length(location1)==1){
auxpst<-location1}else{auxpst<-sample(x=location1,size=nspecial1)}
amostra[auxpst]<-name1
if(nspecial==2){
sobroup<-intersect(which(is.na(amostra)), location2)
if(length(sobroup)==1){
auxpst<-sobroup}else{auxpst<-sample(x=sobroup,size=nspecial2)}
amostra[auxpst]<-name2}
para_pintar<-which(is.na(amostra))
amostra[para_pintar]<-sample(x=colorsname,size=length(para_pintar),replace=TRUE)}
amostra<-as.vector(amostra)
return(amostra)}
