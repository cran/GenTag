escombination <-
function(row_under_review,emptyname,g1=NA, g2=NA, g3=NA, g4=NA, g5=NA, g6=NA){
ngroups<- (6- (is.na(g1[1])+is.na(g2[1])+is.na(g3[1])+is.na(g4[1])+is.na(g5[1])+is.na(g6[1])))
row_under_review<-matrix(row_under_review,ncol=length(row_under_review))
if(ngroups==0){stop("You must inform at least one group")}
columns_set1<-NA; columns_set2<-NA; columns_set3<-NA
columns_set4<-NA; columns_set5<-NA; columns_set6<-NA
if(ngroups==1){
columns_set1<- escode (row_under_review,emptyname,columns_set=g1)
}
if(ngroups==2){
columns_set1<- escode (row_under_review,emptyname,columns_set=g1)
columns_set2<- escode (row_under_review,emptyname,columns_set=g2)
}
if(ngroups==3){
columns_set1<- escode (row_under_review,emptyname,columns_set=g1)
columns_set2<- escode (row_under_review,emptyname,columns_set=g2)
columns_set3<- escode (row_under_review,emptyname,columns_set=g3)
}
if(ngroups==4){
columns_set1<- escode (row_under_review,emptyname,columns_set=g1)
columns_set2<- escode (row_under_review,emptyname,columns_set=g2)
columns_set3<- escode (row_under_review,emptyname,columns_set=g3)
columns_set4<- escode (row_under_review,emptyname,columns_set=g4)
}
if(ngroups==5){
columns_set1<- escode (row_under_review,emptyname,columns_set=g1)
columns_set2<- escode (row_under_review,emptyname,columns_set=g2)
columns_set3<- escode (row_under_review,emptyname,columns_set=g3)
columns_set4<- escode (row_under_review,emptyname,columns_set=g4)
columns_set5<- escode (row_under_review,emptyname,columns_set=g5)
}
if(ngroups==6){
columns_set1<- escode (row_under_review,emptyname,columns_set=g1)
columns_set2<- escode (row_under_review,emptyname,columns_set=g2)
columns_set3<- escode (row_under_review,emptyname,columns_set=g3)
columns_set4<- escode (row_under_review,emptyname,columns_set=g4)
columns_set5<- escode (row_under_review,emptyname,columns_set=g5)
columns_set6<- escode (row_under_review,emptyname,columns_set=g6)
}
if(length(columns_set1)>length(g1)){n1<-length(columns_set1[,1])}else{n1<-1}
if(length(columns_set2)>length(g2)){n2<-length(columns_set2[,1])}else{n2<-1}
if(length(columns_set3)>length(g3)){n3<-length(columns_set3[,1])}else{n3<-1}
if(length(columns_set4)>length(g4)){n4<-length(columns_set4[,1])}else{n4<-1}
if(length(columns_set5)>length(g5)){n5<-length(columns_set5[,1])}else{n5<-1}
if(length(columns_set6)>length(g6)){n6<-length(columns_set6[,1])}else{n6<-1}
nresultado<-n1*n2*n3*n4*n5*n6
tab_aux<-matrix(NA,ncol=ncol(row_under_review),nrow=nresultado)
l<-1
for(gi in 1:n1){
for(gii in 1:n2){
for(giii in 1:n3){
for(giv in 1:n4){
for(gv in 1:n5){
for(gvi in 1:n6){
tab_aux[l,g1]<-if(length(columns_set1)>length(g1)){columns_set1[gi,]} 
else{columns_set1}
tab_aux[l,g2]<-if(length(columns_set2)>length(g2)){columns_set2[gii,]} 
else{columns_set2}
tab_aux[l,g3]<-if(length(columns_set3)>length(g3)){columns_set3[giii,]} 
else{columns_set3}
tab_aux[l,g4]<-if(length(columns_set4)>length(g4)){columns_set4[giv,]} 
else{columns_set4}
tab_aux[l,g5]<-if(length(columns_set5)>length(g5)){columns_set5[gv,]} 
else{columns_set5}
tab_aux[l,g6]<-if(length(columns_set6)>length(g6)){columns_set6[gvi,]} 
else{columns_set6}
l<-l+1}}}}}}
tab_aux<-unique(tab_aux)
colnames(tab_aux)<-names(row_under_review)
return(tab_aux)}
