escode <-
function(row_under_review,emptyname=NA,columns_set=NA){
if(is.na(emptyname)){stop("You must inform the code for empty")}
if(length(columns_set)<1){stop("You must inform the set of coluns from the sampe leg")}
row_under_review<-matrix(row_under_review,ncol=length(row_under_review))
analis<-as.matrix(row_under_review)[,columns_set]
names(analis)<-colnames((row_under_review)[,columns_set])
if( sum(analis==emptyname)>2 ){stop("Only 0, 1 or 2 emptys allowed per leg")}
if((length(analis)==1)|(sum(analis==emptyname)==0)|(sum(analis==emptyname)==length(analis))){
resposta<-analis}else{ 
colunas_usadas<-which(!analis ==emptyname)
colunas_vazias<-which(analis ==emptyname)
n_anilhas<-length(analis)
n_vazio<-length(colunas_vazias)
n_usado<-n_anilhas-n_vazio
aux_cobinacoes_possiveis<-rep(emptyname, ( ( (n_vazio+1)*n_usado)+n_vazio) )
for(i in 1:n_usado){aux_cobinacoes_possiveis[(n_vazio+1)*i]<-analis[colunas_usadas[i]]} 
encontre<-seq.int(from=(n_vazio+1), to=((n_vazio+1)*n_usado), by=(n_vazio+1)) 
onde_estao_vazio<-seq(1:length(aux_cobinacoes_possiveis))[-encontre]
n_caixa_cor_aux<-length(aux_cobinacoes_possiveis)
combinacoes_1<-NULL
combinacoes_2<-NULL
contador<-0
if(n_vazio>1){
for(i in 1:(n_caixa_cor_aux-1)){
for(z in which(1:n_caixa_cor_aux >i)){
if(sum(i==encontre)==1|sum(z==encontre)==1){}else{
contador<-contador+1
combinacoes_1[contador]<-i
combinacoes_2[contador]<-z
}}}}else{
combinacoes_1<-seq(from=1, to=length(aux_cobinacoes_possiveis),by=2)
combinacoes_2<-NULL}
resposta<-matrix(NA,ncol=n_anilhas,nrow=length(combinacoes_1))
for(z in 1:length(combinacoes_1)){
resposta[z,]<-aux_cobinacoes_possiveis[sort(c(encontre,combinacoes_1[z],combinacoes_2[z]))]}
resposta<-unique(resposta)
colnames(resposta)<-names(analis)}
return(resposta)}
