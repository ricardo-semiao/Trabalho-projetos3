#---- Packadges and functions ----
library(tidyverse)
library(plm)
library(factoextra)

set.seed(240) #For the clustering

abbreviator = function(names){
  a = strsplit(names, " ")
  b = sapply(a, function(x){ifelse(length(x)==1,
                                   substr(x,1,3),
                                   paste(lapply(strsplit(x, split=""),
                                                function(x){ifelse(length(x)>2, x[1], "")}),
                                         collapse=""))})
  #b = paste(b, 1:length(names), sep="-")
  return(b)}

#---- Data preparation ----
DF = readxl::read_excel("m:/Users/Marcus/Downloads/NHA indicators.xlsx")

GHED = list(data.frame(pivot_wider(DF[,c(1,2,3)], names_from=Indicators, values_from=3)),
            data.frame(pivot_wider(DF[,c(1,2,4)], names_from=Indicators, values_from=3)))

for(i in 1:2){
  #Setting column and row names
  colnames(GHED[[i]]) = 
    c("Contry","HeExpPPIB","HeExpPCAP","HeExpDom","HeExpGov","HeExpPri",
      "HeExpVol","HeExpExt","GvExpPPIB","PIB","HeExpFin","Com","Pop","","","","","")
  
  Dic = cbind(abbreviator(GHED[[i]]$Contry), GHED[[i]]$Contry)
  
  Dic[duplicated(Dic[,1]),1] = c("Gnb","Mli","Mar","ngr","SaL","Bng","Irq","SdA","Bes",
                                "Blr","ChR","Grc","Mle","Mnt","Slv","Trk","Idn","Mld",
                                "SiL","Ast","Cmb","Chn","Mly","Mng")
  
  rownames(GHED[[i]]) = Dic[,1]#1:nrow(GHED17)#
  GHED[[i]]$Contry = NULL
  
  
  #Removing unused variables
  GHED[[i]] = GHED[[i]][,-which(colnames(GHED[[i]])=="")]
  GHED[[i]] = GHED[[i]][,-c(2,8:12)]}
  

k = 4 #Chosing the number of clusters

clusters = list()

for(i in 1:2){  
  #Cheking for NA
  which(is.na(GHED[[i]]), arr.ind=TRUE)
  GHED[[1]][131,] = GHED[[2]][131,] #Using montenegro 2018 data as proxy for 2017 missing data
  GHED[[i]][178,6] = 0 #Coercing Niue HeExpVol missing value to 0
  
  GHEDstd = dist(scale(GHED[[i]])) #Standardizing values, then getting the distance
  
  
  #---- Hierachical clustering ----
  mod.hc = hclust(GHEDstd, method = "centroid")
  
  plot(mod.hc) #Plotting dendrogram
  
  clusters[[i]] = cbind(Dic, cutree(mod.hc, k=k))
  
  rect.hclust(mod.hc, k=k, border=1:k) #Updating dendrogram
  
  fviz_cluster(list(data=GHEDstd, cluster=cutree(mod.hc, k=k)))
  
  
  #---- kmeans ----
  #plot(fviz_nbclust(GHEDstd, kmeans, "wss"))
  mod.km = kmeans(GHEDstd, k)
  
  cut = rbind(c(mod.km$cluster[Dic[,1]=="Nam"], 1),
              c(mod.km$cluster[Dic[,1]=="Eri"], 2),
              c(mod.km$cluster[Dic[,1]=="Mex"], 3),
              c(mod.km$cluster[Dic[,1]=="Cub"], 4))
  
  clusters2 = numeric()
  for(j in 1:4){
    clusters2[mod.km$cluster==cut[j,1]] = cut[j,2]}
  mod.km$cluster = clusters2
  
  clusters[[i]] = cbind(clusters[[i]], mod.km$cluster)
  
  plot(fviz_cluster(mod.km, GHEDstd, geom="text", labelsize=10))}

CST = data.frame(clusters[[1]][,c(1,2,4)], clusters[[2]][,4])
CST[,1] = as.character(CST[,1])

#---- comparation with IDH ----
IDH = readxl::read_excel("m:/Users/Marcus/Downloads/Human Development Index (HDI).xlsx")

cut = rbind(c(0.000, 0.499),
            c(0.500, 0.799),
            c(0.800, 0.899),
            c(0.900, 1.000))

IDH[,3:4] = sapply(IDH[,3:4], function(x){
  n = round(as.numeric(x),3)
  
  group=0
  for(j in 1:length(n)){
    for(i in 1:4){
      group = ifelse(cut[i,1] <= n[j] & n[j] <= cut[i,2], i, group)}
  n[j] = group}
  return(n)})

IDH = as.data.frame(na.omit(IDH)[,-1])

IDH[which(!IDH[,1] %in% CST[,2]),1] = c("Bolivia Plurinational States of",
                                        "Cabo Verde Republic of",
                                        "Democratic Republic of the Congo",
                                        "Czech Republic",
                                        "Eswatini","",
                                        "Iran","Republic of Korea",
                                        "","",
                                        "Republic of Moldova",
                                        "The Republic of North Macedonia",
                                        "", "",
                                        "United Republic of Tanzania",
                                        "United States of America",
                                        "")
CST[which(!CST[,1] %in% IDH[,1]),1]


Comp = merge(IDH, CST, by.x="Country", by.y="X2")

Comp = Comp[,c(1,4,2,3,5,6)]

Comp[,3:6] = apply(Comp[,3:6], 2, as.numeric)

#eq17 = eq18 = character()
#for(i in 1:nrow(Comp)){
#  eq17[i] = paste(Comp[i,c(2,4)], collapse="-")
#  eq18[i] = paste(Comp[i,c(3,5)], collapse="-")}

#for(j in 1:2){
#  for(i in 1:4){
#    mod = lm(Comp[,1+j*2-1]==i ~ . - 1, as.factor(Comp[,3+j]))
#    coef = mod$coefficients/sum(mod$coefficients)
#    names(coef) = NULL
#    print(paste(i,j,"----"))
#    print(coef)}}

#1-4, 2-2, 3-1, 4-3
#1-1, 2-3, 3-4, 4-2

#Comp = as.data.frame(apply(Comp, 2, as.factor))
#
#levels(Comp$X2) = c(4,2,1,3)
#levels(Comp$clusters..2.....4.) = c(4,3,1,2)
#
#Comp = Comp[,c(1,2,4,3,5)]

apply(Comp, 1, function(x)paste(x,collapse=","))


