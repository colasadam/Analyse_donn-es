library("plot3D")
library("plot3Drgl")
library("scatterplot3d")


#Exercice 1 
A <- read.table("data1TP2.txt", sep="\t", header=TRUE) 
points3D(A$Stature, A$Poids,A$Taille )

moy_stature<-0
moy_poids<-0
moy_taille<-0

for(i in 1:length(A$Stature)){
  moy_stature<-moy_stature+A$Stature[i]
  moy_poids <- moy_poids+ A$Poids[i]
  moy_taille <-moy_taille +A$Taille[i]
}
moy_taille<-moy_taille/length(A$Taille)
moy_poids<-moy_poids/length(A$Poids)
moy_stature<-moy_stature/length(A$Stature)

#Exercice 2 

B<-A
for(i in 1:length(A$Stature)){
  B$Stature[i]<-B$Stature[i]-moy_stature
  B$Poids[i] <- B$Poids[i] -moy_poids
  B$Taille[i] <-B$Taille[i]-moy_taille 
}

V<-matrix(nrow=3,ncol=3)
for(i in 1:3){
  for(j in 1:3){
    if(i==j){
      V[i,j]<-var(A[i])
    }
    else{
      V[i,j]<-cov(A[i],A[j])
    }
  }
}

#exercice 3

x = eigen(V)
x$values 
x$vectors 

#exercice 4 
# l'axe principales avec le plus d'impact est la Stature puis vient le Poids et enfin la Taille

B = as.matrix(B)

#Exercice 5 
C<- B %*% x$vectors
C
princomp(A)$scores
# Avec princomp(A)$scores j'obtiens exactement l'opposé de la matrice trouve apres verfication la matrice trouve est la bonne 
# princomp(A)$scores renvoie la mauvaise valeur 

#Exercice 6 


points3D(A$Stature, A$Poids,A$Taille )

val<-x$values[1]
vec<-x$vectors

scatter3D(x=c(0,-300*vec[1,1]), y=c(0,-300*vec[2,1]),z=c(0,-300*vec[3,1]),add=TRUE, type="l")


#Exercice 7 
plot(C[,1],C[,2])


#Exercice 8 

#les resultats me semble cohérents 

