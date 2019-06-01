#Exercice 1 
exo1=function(){
  nuage<-matrix(nrow=300,ncol=2)
  
  for(i in 1:100){
    nuage[i,1]<-runif(1, min=0, max=1)
    nuage[i,2]<-runif(1, min=0, max=1)
  }
  
  mat2<-matrix(nrow=100,ncol=2)
  mat2[,1]<-rnorm(100, 4, 1)
  mat2[,2]<-rnorm(100,0, 1)
  
  scale(mat2[,2], center = TRUE, scale = TRUE)
  for(i in 101:200){
    nuage[i,1]<-mat2[i-100,1]
    nuage[i,2]<-mat2[i-100,2]
  }
  
  mat3<-matrix(nrow=100,ncol=2)
  mat3[,1]<-rnorm(100, mean = 0.5, sd =sqrt( 2))
  mat3[,2]<-rnorm(100, mean = 6, sqrt(2))
  for(i in 201:300){
    nuage[i,1]<-mat3[i-200,1]
    nuage[i,2]<-mat3[i-200,2]
  }
  return(nuage)
}
nuage=exo1()
plot(nuage)

#Exercice 2
nuage=exo1()
nuage2<-nuage
mat_id <- diag(300)
C<-dist(nuage, method = "euclidean", diag = FALSE, upper = FALSE)
tab_dist<-as.matrix(C)

while(nrow(tab_dist)>3){
  lem<-which(tab_dist==min(C),arr.ind = T) #On récupére le minimum de la matrice euclidienne
  mini=lem[1,1] #on recupere la ligne et la colonne
  minj=lem[1,2] 
  nuage[mini,1]<-(nuage[mini,1]+nuage[minj,1])/2 #on associe au point i la moyenne des points i et j 
  nuage[mini,2]<-(nuage[mini,2]+nuage[minj,2])/2 #(calcul du nouveau bary-centre de la classe)
  mat_id[mini,]<-mat_id[mini,]+mat_id[minj,] #on ajoute a la ligne i de la matrice identité la ligne j 
  mat_id<-mat_id[-minj,] #on supprime la ligne j de la matrice identité 
  nuage<-nuage[-minj,] #on supprime le point j du tableau de point
  C<-dist(nuage, method = "euclidean", diag = FALSE, upper = FALSE) #on recalcule la matrice euclidienne
  tab_dist<-as.matrix(C)
}



#Exercice 3 
mat_id<-t(mat_id)
z<-c(1:300)
#on recupere ici la classe de chaque point du tableau
for (i in 1:nrow(mat_id)){
  if(mat_id[i,1]==1){
    z[i]=1
  }
  if(mat_id[i,2]==1){
    z[i]=2
  }
  if(mat_id[i,3]==1){
    z[i]=3
  }
}
#on affiche le  uage de points avec les couleurs associées a chaque classe ainsi que le centre de gravité de chaque classe 
plot(nuage2,  col = c("red", "blue", "green")[z])
points(nuage,pch=19)

#on compte le nombre d'élément de chaque classe a fin de construire des matrice, correspondant a chaque classe, qui contiendra les pointrs de sa classe
nb_1=0
nb_2=0
nb_3=0
for (i in 1:300){
  if(z[i]==1){
    nb_1=nb_1+1
  }
  if(z[i]==2){
    nb_2=nb_2+1
  }
  if(z[i]==3){
    nb_3=nb_3+1
  }
}

class_1 =matrix(nrow=nb_1,ncol=2)
class_2 =matrix(nrow=nb_2,ncol=2)
class_3 =matrix(nrow=nb_3,ncol=2)

#on remplit les matrix avec les éléments de sa classe
nb_1=1
nb_2=1
nb_3=1
for (i in 1:300){
  if(z[i]==1){
    class_1[nb_1,1]=nuage2[i,1]
    class_1[nb_1,2]=nuage2[i,2]
    nb_1=nb_1+1
  }
  if(z[i]==2){
    class_2[nb_2,1]=nuage2[i,1]
    class_2[nb_2,2]=nuage2[i,2]
    nb_2=nb_2+1
  }
  if(z[i]==3){
    class_3[nb_3,1]=nuage2[i,1]
    class_3[nb_3,2]=nuage2[i,2]
    nb_3=nb_3+1
  }
}

#On va maintenant calculer l'inertie relative intra classe
#On récupére la distance entre tout les points de chaque classe
inertie_intra_class1=dist(class_3, method = "euclidean", p = 2)
inertie_intra_class2=dist(class_3, method = "euclidean", p = 2)
inertie_intra_class3=dist(class_3, method = "euclidean", p = 2)

#on les porte à la puissance 2 
inertie_intra_class1=inertie_intra_class1^2
inertie_intra_class2=inertie_intra_class2^2
inertie_intra_class3=inertie_intra_class3^2

#On fait la somme de toutes les distances
inertie_intra_class=sum(inertie_intra_class1)+sum(inertie_intra_class2)+sum(inertie_intra_class3)
print(inertie_intra_class)
