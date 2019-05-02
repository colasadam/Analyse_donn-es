#COLAS ADAM 


#Exercice 1
culture <- read.table("data1TP1.txt", sep="\t", header=TRUE) 
par(mfrow=c(2,3))
plot(culture$A,culture$Y)
plot(culture$B,culture$Y)
plot(culture$C,culture$Y)
plot(culture$D,culture$Y)
plot(culture$E,culture$Y)

#on peut voir que les graphes sont differents les uns des autres: certain sont monotomes d'autre non...

#Exercice 2
corvariance <- function(X,Y){
  t<-cov(X,Y)
  e_x<-sqrt(var(X))
  e_y<-sqrt(var(Y))
  A<-t/(e_x*e_y)
  A
}
corvariance(culture$A,culture$Y)
corvariance(culture$B,culture$Y)
corvariance(culture$C,culture$Y)
corvariance(culture$D,culture$Y)
corvariance(culture$E,culture$Y)

#on trouve que la variable A a la plus petit valeur parce que on observe que sa courbe est strictement d??croissante


#Exercice 3
spearman <-function(X,Y){
  sum<-0
  r_a<-rank(X)
  r_b<-rank(Y)
  for(i in 1:15){
    sum<-sum+(r_a[i]-r_b[i])^2
  }
  A<-1-6*sum/(15^3-15)
  A
}
spearman(culture$A,culture$Y)
spearman(culture$B,culture$Y)
spearman(culture$C,culture$Y)
spearman(culture$D,culture$Y)
spearman(culture$E,culture$Y)

#nous observons que nous obtenons pas les memes resultats, les nouveaux resultats sont plus precis


#Exercice4

#Je propose de s??parer les donn??es en fonction de leurs cote de la courbe
#on fera par la suite la somme des r??sultats de chaque partie du graphe (la partie montante et la partie descente)


#Exercice5
eleve <- read.table("data2TP1.txt", sep="\t", header=TRUE) 

t <- function(m,y){
  moy<-mean(m)
  abs(moy-y)
  A <-abs(moy-y)/(sqrt(var(m))/sqrt(length(m)))
  A
}


t(eleve$Marseille,19)

# on obtient avec le test 2,177, or en lisant la table on observe qu'il faudrait etre a 2,145 donc on peut en conclure que l'inflation n'affecte pas la vie a Marseille 
# on rejecte le fait que l'afflation l'a affecte



#Exercice 6

inde <- function(m1,m2){
  moy1<-mean(m1)
  moy2<-mean(m2)
  A<-abs(moy1-moy2)/sqrt((var(m1)/length(m1))+(var(m2)/length(m2)))
  A
}
inde(eleve$Marseille,eleve$Aix)

# on obtient avec le test 2,321, or en lisant la table on observe qu'il faudrait etre a 2.048 donc on peut en conclure une independance entre Marseille et Aix
# on rejette le fait qu'il u est une dependance
#pour 2% il faut etre a 2.468  donc on pourrait observe une dependance

#Exercice 7

#1)
val_th <-function(x){
  return(x/16*2132)
}
violet_long<-val_th(9)
violet_rond<-val_th(3)
rouge_long<-val_th(3)
rouge_rond<-val_th(1)

violet_long
violet_rond
rouge_long
rouge_rond

#2)

res <- data.frame(th=c(violet_long,violet_rond,rouge_long,rouge_rond),obs=c(1528,106,117,381),row.names=c("violet long","violet rond","rouge long","rouge rond"))

khi_deux <- function(th,obs){
  sum = 0
  for(i in 1:length(th)){
    sum<-sum+(obs[i]-th[i])^2/th[i]
  }
  return(sum)
}

#3)
print(khi_deux(res$th,res$obs))

#on obtient avec la fonction khi_deux : 966.61 Or avec la table on devrait trouver 7.81 ce qui montre que la r??partition theorique propos?? est rejete



#Exercice 8
absent <-c(29.0,40.0,18.0)
absent_th <-c(34.8,34.8,17.4)

khi_abs<-khi_deux(absent_th,absent)

atyp<-c(5,32,22)
atyp_th<-c(23.6,23.6,11.8)
khi_atyp<-khi_deux(atyp_th,atyp)

typ <-c(46,8,0)
typ_th <-c(21.6,21.6,10.8)

khi_typ<-khi_deux(typ_th,typ)

khi_form <- khi_typ+khi_atyp+khi_abs



color_abs <- c(20,29,12)
color_abs_th <- c(24.4,24.4,12.2)

khi_color_abs <-khi_deux(color_abs_th,color_abs)

color <- c(60,51,28)
color_th <- c(55.6,55.6,27.8)
khi_color <-khi_deux(color_th,color)

khi_color_final <- khi_color+khi_color_abs
print(khi_form)
print(khi_color_final)

# on obtient pour la forme 75.15 et pour la couleur 2.39, Or devrait trouver 5.99 ce qui montre que la couleur est plus importante pour trouver le melanome 
# car comme pour la question 7 la repartition theorique de la forme est rejete



#Exercice 9

#Le test de Student est parametrique car il faut que les donnees qu'il utilisent suivent une distribution normale et si les variances des ??chantillons sont homog??nes
#Le test de Khi deux est non-parametrique car il n'a besoin d'aucune conditions pour s'utiliser 
#On ne peut pas appliquer le test de Student au donnees qualitatives car ces donnees ne suivent pas de loi normal 



#Exercice 10 
#Spearson :Non 
#Spearman : cela depend si on peut classer les donn??es avec rank 
