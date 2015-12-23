setwd("C:/Users/f300053/Documents/Michelin/Etude_R")    #Changer l'espace de travail
sink("resultat_corr.txt")                                 #Enregistrer les résultats dans ce fichier
sink()
rm(list=ls())

library(stats)    #Insertion des librairies
library(cluster)
library(graphics)






#Code création de la matrice des moyennes

matmoyenne<-function(PN)
  
{
  
  C<-matrix(data = c(0), nrow=length(PN), ncol=length(PN))
  y<-0
  
  for (k in 1:length(PN))  
    
  {
    
    for(i in k:length(PN))
      
    {
      for(j in k:i)
        
      {
        y<-y+as.numeric(PN[j])
      } 
      
      
      C[k,i]<-(1/(i-k+1))*y
      y<-0 
    }  
    
    
  }
  
  return(C)
  
}

#Calcul de la matrice finale :

matpourcent<-function(PN)
  
{
  
  Y<-matrix(data = c(0), nrow=length(PN), ncol = length(PN))
  C<-matmoyenne(PN)
  
  for (i in 1:length(PN))  
    
  {
    
    for(k in i:length(PN))
      
    {
      Y[i,k]<-(C[i,k]/mean(PN))      
    }
    
  }      
  
  return(Y)
  
}



#Ne gardez que les cycles d'au moins 3 mois


nettoyage<-function(PN)
{
  
  Y<-matpourcent(PN)
  
  for (i in 1:length(PN))
    
  {
    Y[i,i]<-0
    
  }
  
  for (i in 1:length(PN)-1)
    
  {
    Y[i,i+1]<-0        
  }
  
  return(Y)
}

#Création d'un vecteur pour la visualisation qui contient toutes les valeurs (voir dessin)

vecteurtotal<-function(PN)
  
{
  
  z<-c()
  Y<-nettoyage(PN)
  
  for(k in 1:length(PN))
  {
    for(j in ((k-1)*length(PN)+1):(k*length(PN)))
    {
      z[j]<-Y[k,j-(k-1)*length(PN)]
    }
    
  }
  
  return(z)
  
}


#Ecriture d'un vecteur date de départ w 


datedepart<-function(PN,date)
  
{
  
  depart<-c()
  
  for(k in 1:length(PN))
  {
    for(j in ((k-1)*length(PN)+1):(k*length(PN)))
    {
      depart[j]<-date[k]
    }
    
  }
  
  return(depart)
  
}


#Ecriture d'un vecteur d'arrivée 


datearrivee<-function(PN,date)
  
{
  
  arrivee<-c()
  
  for(k in 1:length(PN))
  {
    for(j in ((k-1)*length(PN)+1):(k*length(PN)))
    {
      arrivee[j]<-date[j-(k-1)*length(PN)]
    }
    
  }
  
  return(arrivee)
  
}




#Fonction qui retourne la matrice des 3 colonnes 

matricetotale<-function(PN,d)
  
{
  
  Z<-matrix(data = c(datedepart(PN,d),datearrivee(PN,d),vecteurtotal(PN)),  ncol = 3)
  
  return(Z)  
  
}





#Ressort la ligne qui correspond au maximum


ligne<-function(PN,d)
  
{
  
  Z<-matricetotale(PN,d)
  c<-c()
  
  c<-Z[which.max(Z[,3]),]
  
  return(c)
}





saisonnalite<-function(dimension,d)
  
{
  
  matricefinale<-matrix(data=c(0),nrow=ncol(dimension),ncol=3)  
  
  for (i in 1:ncol(dimension) )
    
  {
    
    v<-as.vector(dimension[,i])
    matricefinale[i,]<-ligne(v,d)
    
    v<-0
    
  }
  
  return(matricefinale)
  
}



#Insertion des données

data<-read.table("test_saisonnalite.csv",sep=";",header=TRUE)
PN<-as.matrix(data,rownames.force = TRUE) #table des données d'un PN sous forme de matrice
PN

#insertion des dates

d<-read.table("test_saisonnalite_date.csv",sep=";",header=TRUE)  
date<-as.matrix(d,rownames.force = TRUE)
date

#On rentre les données dans une matrice pour les annnées

saisonnalite_periode<-function(dimension,d)
  
{
  
  I<-nrow(dimension)/12  #nombre d'années dans dimension
  matrice_globale<-matrix(data=c(0),nrow=(I)*ncol(dimension),ncol=3)
  matrice_divisee<-matrix(data=c(0),ncol=ncol(dimension),nrow=12)
  
  
  
  
  
  for (k in 0:(I-1)) # création de I matrices des années
    
    
  {
    for(j in 1:12)
      
    { 
      
      matrice_divisee[j,]<-dimension[j+12*k,]
      
    }
    
    
    
    saison<-saisonnalite(matrice_divisee,d)
    
    
    
    for (l in 1:ncol(dimension) )
      
    {    
      matrice_globale[l+ncol(dimension)*k,]<-saison[l,]        
    }
    
  }
  
  
  
  return(matrice_globale)
  
}




#Fonction qui trie dans la matrice pour que ça soit dans le bon ordre

saisonnalite_finale<-function(PN,d)
  
{
  
  
  I<-nrow(PN)/12  #nombre d'années dans PN
  l<-1
  
  Y<-matrix(data=c(0),nrow=I,ncol=3)
  W<-matrix(data=c(0),nrow=(I)*ncol(PN),ncol=3)
  matrice<-saisonnalite_periode(PN,d)
  
  
  
  
  
  for (k in 1:ncol(PN))
    
  {
    for(j in 1:I)
    {
      Y[j,]<-matrice[ncol(PN)*(j-1)+k,]
      W[l,]<-Y[j,]
      l<-l+1
    }
    
  }                                           
  
  return(W)
  
}




#Sortir les données
dototo<-saisonnalite_periode(PN,date)
datata<-saisonnalite_finale(PN,date)

dototo
datata


datatata<-as.data.frame(datata)
write.csv(datatata,file="test_saisonnalite_retour_Z.csv",row.names = TRUE)









