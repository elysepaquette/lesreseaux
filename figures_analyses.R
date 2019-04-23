#produire igraph
#install.packages("igraph")
#install.packages('knitr')
#install.packages('plotrix')
library(igraph)
library(knitr)
library(plotrix)
library(RSQLite)

#connecter a tables.db
tables.db <- dbConnect(SQLite(), dbname="tables.db")

#lier ce script et nettoyage_equipes_1-7.r
bd_cours <- read.csv2('bd_cours_1-7.csv', header = T)
bd_liens <- read.csv2('bd_liens_1-7.csv', header = T)
bd_etudiants <- read.csv2('bd_etudiants_1-7.csv', header = T)

#Creer matrice
mat_liens<-table(bd_liens$etudiant1, bd_liens$etudiant2)
mat_liens<-as.matrix(mat_liens)
mat_liens_classe<-mat_liens[levels(bd_etudiants$id),levels(bd_etudiants$id)]
mat_liens_classe_table<-as.matrix(mat_liens_classe)
g<-graph.adjacency(mat_liens)

#tableau liens vg om
#À noter que élyse est NA pour cela donc juste 25 étudiants
sql_requete_tableau <- "SELECT liens.etudiant1, liens.etudiant2, et1.diete AS 'diete_et1', et2.diete AS 'diete_et2' FROM liens INNER JOIN etudiants AS et1 ON liens.etudiant1 = et1.id INNER JOIN etudiants AS et2 ON liens.etudiant2 = et2.id;"
liens_diete <- dbGetQuery(tables.db,sql_requete_tableau)
tableau_diete<-as.matrix(table(liens_diete$diete_et1,liens_diete$diete_et2))
nb_om_vg<-table(bd_etudiants$diete=='vg')
total<-matrix(nb_om_vg,nrow = 2,ncol = 1)
colnames(total)<-'étudiants avec cette diète'
tableau_diete<-cbind(tableau_diete,total)
colnames(tableau_diete)<-c('omnivore','végétarien','étudiants avec cette diète')
rownames(tableau_diete)<-c('omnivore','végétarien')
tableau_diete
#Mettre dans latex
writeLines(kable(tableau_diete, format = 'latex'),con = 'tableauDiete.tex')


# Calculer le degre pour le graph réseau avec tout le monde
deg <- apply(mat_liens, 2, sum) + apply(mat_liens, 1, sum)
# Le rang pour chaque noeud
rk <- rank(deg)
# Faire un code de couleur
col.vec <- heat.colors(149)
# Attribuer aux noeuds la couleur
V(g)$color = col.vec[rk]

#Fonction pour mettre les etudiants classe au centre des étudiants hors classe
layout_in_circles <- function(g, group=1) {
  layout <- lapply(split(V(g), group), function(x) {
    layout_in_circle(induced_subgraph(g,x))
  })
  layout <- Map(`*`, layout, seq_along(layout))
  x <- matrix(0, nrow=vcount(g), ncol=2)
  split(x, group) <- layout
  x
}

#Créer z pour spécifier que c'est les étudiants de la classe qu'on veux au centre
z<-row.names(mat_liens)
z<-data.frame(z)
k<-as.vector(bd_etudiants$id)
k<-data.frame(k)
for (j in 1:155) {
  if (z[j,1]%in%k$k) z[j,1]=F
}

#Pour la légende
col.vec1 <- heat.colors(5)
degrade<-c('Très peu de liens','Peu de liens','Nombre moyen de liens','Grand nombre de liens','Très grand nombre de liens')

#Créer graphique
#ouvrir pdf
pdf("graphReseau.pdf",width = 6,height = 6) 
# 2. créer graph
plot(g, vertex.label=NA,edge.arrow.mode = 0,
     vertex.frame.color = NA,vertex.size=12,edge.width=0.1,
     layout=layout_in_circles(g,group =!is.na(z)))
legend(bty = "n",
       legend=degrade,x=-1.83,y=1.53,
       fill=col.vec1, border=NA)
# fermer pdf
dev.off() 


#Meme chose pour juste les étudiants de la classe
gg<-graph.adjacency(mat_liens_classe)
# Calculer le degr?
deggg <- apply(mat_liens_classe, 2, sum) + apply(mat_liens_classe, 1, sum)
# Le rang pour chaque noeud
rkgg <- rank(deggg)
# Faire un code de couleur
col.vecgg <- heat.colors(26)
# Attribuer aux noeuds la couleur
V(gg)$color = col.vecgg[rkgg]
# Attribuer aux noeuds la couleur
V(gg)$size = col.vecgg[rkgg]

#Créer graphique et Mettre en pdf
#ouvrir pdf
pdf("graphReseauClasse.pdf",width = 6,height = 6) 
# 2. créer graph
plot(gg, vertex.label=rownames(mat_liens_classe),vertex.label.cex=0.5,vertex.label.color="black",edge.arrow.mode = 0,
     vertex.frame.color = NA,vertex.size=16,edge.width=0.1,layout = layout.kamada.kawai(gg))
legend(bty = "n",
       legend=degrade,x=-1.8,y=1.65,
       fill=col.vec1, border=NA)
# fermer pdf
dev.off() 

#Autres figures
#Nombre de liens avec des personnes différentes en fonction du sexe
bd_liens_freqs<-data.frame(mat_liens)
bd_liens_freq_sans0<-subset(bd_liens_freqs,Freq>0)
liens_etudiants<-table(bd_liens_freq_sans0$Var1)
liens_etudiants<-data.frame(liens_etudiants)
liens_etudiants<-subset(liens_etudiants,liens_etudiants$Var1%in%bd_etudiants$id==T)
#les données de sexe ont été ajoutées à la main, car l'odre des tables était différent et nous ne semblon spas pouvoir le réorganiser
liens_etudiants[,3]<-c('f','h','h','f','f','h','f','a','f','f','f','f','h','f','h','h','f','f','h','f','f','f','f','h','f','h')
for (j in 1:26) {
  if (liens_etudiants[j,3]=='f') liens_etudiants[j,3]='femme'
  else if (liens_etudiants[j,3]=='h') liens_etudiants[j,3]='homme'
  else if (liens_etudiants[j,3]=='a') liens_etudiants[j,3]='autre'
}
names(liens_etudiants)[3]<-'sexe'
mod.sexe<-lm(liens_etudiants$Freq~liens_etudiants$sexe)
summary(mod.sexe)
ano1<-anova(mod.sexe)
#valeur de p pour légende avec 4 décimale
pvalue1<-round(ano1[1,5],4)
#ouvrir pdf
pdf("graphSexe.pdf",width = 6,height = 6) 
# 2. créer graph
plot(as.factor(liens_etudiants$sexe),liens_etudiants$Freq,ylab='Nombre de personnes différentes avec qui un étudiant collabore')
legend('topright',legend = paste('P-value, pvalue1'))
# fermer pdf
dev.off()


##Figure des liens en fonction de la région de naissance
#Création de nouvelle table avec région et liens a partir de la table liensfreq de sql
sql_requete <- "SELECT liensfreq.Freq, liensfreq.Var1, liensfreq.Var2, et1.region_naissance AS 'region_naissance_et1', et2.region_naissance AS 'region_naissance_et2' FROM liensfreq INNER JOIN etudiants AS et1 ON liensfreq.Var1 = et1.id INNER JOIN etudiants AS et2 ON liensfreq.Var2 = et2.id;"
liens_region1 <- dbGetQuery(tables.db,sql_requete)
#mettre si m?me r?gion ou pas
liens_region<-liens_region1
for (j in 1:650) {
  if (is.na(liens_region1[j,4])==T) liens_region[j,6]=NA
  else if (is.na(liens_region1[j,5])==T) liens_region[j,6]=NA
  else if (liens_region1[j,4]==liens_region1[j,5]) liens_region[j,6]=1
  else if (liens_region1[j,4]!=liens_region1[j,5]) liens_region[j,6]=0
}
names(liens_region)[6]<-'meme_region'

#Sans les liens de fréquence 0
liens_region0<-subset(liens_region, liens_region$Freq>0)
test_moyenne<-t.test(liens_region0$Freq~liens_region0$meme_region)
#Ajouter la p-value
pvalue2<-round(test_moyenne$p.value,4)

#Créer graphique et pdf
#ouvrir pdf
pdf("graphRegion.pdf",width = 6,height = 6) 
# 2. créer graph
plot(jitter(liens_region0$meme_region),liens_region0$Freq,xlab = 'Région de naissance identique ou pas',ylab = 'Fréquence des liens')
legend('topright',legend = paste('P-value=',pvalue2))
ablineclip(test_moyenne$estimate[1],-(test_moyenne$estimate[1]-test_moyenne$estimate[2]),x1=0,x2=1)
# fermer pdf
dev.off()


