#install.packages('RSQLite')
library(RSQLite)

#lier ce script et nettoyage_equipes_1-7.r
bd_cours <- read.csv2('bd_cours_1-7.csv', header = T)
bd_liens <- read.csv2('bd_liens_1-7.csv', header = T)
bd_etudiants <- read.csv2('bd_etudiants_1-7.csv', header = T)


tables.db <- dbConnect(SQLite(), dbname="tables.db")

#creation de la table cours
cours_sql<- "CREATE TABLE cours (sigle CHAR(6),pratique BOLEAN(1),credits INTEGER(1),concentration BOLEAN(1),option BOLEAN(1),PRIMARY KEY(sigle));"
dbSendQuery(tables.db,cours_sql)
#injection des données
dbWriteTable(tables.db,append=TRUE,name="cours",value=bd_cours, row.names=FALSE)


#creation de la table etudiants
etudiants_sql <- "CREATE TABLE etudiants (id VARCHAR,prenom VARCHAR,nom VARCHAR,sexe CHAR(1),naissance INTEGER(4),faune BOLEAN(1),programme BOLEAN(1),pays_naissance CHAR(2),region_naissance INTEGER(2),diete CHAR(2),PRIMARY KEY(id) FOREIGN KEY(id) REFERENCES liens(etudiant1));"
dbSendQuery(tables.db,etudiants_sql)
#injection des données
dbWriteTable(tables.db,append=TRUE,name="etudiants",value=bd_etudiants, row.names=FALSE)



#creation de la table liens
liens_sql <- "CREATE TABLE liens (sigle CHAR(6),etudiant1 VARCHAR,etudiant2 VARCCHAR,session CHAR(3),id VARCHAR,PRIMARY KEY(id) FOREIGN KEY(sigle) REFERENCES cours(sigle), FOREIGN KEY(etudiant1) REFERENCES etudiants (id) ON DELETE CASCADE, FOREIGN KEY(etudiant2) REFERENCES etudiants (id) ON DELETE CASCADE);"
dbSendQuery(tables.db,liens_sql)
#injection de donn?es
dbWriteTable(tables.db,append=TRUE,name="liens",value=bd_liens, row.names=FALSE)


#creation de la table liens avec fréquence
liens_freq_sql <- "CREATE TABLE liensfreq (Var1 VARCHAR,Var2 VARCCHAR,Freq INTEGER(2),id VARCHAR,PRIMARY KEY(id) FOREIGN KEY(Var1) REFERENCES etudiants (id) ON DELETE CASCADE, FOREIGN KEY(Var2) REFERENCES etudiants (id) ON DELETE CASCADE);"
dbSendQuery(tables.db,liens_freq_sql)
# Creer table liens avec frequences
mat_liens<-table(bd_liens$etudiant1, bd_liens$etudiant2)
mat_liens<-as.matrix(mat_liens)
bd_liens_freq<-data.frame(mat_liens)
bd_liens_freq<-subset(bd_liens_freq,bd_liens_freq$Var1%in%bd_etudiants$id==T)
bd_liens_freq1<-data.frame(table(bd_liens$etudiant1,bd_liens$etudiant2))
bd_liens_freq1<-subset(bd_liens_freq1,bd_liens_freq1$Var1%in%bd_etudiants$id==T)
for (j in 1:length(bd_liens_freq1$Var1)) {
  if (bd_liens_freq1[j,1]==bd_liens_freq1[j,2]) bd_liens_freq=bd_liens_freq[-j,]
}
# #Ajouté id
bd_liens_freq[,4]<- paste0(bd_liens_freq$Var1,'-',bd_liens_freq$Var2)
names(bd_liens_freq)[4]<-'id'
#injection de données
dbWriteTable(tables.db,append=TRUE,name="liensfreq",value=bd_liens_freq, row.names=FALSE)

