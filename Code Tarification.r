## Analyse unidimensionnelle de la fréquence
## de la sévérité et de la prime pure

donnees.h <- read.table("ProfilsHistoriques.txt", dec=",",sep="\t",header=TRUE)
donnees.c <- read.table("ProfilsCandidats.txt", dec=",",sep="\t",header=TRUE)

require("taRifx")
require(faraway)
require(leaps)
require(MASS)
library("MASS")
require(car)
require(lattice)

dat.h <- unfactor.data.frame(donnees.h)
dat.c <- unfactor.data.frame(donnees.c)

f.donnees <- function(yy)
    {
        E <- tapply(dat.h$Nbre_risques,yy,sum)
        N <- tapply(dat.h$Nbre_sinistres,yy,sum)
        S <- tapply(dat.h$Encouru_total,yy,sum)
        donnees <- data.frame(var.exo=sort(unique(yy)),E=E,N=N,S=S,
                              freq=N/E,sev=S/N,PP=S/E)
        donnees
    }

frame.annee <- f.donnees(dat.h$Annee)
frame.age <- f.donnees(dat.h$Age_assure)
frame.sex <- f.donnees(dat.h$Sexe_assure)
frame.fumeur <- f.donnees(dat.h$Ind_fumeur)
frame.emploi <- f.donnees(dat.h$Emploi_assure)
frame.statut <- f.donnees(dat.h$Statut_marital)
frame.enfant <- f.donnees(dat.h$Nbre_enfants)
frame.conduite <- f.donnees(dat.h$Exp_cond)
frame.kmannuel <- f.donnees(dat.h$km_annuel)
frame.utiltravail <- f.donnees(dat.h$Ind_util_travail)
frame.utilaffaire <- f.donnees(dat.h$Ind_util_affaire)
frame.scorecredit <- f.donnees(dat.h$Score_credit)
frame.region <- f.donnees(dat.h$Region)
frame.clientdepuis <- f.donnees(dat.h$Client_depuis)
frame.indoccas <- f.donnees(dat.h$Ind_occas)
frame.sport <- f.donnees(dat.h$Ind_vehicule_sport)
frame.valeur.auto <- f.donnees(dat.h$Valeur_vehicule)
frame.age.auto <- f.donnees(dat.h$Age_vehicule)
frame.couleur.auto <- f.donnees(dat.h$Couleur_vehicule)
frame.auto.loue <- f.donnees(dat.h$Ind_vehicule_loue)
frame.infractions <- f.donnees(dat.h$Nbre_infract)
frame.temps.sinistre <- f.donnees(dat.h$Temps_sn_sin)

#liste des tableaux

frame.annee
frame.age
frame.sex
frame.fumeur
frame.emploi
frame.statut
frame.enfant
frame.conduite
frame.kmannuel
frame.utiltravail
frame.utilaffaire
frame.scorecredit
frame.region
frame.clientdepuis
frame.indoccas
frame.sport
frame.valeur.auto
frame.age.auto
frame.couleur.auto
frame.auto.loue
frame.infractions
frame.temps.sinistre

f.graphe <- function(frame,xvec,arg=TRUE)
    {
        if (arg)
            {
                op <- par(mfrow=c(2,2))

                plot(frame$freq~frame$var.exo,
                   main="Fréquence des sinistres",
                     xlab=xvec,ylab="Fréquences",pch=20)
                plot(frame$sev~frame$var.exo,
                 main="Sévérité des sinistres",
                     xlab=xvec,ylab="Sévérité",pch=20)
                plot(frame$PP~frame$var.exo,main="Prime Pure",
                     xlab=xvec,ylab="Prime Pure",pch=20)

                par(op)
            }
        else
            {
                op <- par(mfrow=c(2,2))
                barplot(frame$freq,names=frame$var.exo,
                        main="Fréquence des sinistres",
                        ylab="Fréquences",xlab=xvec)
                 barplot(frame$sev,names=frame$var.exo,
                        main="Sévérité des sinistres",
                         ylab="Sévérité", xlab=xvec)
                 barplot(frame$PP,names=frame$var.exo,
                        main="Prime Pure",
                         ylab="Prime Pure",xlab=xvec)
                par(op)
            }
    }

#liste des graphiques

f.graphe(frame.annee,"Année")
f.graphe(frame.age,"Âge")
f.graphe(frame.sex,"Sexe de l'assurée",FALSE)
f.graphe(frame.fumeur,"Indicateur Fumeur",FALSE)
f.graphe(frame.emploi,"Type d'emploi",FALSE)
f.graphe(frame.statut,"Statut Marital",FALSE)
f.graphe(frame.enfant,"Nombre d'enfants")
f.graphe(frame.conduite,"Expérience de conduite")
f.graphe(frame.kmannuel,"Kilométrage annuel")
f.graphe(frame.utiltravail,"Utiliser pour aller travailler",FALSE)
f.graphe(frame.utilaffaire,"Utiliser pour affaires",FALSE)
f.graphe(frame.scorecredit,"Score de Crédit")
f.graphe(frame.region,"Région de résidence",FALSE)
f.graphe(frame.clientdepuis,"Client Depuis")
f.graphe(frame.indoccas,"Conducteur occasionnel",FALSE)
f.graphe(frame.sport,"Véhicule Sport",FALSE)
f.graphe(frame.valeur.auto,"Valeur Véhicule à l'état neuf")
f.graphe(frame.age.auto,"Âge du Véhicule")
f.graphe(frame.couleur.auto,"Couleur du véhicule",FALSE)
f.graphe(frame.auto.loue, "Véhicule loué",FALSE)
f.graphe(frame.infractions,"Nombre infractions")
f.graphe(frame.temps.sinistre, "Nombre d'année depuis dernier sinistre")

f.log <- function(frame,xvec)
    {
        op <- par(mfrow=c(2,2))
        plot(frame$freq~I(log(frame$var.exo)),
        main="Fréquence des sinistres",
             xlab=xvec,ylab="Fréquences",pch=20)
        plot(frame$sev~I(log(frame$var.exo)),
        main="Sévérité des sinistres",
             xlab=xvec,ylab="Sévérité",pch=20)
        plot(frame$PP~I(log(frame$var.exo)),main="Prime Pure",
             xlab=xvec,ylab="Prime Pure",pch=20)

        par(op)
    }

f.carre <- function(frame,xvec)
    {
        op <- par(mfrow=c(2,2))

        plot(frame$freq~I((frame$var.exo)^2),
main="Fréquence des sinistres",
             xlab=xvec,ylab="Fréquences",pch=20)
        plot(frame$sev~I((frame$var.exo)^2),
main="Sévérité des sinistres",
             xlab=xvec,ylab="Sévérité",pch=20)
        plot(frame$PP~I((frame$var.exo)^2),main="Prime Pure",
             xlab=xvec,ylab="Prime Pure",pch=20)

        par(op)
    }

f.expo <- function(frame,xvec)
    {
        op <- par(mfrow=c(2,2))

        plot(frame$freq~I(exp(frame$var.exo)),
main="Fréquence des sinistres",
             xlab=xvec,ylab="Fréquences",pch=20)
        plot(frame$sev~I(exp(frame$var.exo)),
main="Sévérité des sinistres",
             xlab=xvec,ylab="Sévérité",pch=20)
        plot(frame$PP~I(exp(frame$var.exo)),main="Prime Pure",
             xlab=xvec,ylab="Prime Pure",pch=20)

        par(op)
    }

f.inv <- function(frame,xvec)
    {
        op <- par(mfrow=c(2,2))
        plot(frame$freq~I(1/(frame$var.exo)),
main="Fréquence des sinistres",
             xlab=xvec,ylab="Fréquences",pch=20)
        plot(frame$sev~I(1/(frame$var.exo)),
main="Sévérité des sinistres",
             xlab=xvec,ylab="Sévérité",pch=20)
        plot(frame$PP~I(1/frame$var.exo),main="Prime Pure",
             xlab=xvec,ylab="Prime Pure",pch=20)
        par(op)
    }

f.sqrt <- function(frame,xvec)
    {
        op <- par(mfrow=c(2,2))
        plot(frame$freq~I(sqrt(frame$var.exo)),
main="Fréquence des sinistres",
             xlab=xvec,ylab="Fréquences",pch=20)
        plot(frame$sev~I(sqrt(frame$var.exo)),
main="Sévérité des sinistres",
             xlab=xvec,ylab="Sévérité",pch=20)
        plot(frame$PP~I(sqrt(frame$var.exo)),main="Prime Pure",
             xlab=xvec,ylab="Prime Pure",pch=20)
        par(op)
    }

#Graphes données transformées

#Expérience de conduite
op <- par(mfrow=c(2,1))
plot(frame.conduite$freq~I(log(frame.conduite$var.exo+1)),
     main="Fréquence des sinistres",xlab="Log(Exp_Cond+1)",
     ylab="Fréquences",pch=20)
plot(frame.conduite$PP~I(log(frame.conduite$var.exo+1)),
     main="Prime Pure",
     xlab="Log(Exp_Cond+1)",ylab="Prime Pure",pch=20)

par(op)

#Client depuis
op <- par(mfrow=c(2,1))
plot(frame.clientdepuis$freq~I(log(frame.clientdepuis$var.exo+1)),
     main="Fréquence des sinistres",xlab="Log(Client_depuis+1)",
     ylab="Fréquences",pch=20)
plot(frame.clientdepuis$PP~I(log(frame.clientdepuis$var.exo+1)),
     main="Prime Pure",
     xlab="Log(Client_depuis+1)",ylab="Prime Pure",pch=20)
par(op)

#Âge
op <- par(mfrow=c(2,1))
plot(frame.age$freq~I(1/frame.age$var.exo),
     main="Fréquence des sinistres",xlab="1/Âge",
     ylab="Fréquences",pch=20)
plot(frame.age$PP~I(1/frame.age$var.exo),
     main="Prime Pure",
     xlab="1/Âge",ylab="Prime Pure",pch=20)
par(op)

#Âge Véhicule
op <- par(mfrow=c(2,1))
plot(frame.age.auto$freq~I(frame.age.auto$var.exo^2),
     main="Fréquence des sinistres",xlab="(Age_vehicule)^2",
     ylab="Fréquences",pch=20)
plot(frame.age.auto$sev~I(sqrt(frame.age.auto$var.exo)),
     main="Prime Pure",
     xlab="Racine.Carre(Age_vehicule)",ylab="Prime Pure",pch=20)
par(op)

attach(donnees.h)

freq <- Nbre_sinistres/Nbre_risques
sev <- Encouru_total/Nbre_sinistres
PP <- Encouru_total/Nbre_risques
data.expl <- donnees.h[,2:23]
data.factor <- data.expl
data.factor$Nbre_enfants <- factor(Nbre_enfants)
data.factor$Nbre_infract <- factor(Nbre_infract)
data.factor$Temps_sn_sin <- factor(Temps_sn_sin)

data.t.freq <- data.factor
data.t.freq$Exp_cond <- log(Exp_cond+1)
data.t.freq$Client_depuis <- log(Client_depuis+1)
data.t.freq$Age_assure<- 1/Age_assure
data.t.freq$Age_vehicule <- Age_vehicule^2

data.t.sev <- data.t.freq

data.t.sev$Exp_cond <- Exp_cond
data.t.sev$Client_depuis <- Client_depuis
data.t.sev$Age_assure<- Age_assure
data.t.sev$Age_vehicule <- sqrt(Age_vehicule)

data.t.pp <- data.t.freq
data.t.pp$Age_vehicule <- Age_vehicule

data.pp <- data.t.pp
data.pp$Exp_cond <- Exp_cond
data.freq <- data.t.freq
data.freq$Exp_cond <- Exp_cond
data.freq$Age_vehicule <- Age_vehicule
data.sev <- data.t.sev
data.sev$Age_vehicule <- log(Age_vehicule+0.01)

f.summary <- function(modele,arg=TRUE)
{
    aovv <- anova(modele)
    AIC <- extractAIC(modele)[2]
    Ra2 <- summary(modele)$adj.r.squared
    R2 <- summary(modele)$r.squared
    sumsq1 <- sum(head(aovv$Sum,-1))
    df1 <- sum(head(aovv$Df,-1))
    meansq1 <- sumsq1/df1
    sumsq2 <- tail(aovv$Sum,1)
    df2 <- tail(aovv$Df,1)
    meansq2 <- sumsq2/df2
    fvalue <- meansq1/meansq2
    prob <- pf(fvalue,df1,df2,low=FALSE)

    if (arg)
        {
            data.frame(Source=c("Regression","Erreur"),Df=c(df1,df2),
                       SumSq=c(sumsq1,sumsq2),
                       MeanSq=c(meansq1,meansq2),Fvalue=c(fvalue,NA),
                       Prob.F=c(prob,NA))
        }
    else
        {
            data.frame(R2=R2,Ra2=Ra2,AIC=AIC)
        }
}

f.summary.glm <- function(modele)
{
    mod <- summary(modele)
 data.frame(Null.deviance=mod$null.deviance,Res.deviance=mod$deviance,
               AIC=mod$aic)
}

f.postulat.glm <- function(modele)
{
    r.pearson <- residuals(modele,type="pearson")
    r.deviance <- residuals(modele,type="deviance")
    n <- length(r.pearson)
    previsions <- fitted(modele)
    op <- par(mfrow=c(2,2))
    plot(1:n,r.pearson,pch=16,xlab="i",ylab="residus de Pearson",
         main=paste("Residus en fonction",
"du numéro d'observation",sep="\n"))
     plot(1:n,r.deviance,pch=16,xlab="i",ylab="residus de Déviance",
         main=paste("Residus en fonction",
"du numéro d'observation",sep="\n"))
    qqnorm(r.pearson,main="QQ-Plot - Résidus de Pearson")
    qqline(r.pearson)
    qqnorm(r.deviance,main="QQ-Plot - Résidus de déviance")
    qqline(r.deviance)
    par(op)
}

f.postulat <- function(modele,arg=TRUE)
{
    residus <- residuals(modele)
    previsions <- fitted(modele)
    n <- length(residus)
    if (arg)
        {
            op <- par(mfrow=c(2,2))
            plot(previsions,residus,pch=16,
xlab="Valeurs Predites",ylab="Résidus",
                 main=paste("Graphe des résidus en fonction",
                     "des valeurs prédites",sep="\n"))
            plot(1:n,residus,pch=16,xlab="i",ylab="residus",
                 main=paste("Residus en fonction",
"du numéro d'observation",sep="\n"))
            hist(rstandard(modele),freq=FALSE,
                 main="Histogramme des résidus studentisés")
            curve(dnorm(x),add=TRUE,col="red",lwd=2)
            qqnorm(rstandard(modele),pch=16,
main=paste("QQ-Plot normal pour",
                                                
"les résidus studentisés",sep="\n"))
            qqline(rstandard(modele))
            par(op)
        }
    else
        qqPlot(modele,main="QQ-Plot avec Intervalle de Confiance",
               xlab="Quantiles théroques",
               ylab="Résidus studentisés",pch=16)
}

# Modèle 1

mod.freq11 <- lm(freq~.,data=data.expl)
mod.sev11 <- lm(sev~.,data=data.expl)

f.summary(mod.freq11)
f.summary(mod.sev11)

f.summary(mod.freq11,FALSE)
f.summary(mod.sev11,FALSE)

#analyse avec variable indicatrice pour variable numérique
#tout indique qu'il faut mettre ces variables numériques en factor
#création de variables indicatrices

mod.freq.fact11 <- lm(freq~.,data=data.factor)
mod.sev.fact11 <- lm(sev~.,data=data.factor)

f.summary(mod.freq.fact11)
f.summary(mod.sev.fact11)
summary(mod.sev.fact11)
f.summary(mod.freq.fact11,FALSE)
f.summary(mod.sev.fact11,FALSE)

#analyse des transformations

mod.freq.fact12 <- lm(freq~.,data=data.t.freq) #modeles avec #transformations
mod.sev.fact12 <- lm(sev~.,data=data.t.sev)

f.summary(mod.freq.fact12)
f.summary(mod.freq.fact12,FALSE)
f.summary(mod.sev.fact12)
f.summary(mod.sev.fact12,FALSE)

#modele 2

mod.freq.fact21 <- lm(log(freq)~.,data=data.factor)
mod.sev.fact21 <- lm(log(sev)~.,data=data.factor)
f.summary(mod.freq.fact21)
f.summary(mod.freq.fact21,FALSE)
f.summary(mod.sev.fact21)
f.summary(mod.sev.fact21,FALSE)
summary(mod.sev.fact21)

mod.freq.fact22 <- lm(log(freq)~.,data=data.t.freq)#modeles avec transformations
mod.sev.fact22 <- lm(log(sev)~.,data=data.t.sev)
mod.sev.fact23 <- lm(log(sev)~.,data=data.sev)
mod.freq.fact23 <- lm(log(freq)~.,data=data.freq)

f.summary(mod.freq.fact23)
f.summary(mod.freq.fact23,FALSE)
f.summary(mod.freq.fact22)
f.summary(mod.freq.fact22,FALSE)
f.summary(mod.sev.fact22)
f.summary(mod.sev.fact22,FALSE)
f.summary(mod.sev.fact23)
f.summary(mod.sev.fact23,FALSE)

#modele 3

mod.pp31 <- lm(PP~.,data=data.factor)
mod.pp32 <- lm(PP~.,data=data.t.pp) #modele avec transformations
mod.pp33 <- lm(PP~.,data=data.pp)

f.summary(mod.pp31)
f.summary(mod.pp31,FALSE)
f.summary(mod.pp32)
f.summary(mod.pp32,FALSE)
f.summary(mod.pp33)
f.summary(mod.pp33,FALSE)
summary(mod.pp32)

#modele 4

mod.pp41 <- lm(log(PP)~.,data=data.factor)
mod.pp42 <- lm(log(PP)~.,data=data.t.pp)
mod.pp43 <- lm(log(PP)~.,data=data.pp)
summary(mod.pp43)
f.postulat(mod.pp43)
f.postulat(mod.pp43,FALSE)
f.summary(mod.pp41)
f.summary(mod.pp41,FALSE)
f.summary(mod.pp42)
f.summary(mod.pp42,FALSE)
f.summary(mod.pp43)
f.summary(mod.pp43,FALSE)
summary(mod.pp42)

#modele 5 glm séparés

mod.freq51 <- glm(Nbre_sinistres~.+offset(log(Nbre_risques))
                  ,data=data.factor,family=poisson)
mod.sev51 <- glm(sev~.,data=data.factor,family=Gamma(link=log),
                 weights=Nbre_sinistres)
mod.freq53 <- glm(Nbre_sinistres~.+offset(log(Nbre_risques))
                  ,data=data.freq,family=poisson)
mod.sev53 <- glm(sev~.,data=data.sev,family=Gamma(link=log),
                 weights=Nbre_sinistres)
summary(mod.freq53)
summary(mod.sev53)
f.summary.glm(mod.freq53)
f.summary.glm(mod.sev53)
summary(mod.freq51)
summary(mod.sev51)
f.summary.glm(mod.freq51)
f.summary.glm(mod.sev51)

#avec données transformées

mod.freq52 <- glm(Nbre_sinistres~.+offset(log(Nbre_risques))
                  ,data=data.t.freq,family=poisson)
mod.sev52 <- glm(sev~.,data=data.t.sev,family=Gamma(link=log),
                 weights=Nbre_sinistres)
f.summary.glm(mod.freq52)
f.summary.glm(mod.sev52)

#modele 6 

Encouru.new <- floor(Encouru_total)
mod61 <- glm(Encouru.new~.+offset(log(Nbre_risques))
                  ,data=data.factor,family=poisson)
f.summary.glm(mod61)

#avec donnees transformées
mod62 <- glm(Encouru.new~.+offset(log(Nbre_risques))
                  ,data=data.t.freq,family=poisson)

f.summary.glm(mod62)

## Modele 4 (choix final)

#Données transformeés pour le modèle final
data.pp.test <- data.factor
data.pp.test$Client_depuis <- log(Client_depuis+1)
data.pp.test$Age_assure <- 1/Age_assure^2
data.pp.test$Exp_cond <- Exp_cond
data.pp.test$km_annuel <- log(km_annuel)
data.pp.test$Valeur_vehicule <- log(Valeur_vehicule)

#########MODELE FINAL###########################

mod.final <- lm(log(PP)~.,data=data.pp.test)
mod.final

#selection stepwise

mod.final <- stepAIC(mod.final,mod.final,direction="both")
f.postulat(mod.final)
f.postulat(mod.final,arg=FALSE)
qqPlot(mod.final,main="QQ-Plot avec Intervalle de Confiance",
               xlab="Quantiles théroques",
               ylab="Résidus studentisés")

#intervalles de confiance pour les paramètres

int.conf <- confint(mod.final)
int.conf
coefficients(mod.final)

#Graphique prime pure prédite moyenne en fonction des variables explicatives

fitted.pred <- exp(fitted(mod.final))
data.pred <- dat.h[,2:23]
data.pred$PP.pred <- fitted.pred

f.graphe.pred <- function(yy,xvec,arg=TRUE)
    {
        count <- as.data.frame(table(yy))$Freq
        PP.prev <- tapply(data.pred$PP.pred,yy,sum)
  	  frame <- data.frame(var.exo=sort(unique(yy)),
PP.pred=PP.prev/count)
        if (arg)
            {
                plot(frame$PP.pred~frame$var.exo,
main="Prime Pure prédite",
xlab=xvec,col="blue",ylab="Prime Pure prédite",pch=20)
            }
        else
            {
                barplot(frame$PP.pred,names=frame$var.exo,
                        main="Prime Pure Prédite",
                        ylab="Prime Pure Prédite",xlab=xvec)
            }
    }

f.graphe.uni <- function(frame,xvec,arg=TRUE)
    {
        if (arg)
            {
                plot(frame$PP~frame$var.exo,main=paste("Prime Pure",
                  "(de l'analyse unidimensionnelle)",sep="\n"),
                     xlab=xvec,col="blue",ylab="Prime Pure",pch=20)
            }
        else
            {
                barplot(frame$PP,names=frame$var.exo,
main=paste("Prime Pure",
                  "(de l'analyse unidimensionnelle)",sep="\n"),
                        ylab="Prime Pure",xlab=xvec)
            }
    }

op <- par(mfrow=c(2,2))
f.graphe.pred(Annee,"Annee")
f.graphe.uni(frame.annee,"Année")
f.graphe.pred(Age_assure,"Âge de l'assure")
f.graphe.uni(frame.age,"Âge")
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Exp_cond,"Expérience de Conduite")
f.graphe.uni(frame.conduite,"Expérience de conduite")
f.graphe.pred(km_annuel,"Kilométrage annuel")
f.graphe.uni(frame.kmannuel,"Kilométrage annuel")
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Score_credit,"Score de crédit")
f.graphe.uni(frame.scorecredit,"Score de Crédit")
f.graphe.pred(Client_depuis,"Client depuis")
f.graphe.uni(frame.clientdepuis,"Client Depuis")
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Valeur_vehicule,"Valeur du véhicule")
f.graphe.uni(frame.valeur.auto,"Valeur Véhicule à l'état neuf")
f.graphe.pred(Age_vehicule,"Âge du vehicule")
f.graphe.uni(frame.age.auto,"Âge du Véhicule")
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Nbre_infract,"Nombre d'infractions",FALSE)
f.graphe.uni(frame.infractions,"Nombre infractions",FALSE)
f.graphe.pred(Temps_sn_sin,"Temps depuis le dernier sinistre")
f.graphe.uni(frame.temps.sinistre, "Nombre d'année depuis dernier sinistre")
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Nbre_enfants,"Nombre d'enfants",FALSE)
f.graphe.uni(frame.enfant,"Nombre d'enfants",FALSE)
f.graphe.pred(Sexe_assure,"Sexe de l'assuré",FALSE)
f.graphe.uni(frame.sex,"Sexe de l'assurée",FALSE)
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Ind_fumeur,"Indicateur Fumeur",FALSE)
f.graphe.uni(frame.fumeur,"Indicateur Fumeur",FALSE)
f.graphe.pred(Emploi_assure,"Emploi de l'assuré",FALSE)
f.graphe.uni(frame.emploi,"Type d'emploi",FALSE)
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Statut_marital,"Statut marital",FALSE)
f.graphe.uni(frame.statut,"Statut Marital",FALSE)
f.graphe.pred(Ind_util_travail,"Utiliser pour travailler",FALSE)
f.graphe.uni(frame.utiltravail,"Utiliser pour aller travailler",FALSE)
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Ind_util_affaires,"Utiliser pour affaires",FALSE)
f.graphe.uni(frame.utilaffaire,"Utiliser pour affaires",FALSE)
f.graphe.pred(Region,"Région de résidence",FALSE)
f.graphe.uni(frame.region,"Région de résidence",FALSE)
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Ind_occas,"Conducteur occasionnel",FALSE)
f.graphe.uni(frame.indoccas,"Conducteur occasionnel",FALSE)
f.graphe.pred(Ind_vehicule_sport,"Vehicule de type sport",FALSE)
f.graphe.uni(frame.sport,"Véhicule Sport",FALSE)
par(op)

op <- par(mfrow=c(2,2))
f.graphe.pred(Couleur_vehicule,"Couleur du véhicule",FALSE)
f.graphe.uni(frame.couleur.auto,"Couleur du véhicule",FALSE)
f.graphe.pred(Ind_vehicule_loue,"Véhicule",FALSE)
f.graphe.uni(frame.auto.loue, "Véhicule loué",FALSE)
par(op)

#anova

table.anova <- f.summary(mod.final)
table.anova

#R2

coef.det <- f.summary(mod.final,FALSE)
coef.det

#Graphe des observations en fonction des valeurs prédites

fitted.obs <- PP
fitted.pred <- exp(fitted(mod.final))

plot(fitted.obs,fitted.pred,pch=16,xlab="Observations",
ylab="Previsions",
     main=paste("Graphiques des observations en fonctions",
         "des prévisions pour la prime pure",sep="\n"),col="blue")

x.bar <- mean(PP) #moyennne
x.bar

#Analyse générale des résidus

f.postulat(mod.final)
f.postulat(mod.final,FALSE)

##Prévision de la prime pure

mod.final.pred <- lm(log(PP) ~ Annee + I(1/Age_assure^2) + Sexe_assure +Ind_fumeur + Emploi_assure + Statut_marital + factor(Nbre_enfants)
    + Exp_cond + I(log(km_annuel)) + Ind_util_travail + Ind_util_affaires +Score_credit + Region + I(log(Client_depuis+1)) + Ind_occas +Ind_vehicule_sport + I(log(Valeur_vehicule)) + Age_vehicule +Ind_vehicule_loue +factor(Nbre_infract) + factor(Temps_sn_sin),
                     data = donnees.h)

Prev.pp <- predict(mod.final.pred,newdata=donnees.c,
   interval="prediction")
Prev.pp <- exp(Prev.pp[,1])
Prev.pp <- matrix(Prev.pp,length(Prev.pp),1)
Prev.pp
