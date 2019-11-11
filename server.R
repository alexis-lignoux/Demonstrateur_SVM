library(shiny)
library(e1071)
library(plotly)
library(kableExtra)
library(ipred)
library(gbm)
library(randomForest)
library(gdata)
library(ROCR)
library(class)

options(shiny.maxRequestSize = 180*1024^2)

shinyServer(function(input, output) {

## Chargement des données + création des échantillons
  
    data_intro=as.data.frame(read.csv("templatePDF/exemple.csv",header=TRUE))
    data_intro[,"Class"]=as.factor(data_intro[,"Class"])
    
    data2=as.data.frame(read.csv("templatePDF/exemple2.csv"))
    data2[,"Class"]=as.factor(data2[,"Class"])
    
    data3=read.csv("templatePDF/exemple_kernel.csv",header = TRUE)
    data3[,"Class"]=as.factor(data3[,"Class"])
    
    data4=read.csv("templatePDF/exemple_kernel3D.csv",header = TRUE)
    data4[,"Class"]=as.factor(data4[,"Class"])

    donnees=readRDS("templatePDF/creditcard.rds")
    donnees[,"Class"]=as.factor(donnees[,"Class"])
    set.seed(1)
    s=sample(dim(donnees)[1],round(dim(donnees)[1]*0.3))
    apprentissage=donnees[-s,]
    test=donnees[s,]
    frd=apprentissage[apprentissage[,"Class"]==1,]
    no_frd=apprentissage[apprentissage[,"Class"]==0,]
    reech=sample(dim(no_frd)[1],dim(frd)[1]*4)
    echantillon=rbind(frd,no_frd[reech,])
    cout=10
    gamma=0.01
    kern="radial"
    svm.fit=svm(Class~.,data=echantillon,gamma=0.01,cost=10,kernel="radial")
    print(svm.fit)
    svm.pred=predict(svm.fit,newdata=test,type="Class")
    table(svm.pred,test[,"Class"])
    tx_erreur=mean(svm.pred!=test[,"Class"])
    tx_exactitude=1-tx_erreur
    specificite=mean(svm.pred[test[,"Class"]==0]==test[test[,"Class"]==0,"Class"])
    sensibilite=mean(svm.pred[test[,"Class"]==1]==test[test[,"Class"]==1,"Class"])
    
## Reactive Values

    event=reactiveValues(
        indicateur1=FALSE,
        indicateur2=FALSE,
        indicateur3=FALSE,
        indicateur4=FALSE,
        indicateur5=FALSE,
        parameters=NULL,
        cost2=NULL,
        gamma=NULL,
        weight=NULL,
        kernel=NULL,
        app=NULL,
        tes=NULL,
        ech=NULL,
        fit=NULL,
        pred=NULL,
        err=NULL,
        spe=NULL,
        sensi=NULL,
        pred_boost=NULL,
        err_boost=NULL,
        spe_boost=NULL,
        sensi_boost=NULL,
        fit_rf=NULL,
        pred_rf=NULL,
        err_rf=NULL,
        spe_rf=NULL,
        sensi_rf=NULL,
        prob_log=NULL,
        pred_log=NULL,
        err_log=NULL,
        spe_log=NULL,
        sensi_log=NULL,
        pred_knn=NULL,
        err_knn=NULL,
        spe_knn=NULL,
        sensi_knn=NULL
    )
    
## Calculs onglet SVM
    
    observeEvent(input$generate1,{
        set.seed(input$seed1)
        samp=sample(dim(donnees)[1],round(dim(donnees)[1]*input$prop1/100))
        event$app=donnees[samp,]
        event$tes=donnees[-samp,]
        event$indicateur1=TRUE
    })
    
    output$gen1<-renderText(event$indicateur1)
    observeEvent(input$generate2,{
        set.seed(input$seed2)
        samp=sample(dim(event$app[event$app[,"Class"]==0,])[1],round(dim(event$app[event$app[,"Class"]==1,])[1]*(1-(input$prop2/100))/(input$prop2/100)))
        event$ech=rbind(event$app[event$app[,"Class"]==1,],(event$app[event$app[,"Class"]==0,])[samp,])
        event$indicateur2=TRUE
    })
    
    output$gen2<-renderText(event$indicateur2)
    observeEvent(input$estim,{
        event$cost=ifelse(input$cost2==0,0.000001,input$cost2)
        event$gamma=input$gamma
        event$weight=ifelse(input$weight==0,0.000001,input$weight)
        event$kernel=input$kernel
        a=cbind(event$cost,event$gamma,event$weight,event$kernel)
        colnames(a)=c("Cost","Gamma","Weight","Kernel")
        event$parameters=a
        w=cbind(1,event$weight)
        names(w)=c("0","1")
        event$fit=svm(Class~.,data=event$ech,gamma=event$gamma,cost=event$cost,kernel=event$kernel,class.weights=w)
        event$indicateur3=TRUE
        event$indicateur4=FALSE
    })
    
    observeEvent(input$eval,{
        event$pred=predict(event$fit,newdata=event$tes,type="class")
        event$err=mean(event$pred!=event$tes[,"Class"])
        event$spe=mean(event$pred[event$tes[,"Class"]==0]==event$tes[event$tes[,"Class"]==0,"Class"])
        event$sensi=mean(event$pred[event$tes[,"Class"]==1]==event$tes[event$tes[,"Class"]==1,"Class"])
        event$indicateur4=TRUE
    })
    
    output$est<-renderText(event$indicateur3)
    outputOptions(output,"gen1",suspendWhenHidden = FALSE)
    outputOptions(output,"gen2",suspendWhenHidden = FALSE)
    outputOptions(output,"est",suspendWhenHidden=FALSE)
    observeEvent(input$reinit,{
        event$indicateur1=FALSE
        event$indicateur2=FALSE
        event$indicateur3=FALSE
        event$indicateur4=FALSE
        event$parameters=NULL
        event$cost2=NULL
        event$gamma=NULL
        event$weight=NULL
        event$kernel=NULL
        event$fit=NULL
        event$pred=NULL
        event$err=NULL
        event$spe=NULL
        event$sensi=NULL
    })
    
## Calculs Régressions logistique, Boosting, Random forest et KNN
    
    observeEvent(input$compar,{
        glm.fit=glm(Class~.,data=echantillon,family = binomial)
        event$prob_log=predict(glm.fit,newdata=test,type="response")
        event$pred_log = rep(1,nrow(test))
        event$pred_log[event$prob_log<0.5]=0
        event$err_log=mean(event$pred_log!=test[,"Class"])
        event$spe_log=mean(event$pred_log[test[,"Class"]==0]==test[test[,"Class"]==0,"Class"])
        event$sensi_log=mean(event$pred_log[test[,"Class"]==1]==test[test[,"Class"]==1,"Class"])
        
        boost.fit=gbm(as.numeric(Class)-1~.,data=echantillon, distribution="adaboost",n.trees=200, n.minobsinnode = 5,shrinkage=0.03,verbose=FALSE) 
        event$pred_boost=predict(boost.fit,newdata=test, n.trees=200)
        a=table(as.factor(sign(event$pred_boost)), test[,"Class"])
        b=c(a[1],a[2])
        c=c(a[3],a[4])
        d=cbind(b,c)
        event$err_boost=(d[1,2]+d[2,1])/(dim(test)[1])
        event$spe_boost=(d[1,1])/(d[1,1]+d[2,1])
        event$sensi_boost=(d[2,2])/(d[1,2]+d[2,2])
        
        event$fit_rf=randomForest(Class~., data=echantillon,xtest=test[,-31],ytest=test[, "Class"],ntree=180,importance=TRUE)
        event$pred_rf=event$fit_rf$test$predicted
        event$err_rf=mean(event$pred_rf!=test[,"Class"])
        event$spe_rf=mean(event$pred_rf[test[,"Class"]==0]==test[test[,"Class"]==0,"Class"])
        event$sensi_rf=mean(event$pred_rf[test[,"Class"]==1]==test[test[,"Class"]==1,"Class"])
        
        event$pred_knn=knn(echantillon[,-31],test[,-31],echantillon[,"Class"],k=13,prob = TRUE)
        event$err_knn=mean(event$pred_knn!=test[,"Class"])
        event$spe_knn=mean(event$pred_knn[test[,"Class"]==0]==test[test[,"Class"]==0,"Class"])
        event$sensi_knn=mean(event$pred_knn[test[,"Class"]==1]==test[test[,"Class"]==1,"Class"])
        
        event$indicateur5=TRUE
    })
    
    output$comp<-renderText(event$indicateur5)
    outputOptions(output,"comp",suspendWhenHidden = FALSE)
    
## Outputs de l'onglet "Introduction"

    output$plotintro<- renderPlot({
        plot(data_intro[,"Y"]~data_intro[,"X"],col=ifelse(data_intro[,"Class"]=="A","blue","red"),xlab="X",ylab="Y",pch=19,lwd=3,type="p")
        title(main="Représentation des classes A et B dans l'espace (X,Y)")
        legend("topright",inset = 0.02,legend = c("A","B"),col = c("blue","red"),pch = 19,lwd=3,box.lty = 0,lty = 0)
    })
    
    output$plotintro2<- renderPlot({
        plot(data_intro[,"Y"]~data_intro[,"X"],col=ifelse(data_intro[,"Class"]=="A","blue","red"),xlab="X",ylab="Y",pch=19,lwd=3,type="p")
        title(main="Représentation des classes A et B dans l'espace (X,Y)")
        legend("topright",inset = 0.02,legend = c("A","B"),col = c("blue","red"),pch = 19,lwd=3,box.lty = 0,lty = 0)
        points(x=3,y=7,lwd=3,pch=3,col="darkgreen")
        points(x=3,y=7,cex=5,lwd=3,col="black")
    })
    
    output$plotintro3<- renderPlot({
        plot(data_intro[,"Y"]~data_intro[,"X"],col=ifelse(data_intro[,"Class"]=="A","blue","red"),xlab="X",ylab="Y",pch=19,lwd=3,type="p")
        title(main="Représentation des classes A et B dans l'espace (X,Y)")
        legend("topright",inset = 0.02,legend = c("A","B"),col = c("blue","red"),pch = 19,lwd=3,box.lty = 0,lty = 0)
        points(x=3,y=7,lwd=3,pch=3,col="blue")
        points(x=3,y=7,cex=5,lwd=3,col="black")
        svm.fit=svm(Class~.,data=data_intro,kernel="linear",scale=FALSE)
        beta.0=-svm.fit$rho
        beta.1=sum(svm.fit$coefs*data_intro$X[svm.fit$index])
        beta.2=sum(svm.fit$coefs*data_intro$Y[svm.fit$index])
        abline(-beta.0/beta.2,-beta.1/beta.2,col="black",lwd=3)
    })
    
## Outputs de l'onglet "Échantillon linéairement séparable"

    output$plot1<- renderPlot({
        svm.fit=svm(Class~.,data=data_intro,kernel="linear",scale=FALSE)
      
        plot(Y ~ X, data = data_intro, col=ifelse(Class=="B","red","blue"),lwd=1,type="p",pch=19)
      
        beta.0=-svm.fit$rho
        beta.1=sum(svm.fit$coefs*data_intro$X[svm.fit$index])
        beta.2=sum(svm.fit$coefs*data_intro$Y[svm.fit$index])
      
        if(input$bouton1=="Oui"){
            abline(-beta.0/beta.2,-beta.1/beta.2,col="black",lwd=3)
            if (input$bouton2=="Oui"){
                abline((-beta.0-1)/beta.2,-beta.1/beta.2,col="darkgrey",lwd=2)
                abline((-beta.0+1)/beta.2,-beta.1/beta.2,col="darkgrey",lwd=2)
                if (input$bouton3=="Oui"){
                    points(svm.fit$SV, cex = 3,lwd=2,col="darkgreen")
                }
            }
        }
    })
    
## Outputs de l'onglet "Échantillon non-linéairement séparable"
    
    output$plot2<- renderPlotly({
        plot_ly(data = data2,x=~X,y=~Y,type="scatter",mode="markers",color=as.factor(data2[,"Class"]),colors=c("blue","red"))
    })
    
    output$plot3<- renderPlot({
        cout=ifelse(input$cost==0,0.0001,input$cost)
        
        svm.fit=svm(Class~.,data=data2,kernel="linear",cost=cout,scale=FALSE)
      
        plot(Y ~ X, data = data2, col=ifelse(Class=="B","red","blue"),lwd=1,type="p",pch=19)
      
        beta.0=-svm.fit$rho
        beta.1=sum(svm.fit$coefs*data2$X[svm.fit$index])
        beta.2=sum(svm.fit$coefs*data2$Y[svm.fit$index])
      
        abline(-beta.0/beta.2,-beta.1/beta.2,col="black",lwd=3)
        abline((-beta.0-1)/beta.2,-beta.1/beta.2,col="darkgrey",lwd=2)
        abline((-beta.0+1)/beta.2,-beta.1/beta.2,col="darkgrey",lwd=2)
        points(svm.fit$SV, cex = 3,lwd=2,col="darkgreen")
    })
    
    output$plot4 <- renderPlot({
        plot(data3[,"X"],data3[,"droite"],xlab="X",ylab="Droite",col=ifelse(data3[,"Class"]=="A","blue","red"),pch=19,type="p",lwd=3)
        lines(c(-4.5,6.5),c(0,0))
    })
    
    output$plot5 <- renderPlot({
        plot(data3[,"X"],data3[,"X_square"],xlab="X",ylab="X_Square",col=ifelse(data3[,"Class"]=="A","blue","red"),pch=19,type="p",lwd=3)
    })
    
    output$plot6<- renderPlot({
        cout=10
        svm.fit=svm(Class~X+X_square,data=data3,kernel="linear",cost=cout,scale=FALSE)
        
        plot(X_square ~ X, data = data3, col=ifelse(data3[,"Class"]=="A","blue","red"),lwd=1,type="p",pch=19)
        
        beta.0=-svm.fit$rho
        beta.1=sum(svm.fit$coefs*data3$X[svm.fit$index])
        beta.2=sum(svm.fit$coefs*data3$X_square[svm.fit$index])
        
        abline(-beta.0/beta.2,-beta.1/beta.2,col="black",lwd=3)
        abline((-beta.0-1)/beta.2,-beta.1/beta.2,col="darkgrey",lwd=2)
        abline((-beta.0+1)/beta.2,-beta.1/beta.2,col="darkgrey",lwd=2)
        points(svm.fit$SV, cex = 3,lwd=2,col="darkgreen")
    })
    
    output$plot7 <- renderPlotly({
        pal=c("blue","red")
        plot_ly(data=data4,x=~X,y=~Y,type="scatter",mode="markers",color=~as.factor(data4[,"Class"]),colors=pal)
    })
    
    output$plot9<-renderPlotly({
        pal=c("blue","red")
        titre=list(text="Représentation des données dans l'espace")
        if (input$plan=="Non"){
            o <- plot_ly(data=data4,x=~X,y=~Y,z=~Z/4,type="scatter3d",mode="markers",color=~as.factor(data4[,"Class"]),colors=pal)
            o <- layout(o,title=titre)
            o
        }
        else {
            trace1=list(
                name="first p",
                type="surface",
                x=c(-6,6),
                y=c(-6,6),
                z=matrix(c(25/4,24/4,26/4,25/4),2,2)
            )
            p <- plot_ly(data=data4,x=~X,y=~Y,z=~Z/4,type="scatter3d",color=~as.factor(data4[,"Class"]),colors=pal)
            p <- add_trace(p,name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, z=trace1$z,colorscale=list(c(0,"red"),c(1,"blue")))
            p <- layout(p,title=titre)
            hide_colorbar(p)
        }
    })
    
## Outputs de l'onglet "Données"
    
    output$description1 <- function()({
        a=dim(donnees)
        nb_obs=a[1]
        nb_var=a[2]
        a=rbind(nb_var,nb_obs)
        rownames(a)=c("Nombre de variables","Nombre d'individus")
        a %>%
            knitr::kable("html")%>%
            kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
            column_spec(1,bold=TRUE,background = "whitesmoke")
    })
    
    output$description2 <- function()({
        a=echantillon[346:365,1:31]
        row.names(a)=NULL
        a %>%
            knitr::kable("html")%>%
            kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
            row_spec(0,background = "whitesmoke")%>%
            scroll_box(height = "300px")
    })
    
## Outputs de l'onglet "Traitement des données"
    
    output$plot_class<- renderPlot({
        h=hist(as.numeric(donnees[,"Class"]),main="Répartition des données de Class",cex.main=1.5,xlab="Class",labels=TRUE,
               ylab="Fréquence",font.lab=2, col=c("coral1","#4AD6E5"),nclass = 2,ylim=c(0,300000),xaxt="n")
        axis(side=1,at=h$mids,labels = c("0","1"),tick=F)
        legend(1.7,300000,c("Pas de fraude","Fraude"),fill=c("coral1","#4AD6E5"),horiz=F,cex=0.8)
    }, height=370, width=650)
    
    output$plot_class2<- renderPlot({
        par(mfrow=c(1,2))
      
        h=hist(as.numeric(as.factor(apprentissage[,"Class"])),main="Échantillon d'apprentissage",
               cex.main=1.5,xlab="Class",labels=TRUE,ylab="Fréquence",font.lab=2,col=c("coral1","#4AD6E5"),nclass = 2,ylim=c(0,210000),xaxt="n")
        axis(side=1,at=h$mids,labels = c("0","1"),tick=F)
        legend(1.7,190000,c("Pas de fraude","Fraude"),fill=c("coral1","#4AD6E5"),horiz=F,cex=0.8)
        
        h=hist(as.numeric(as.factor(test[,"Class"])),main="Échantillon test",cex.main=1.5,xlab="Class",labels=TRUE,
               ylab="Fréquence",font.lab=2,col=c("coral1","#4AD6E5"),nclass = 2,ylim=c(0,90000),xaxt="n")
        axis(side=1,at=h$mids,labels = c("0","1"),tick=F)
        legend(1.7,85000,c("Pas de fraude","Fraude"),fill=c("coral1","#4AD6E5"),horiz=F,cex=0.8)
    }, height=370, width=850)
    
    output$plot_class3<- renderPlot({
        h=hist(as.numeric(as.factor(echantillon[,"Class"])),main="Après rééchantillonnage 80% et 20%",
               cex.main=1.5,xlab="Class",labels=TRUE,ylab="Fréquence",font.lab=2,col=c("coral1","#4AD6E5"),nclass = 2,ylim=c(0,1500),xaxt="n")
        axis(side=1,at=h$mids,labels = c("0","1"),tick=F)
        legend(1.7,1500,c("Pas de fraude","Fraude"),fill=c("coral1","#4AD6E5"),horiz=F,cex=0.8)
    }, height=370, width=650)
    
## Outputs de l'onglet "Estimation"
    
    output$text3 <- renderPrint({
        if (event$indicateur3==TRUE){
            print(event$fit)
        }
    })
    
    output$rendu4 <- function()({
        if (event$indicateur4==TRUE){
            a=table(event$pred,event$tes[,"Class"])
            a=cbind(a,c(sum(a[1,]),sum(a[2,])))
            a=rbind(a,c(sum(a[,1]),sum(a[,2]),sum(a[,3])))
            colnames(a)=c("0 Observé","1 Observé","Total")
            rownames(a)=c("0 Prédit","1 Prédit","Total")
            a %>%
                knitr::kable("html")%>%
                kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
                add_header_above(c("Matrice de confusion"=4),background = "whitesmoke")%>%
                column_spec(1,bold=TRUE,background = "whitesmoke")%>%
                row_spec(0,background = "whitesmoke")
        }
    })
    
    output$rendu5 <- function()({
        if(event$indicateur4==TRUE){
            resume=cbind(event$err,event$sensi,event$spe)
            colnames(resume)=c("Taux d'erreur","Sensibilité","Spécificité")
            resume %>%
                knitr::kable("html")%>%
                kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
                row_spec(0,background = "whitesmoke")
        }
    })
    
## Outputs de l'onglet "Optimisation"
    
    output$rendu0 <- function()({
        p=cbind(gamma,cout,kern)
        colnames(p)=c("Paramètre gamma","Paramètre de coût","Type de Kernel")
        p %>%
            knitr::kable("html")%>%
            kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
            add_header_above(c("Hyper-Paramètres Optimaux"=3),background = "whitesmoke")%>%
            row_spec(0,background = "whitesmoke")
    })
    
    output$rendu1 <- renderPrint({
        print(svm.fit)
    })
    
    output$rendu2 <- function()({
        a=table(svm.pred,test[,"Class"])
        a=cbind(a,c(sum(a[1,]),sum(a[2,])))
        a=rbind(a,c(sum(a[,1]),sum(a[,2]),sum(a[,3])))
        colnames(a)=c("0 Observé","1 Observé","Total")
        rownames(a)=c("0 Prédit","1 Prédit","Total")
        a %>%
            knitr::kable("html")%>%
            kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
            add_header_above(c("Matrice de confusion"=4),background = "whitesmoke")%>%
            column_spec(1,bold=TRUE,background = "whitesmoke")%>%
            row_spec(0,background = "whitesmoke")
      
    })
    
    output$rendu3 <- function()({
        resume=cbind(tx_erreur,sensibilite,specificite)
        colnames(resume)=c("Taux d'erreur","Sensibilité","Spécificité")
        resume %>%
            knitr::kable("html")%>%
            kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
            row_spec(0,background = "whitesmoke")
    })
    
## Outputs de l'onglet "Visualisation de notre échantillon"
    
    output$text <- renderText({
        if(length(input$Choix_var)>3){
            "Vous devez retirer des variables. Il faut que vous en sélectionniez 2 ou 3."
        }
        else if (length(input$Choix_var)<2){
            "Vous devez ajouter des variables. Il faut que vous en sélectionniez 2 ou 3."
        }
    })
    
    output$choice<-renderText(length(input$Choix_var))
    outputOptions(output,"choice",suspendWhenHidden = FALSE)
    
    output$plot10 <- renderPlotly({
        if(length(input$Choix_var)==3){
            plot_ly(data=echantillon,x=~echantillon[,input$Choix_var[1]],y=~echantillon[,input$Choix_var[2]],
                    z=~echantillon[,input$Choix_var[3]],type="scatter3d",mode="markers",color=~echantillon[,"Class"],colors=c("coral1","#4AD6E5"))%>%
            add_markers()%>%
            layout(title="Représentation graphique de 3 variables dans l'espace",
                  scene=list(xaxis=list(title=input$Choix_var[1]),
                   yaxis=list(title=input$Choix_var[2]),
                   zaxis=list(title=input$Choix_var[3])
                   ))
        }
        else if(length(input$Choix_var)==2){
            plot_ly(data=echantillon,x=~echantillon[,input$Choix_var[1]],y=~echantillon[,input$Choix_var[2]],
                    type="scatter",mode="markers",color=~echantillon[,"Class"],colors=c("coral1","#4AD6E5"))%>%
            layout(title="Représentation graphique de 2 variables",
                   xaxis=list(title=input$Choix_var[1]),
                   yaxis=list(title=input$Choix_var[2])
            )
        }
    })
    
## Outputs de l'onglet "Comparaison"
    
    output$Matrixconfus <- function()({
        if (event$indicateur5==TRUE){
            if (input$test=="Reg"){
                a=table(event$pred_log,test[,"Class"])
                a=cbind(a,c(sum(a[1,]),sum(a[2,])))
                a=rbind(a,c(sum(a[,1]),sum(a[,2]),sum(a[,3])))
                colnames(a)=c("0 Observé","1 Observé","Total")
                rownames(a)=c("0 Prédit","1 Prédit","Total")
                a %>%
                    knitr::kable("html")%>%
                    kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
                    add_header_above(c("Matrice de confusion"=4),background = "whitesmoke")%>%
                    column_spec(1,bold=TRUE,background = "whitesmoke")%>%
                    row_spec(0,background = "whitesmoke")
            }  
            else if (input$test=="Boosting"){
                a=table(as.factor(sign(event$pred_boost)),test[,"Class"])
                a=cbind(a,c(sum(a[1,]),sum(a[2,])))
                a=rbind(a,c(sum(a[,1]),sum(a[,2]),sum(a[,3])))
                colnames(a)=c("0 Observé","1 Observé","Total")
                rownames(a)=c("0 Prédit","1 Prédit","Total")
                a %>%
                    knitr::kable("html")%>%
                    kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
                    add_header_above(c("Matrice de confusion"=4),background = "whitesmoke")%>%
                    column_spec(1,bold=TRUE,background = "whitesmoke")%>%
                    row_spec(0,background = "whitesmoke")
            }
            else if (input$test=="Rf"){
                a=table(event$pred_rf,test[,"Class"])
                a=cbind(a,c(sum(a[1,]),sum(a[2,])))
                a=rbind(a,c(sum(a[,1]),sum(a[,2]),sum(a[,3])))
                colnames(a)=c("0 Observé","1 Observé","Total")
                rownames(a)=c("0 Prédit","1 Prédit","Total")
                a %>%
                    knitr::kable("html")%>%
                    kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
                    add_header_above(c("Matrice de confusion"=4),background = "whitesmoke")%>%
                    column_spec(1,bold=TRUE,background = "whitesmoke")%>%
                    row_spec(0,background = "whitesmoke")
            }
            else if (input$test=="KNN"){
                a=table(event$pred_knn,test[,"Class"])
                a=cbind(a,c(sum(a[1,]),sum(a[2,])))
                a=rbind(a,c(sum(a[,1]),sum(a[,2]),sum(a[,3])))
                colnames(a)=c("0 Observé","1 Observé","Total")
                rownames(a)=c("0 Prédit","1 Prédit","Total")
                a %>%
                    knitr::kable("html")%>%
                    kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
                    add_header_above(c("Matrice de confusion"=4),background = "whitesmoke")%>%
                    column_spec(1,bold=TRUE,background = "whitesmoke")%>%
                    row_spec(0,background = "whitesmoke")
            }
        }
    })
    
    output$plot_ROC<- renderPlot({
        if (event$indicateur5==TRUE){
            svm.fit=svm(Class~.,data=echantillon,gamma=0,cost=10,kernel="linear",probability=TRUE)
            svm.pred=predict(svm.fit,newdata=test,type="class",probability = TRUE)
            ROCsvm=attributes(svm.pred)$probabilities[,1]
            predsvm=prediction(ROCsvm,test$Class)
            perfsvm=performance(predsvm,"tpr","fpr")
          
            predreglog=prediction(event$prob_log,test$Class) 
            perfreglog=performance(predreglog,"tpr","fpr")
            
            ROCrf=event$fit_rf$test$vote[,2] 
            predrf=prediction(ROCrf,test$Class) 
            perfrf=performance(predrf,"tpr","fpr")
            
            predboost=prediction(event$pred_boost,test$Class) 
            perfboost=performance(predboost,"tpr","fpr")
            
            prob=attr(event$pred_knn,"prob")
            prob <- 2*ifelse(event$pred_knn == "0", 1-prob, prob) - 1
            predknn=prediction(prob,test$Class)
            perfknn=performance(predknn,"tpr","fpr")
            
            plot(perfsvm,col=1,lwd=2,main="Comparaison des courbes ROC",cex.main=1.5) 
            plot(perfreglog,col=2,add=TRUE,lwd=2) 
            plot(perfrf,col=3,add=TRUE,lwd=2) 
            plot(perfboost,col=4,add=TRUE,lwd=2) 
            plot(perfknn,col=5,add=TRUE,lwd=2)
            abline(c(0,0),c(1,1),lwd=2,lty="dashed")
            legend("bottomright",legend=c("SVM","reglog", "randForest","boosting","KNN"),col=c(1:5),pch="_")
        }
    })
    
    output$comparaison_erreur <- function()({
        if (event$indicateur5==TRUE){
            svm.fit=svm(Class~.,data=echantillon,gamma=0,cost=10,kernel="linear",probability=TRUE)
            svm.pred=predict(svm.fit,newdata=test,type="class",probability = TRUE)
            ROCsvm=attributes(svm.pred)$probabilities[,1]
            predsvm=prediction(ROCsvm,test$Class)
            auc_svm=performance(predsvm,"auc")
            gini_svm=2*auc_svm@y.values[[1]]-1

            predreglog=prediction(event$prob_log,test$Class)
            auc_log=performance(predreglog,'auc')
            gini_log=2*auc_log@y.values[[1]]-1
            
            ROCrf=event$fit_rf$test$vote[,2] 
            predrf=prediction(ROCrf,test$Class)
            auc_rf=performance(predrf,'auc')
            gini_rf=2*auc_rf@y.values[[1]]-1
            
            predboost=prediction(event$pred_boost,test$Class)
            auc_boost=performance(predboost,'auc')
            gini_boost=2*auc_boost@y.values[[1]]-1
          
            prob=attr(event$pred_knn,"prob")
            prob <- 2*ifelse(event$pred_knn == "0", 1-prob, prob) - 1
            predknn=prediction(prob,test$Class)
            auc_knn=performance(predknn,'auc')
            gini_knn=2*auc_knn@y.values[[1]]-1
          
            a=cbind(tx_erreur,sensibilite,specificite,auc_svm@y.values[[1]],gini_svm)
            b=cbind(event$err_log,event$sensi_log,event$spe_log,auc_log@y.values[[1]],gini_log)
            c=cbind(event$err_boost,event$sensi_boost,event$spe_boost,auc_boost@y.values[[1]],gini_boost)
            d=cbind(event$err_rf,event$sensi_rf,event$spe_rf,auc_rf@y.values[[1]],gini_rf)
            e=cbind(event$err_knn,event$sensi_knn,event$spe_knn,auc_knn@y.values[[1]],gini_knn)
            comp_erreur=round(rbind(a,b,c,d,e),digits = 5)
            colnames(comp_erreur)=c("Taux d'erreur","Sensibilité","Spécificité","AUC","Indice de Gini*")
            rownames(comp_erreur)=c('SVM','Régression logistique','Boosting','Random Forest','KNN')
            comp_erreur %>%
                knitr::kable("html")%>%
                kable_styling(bootstrap_options = c("hover","bordered"),position="center",full_width = F)%>%
                add_header_above(c("Comparaison"=6),background = "whitesmoke")%>%
                column_spec(1,bold=TRUE,background = "whitesmoke")%>%
                row_spec(0,background = "whitesmoke")
        }
    })

## Téléchargement de la notice
    
    output$dlPDF <- downloadHandler(
        filename = function() {
            paste('Notice-PDF-', format(Sys.Date(), "%d-%m-%Y"), '.pdf', sep='')
        },
        content = function(con) {
            fileRmd   <- paste0("templatePDF/", "Notice.Rmd")
            fileTex   <- paste("templatePDF/pdfTest_", Sys.Date(), ".tex", sep = "")
            folderTex <- paste0("templatePDF/pdfTest_", Sys.Date(), "_files")
            
            try(detach("package:kableExtra", unload = TRUE))
            try(detach("package:rmarkdown", unload = TRUE))
            
            options(kableExtra.latex.load_packages = FALSE)
            library(kableExtra)
            library(rmarkdown)
            
            try(rmarkdown::render(fileRmd,
                                  pdf_document(latex_engine = "pdflatex"),
                                  output_file = con,
                                  encoding = "UTF-8",
                                  clean = TRUE))
            
            unlink(folderTex, recursive = T)
        }
    )
})