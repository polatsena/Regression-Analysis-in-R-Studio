#�LK VER� SET�
veri <- read.csv("C:/Users/teknosa/Desktop/REGRESYON/�dev/veri.txt", sep="")
View(veri)
names(veri)<-c("Kalite","Aroma","Lezzet","G�vde","B�lge")
names(veri)
attach(veri)
b�lge<-as.factor(B�lge)


summary(Aroma)
summary(Lezzet)
summary(G�vde)

levels(b�lge)<-c("Burgonya","Willamette","Martinborough")
summary(b�lge)
plot(b�lge,col=c("#00AFBB", "#E7B801", "#FC4E09"))
#Normallik incelemesi
qqnorm(Kalite,col="blue")
qqline(Kalite,col="green", lwd = 2)

ks.test(Kalite,"pnorm")

install.packages("nortest") #Anderson-Darling testi i�in indirilen paket
library(nortest)
ad.test(Kalite)

#d�n���m
lnkalite<-log(Kalite)
qqnorm(lnkalite,col="blue")
qqline(lnkalite,col="green")
ks.test(lnkalite,"pnorm")
ad.test(lnkalite)
shapiro.test(lnkalite)


tkalite<-1/Kalite
ks.test(tkalite,"qnorm")
ad.test(tkalite)

#boxplot ile ayk�r� de�er 
boxplot(Kalite,col="plum",outcol="orange",outpch=19)

boxplot.stats(Kalite)$out 

#2.VER� SET� 
yeniveri <- read.csv("C:/Users/teknosa/Desktop/REGRESYON/�dev/yeniveri.txt", sep="")
View(yeniveri)
names(yeniveri)
attach(yeniveri)
names(yeniveri)<-c("Kalite","Aroma","Lezzet","G�vde","B�lge")
names(yeniveri)
attach(yeniveri)
b�lge<-as.factor(B�lge)

#Normallik
qqnorm(yeniveri$Kalite,col="blue")
qqline(yeniveri$Kalite,col="plum", lwd = 3)


library(nortest) #Anderson-Darling testi i�in kullan�lan k�t�phane
ad.test(yeniveri$Kalite)

#Do�rusall�k
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(yeniveri, pch = 19,  cex = 0.8,main="�kili Korelasyon Grafikleri",
      col = my_cols)

#Model olu�turma
sonuc<-lm(Kalite~Aroma+Lezzet+G�vde+B�lge)
summary(sonuc)

#Art�klar�n incelenmesi
install.packages("zoo")
library(zoo)

inf<-ls.diag(sonuc)
inf

#Cook y�ntemi
cooksd <- cooks.distance(sonuc)
plot(cooksd, pch=19, cex=1,col="blue", main="Cook Uzakl���na g�re Etkin G�zlemler")
abline(h = if (length(Kalite)>50) 4/length(Kalite) else 4/(length(Kalite)-(length(yeniveri)-1)-1) ,
       col="orange")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(Kalite)>50) 4/length(Kalite)
                                                   else 4/(length(Kalite)-(length(yeniveri)-1)-1),names(cooksd),""), col="purple")

#U� de�er
hat<-inf$hat
plot(hat, pch=20, cex=1,col="#00AFBB", main="G�zlem Uzakl���na g�re U� De�erler") 
abline(h = 2*length(yeniveri)/length(Kalite) , col="#FC4E07")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(yeniveri)/length(Kalite),index(hat),""), col="#E7B800")


# Standartla�t�r�lm�� art�klar ile ayk�r� de�er  incelemesi
std<-inf$std.res
plot(std, pch=19, cex=1,col="chartreuse3", main="Standartla�t�r�lm�� Art�klara g�re Ayk�r� De�erler",
     +ylab="Standartla�t�r�lm�� Art�klar", xlab="Index") 
abline(h = c(-2,2) ,col="cornflowerblue")   
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="turquoise3")

#Student tipi art�k
stud<-inf$stud.res
plot(stud, pch=19, cex=1,col="chartreuse3", main="Student t�r� Art�klara g�re Ayk�r� De�erler",
     +ylab="Student Art�klar�", xlab="Index") 
abline(h = c(-3,3) ,col="darkgoldenrod1")   
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="#7570B3")


#VER� SET�N�N SON HAL�
data <- read.csv("C:/Users/teknosa/Desktop/REGRESYON/�dev/data.txt", sep="")
View(data)
names(data)<-c("Kalite","Aroma","Lezzet","G�vde","B�lge")
names(data)
attach(data)
b�lge<-as.factor(B�lge)

levels(b�lge)<-c("1","2","3")
#Normallik
qqnorm(data$Kalite,col="#7570B3",pch=15)
qqline(data$Kalite,col="cornflowerblue",lwd=2)

ad.test(data$Kalite)

#Do�rusall�k
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(data, pch = 19,  cex = 0.8,main="�kili Korelasyon Grafikleri",
      col = my_cols)

#Model
sonuc<-lm(Kalite~Aroma+Lezzet+G�vde+b�lge)
summary(sonuc)

#G�ven aral���
confint(sonuc,level=0.95)

#DE���EN VARYANSLILIK
#Grafik y�ntemi
predict(sonuc)
inf<-ls.diag(sonuc) #�Ayk�r� de�erler ile ilgili kod�
par(mfrow=c(2,2)) # �Grafik �izimi ile ilgili kod�
plot(predict(sonuc), abs(inf$stud.res), ylab="Student Art�klar�", xlab="Kestirim De�eri",col="blue",pch=18)

lines(predict(sonuc), abs(inf$stud.res), type = "l", lty = 1,col="orange")

#Pagan testi
library(lmtest)
bptest(sonuc)

#Model kurma y�ntemi
summary(lm(abs(residuals(sonuc)) ~ fitted(sonuc)))


#�Z�L��K�
#Durbin Watson
dwtest(sonuc)

#�OKLU BA�LANTI
#Korelasyon
cor(data)

#VIF De�erleri
install.packages("DAAG")
library(DAAG) # car k�t�phanesi aktif ise nitel katsay�lar� vermiyor. 
detach("package:car", unload=TRUE) #car paketinin k�t�phaneden de-aktif yap�yor.
vif(sonuc)

vif_values <- vif(sonuc)           #create vector of VIF values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value
abline(v = 5, lwd = 3, lty = 2)


ols_vif_tol(sonuc)

#Ko�ul Say�s�

install.packages("olsrr")
library(olsrr)
ols_eigen_cindex(sonuc)
ols_coll_diag(sonuc)

#�zde�er ve �zvekt�r
install.packages("fastDummies")
library(fastDummies) 
dummy<-dummy_cols(b�lge) 
x41<-dummy$.data_1 
x42<-dummy$.data_2 
x43<-dummy$.data_3 

#Standartla�t�rma i�lemleri  
ort1<-mean(Aroma) 
kt1<-sum((Aroma-ort1)^2) 
skx1<-(Aroma-ort1)/(kt1^0.5) 
ort2<-mean(Lezzet) 
kt2<-sum((Lezzet-ort2)^2) 
skx2<-(Lezzet-ort2)/(kt2^0.5) 
ort3<-mean(G�vde) 
kt3<-sum((G�vde-ort3)^2) 
skx3<-(G�vde-ort3)/(kt3^0.5) 
ort42<-mean(x42) 
kt42<-sum((x42-ort42)^2) 
skx42<-(x42-ort42)/(kt42^0.5) 
ort43<-mean(x43) 
kt43<-sum((x43-ort43)^2) 
skx43<-(x43-ort43)/(kt43^0.5) 
x<-cbind(skx1,skx2,skx3,skx42,skx43) 
sm<- eigen (t(x)%*%x) 
signif(sm$values,3) 
signif(sm$vectors,3)

#uyum kestirimi
predict(sonuc, data.frame(Aroma=6.123612,Lezzet=6.213497 , 
                            G�vde=7.451064 , b�lge="2"), interval = 'confidence')
#�n kestirim
predict(sonuc, data.frame(Aroma=5.872,Lezzet=7.2521 , 
                          G�vde=4.3262 , b�lge="1"), interval = 'confidence')

#DE���KEN SE��M�
#�leriye do�ru
install.packages("stats")
library(stats)
lm.null <- lm(Kalite ~ 1)
forward <- step(lm.null,Kalite~Aroma+Lezzet+G�vde+b�lge, 
                  direction = "forward")
forward
summary(forward)

#Geriye do�ru
backward<-step(sonuc,direction = "backward")
backward
summary(backward)

#Ad�msal
install.packages("MASS")
library(MASS)
step.model <- stepAIC(sonuc, direction = "both", trace = FALSE)
step.model
summary(step.model)

#RIDGE
install.packages("MASS")
library(MASS)
ridge <- lm.ridge(Kalite~Aroma+Lezzet+G�vde+b�lge ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),ylab=expression(hat(beta)))
abline(h=0,lwd=3)
ridge$coef
select(ridge)
ridge$coef[,ridge$lam == 0.4]
