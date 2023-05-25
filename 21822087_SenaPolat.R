#ÝLK VERÝ SETÝ
veri <- read.csv("C:/Users/teknosa/Desktop/REGRESYON/ödev/veri.txt", sep="")
View(veri)
names(veri)<-c("Kalite","Aroma","Lezzet","Gövde","Bölge")
names(veri)
attach(veri)
bölge<-as.factor(Bölge)


summary(Aroma)
summary(Lezzet)
summary(Gövde)

levels(bölge)<-c("Burgonya","Willamette","Martinborough")
summary(bölge)
plot(bölge,col=c("#00AFBB", "#E7B801", "#FC4E09"))
#Normallik incelemesi
qqnorm(Kalite,col="blue")
qqline(Kalite,col="green", lwd = 2)

ks.test(Kalite,"pnorm")

install.packages("nortest") #Anderson-Darling testi için indirilen paket
library(nortest)
ad.test(Kalite)

#dönüþüm
lnkalite<-log(Kalite)
qqnorm(lnkalite,col="blue")
qqline(lnkalite,col="green")
ks.test(lnkalite,"pnorm")
ad.test(lnkalite)
shapiro.test(lnkalite)


tkalite<-1/Kalite
ks.test(tkalite,"qnorm")
ad.test(tkalite)

#boxplot ile aykýrý deðer 
boxplot(Kalite,col="plum",outcol="orange",outpch=19)

boxplot.stats(Kalite)$out 

#2.VERÝ SETÝ 
yeniveri <- read.csv("C:/Users/teknosa/Desktop/REGRESYON/ödev/yeniveri.txt", sep="")
View(yeniveri)
names(yeniveri)
attach(yeniveri)
names(yeniveri)<-c("Kalite","Aroma","Lezzet","Gövde","Bölge")
names(yeniveri)
attach(yeniveri)
bölge<-as.factor(Bölge)

#Normallik
qqnorm(yeniveri$Kalite,col="blue")
qqline(yeniveri$Kalite,col="plum", lwd = 3)


library(nortest) #Anderson-Darling testi için kullanýlan kütüphane
ad.test(yeniveri$Kalite)

#Doðrusallýk
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(yeniveri, pch = 19,  cex = 0.8,main="Ýkili Korelasyon Grafikleri",
      col = my_cols)

#Model oluþturma
sonuc<-lm(Kalite~Aroma+Lezzet+Gövde+Bölge)
summary(sonuc)

#Artýklarýn incelenmesi
install.packages("zoo")
library(zoo)

inf<-ls.diag(sonuc)
inf

#Cook yöntemi
cooksd <- cooks.distance(sonuc)
plot(cooksd, pch=19, cex=1,col="blue", main="Cook Uzaklýðýna göre Etkin Gözlemler")
abline(h = if (length(Kalite)>50) 4/length(Kalite) else 4/(length(Kalite)-(length(yeniveri)-1)-1) ,
       col="orange")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(Kalite)>50) 4/length(Kalite)
                                                   else 4/(length(Kalite)-(length(yeniveri)-1)-1),names(cooksd),""), col="purple")

#Uç deðer
hat<-inf$hat
plot(hat, pch=20, cex=1,col="#00AFBB", main="Gözlem Uzaklýðýna göre Uç Deðerler") 
abline(h = 2*length(yeniveri)/length(Kalite) , col="#FC4E07")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(yeniveri)/length(Kalite),index(hat),""), col="#E7B800")


# Standartlaþtýrýlmýþ artýklar ile aykýrý deðer  incelemesi
std<-inf$std.res
plot(std, pch=19, cex=1,col="chartreuse3", main="Standartlaþtýrýlmýþ Artýklara göre Aykýrý Deðerler",
     +ylab="Standartlaþtýrýlmýþ Artýklar", xlab="Index") 
abline(h = c(-2,2) ,col="cornflowerblue")   
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="turquoise3")

#Student tipi artýk
stud<-inf$stud.res
plot(stud, pch=19, cex=1,col="chartreuse3", main="Student türü Artýklara göre Aykýrý Deðerler",
     +ylab="Student Artýklarý", xlab="Index") 
abline(h = c(-3,3) ,col="darkgoldenrod1")   
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="#7570B3")


#VERÝ SETÝNÝN SON HALÝ
data <- read.csv("C:/Users/teknosa/Desktop/REGRESYON/ödev/data.txt", sep="")
View(data)
names(data)<-c("Kalite","Aroma","Lezzet","Gövde","Bölge")
names(data)
attach(data)
bölge<-as.factor(Bölge)

levels(bölge)<-c("1","2","3")
#Normallik
qqnorm(data$Kalite,col="#7570B3",pch=15)
qqline(data$Kalite,col="cornflowerblue",lwd=2)

ad.test(data$Kalite)

#Doðrusallýk
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(data, pch = 19,  cex = 0.8,main="Ýkili Korelasyon Grafikleri",
      col = my_cols)

#Model
sonuc<-lm(Kalite~Aroma+Lezzet+Gövde+bölge)
summary(sonuc)

#Güven aralýðý
confint(sonuc,level=0.95)

#DEÐÝÞEN VARYANSLILIK
#Grafik yöntemi
predict(sonuc)
inf<-ls.diag(sonuc) #“Aykýrý deðerler ile ilgili kod”
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
plot(predict(sonuc), abs(inf$stud.res), ylab="Student Artýklarý", xlab="Kestirim Deðeri",col="blue",pch=18)

lines(predict(sonuc), abs(inf$stud.res), type = "l", lty = 1,col="orange")

#Pagan testi
library(lmtest)
bptest(sonuc)

#Model kurma yöntemi
summary(lm(abs(residuals(sonuc)) ~ fitted(sonuc)))


#ÖZÝLÝÞKÝ
#Durbin Watson
dwtest(sonuc)

#ÇOKLU BAÐLANTI
#Korelasyon
cor(data)

#VIF Deðerleri
install.packages("DAAG")
library(DAAG) # car kütüphanesi aktif ise nitel katsayýlarý vermiyor. 
detach("package:car", unload=TRUE) #car paketinin kütüphaneden de-aktif yapýyor.
vif(sonuc)

vif_values <- vif(sonuc)           #create vector of VIF values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value
abline(v = 5, lwd = 3, lty = 2)


ols_vif_tol(sonuc)

#Koþul Sayýsý

install.packages("olsrr")
library(olsrr)
ols_eigen_cindex(sonuc)
ols_coll_diag(sonuc)

#Özdeðer ve Özvektör
install.packages("fastDummies")
library(fastDummies) 
dummy<-dummy_cols(bÖlge) 
x41<-dummy$.data_1 
x42<-dummy$.data_2 
x43<-dummy$.data_3 

#Standartlaþtýrma iþlemleri  
ort1<-mean(Aroma) 
kt1<-sum((Aroma-ort1)^2) 
skx1<-(Aroma-ort1)/(kt1^0.5) 
ort2<-mean(Lezzet) 
kt2<-sum((Lezzet-ort2)^2) 
skx2<-(Lezzet-ort2)/(kt2^0.5) 
ort3<-mean(GÖvde) 
kt3<-sum((GÖvde-ort3)^2) 
skx3<-(GÖvde-ort3)/(kt3^0.5) 
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
                            GÖvde=7.451064 , bÖlge="2"), interval = 'confidence')
#ön kestirim
predict(sonuc, data.frame(Aroma=5.872,Lezzet=7.2521 , 
                          GÖvde=4.3262 , bÖlge="1"), interval = 'confidence')

#DEÐÝÞKEN SEÇÝMÝ
#Ýleriye doðru
install.packages("stats")
library(stats)
lm.null <- lm(Kalite ~ 1)
forward <- step(lm.null,Kalite~Aroma+Lezzet+Gövde+bölge, 
                  direction = "forward")
forward
summary(forward)

#Geriye doðru
backward<-step(sonuc,direction = "backward")
backward
summary(backward)

#Adýmsal
install.packages("MASS")
library(MASS)
step.model <- stepAIC(sonuc, direction = "both", trace = FALSE)
step.model
summary(step.model)

#RIDGE
install.packages("MASS")
library(MASS)
ridge <- lm.ridge(Kalite~Aroma+Lezzet+Gövde+bölge ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),ylab=expression(hat(beta)))
abline(h=0,lwd=3)
ridge$coef
select(ridge)
ridge$coef[,ridge$lam == 0.4]
