install.packages("tseries")
library("tseries")
install.packages("nortest")
library(nortest)

#Rozklad t-studenta
plot(rt(15, 3))

#1.Test Jarque-Bera
#Badanie mocy testu Jarque-Bera
pv <- c(1:1000)
wektor_srednich_3_st_jb <- c(1:186)
#Badanie mocy testu w zaleznosci od liczby danych (zakres od 15 do 200 danych)
#3 stopnie swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(j, 3))$p.value
  }
  wektor_srednich_3_st_jb[j-14] <- mean(pv<0.05)
}
l_danych <- seq(15, 200)
plot(l_danych, wektor_srednich_3_st_jb, type="l",main="Moc testu Jarque-Bera", ylab = "Moc testu", xlab="Liczba danych")
#Wniosek - im wiecej obserwacji w rozkladzie t-student tym wieksze prawdopodobienstwo odrzucenia hipotezy zerowej (tym wieksza moc testu).

#4 stopnie swobody
wektor_srednich_4_st <- c(1:186)
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(j, 4))$p.value
  }
  wektor_srednich_4_st[j-14] <- mean(pv<0.05)
}

#5 stopni swobody
wektor_srednich_5_st <- c(1:186)
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(j, 5))$p.value
  }
  wektor_srednich_5_st[j-14] <- mean(pv<0.05)
}

#6 stopni swobody
wektor_srednich_6_st <- c(1:186)
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(j, 6))$p.value
  }
  wektor_srednich_6_st[j-14] <- mean(pv<0.05)
}

#7 stopni swobody
wektor_srednich_7_st <- c(1:186)
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(j, 7))$p.value
  }
  wektor_srednich_7_st[j-14] <- mean(pv<0.05)
}

#Wykres - polaczony dla roznych stopni swobody
plot(l_danych, wektor_srednich_3_st_jb, type="l", col="coral",main="Moc testu Jarque-Bera", ylab = "Moc testu", xlab="Liczba danych")
lines(l_danych, wektor_srednich_4_st, type="l", col="coral1")
lines(l_danych, wektor_srednich_5_st, type="l", col="coral2")
lines(l_danych, wektor_srednich_6_st, type="l", col="coral3")
lines(l_danych, wektor_srednich_7_st, type="l", col="coral4")
legend("topleft",inset=0.02,legend=c("3 st. swobody","4 st. swobody", "5 st. swobody", "6 st. swobody", "7 st. swobody"), col=c("coral","coral1", "coral2", "coral3", "coral4"), pch=c(15,15), cex=0.60)

#Wniosek - Dla tej samej symulacji, jesli bedziemy zwiekszali liczbe stopni swobody to moc testu bedzie malala. Czyli im wieksza jest liczba stopni swobody tym mniejsza jest moc testu.

#Badanie mocy testu w zaleznosci od liczby stopni swobody (zakres od 3 do 50 stopni swobody)
#30 danych
wektor_srednich2_30_jb <- c(1:48)
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(30, j))$p.value
  }
  wektor_srednich2_30_jb[j-2] <- mean(pv<0.05)
}
l_stopni_swobody <- seq(3, 50)
plot(l_stopni_swobody, wektor_srednich2_30_jb,type="l",main="Moc testu Jarque-Bera", ylab = "Moc testu", xlab="Liczba stopni swobody")
#Wniosek - im wiecej stopni swobody w rozkladzie t-studenta tym miejsze prawdopodobienstwo odrzucenia hipotezy zerowej (tym mniejsza moc testu).

#20 danych
wektor_srednich2_20 <- c(1:48)
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(20, j))$p.value
  }
  wektor_srednich2_20[j-2] <- mean(pv<0.05)
}

#40 danych
wektor_srednich2_40 <- c(1:48)
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(40, j))$p.value
  }
  wektor_srednich2_40[j-2] <- mean(pv<0.05)
}

#50 danych
wektor_srednich2_50 <- c(1:48)
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(50, j))$p.value
  }
  wektor_srednich2_50[j-2] <- mean(pv<0.05)
}

#60 danych
wektor_srednich2_60 <- c(1:48)
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- jarque.bera.test(rt(60, j))$p.value
  }
  wektor_srednich2_60[j-2] <- mean(pv<0.05)
}

#Wykres - polaczony dla roznej liczby danych
plot(l_stopni_swobody, wektor_srednich2_20, type="l", col="coral",main="Moc testu Jarque-Bera", ylab = "Moc testu", xlab="Liczba stopni swobody")
lines(l_stopni_swobody, wektor_srednich2_30_jb, type="l", col="coral1")
lines(l_stopni_swobody, wektor_srednich2_40, type="l", col="coral2")
lines(l_stopni_swobody, wektor_srednich2_50, type="l", col="coral3")
lines(l_stopni_swobody, wektor_srednich2_60, type="l", col="coral4")
legend("topright",inset=0.02,legend=c("20 obserwcaji","30 obserwcaji", "40 obserwcaji", "50 obserwcaji", "60 obserwcaji"), col=c("coral","coral1", "coral2", "coral3", "coral4"), pch=c(15,15), cex=0.60)

#Wniosek - Dla tej samej symulacji, jesli bedziemy zwiekszali liczbe danych to moc testu bedzie rosla. Czyli im wieksza jest liczba danych tym wieksza jest moc testu.




#2. Test Shapiro-Wilka
#Badanie mocy testu Shapiro-Wilka
#Badanie mocy testu w zaleznosci od liczby danych (zakres od 15 do 200 danych)
#3 stopnie swobody
wektor_srednich_3_st_sw <- c(1:186)
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(j, 3))$p.value
  }
  wektor_srednich_3_st_sw[j-14] <- mean(pv<0.05)
}
plot(l_danych, wektor_srednich_3_st_sw, type="l",main="Moc testu Shapiro-Wilka", ylab = "Moc testu", xlab="Liczba danych")
#Wniosek - im wiecej obserwacji w rozkladzie t-student tym wieksze prawdopodobienstwo odrzucenia hipotezy zerowej (tym wieksza moc testu).
#Wniosek taki sam jak w przypadku testu Jarque-Bera
#4 stopnie swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(j, 4))$p.value
  }
  wektor_srednich_4_st[j-14] <- mean(pv<0.05)
}

#5 stopni swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(j, 5))$p.value
  }
  wektor_srednich_5_st[j-14] <- mean(pv<0.05)
}

#6 stopni swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(j, 6))$p.value
  }
  wektor_srednich_6_st[j-14] <- mean(pv<0.05)
}

#7 stopni swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(j, 7))$p.value
  }
  wektor_srednich_7_st[j-14] <- mean(pv<0.05)
}

#Wykres - polaczony dla roznych stopni swobody
plot(l_danych, wektor_srednich_3_st_sw, type="l", col="aquamarine1",main="Moc testu Shapiro-Wilka", ylab = "Moc testu", xlab="Liczba danych")
lines(l_danych, wektor_srednich_4_st, type="l", col="aquamarine2")
lines(l_danych, wektor_srednich_5_st, type="l", col="aquamarine3")
lines(l_danych, wektor_srednich_6_st, type="l", col="aquamarine4")
lines(l_danych, wektor_srednich_7_st, type="l", col="darkslategray")
legend("topleft",inset=0.02,legend=c("3 st. swobody","4 st. swobody", "5 st. swobody", "6 st. swobody", "7 st. swobody"), col=c("aquamarine1","aquamarine2","aquamarine3","aquamarine4","darkslategray"), pch=c(15,15), cex=0.60)
#Wniosek - Dla tej samej symulacji, jesli bedziemy zwiekszali liczbe stopni swobody to moc testu bedzie malala. Czyli im wieksza jest liczba stopni swobody tym mniejsza jest moc testu.


#Badanie mocy testu w zaleznosci od liczby stopni swobody (zakres od 3 do 50 stopni swobody)
wektor_srednich2_30_sw <- c(1:48)
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(30, j))$p.value
  }
  wektor_srednich2_30_sw[j-2] <- mean(pv<0.05)
}
plot(l_stopni_swobody, wektor_srednich2_30_sw, type="l",main="Moc testu Shapiro-Wilka", ylab = "Moc testu", xlab="Liczba stopni swobody")
#Wniosek - im wiecej stopni swobody w rozkladzie t-studenta tym miejsze prawdopodobienstwo odrzucenia hipotezy zerowej (tym mniejsza moc testu).
#Wniosek taki sam jak w przypadku testu Jarque-Bera

#20 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(20, j))$p.value
  }
  wektor_srednich2_20[j-2] <- mean(pv<0.05)
}

#40 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(40, j))$p.value
  }
  wektor_srednich2_40[j-2] <- mean(pv<0.05)
}

#50 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(50, j))$p.value
  }
  wektor_srednich2_50[j-2] <- mean(pv<0.05)
}

#60 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- shapiro.test(rt(60, j))$p.value
  }
  wektor_srednich2_60[j-2] <- mean(pv<0.05)
}

#Wykres - polaczony dla roznej liczby danych
plot(l_stopni_swobody, wektor_srednich2_20, type="l", col="aquamarine1",main="Moc testu Shapiro-Wilka", ylab = "Moc testu", xlab="Liczba stopni swobody")
lines(l_stopni_swobody, wektor_srednich2_30_sw, type="l", col="aquamarine2")
lines(l_stopni_swobody, wektor_srednich2_40, type="l", col="aquamarine3")
lines(l_stopni_swobody, wektor_srednich2_50, type="l", col="aquamarine4")
lines(l_stopni_swobody, wektor_srednich2_60, type="l", col="darkslategray")
legend("topright",inset=0.02,legend=c("20 obserwcaji","30 obserwcaji", "40 obserwcaji", "50 obserwcaji", "60 obserwcaji"), col=c("aquamarine1","aquamarine2","aquamarine3","aquamarine4","darkslategray"), pch=c(15,15), cex=0.60)
#Wniosek - Dla tej samej symulacji, jesli bedziemy zwiekszali liczbe danych to moc testu bedzie rosla. Czyli im wieksza jest liczba danych tym wieksza jest moc testu.



#3. Test Lillieforsa
#Badanie mocy testu Lillieforsa
#Badanie mocy testu w zaleznosci od liczby danych (zakres od 15 do 200 danych)
wektor_srednich_3_st_l <- c(1:186)
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(j, 3))$p.value
  }
  wektor_srednich_3_st_l[j-14] <- mean(pv<0.05)
}
plot(l_danych, wektor_srednich_3_st_l,type="l",main="Moc testu Lillieforsa", ylab = "Moc testu", xlab="Liczba danych")
#Wniosek - im wiecej obserwacji w rozkladzie t-studenta tym wieksze prawdopodobienstwo odrzucenia hipotezy zerowej.
#Wnioski takie same jak w przypadku dwóch poprzednich testow

#4 stopnie swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(j, 4))$p.value
  }
  wektor_srednich_4_st[j-14] <- mean(pv<0.05)
}

#5 stopni swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(j, 5))$p.value
  }
  wektor_srednich_5_st[j-14] <- mean(pv<0.05)
}

#6 stopni swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(j, 6))$p.value
  }
  wektor_srednich_6_st[j-14] <- mean(pv<0.05)
}

#7 stopni swobody
for(j in 15:200)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(j, 7))$p.value
  }
  wektor_srednich_7_st[j-14] <- mean(pv<0.05)
}

#Wykres - polaczony dla roznych stopni swobody
plot(l_danych, wektor_srednich_3_st_l, type="l", col="darkseagreen1",main="Moc testu Lillieforsa", ylab = "Moc testu", xlab="Liczba danych")
lines(l_danych, wektor_srednich_4_st, type="l", col="darkseagreen2")
lines(l_danych, wektor_srednich_5_st, type="l", col="darkseagreen3")
lines(l_danych, wektor_srednich_6_st, type="l", col="darkseagreen4")
lines(l_danych, wektor_srednich_7_st, type="l", col="darkslategray")
legend("topleft",inset=0.02,legend=c("3 st. swobody","4 st. swobody", "5 st. swobody", "6 st. swobody", "7 st. swobody"), col=c("darkseagreen1","darkseagreen2","darkseagreen3","darkseagreen4","darkslategray"), pch=c(15,15), cex=0.60)
#Wnioski takie same jak w przypadku dwóch poprzednich testow


#Badanie mocy testu w zaleznosci od liczby stopni swobody (zakres od 3 do 50 stopni swobody)
wektor_srednich2_30_l <- c(1:48)
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(30, j))$p.value
  }
  wektor_srednich2_30_l[j-2] <- mean(pv<0.05)
}
plot(l_stopni_swobody, wektor_srednich2_30_l, type="l",main="Moc testu Lillieforsa", ylab = "Moc testu", xlab="Liczba stopni swobody")
#Wniosek - im wiecej stopni swobody w rozkladzie t-student tym miejsze prawdopodobienstwo odrzucenia hipotezy zerowej (tym mniejsza moc testu).
#Wnioski takie same jak w przypadku dwóch poprzednich testow

#20 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(20, j))$p.value
  }
  wektor_srednich2_20[j-2] <- mean(pv<0.05)
}

#40 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(40, j))$p.value
  }
  wektor_srednich2_40[j-2] <- mean(pv<0.05)
}

#50 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(50, j))$p.value
  }
  wektor_srednich2_50[j-2] <- mean(pv<0.05)
}

#60 danych
for(j in 3:50)
{
  for(i in 1:1000)
  {
    pv[i] <- lillie.test(rt(60, j))$p.value
  }
  wektor_srednich2_60[j-2] <- mean(pv<0.05)
}

#Wykres - polaczony dla roznej liczby danych
plot(l_stopni_swobody, wektor_srednich2_20, type="l", col="darkseagreen1",main="Moc testu Lillieforsa", ylab = "Moc testu", xlab="Liczba stopni swobody")
lines(l_stopni_swobody, wektor_srednich2_30_l, type="l", col="darkseagreen2")
lines(l_stopni_swobody, wektor_srednich2_40, type="l", col="darkseagreen3")
lines(l_stopni_swobody, wektor_srednich2_50, type="l", col="darkseagreen4")
lines(l_stopni_swobody, wektor_srednich2_60, type="l", col="darkslategray")
legend("topright",inset=0.02,legend=c("20 obserwcaji","30 obserwcaji", "40 obserwcaji", "50 obserwcaji", "60 obserwcaji"), col=c("darkseagreen1","darkseagreen2","darkseagreen3","darkseagreen4","darkslategray"), pch=c(15,15), cex=0.60)
#Wnioski takie same jak w przypadku dwóch poprzednich testow


#4. Porownanie testow miedzy soba
plot(l_danych, wektor_srednich_3_st_jb, main="Porownanie mocy testow w zaleznosci od l. danych", ylab = "Moc testu", xlab="Liczba danych", type="l", col="red")
lines(l_danych, wektor_srednich_3_st_sw, ylab = "Moc testu", col="blue")
lines(l_danych, wektor_srednich_3_st_l, ylab = "Moc testu", col="green")
legend("topleft",inset=0.02, legend=c("J-B", "S-W", "L"),
       col=c("red", "blue", "green"), lty=1:1, cex=0.8)
#Wniosek: Z wykresu wynika, ze najwieksza moc ma test Jarque-Bera, niewiele mniejsza ma test Shapiro-Wilka, natomiast najmniejsza moc sposrod pokazanych ma test Lillieforsa


plot(l_stopni_swobody, wektor_srednich2_30_jb,main="Porownanie mocy testow w zaleznosci od l. stopni swobody", ylab = "Moc testu",xlab="Liczba stopni swobody", type="l", col="red")
lines(l_stopni_swobody, wektor_srednich2_30_sw, ylab = "Moc testu", col="blue")
lines(l_stopni_swobody, wektor_srednich2_30_l, ylab = "Moc testu", col="green")
legend("topright",inset=0.02, legend=c("J-B", "S-W", "L"),
       col=c("red", "blue", "green"), lty=1:1, cex=0.8)
#Wniosek: Podobnie jak wczesniej, najmniejsza moc ma test Lillieforsa, natomiast nie widac wyraznej roznicy miedzy testami Jarque-Bera i Shapiro-Wilka. Od okolo 20-tu stopni swobody nie da sie zauwazyc roznicy w mocy testow. 
