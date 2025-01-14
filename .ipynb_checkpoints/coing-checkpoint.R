## coding for 7.1

## Numerical calculation for estimating main effects

yy = c(14.59, 13.59, 14.24, 14.05, 14.65, 13.94, 
       14.40, 14.14, 14.67, 13.72, 13.84, 13.90, 
       14.56, 13.88, 14.30, 14.11)
aa = c(rep(-1, 8), rep(1, 8))
zaplus = sum( (1+aa)*yy)/16;
zaplus
zaminus = sum( (1-aa)*yy)/16;
zaminus
mu.a = zaplus - zaminus
mu.a

### note that we could code differently.

print("other way of computing effect of A")
print(mean(yy[aa ==1 ])- mean(yy[aa == -1]))

bb= rep(c(rep(-1, 4), rep(1, 4)), 2)
zbplus = sum( (1+bb)*yy)/16;
zbplus
zbminus = sum( (1-bb)*yy)/16;
zbminus
mu.b = zbplus - zbminus; 
print(aa); print(bb)
print("estimated effects of A and B")
print(c(mu.a, mu.b))

### Estimating interaction effects.

yy = c(14.59, 13.59, 14.24, 14.05, 14.65, 13.94, 14.40, 14.14, 14.67, 13.72, 13.84, 13.90, 14.56, 13.88, 14.30, 14.11)
aa= c(rep(-1, 8), rep(1, 8))
bb= rep(c(rep(-1, 4), rep(1, 4)), 2)
cc= rep(c(-1, -1, 1, 1), 4)
dd= rep(c(-1, 1), 8)

AB = aa*bb
zABplus = sum( (1+AB)*yy)/16;
zABplus
zABminus = sum( (1-AB)*yy)/16;
zABminus
mu.ab = zABplus - zABminus;
print(c("AB interaction=", mu.ab))
### ALTERNATIVE CALCULATION
print(mean(yy[AB == 1])- mean(yy[AB == -1]))

## interaction plot 
## changing place 
x = aa
y = bb
zzmm = mean(yy[(x==-1)&(y==-1)])
zzmp = mean(yy[(x==-1)&(y==1)])
zzpm = mean(yy[(x==1)&(y==-1)])
zzpp = mean(yy[(x==1)&(y==1)])

options(repr.plot.width=6, repr.plot.height=6)
plot(1:2, c(zzpm, zzpp), "l", xlim = c(0, 3), ylim=c(13.8, 14.5), xlab="", ylab="", xaxt="n")
legend(0.5, zzpm+.03, "A=+", bty="n") 
axis(1, at=1:2, labels = expression("B=-", "B=+"))## ?
lines(1:2, c(zzmm, zzmp))
legend(0.5, zzmm+.04, "A=-", bty="n") 
qt(0.975,80)

## 如果给了 s^2 可以直接sum up s
### Two ways of computing pooled variance estimator


ss.run = c(
  .270, .291, .268, .197,
  .221, .205, .222, .215,
  .269, .272, .220, .229,
  .227, .253, .250, .192)

sigma2hat = mean(ss.run)
print(sigma2hat)

df = (6-1)*16

qt(0.975, 80)


round(-0.077 + c(-1, 1)* 1.99*(0.2375/24)^.5, 3)

##7.3
## 包含 y 和 s 的 code 详情看formula 7.3 的repeat table
## Input data

yy = c(14.59,13.59,14.24,14.05,14.65,13.94,14.40,14.14,14.67,13.72,13.84,13.90,14.56,13.88,14.30,14.11)
ss = c(.270,.291,.268,.197,.221,.205,.222,.215,.269,.272,
       .220,.229,.227,.253,.250,.192)

## columns for various factors
aa = c(rep(-1, 8), rep(1, 8)); bb= rep(c(rep(-1, 4), rep(1, 4)), 2);
cc= rep(c(-1, -1, 1, 1), 4); dd= rep(c(-1, 1), 8)

zz = cbind(aa,bb,cc,dd, aa*bb, aa*cc, aa*dd, bb*cc)
zz = cbind(zz, bb*dd, cc*dd, aa*bb*cc, aa*bb*dd, aa*cc*dd, bb*cc*dd, aa*bb*cc*dd)
print(zz)



## 求mu 和sigma 
mu.all = yy%*%zz/8;
sighat = mean(ss)^0.5
print(mu.all)
print(sighat)



# Normal plot

ii = order(mu.all)
sym =c("A", "B", "C", "D", "AB", "AC", "AD",   
       "BC", "BD", "CD", "ABC", "ABD", "ACD", "BCD","ABCD")
sym=sym[ii]; zz = mu.all[ii];
## Matching the symbols and their values.

qq = qnorm((1:15-0.5)/15)
## 15 effects. 
## quantiles at (0.5, 1.5, 2.5, ..;, 14.5)/15 levels

options(repr.plot.width=8, repr.plot.height=6)
plot(qq, zz, ylim=c(-.5, .5), xlab=" ", ylab = " ", 
     main=
       "Normal plot of the effects in current Example", cex=0.5)
text(qq, zz+0.05,labels=sym, cex=0.7)
## Marking these effects.

## Matching symbols manually works better in exams.


zz= abs(mu.all)
### Take absolution values.
ii = order(zz)
sym =c("A", "B", "C", "D", "AB", "AC", "AD",   
       "BC", "BD", "CD", "ABC", "ABD", "ACD", "BCD","ABCD")
sym=sym[ii]; zz = zz[ii]; 
qq = qnorm( 0.5+(1:15-0.5)/(2*15))
### quantiles at 0.5 + (0.5, 1.5, ..., 14.5)/(2*15)

options(repr.plot.width=8, repr.plot.height=6)
plot(qq, zz, ylim=c(0, .6), xlab=" ", ylab = " ", 
     main= "Half-Normal plot",   cex=0.5)
text(qq, zz-0.02,labels=sym, cex=0.7)
print(0.5 + c(0.5, 1.5, 14.5)/(2*15))

