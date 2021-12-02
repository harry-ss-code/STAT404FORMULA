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
print(mean(yy[9:16])- mean(yy[1:8]))

bb= rep(c(rep(-1, 4), rep(1, 4)), 2)
zbplus = sum( (1+bb)*yy)/16;
zbplus
zbminus = sum( (1-bb)*yy)/16;
zbminus
mu.b = zbplus - zbminus; 
print(aa); print(bb)
print("estimated effects of A and B")
print(c(mu.a, mu.b))