# Weak boolean implication
# 0: no class; 1: and; 2: rn4c; 3: cn4r; 4: xor; 5: mix
# err: number of error bits
weak.couple <- function(row,col,err=1) {
  x <- row; y <- col
  mask <- x|y
  n <- sum(mask)
  if(n <= err)
    return("nc") # exception case: no class
  x <- x[mask]; y <- y[mask]

  and.b  <- sum(x & y)
  rn4c.b <- sum(x | !y)
  rn4c.o <- sum(x)-and.b
  cn4r.b <- sum(y | !x)
  cn4r.o <- sum(y)-and.b
  xor.b  <- sum(xor(x,y))
  
  if (and.b >= (n-err))
    return("and")
  if (xor.b >= (n-err))
    return("xor")
  if (rn4c.b >= (n-err) && rn4c.o >= err+1)
    return("rn4c")
  if (rn4c.b >= (n-err) && rn4c.o >= err+1)
    return("cn4r")
  return("mix")
}

# convert weak.couple string result to integer enumeration
weak.couple.enum <- function(s) {
  if(s == "nc")   return(0)
  if(s == "and")  return(1)
  if(s == "rn4c") return(2) 
  if(s == "cn4r") return(3)
  if(s == "xor")  return(4)
  if(s == "mix")  return(5)
}

# 0: no class; 1: and; 2: rn4c (row necessary for col); 3: cn4r (col necessary for row); 4: xor; 5: mix
all.pairs.weak <- function(M, th=0.2) {
  n <- dim(M)[1]
  D <- matrix(data=1,nrow=n,ncol=n)
  rownames(D) <- rownames(M)
  colnames(D) <- rownames(M)
  B <- M > th
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      p <- weak.couple(B[i,],B[j,])
      s <- weak.couple.enum(p)
      D[i,j] <- s
      if (D[i,j] == 2) {
        D[j,i] <- 3
      } else {
        if (D[i,j] == 3) {
          D[j,i] <- 2
        } else {
          D[j,i] <- D[i,j]
        }
      }
    }
  }
  D
}

# EXAMPLE USE
# ------------------------------
#D.expr.gold$WEAK <- all.pairs.weak(exprs(Eg.expr.gold))
#rownames(D.expr.gold$WEAK) <- featureData(Eg.expr.gold)$sym
#colnames(D.expr.gold$WEAK) <- featureData(Eg.expr.gold)$sym

#WEAK <- D.expr.gold$WEAK
#save(WEAK, file="../gold.weak.RData")
#write.table(WEAK, file="../gold.weak.tab", sep="\t", quote=F, col.names=NA, row.names=T)
