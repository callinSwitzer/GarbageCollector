

# list all p-values in the paper
pvals= c(0.0074, 
         2.2e-16, 
         0.0000006, 
         0.3, 
         0.004, 
         0.067)
pvals
plot(pvals)
abline(h = 0.05)


p.adjust(pvals, method = "fdr")
points(p.adjust(pvals, method = "fdr"), col = 'red')


points(p.adjust(pvals, method = "bonferroni"), col = 'blue')
