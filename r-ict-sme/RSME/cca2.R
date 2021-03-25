#https://www.jstatsoft.org/article/view/v023i12

library("CCA")
data("nutrimouse")
help(nutrimouse)

X <- as.matrix(nutrimouse$gene)
Y <- as.matrix(nutrimouse$lipid)

head(nutrimouse)
correl <- matcor(X, Y)
img.matcor(correl, type = 2)

Xr <- as.matrix(nutrimouse$gene[, sample(1:120, size = 10)])
res.cc <- cc(Xr, Y)
barplot(res.cc$cor, xlab = "Dimension",
  ylab = "Canonical correlations", names.arg = 1:10, ylim = c(0,1))
plt.cc(res.cc)

res.regul <- estim.regul(X, Y, plt = TRUE,
  grid1 = seq(0.0001, 0.2, l=51),
  grid2 = seq(0, 0.2, l=51))

contour(res.regul$grid1, res.regul$grid2, res.regul$mat, add = TRUE,
  levels = c(0,0.5,0.7), col = "blue")

contour(res.regul$grid1, res.regul$grid2, res.regul$mat, add = TRUE,
  levels = c(0.8,0.85,0.88), col = "darkgreen")

res.rcc <- rcc(X, Y, 0.008096, 0.064)

barplot(res.rcc$cor, xlab = "Dimension",
  ylab = "Canonical correlations", names.arg = 1:21, ylim = c(0,1))

plt.cc(res.rcc, var.label = TRUE,
  ind.names = paste(nutrimouse$genotype, nutrimouse$diet, sep = "-"))
