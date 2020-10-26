secher<-read.table("secher.txt", header=T)

#model one
mode1<-lm(bwt~bpd, data=secher)
sum.mode1<-summary(mode1)
R2<-summary(mode1)$r.squared
f<-sum.mode1$fstatistic
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output)
intercept<-mode1$coefficients[1]
slope<-mode1$coefficients[2]
output<-sprintf("slope=%f  intercept=%f",slope, intercept)
cat(output)
png("bwt_bpd.png")
plot(bwt~bpd, data=secher)
abline(mode1)
dev.off()

#model 2
mode2<-lm(bwt~ad, data=secher)
sum.mode2<-summary(mode2)
R2<-sum.mode2$r.squared
f<-sum.mode2$fstatistic
p.value<-pf(f[1],f[2],f[3],lower.tail=f)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output)
intercept<-mode2$coefficients[1]
slope<-mode2$coefficients[2]
output<-sprintf("slope=%f  intercept=%f",slope, intercept)
cat(output)
png("bwt_ad.png")
plot(bwt~ad, data=secher)
abline(mode2)
dev.off()


