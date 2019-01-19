# Examples for simple dynamical system as outlined in section 2 of the paper

F = function(x) {
  -x/2
}
tiff(filename='figures/Fig2.tiff', width=4.5, height=5, units="in", pointsize=12, res=250)
curve(F(x),from=-2,to=2,lwd=2,col="blue",ylim=c(-1,1))
abline(v=0,h=0)
dev.off()

states=c()
x=-0.5
for (i in 0:15) {
  states=c(states,x)
  x=x+F(x)
}

tiff(filename='figures/Fig3.tiff', width=4.5, height=5, units="in", pointsize=12, res=250)
plot(0:15,states,ylim=c(-0.5,0.5),pch=16,cex=0.8,col="red",xlab="time",ylab="x")
lines(0:15,states,col="red")
abline(h=0)
dev.off()

states=c()
x=0.3
for (i in 0:15) {
  states=c(states,x)
  x=x+F(x)
}

tiff(filename='figures/Fig4.tiff', width=4.5, height=5, units="in", pointsize=12, res=250)
plot(0:15,states,ylim=c(-0.5,0.5),pch=16,cex=0.8,col="red",xlab="time",ylab="x")
lines(0:15,states,col="red")
abline(h=0)
dev.off()

V = function(x) {
  x^2/4
}
tiff(filename='figures/Fig5.tiff', width=4.5, height=5, units="in", pointsize=12, res=250)
curve(V(x),from=-2,to=2,lwd=2,col="blue",ylim=c(-1,1))
abline(v=0,h=0)
dev.off()

bistable = function(x, k) {
  x^4 - x^2 - k*x
}

tiff(filename='figures/Fig6.tiff', width=7, height=1.5, units="in", pointsize=12, res=250)
par(mfrow = c(1,5), mar=c(2,2,2,2))
curve(bistable(x, -0.9), from = -1.5, to = 1.5, xlab = "", ylab = "", ylim = c(-1,1), lty = 1, lwd = 2, main = paste("k = ", -0.9), yaxt = "n", xaxt = "n")
axis(side = 2, at = seq(-1,1,1))
axis(side = 1, at = seq(-1,1,1))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)
curve(bistable(x, -0.3), from = -1.5, to = 1.5, xlab = "", ylab = "", ylim = c(-1,1), lty = 1, lwd = 2, main = paste("k = ", -0.3), yaxt = "n", xaxt = "n")
axis(side = 2, at = seq(-1,1,1))
axis(side = 1, at = seq(-1,1,1))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)
curve(bistable(x, 0), from = -1.5, to = 1.5, xlab = "", ylab = "", ylim = c(-1,1), lty = 1, lwd = 2, main = paste("k = ", 0), yaxt = "n", xaxt = "n")
axis(side = 2, at = seq(-1,1,1))
axis(side = 1, at = seq(-1,1,1))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)
curve(bistable(x, 0.3), from = -1.5, to = 1.5, xlab = "", ylab = "", ylim = c(-1,1), lty = 1, lwd = 2, main = paste("k = ", +0.3), yaxt = "n", xaxt = "n")
axis(side = 2, at = seq(-1,1,1))
axis(side = 1, at = seq(-1,1,1))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)
curve(bistable(x, 0.9), from = -1.5, to = 1.5, xlab = "", ylab = "", ylim = c(-1,1), lty = 1, lwd = 2, main = paste("k = ", +0.9), yaxt = "n", xaxt = "n")
axis(side = 2, at = seq(-1,1,1))
axis(side = 1, at = seq(-1,1,1))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)
dev.off()



