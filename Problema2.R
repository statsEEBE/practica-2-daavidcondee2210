#Codigo para problema 2
iris
mis_dades <- iris
y <- mis_dades$Sepal.Length
y
x <- mis_dades$Petal.Length
x

#grafica
plot(x,y)
#media de x i y (que son vectores) (mean)
xbar <- mean(x)
ybar <- mean(y)

#encontrar pendiente para la recta de regresión
#hacer sumatiorio con la funcion sum
m <- sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)

#encontrar b en el tipo de recta y=mx+b
b <- ybar-m*xbar
#entre x i y para escribir eso hay que hacer altgr+4+espacio
mod <- lm(y~x)
data.frame(x=x)
ypredicted <- predict(mod,data.frame(x=x))
ypredicted
#para dibujar la recta en la grafica
plot(x,y)
lines(x,ypredicted)
#calcular r^2 (coeficiente de correlación) de la recta
Rsq <- sum((ypredicted-ybar)^2)/sum((y-ybar)^2)
summary(mod)
#lo mismo que la raiz de r^2
cor.test(x,y)
