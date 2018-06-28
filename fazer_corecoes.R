# Melhore a funcao que calcula o intervalo de confianca de uma amostra normal com variancia conhecida ( bilateral)

IC.media = function(x, conf, sigma){
  n = length(na.omit(x))
  x.barra = mean(x,na.rm=TRUE)
  alfa = 1 - conf
  z = qnorm(conf + alfa/2)
  LI = x.barra - z*sigma/sqrt(n)
  LS = x.barra + z*sigma/sqrt(n)
  cat("[", LI, ";", LS, "]")
}
