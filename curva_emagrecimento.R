# Curvas de emagrecimento baseadas em TMB e formula rápida.

#### Definindo Funções ####

# Formula do cálculo baseada em Harris Benedict (HB)
# VCT = HB + 10%(HB) + Atividade Física
TMB_h <- function(peso, altura, idade, atividade_fisica, gen)
{
  if (gen == 'M')
  {
    tmb_val <- 66 + 13.8*peso + 5*altura - 6.8*idade
  }
  else if (gen == 'F')
  {
    tmb_val <- 665 + 9.6*peso + 1.8*altura - 4.7*idade
  }
  return(tmb_val*1.1 + atividade_fisica)
}

# Valor de referência: 7700 kcal por kg de peso
delta_p <- function(entrada, consumo)
{
  peso <- (entrada-consumo)/7700
}

#### Aplicação ####

# intervalo: Numero de dias onde será calculado
# dieta: Número de kcal previsto na dieta
# peso_inicial: Peso em kg no início do período
# altura: altura em cm
# idade: idade em anos
# gen: sexo biológico do indivíduo
# atv: kcal diários gastos em atividade física

intervalo <- 30
dieta <- 1900
peso_inicial <- 122
altura <- 195
idade <- 28
gen <- 'M'
atv <- 120

#Aplicação
tempo <- 1:intervalo
peso_atual_t <- peso_inicial
pesos_t <- vector(mode = 'numeric', length = length(tempo))


for (i in tempo){
  pesos_t[i] <- peso_atual_t

  if (i %% 365 == 0)
  {
    idade <- idade + 1
  }
  
  kcal_tmb <- TMB_h(peso_atual_t,altura,idade, atv, gen)
  dpt <- delta_p(dieta,kcal_tmb)
  peso_atual_t <- peso_atual_t + dpt
}


plot(tempo,pesos_t, type = 'l', col = 'blue',
     xlim = c(0,length(tempo)), ylab = 'Peso (kg)', xlab = 'Tempo (dias)'
     , main = 'Curva de perda de peso')

print('Variação de peso')
print(peso_atual_t - peso_inicial)
print('Variação estimada diária')
print((peso_atual_t - peso_inicial)/length(tempo))