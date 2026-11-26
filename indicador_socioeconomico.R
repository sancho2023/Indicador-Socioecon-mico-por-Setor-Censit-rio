##############################################################################
# INDICADOR SOCIOECONÔMICO POR SETOR CENSITÁRIO - PCA - ANO 2022
# São José do Rio Preto, SP (código IBGE 3549805)
#
# Autor : Sancho Pedro Xavier (Faculdade de Saúde Pública, USP)
# ORCID : 0000-0001-9493-4098
#
# VERSÃO COMENTADA (com explicações detalhadas de cada etapa)
#
# O QUE ESTE SCRIPT FAZ, EM RESUMO
#   A ideia é resumir várias variáveis do Censo 2022 (renda, escolaridade,
#   composição do domicílio, banheiros) em poucos números por setor
#   censitário, que indiquem o nível socioeconômico daquele setor.
#   Para isso usamos a Análise de Componentes Principais (PCA), que encontra
#   combinações das variáveis originais que concentram a maior parte da
#   variação entre os setores.
#
#   Etapas:
#     1. Carregar os dados e garantir que o código do setor fique como texto.
#     2. Checar os dados (valores impossíveis, extremos, setores vazios).
#     3. Excluir o setor outlier com dados insuficientes.
#     4. Aplicar log na variância da renda e checar se a PCA é adequada.
#     5. Imputar os valores ausentes (porcentagens na escala logit).
#     6. Rodar a PCA sem rotação (diagnóstico: quantos componentes reter).
#     7. Rodar a PCA com rotação varimax e construir os indicadores.
#     8. Classificar os setores em 4 níveis (Jenks e quartis).
#     9. Descrever as características de cada nível (Tabela 1).
#    10. Exportar os resultados.
##############################################################################
# Variáveis do banco:
#  setor_censi        código do setor censitário (identificador, fica fora da PCA)
#  vari_renda         variância da renda (unidade: R$ AO QUADRADO)
#  renda_med          renda média (sai: é a mesma informação de salario_min)
#  salario_min        renda média em salários mínimos (R$ 1.212,00 em 2022)
#  perc_m_naoalf      % de mulheres com 15+ anos não alfabetizadas
#  prop_chef_mulher   % de domicílios chefiados por mulher
#  prop_0ou1_banheiro % de domicílios com 0 ou 1 banheiro
#  prop_morador       % de domicílios com 1 ou 2 moradores
##############################################################################

# Para instalar os pacotes (só na primeira vez):
# install.packages(c("readxl", "missMDA", "FactoMineR", "psych",
#                    "classInt", "car", "RcmdrMisc"))

library(readxl)      # lê arquivos do Excel (.xlsx)
library(missMDA)     # imputação de dados ausentes baseada em PCA
library(FactoMineR)  # PCA com gráficos e diagnósticos completos
library(psych)       # KMO, teste de Bartlett e PCA com rotação varimax
library(classInt)    # quebras naturais de Jenks para classificar em níveis
library(car)         # matriz de gráficos de dispersão (scatterplotMatrix)
library(RcmdrMisc)   # matriz de correlação com p-valores ajustados


# ---- 1. Carregar os dados ----------------------------------------------------

# Lê a planilha oficial e transforma em data.frame comum
# (read_excel devolve um "tibble", que se comporta um pouco diferente
#  na seleção de colunas com [ , ])
dados_2022 <- read_excel("- Atualizado _PCA 2022_ - Oficial.xlsx")
dados_2022 <- as.data.frame(dados_2022)

# POR QUE O CÓDIGO DO SETOR PRECISA SER TEXTO?
# O código do setor tem 15 dígitos (ex.: 354980520000015). Se o Excel guardar
# esse código como número, o R pode exibi-lo em notação científica
# (3.549805e+14) e perder dígitos. Aí a comparação com o código do outlier
# (passo 3) e a junção com o shapefile falham SEM dar erro nenhum.
# Por isso:
#   - se vier como número, format(scientific = FALSE) escreve todos os dígitos;
#   - se já vier como texto, só removemos espaços em branco nas pontas.
if (is.numeric(dados_2022$setor_censi)) {
  dados_2022$setor_censi <- format(dados_2022$setor_censi, scientific = FALSE, trim = TRUE)
} else {
  dados_2022$setor_censi <- trimws(as.character(dados_2022$setor_censi))
}

# Conferência: todos os códigos devem ter o mesmo número de dígitos (15).
# Se aparecer mais de um tamanho, algum código foi cortado ou tem espaço.
table(nchar(dados_2022$setor_censi))

# Variáveis que entram na PCA
vars <- c("vari_renda", "salario_min", "prop_morador", "prop_chef_mulher",
          "perc_m_naoalf", "prop_0ou1_banheiro")

# Subconjunto que é porcentagem (vai receber tratamento especial na imputação)
vars_porcentagem <- c("prop_morador", "prop_chef_mulher",
                      "perc_m_naoalf", "prop_0ou1_banheiro")

# renda_med fica de fora: é a mesma informação de salario_min em outra
# unidade (R$ em vez de salários mínimos). Duas variáveis praticamente
# idênticas na PCA dariam peso dobrado à renda.
dados <- dados_2022[, c("setor_censi", vars)]


# ---- 2. Checar os dados ANTES de qualquer análise ----------------------------
# A checagem precisa vir antes da PCA. Se viesse depois, um erro de digitação
# ou um valor impossível já teria contaminado os componentes.

# Resumo geral: mínimo, quartis, média, máximo e nº de NA de cada variável
summary(dados[, vars])

# 2a. ESCALA DAS PORCENTAGENS
# Os nomes misturam "prop_" (proporção, 0 a 1) e "perc_" (percentual, 0 a 100).
# É preciso saber qual escala foi usada, porque os limites dos testes e a
# transformação logit (passo 5) dependem disso. Olhe os intervalos abaixo:
sapply(dados[, vars_porcentagem], range, na.rm = TRUE)
limite_pct <- 100   # troque para 1 se estiverem em proporção (0 a 1)

# 2b. VALORES IMPOSSÍVEIS
# Uma porcentagem não pode ser negativa nem passar de 100%. Para cada setor
# (linha), testamos se alguma das porcentagens está fora desse intervalo.
erro_pct <- apply(dados[, vars_porcentagem], 1, function(l) {
  any(l < 0 | l > limite_pct, na.rm = TRUE)
})
sum(erro_pct)   # quantos setores têm pelo menos um valor impossível

# 2c. VALORES EXTREMOS (regra de 3 x IQR)
# IQR = intervalo interquartil (Q3 - Q1). Valores abaixo de Q1 - 3*IQR ou
# acima de Q3 + 3*IQR são considerados extremos ("outliers fortes").
# Usamos 3 (e não 1,5) para marcar só os casos realmente fora do padrão.
# vari_renda é muito assimétrica (poucos setores com variância enorme), então
# a regra é aplicada na escala log; na escala bruta, dezenas de setores
# normais seriam marcados como extremos.
dados_check <- dados[, vars]
dados_check$vari_renda <- log1p(dados_check$vari_renda)
extremo <- sapply(dados_check, function(x) {
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  x < q[1] - 3 * iqr | x > q[2] + 3 * iqr
})
colSums(extremo, na.rm = TRUE)   # quantos extremos por variável

# Relatório com todos os setores suspeitos (extremo OU valor impossível),
# para conferir um a um com os dados brutos do IBGE
relatorio_erros <- dados[rowSums(extremo, na.rm = TRUE) > 0 | erro_pct, ]
if (interactive()) View(relatorio_erros)   # View só funciona no RStudio

# 2d. SETORES SEM NENHUMA INFORMAÇÃO
# Um setor com as 6 variáveis vazias não pode ser imputado (não há nada em
# que se basear). Esses setores saem da análise.
todas_vazias <- rowSums(is.na(dados[, vars])) == length(vars)
sum(todas_vazias)   # esperado: 0
dados <- dados[!todas_vazias, ]


# ---- 3. Outlier de vari_renda (setor 354980520000015) ------------------------
# POR QUE ESTE SETOR FOI EXCLUÍDO?
# A variância está em R$²: 53,4 bilhões de R$² correspondem a um desvio-padrão
# de cerca de R$ 231 mil, muito acima de qualquer outro setor.
# Além disso, o setor tem:
#   - renda média de 45 salários mínimos;
#   - prop_morador e prop_chef_mulher AUSENTES;
#   - prop_0ou1_banheiro = 33,33% (um valor "redondo", típico de setores com
#     pouquíssimos domicílios, por exemplo 1 de 3).
# Testamos apagar só a vari_renda e imputá-la: com 3 das 6 variáveis
# ausentes, a imputação gerou prop_morador = 236%, um valor impossível.
# Decisão: EXCLUIR o setor por dados insuficientes. Isso deve ser relatado
# na seção de métodos do artigo.

outlier_setor <- "354980520000015"

# stopifnot interrompe o script se o código não for encontrado. Sem isso,
# um código mal formatado faria o filtro abaixo não excluir nada, em silêncio.
stopifnot(outlier_setor %in% dados$setor_censi)

dados <- dados[dados$setor_censi != outlier_setor, ]

nrow(dados)   # esperado: 1088 setores

# Quantas variáveis ausentes por setor (0, 1, 2...). Reportar no artigo.
table(n_ausentes = rowSums(is.na(dados[, vars])))


# ---- 4. Log e correlações ----------------------------------------------------

# POR QUE APLICAR LOG NA vari_renda?
# A PCA trabalha com correlações de Pearson, que são muito sensíveis a
# valores extremos. vari_renda é extremamente assimétrica; o log "comprime"
# os valores muito altos e deixa a distribuição mais simétrica.
# log1p(x) = log(1 + x): funciona mesmo quando x = 0 (log(0) seria -Inf).
dados_log <- dados[, vars]
dados_log$vari_renda <- log1p(dados_log$vari_renda)

# Matriz de correlação de Pearson com p-valores ajustados para múltiplas
# comparações, já na escala log. use = "complete" usa só setores sem NA.
rcorr.adjust(dados_log, type = "pearson", use = "complete")

# Matriz de gráficos de dispersão com reta de regressão: mostra visualmente
# se as relações entre as variáveis são aproximadamente lineares (a PCA
# assume relações lineares)
scatterplotMatrix(
  ~ vari_renda + salario_min + prop_morador + prop_chef_mulher +
    perc_m_naoalf + prop_0ou1_banheiro,
  data = dados_log, regLine = TRUE, smooth = FALSE,
  diagonal = list(method = "none")
)

# ADEQUAÇÃO DOS DADOS PARA A PCA (costuma ir no artigo)
# KMO (Kaiser-Meyer-Olkin): mede se as variáveis compartilham variância
#   suficiente para serem resumidas em componentes. Vai de 0 a 1;
#   KMO geral >= 0,6 é considerado aceitável.
# Teste de Bartlett: testa se a matriz de correlação é diferente de uma
#   matriz identidade (variáveis sem nenhuma correlação). p < 0,05 indica
#   que existe correlação suficiente para a PCA fazer sentido.
matriz_cor <- cor(dados_log, use = "complete.obs")
KMO(matriz_cor)
cortest.bartlett(matriz_cor, n = sum(complete.cases(dados_log)))

# CHECAGENS DE UM ERRO COMUM EM PLANILHAS
# Se alguém ordenar uma coluna do Excel sem ordenar as outras junto, os
# valores deixam de pertencer ao setor certo. Os testes abaixo detectam isso.

# 1. Spearman entre as variáveis: colunas ordenadas separadamente dão
#    rho ≈ 1,000 entre si (correlação perfeita artificial)
round(cor(dados[, vars], method = "spearman", use = "pairwise.complete.obs"), 3)

# 2. Correlação de cada coluna com a posição da linha (1, 2, 3...):
#    valores perto de ±1 indicam que a coluna está ordenada
sapply(dados[, vars], function(x)
  round(cor(seq_along(x), x, method = "spearman", use = "complete.obs"), 3))

# 3. Gráfico: dados reais formam uma nuvem de pontos;
#    colunas ordenadas formam uma curva fina e contínua
plot(dados$prop_morador, log1p(dados$vari_renda),
     xlab = "prop_morador", ylab = "log(vari_renda)")

# 4. Primeiras linhas, para comparar manualmente com os dados brutos do IBGE
head(dados_2022, 10)


# ---- 5. Imputação dos valores ausentes ---------------------------------------
# COMO FUNCIONA A IMPUTAÇÃO POR PCA (missMDA::imputePCA)
# O algoritmo preenche os NA com valores iniciais, roda uma PCA, reconstrói
# os dados a partir dos componentes, substitui os NA pelos valores
# reconstruídos e repete até convergir. O valor imputado de um setor é
# portanto "o que seria esperado" dadas as suas outras variáveis.
#
# O PROBLEMA: a reconstrução é linear e não sabe que porcentagens ficam entre
# 0 e 100%. Numa primeira tentativa, gerou valores de -1% a 144%.
#
# A SOLUÇÃO: imputar as porcentagens na escala LOGIT.
#   logit(p) = log(p / (1 - p)), com p em proporção (0 a 1)
#   O logit vai de -infinito a +infinito. Qualquer valor imputado nessa
#   escala, ao voltar com a função inversa (plogis), cai obrigatoriamente
#   entre 0 e 100%.
# Como logit(0) = -infinito e logit(100%) = +infinito, os valores 0% e 100%
# são aproximados para 0,5% e 99,5% SÓ durante a imputação. No final, os
# valores observados são devolvidos exatamente como estavam.

eps <- 0.005 * limite_pct   # 0,5 ponto percentual

# logit_pct: limita entre eps e (limite - eps), converte para proporção e
# aplica o logit (qlogis)
logit_pct     <- function(p) qlogis(pmin(pmax(p, eps), limite_pct - eps) / limite_pct)

# inv_logit_pct: volta do logit para a escala original (0 a limite_pct)
inv_logit_pct <- function(z) plogis(z) * limite_pct

# Aplica o logit só nas porcentagens; vari_renda (log) e salario_min ficam
dados_trans <- dados_log
dados_trans[vars_porcentagem] <- lapply(dados_trans[vars_porcentagem], logit_pct)

# QUANTOS COMPONENTES USAR NA IMPUTAÇÃO?
# estim_ncpPCA estima o número ideal por validação cruzada. Ele escolheu 4,
# que é o máximo testado com 6 variáveis: isso indica que não achou um ótimo
# claro. Usamos 2, o mesmo número de componentes retido pela PCA (critério
# de Kaiser, passo 6), para manter coerência entre imputação e análise.
estim <- estim_ncpPCA(dados_trans, ncp.min = 1, ncp.max = 4, scale = TRUE)
estim$ncp        # só para registro
ncp_imputacao <- 2

# scale = TRUE padroniza as variáveis (média 0, DP 1) durante a imputação,
# para que variáveis com números maiores não dominem
imp <- imputePCA(dados_trans, ncp = ncp_imputacao, scale = TRUE)
dados_imp <- as.data.frame(imp$completeObs)

# Volta as porcentagens para 0-100% e restaura os valores observados.
# (Os observados passaram pelo logit e pelos limites de 0,5%/99,5%; por isso
#  são substituídos pelos valores originais, sem nenhuma alteração.)
for (v in vars_porcentagem) {
  observado <- !is.na(dados_log[[v]])
  dados_imp[[v]] <- inv_logit_pct(dados_imp[[v]])
  dados_imp[[v]][observado] <- dados_log[[v]][observado]
}
sum(is.na(dados_imp))                         # esperado: 0 (nenhum NA restante)
sapply(dados_imp[, vars_porcentagem], range)  # tudo dentro de 0 e limite_pct

# ANÁLISE DE SENSIBILIDADE DA IMPUTAÇÃO
# Se trocarmos ncp = 2 por ncp = 1, os valores imputados mudam muito?
# Calculamos a correlação entre as duas imputações SÓ nas células que eram
# ausentes. Correlação perto de 1 = imputação estável (pouco dependente
# da escolha do ncp).
imp_ncp1 <- imputePCA(dados_trans, ncp = 1, scale = TRUE)$completeObs
ausentes <- is.na(dados_trans)
cor(imp$completeObs[ausentes], imp_ncp1[ausentes])

# Quais setores tinham dados ausentes, e quantos. Serve para o artigo e para
# avaliar se algum setor tem ausência demais para ser imputado com confiança.
n_aus <- rowSums(is.na(dados[, vars]))
table(n_aus)
dados[n_aus >= 3, ]   # setores com metade ou mais das variáveis ausentes

# Cópia com vari_renda de volta à escala bruta (R$²), usada só nas tabelas
# descritivas. expm1(x) = exp(x) - 1 é a inversa de log1p.
dados_imp_raw <- dados_imp
dados_imp_raw$vari_renda <- expm1(dados_imp_raw$vari_renda)


# ---- 6. PCA sem rotação (diagnóstico) ----------------------------------------
# Esta PCA serve para decidir QUANTOS componentes reter e para comparação.
# scale.unit = TRUE: a PCA é feita na matriz de CORRELAÇÃO (variáveis
# padronizadas). Sem isso, vari_renda e as porcentagens, que estão em
# escalas diferentes, teriam pesos arbitrários.

pca_2022 <- PCA(dados_imp, scale.unit = TRUE, ncp = 6, graph = TRUE)

# Tabela de autovalores: cada linha é um componente, com o autovalor
# (variância explicada em "número de variáveis"), a % de variância e a %
# acumulada. $eig sempre traz as 6 dimensões; o ncp só limita quantas
# aparecem em $ind e $var.
pca_2022$eig

# CRITÉRIO DE KAISER: reter os componentes com autovalor > 1, ou seja, que
# explicam mais variância do que uma variável original sozinha.
n_comp <- sum(pca_2022$eig[, "eigenvalue"] > 1)
n_comp   # esperado: 2
if (n_comp != 2) warning("Kaiser sugere ", n_comp, " componentes, não 2.")

# Cargas (correlação de cada variável com cada componente), sem rotação
round(pca_2022$var$coord[, 1:2], 2)


# ---- 7. PCA com rotação varimax: os indicadores ------------------------------
# POR QUE ROTACIONAR?
# Sem rotação, o 1º componente captura o máximo de variância possível e o 2º
# é, por construção, um "contraste" ortogonal (independente) ao 1º. Na
# prática, as variáveis costumam carregar nos dois componentes ao mesmo
# tempo, e chamar o 2º de "infraestrutura" fica frágil.
# A rotação VARIMAX gira os eixos (mantendo-os independentes) para que cada
# variável fique com carga alta em UM componente e baixa no outro. Assim cada
# componente passa a representar um constructo interpretável.
#
# Os escores do psych::principal já saem PADRONIZADOS (média 0, DP 1). Isso
# coloca os dois indicadores na mesma escala antes de combiná-los.

pc_rot <- principal(dados_imp, nfactors = 2, rotate = "varimax", scores = TRUE)

# Cargas rotacionadas; cutoff = 0.4 esconde as cargas pequenas para
# facilitar a leitura de qual variável pertence a qual componente
print(pc_rot$loadings, cutoff = 0.4)
cargas <- unclass(pc_rot$loadings)

# QUAL COMPONENTE É "RENDA" E QUAL É "INFRAESTRUTURA"?
# A identificação é feita pelas cargas, não pela posição: em outros anos
# (2000, 2010) a ordem dos componentes pode trocar.
#   - componente de renda = aquele em que salario_min tem a maior carga;
#   - componente de infraestrutura = o outro.
# Conferência: perc_m_naoalf (analfabetismo) deveria carregar mais no
# componente de infraestrutura; se não carregar, há um aviso.
comp_renda <- which.max(abs(cargas["salario_min", ]))
comp_infra <- setdiff(1:2, comp_renda)
if (which.max(abs(cargas["perc_m_naoalf", ])) != comp_infra)
  warning("perc_m_naoalf não ficou no componente de infraestrutura: rever a interpretação.")

escores <- pc_rot$scores
indicador_renda          <- escores[, comp_renda]
indicador_infraestrutura <- escores[, comp_infra]

# SINAL DOS INDICADORES: "valor alto = melhor" nos dois.
# O sinal de um componente principal é arbitrário (o mesmo componente pode
# sair multiplicado por -1, e isso pode mudar entre os anos). Por isso o
# sinal é conferido pelos dados:
#   - renda: deve ter correlação POSITIVA com salario_min;
#   - infraestrutura: deve ter correlação NEGATIVA com prop_0ou1_banheiro
#     (mais domicílios com 0 ou 1 banheiro = pior infraestrutura).
if (cor(indicador_renda, dados_imp$salario_min) < 0)
  indicador_renda <- -indicador_renda
if (cor(indicador_infraestrutura, dados_imp$prop_0ou1_banheiro) > 0)
  indicador_infraestrutura <- -indicador_infraestrutura

# INDICADOR ÚNICO: média dos dois indicadores, ponderada pela proporção de
# variância explicada por cada componente rotacionado. O componente que
# resume mais informação recebe peso maior.
var_prop   <- pc_rot$Vaccounted["Proportion Var", ]
peso_renda <- unname(var_prop[comp_renda])
peso_infra <- unname(var_prop[comp_infra])

indicador_socioeconomico <-
  (indicador_renda * peso_renda + indicador_infraestrutura * peso_infra) /
  (peso_renda + peso_infra)

# Checagem: o indicador combinado deve ser bem correlacionado com o 1º
# componente sem rotação (ambos resumem a variação geral). Um valor negativo
# só indica sinal invertido da Dim.1; um valor baixo merece investigação.
cor(indicador_socioeconomico, pca_2022$ind$coord[, 1])


# ---- 8. Classificação em 4 níveis --------------------------------------------
# Nível 1 = melhor condição socioeconômica, Nível 4 = pior

tabela_final <- data.frame(
  setor_censi              = dados$setor_censi,
  indicador_renda          = indicador_renda,
  indicador_infraestrutura = indicador_infraestrutura,
  indicador_socioeconomico = indicador_socioeconomico
)

# QUEBRAS NATURAIS DE JENKS
# O método procura os pontos de corte que tornam os grupos o mais homogêneos
# possível por dentro e o mais diferentes possível entre si. É muito usado em
# mapas temáticos porque respeita os "saltos" naturais da distribuição.
quebras_jenks <- classIntervals(tabela_final$indicador_socioeconomico,
                                n = 4, style = "jenks")

# cut() divide o indicador nos intervalos encontrados. Os rótulos vão de 4 a
# 1 porque valores baixos do indicador = pior condição = Nível 4.
tabela_final$nivel_socioeconomico <- cut(
  tabela_final$indicador_socioeconomico,
  breaks = quebras_jenks$brks,
  labels = paste("Nivel", 4:1),
  include.lowest = TRUE   # inclui o menor valor no primeiro intervalo
)
# Reordena os níveis para aparecerem de 1 (melhor) a 4 (pior) nas tabelas
tabela_final$nivel_socioeconomico <- factor(tabela_final$nivel_socioeconomico,
                                            levels = paste("Nivel", 1:4))

# ALTERNATIVA: QUARTIS
# Divide os setores em 4 grupos com o mesmo número de setores (25% cada).
# Útil como análise de sensibilidade da classificação por Jenks.
tabela_final$nivel_quartil <- cut(
  tabela_final$indicador_socioeconomico,
  breaks = quantile(tabela_final$indicador_socioeconomico, probs = seq(0, 1, 0.25)),
  labels = paste("Nivel", 4:1),
  include.lowest = TRUE
)
tabela_final$nivel_quartil <- factor(tabela_final$nivel_quartil,
                                     levels = paste("Nivel", 1:4))

# Quantos setores em cada nível (nº e %) pela classificação de Jenks
tabela_contagem <- data.frame(
  Nivel       = levels(tabela_final$nivel_socioeconomico),
  Qtd_Setores = as.numeric(table(tabela_final$nivel_socioeconomico)),
  Percentual  = round(100 * as.numeric(prop.table(table(tabela_final$nivel_socioeconomico))), 1)
)
print(tabela_contagem)

# Tabela cruzada Jenks x Quartis: mostra o quanto os dois métodos concordam
# (a diagonal principal são os setores classificados igual nos dois)
table(Jenks = tabela_final$nivel_socioeconomico, Quartil = tabela_final$nivel_quartil)


# ---- 9. Tabela 1: características de cada nível ------------------------------
# Descreve as variáveis originais em cada nível socioeconômico, para mostrar
# no artigo o que diferencia um nível do outro.
# Média E mediana: vari_renda é muito assimétrica e a média sozinha engana
# (alguns setores com variância altíssima puxam a média para cima).

# Junta as variáveis (na escala bruta) com o nível de cada setor
dados_com_nivel <- data.frame(dados_imp_raw, nivel = tabela_final$nivel_socioeconomico)

# Função auxiliar: transpõe o resultado do aggregate para que as variáveis
# fiquem nas linhas e os níveis nas colunas (formato de Tabela 1)
formatar <- function(tab) {
  m <- t(round(as.matrix(tab[, -1]), 2))
  colnames(m) <- as.character(tab$nivel)
  m
}

# aggregate(. ~ nivel): calcula a função (média ou mediana) de TODAS as
# variáveis (o ponto ".") para cada nível
tabela_1_media   <- formatar(aggregate(. ~ nivel, data = dados_com_nivel, FUN = mean))
tabela_1_mediana <- formatar(aggregate(. ~ nivel, data = dados_com_nivel, FUN = median))
tabela_1_media
tabela_1_mediana

# Setor com o maior indicador (conferência: deve ser um setor reconhecidamente
# de alto padrão no município)
tabela_final[which.max(tabela_final$indicador_socioeconomico), ]


# ---- 10. Exportar ------------------------------------------------------------
# setor_censi sai como texto, pronto para juntar com o shapefile dos setores
# e com os dados de dengue.
# write.csv2 usa ";" como separador e "," como decimal: abre direto no Excel
# configurado em português.

write.csv2(tabela_final, "indicador_socioeconomico_2022.csv", row.names = FALSE)
write.csv2(round(cargas, 3), "cargas_varimax_2022.csv")
write.csv2(tabela_1_media, "tabela1_media_2022.csv")
write.csv2(tabela_1_mediana, "tabela1_mediana_2022.csv")
