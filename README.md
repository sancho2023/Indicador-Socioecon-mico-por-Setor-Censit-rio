# Indicador Socioeconômico por Setor Censitário

Código em R para construir um indicador socioeconômico por setor censitário de São José do Rio Preto, SP (código IBGE 3549805). O indicador usa variáveis do Censo Demográfico 2022 e Análise de Componentes Principais (PCA).

O indicador resume renda, escolaridade e características dos domicílios em dois componentes interpretáveis (**renda** e **infraestrutura**) e em um **indicador socioeconômico** combinado. Depois, os setores são classificados em quatro níveis socioeconômicos, para mapeamento e análises epidemiológicas espaciais.

## Conteúdo do repositório

| Arquivo | Descrição |
|---|---|
| `socioeconomic_index_2022.R` | Script completo da análise, comentado em inglês |
| `indicador_socioeconomico_2022_comentado.R` | A mesma análise, com explicações detalhadas em português |

## Variáveis

| Variável | Descrição |
|---|---|
| `setor_censi` | Código do setor censitário (identificador, fica fora da PCA) |
| `vari_renda` | Variância da renda (R$²), com transformação log antes da análise |
| `salario_min` | Renda média em salários mínimos (R$ 1.212,00 em 2022) |
| `perc_m_naoalf` | % de mulheres com 15 anos ou mais não alfabetizadas |
| `prop_chef_mulher` | % de domicílios chefiados por mulher |
| `prop_0ou1_banheiro` | % de domicílios com 0 ou 1 banheiro |
| `prop_morador` | % de domicílios com 1 ou 2 moradores |

## Métodos

1. **Checagem dos dados:** porcentagens impossíveis, valores extremos (regra de 3 × IQR, na escala log para a variância da renda) e setores sem nenhuma informação.
2. **Exclusão:** um setor (354980520000015) foi excluído por dados insuficientes: variância da renda extrema e 2 das 6 variáveis ausentes.
3. **Transformação e adequação:** `log1p` da variância da renda, correlações de Pearson, medida de Kaiser–Meyer–Olkin (KMO) e teste de esfericidade de Bartlett.
4. **Dados ausentes:** imputação baseada em PCA (`missMDA::imputePCA`, 2 componentes). As porcentagens são imputadas na escala logit, para que todo valor imputado fique entre 0 e 100%. Os valores observados são mantidos sem alteração. Uma análise de sensibilidade compara as imputações com 1 e 2 componentes.
5. **PCA sem rotação:** usada para decidir quantos componentes reter (critério de Kaiser, autovalor > 1).
6. **PCA com rotação varimax** (`psych::principal`, 2 componentes):
   - Os componentes de *renda* e de *infraestrutura* são identificados pelas cargas, e não pela posição, e o sinal é ajustado para que valores maiores indiquem melhores condições.
   - O indicador combinado é a média dos dois escores padronizados, ponderada pela proporção de variância explicada por cada componente.
7. **Classificação:** 4 níveis (Nível 1 = melhor, Nível 4 = pior) pelas quebras naturais de Jenks, com os quartis como análise de sensibilidade.
8. **Tabelas descritivas:** média e mediana de cada variável por nível.

## Requisitos

R (≥ 4.1) e os pacotes:

```r
install.packages(c("readxl", "missMDA", "FactoMineR", "psych",
                   "classInt", "car", "RcmdrMisc"))
```

## Como rodar

1. Coloque o arquivo de entrada `- Atualizado _PCA 2022_ - Oficial.xlsx` (uma linha por setor censitário) na pasta do projeto.
2. Defina essa pasta como diretório de trabalho.
3. Rode `socioeconomic_index_2022.R` (inglês) ou `indicador_socioeconomico_2022_comentado.R` (português).

O script grava quatro arquivos CSV (separador `;` e decimal `,`). Os nomes abaixo são os da versão em português; os da versão em inglês estão no [README em inglês](README.md).

- `indicador_socioeconomico_2022.csv`: indicadores e nível de cada setor, prontos para juntar ao shapefile dos setores censitários por `setor_censi`
- `cargas_varimax_2022.csv`: cargas rotacionadas
- `tabela1_media_2022.csv` e `tabela1_mediana_2022.csv`: características de cada nível

## Dados

As variáveis vêm do Censo Demográfico 2022 (IBGE). A planilha de entrada processada não está incluída neste repositório.
