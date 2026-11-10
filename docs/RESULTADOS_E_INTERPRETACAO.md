# Resultados e interpretação — estudo de acesso à água em Pernambuco

> Esta síntese reproduz fielmente os dados e as conclusões centrais do **relatório entregue pelos autores**. Não se trata de uma reexecução independente dos scripts nem de revisão por pares.

## 1. Unidade de análise e variável de acesso

O trabalho combina informações do Censo 2022 e indicadores municipais do PIB per capita. A variável de interesse está relacionada à **ligação à rede geral de distribuição de água**, independentemente de seu uso como forma principal de abastecimento. Portanto, a pesquisa não mensura diretamente potabilidade, regularidade do fornecimento ou tratamento.

O relatório apresenta **9.018.330 pessoas**, sendo **6.892.175 com ligação** e **2.126.155 sem ligação**. Esses totais não devem ser confundidos com o percentual que declara usar a rede como principal fonte (o trabalho também discute a distinção).

## 2. Amostras, variabilidade e teste de Levene

A análise usa três amostras municipais e observações por raça/cor. Os boxplots mostram ampla heterogeneidade entre municípios, inclusive dentro de um mesmo grupo racial. O teste de Levene apontou heterogeneidade das variâncias, motivando o uso da **ANOVA de Welch**, que não exige a mesma homogeneidade de variâncias que a ANOVA clássica.

### ANOVA de Welch

| Amostra | F de Welch | p-valor reportado | Interpretação |
| --- | ---: | ---: | --- |
| 1 | 0,5677 | 0,686529 | Não rejeita igualdade global das médias |
| 2 | 0,5575 | 0,693967 | Não rejeita igualdade global das médias |
| 3 | 0,2607 | 0,902616 | Não rejeita igualdade global das médias |

Essa conclusão diz respeito à comparação global de médias dentro do desenho analisado; **não demonstra igualdade social completa entre os grupos**.

## 3. Qui-quadrado e V de Cramér

Os testes de independência identificaram associação estatisticamente detectável entre raça/cor e acesso à rede (p < 0,001, conforme relatório). Entretanto, os coeficientes **V de Cramér** foram baixos:

| Amostra | V de Cramér | Leitura |
| --- | ---: | --- |
| 1 | 0,083 | Associação de pequena magnitude |
| 2 | 0,0760 | Associação de pequena magnitude |
| 3 | 0,0537 | Associação de pequena magnitude |

O teste de significância e a medida de tamanho de associação respondem a perguntas diferentes. Em bases volumosas, associações pequenas podem apresentar p-valores muito baixos.

## 4. Regressões e modelagem

O relatório examina **regressão linear múltipla**, **transformação logarítmica** e **regressão beta**, incluindo diagnósticos de resíduos e informações como AIC e pseudo-R². O texto relata melhoria relativa do ajuste em modelos alternativos ao linear clássico e associação positiva do PIB per capita em modelos específicos.

A regressão beta é adequada à modelagem de proporções sob seus pressupostos; as comparações por AIC devem ser interpretadas com atenção à verossimilhança, à variável resposta utilizada e às transformações. O próprio relatório registra **pseudo-R² entre aproximadamente 1,12% e 5,37%**, reforçando que muitas fontes de variação continuam sem explicação pelos preditores considerados.

**Importante:** os resultados das amostras não devem ser confundidos automaticamente com conclusões sobre cada indivíduo do estado. Os gráficos e resultados de regressão podem variar com a composição municipal das amostras.

## 5. Conclusão acadêmica resumida

Os resultados discutidos apontam que as diferenças municipais e socioeconômicas são componentes importantes do acesso à infraestrutura hídrica, enquanto a variável raça/cor apresenta padrões que exigem interpretação conjunta entre significância estatística, magnitude do efeito e contexto social. A pesquisa defende a necessidade de políticas de abastecimento sensíveis às particularidades territoriais e aos grupos historicamente vulnerabilizados.

A análise é **observacional**: associações estatísticas não identificam, por si só, mecanismos causais. Para formulações completas, nuances por amostra e referências, consulte o [relatório de 30 páginas](../relatorio/Relatorio_Metodos_Estatisticos_Agua_PE_2026.pdf).
