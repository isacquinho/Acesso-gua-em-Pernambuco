# Reprodução dos scripts em R

## Estrutura dos arquivos entregues

O pacote fornecido pelos autores contém **três amostras**, cada uma com:

- `T7.R` — script de análise estatística original, em **R**;
- `ABASTECIMENTO_AGUA_POR_COR_RACA(PE).xlsx` — dados de abastecimento por raça/cor;
- `PIB_CIDADES_DE_PERNAMBUCO_2022.xlsx` — dados de PIB per capita;
- `amostra_fixa_municipios.xlsx` — seleção de municípios da amostra;
- planilhas de resultados e diagnósticos em formato `.xlsx`;
- `graficos/` — imagens PNG geradas pela análise;
- `mapa_abastecimento_amostral.html` — visualização HTML.

Os scripts `T7.R` fornecidos nas três pastas **possuem conteúdo idêntico**, mas foram mantidos em cada uma delas porque os dados da amostra são diferentes e o script lê arquivos em caminhos relativos ao diretório de execução. Isso preserva a organização original da entrega.

## Dependências

Instale uma versão recente da linguagem **R** e os pacotes solicitados pelo script:

```r
install.packages(c(
  "readxl", "writexl", "ggplot2", "dplyr", "tidyr",
  "car", "nortest", "gridExtra", "patchwork", "lmtest", "betareg"
))
```

As versões específicas do R e dos pacotes **não foram fixadas** nos arquivos originais. Reproduzir exatamente a configuração computacional original pode exigir investigação adicional do ambiente usado pelos autores.

## Executando a Amostra 1

Em um terminal, a partir da raiz deste repositório:

```bash
cd analises/amostra-1
Rscript T7.R
```

Para a Amostra 2, execute de dentro de `analises/amostra-2` e assim sucessivamente. O script faz verificações de arquivos e tenta instalar pacotes ausentes. Para evitar surpresas, é preferível instalar os pacotes antes da execução e examinar o script.

**Atenção:** a execução pode **sobrescrever** planilhas e gráficos de saída existentes. Faça uma cópia da pasta antes de reexecutar se quiser preservar os resultados entregues. Os scripts não foram executados novamente durante a organização deste repositório.

## Sobre os dados

Os arquivos Excel derivados das fontes oficiais foram conservados tal como fornecidos. Consulte no relatório original as definições de variáveis e o histórico de tratamento das informações. Dados de terceiros não se tornam automaticamente licenciados sob eventual licença do código dos estudantes.

## Organização e limpeza

Foram removidos somente arquivos de bloqueio temporário do Excel/Word com prefixo `~$`, que não representam dados de pesquisa. Nenhum script estatístico, planilha substantiva, gráfico ou mapa foi eliminado.
