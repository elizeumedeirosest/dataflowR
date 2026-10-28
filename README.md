<div align="center">

<p align="center">
  <img src="man/figures/dataflow_title.svg" alt="dataflow" width="340" />
</p>

### Edição Visual, Engenharia de Atributos e Linhagem Reprodutível em R

[![Documentação Oficial](https://img.shields.io/badge/docs-oficial-0071e3?style=for-the-badge&logo=google-chrome&logoColor=white)](https://elizeumedeirosest.github.io/dataflow) 
[![Licença MIT](https://img.shields.io/badge/licença-MIT-10b981?style=for-the-badge)](LICENSE) 
[![R >= 4.0](https://img.shields.io/badge/R-%3E%3D_4.0-38bdf8?style=for-the-badge&logo=R&logoColor=white)](https://cran.r-project.org/) 
[![UI Interativa](https://img.shields.io/badge/interface-Shiny%20%7C%20Handsontable-0284c7?style=for-the-badge)](https://elizeumedeirosest.github.io/dataflow) 
[![Código Reprodutível](https://img.shields.io/badge/código-dplyr%20%7C%20tidyverse-818cf8?style=for-the-badge)](https://elizeumedeirosest.github.io/dataflow)

<p align="center">
  <b>Uma interface visual e interativa para edição, faxina e engenharia de dados em R.</b><br>
  Edite células diretamente em planilha interativa, aplique transformações e filtros em português, gere scripts reproduzíveis comentados passo a passo e visualize o fluxograma completo de alterações.
</p>

</div>

---

## O que é o dataflow?

O **`dataflow`** une a simplicidade visual de uma planilha eletrônica interativa com o rigor da ciência de dados reproduzível:

1. **Edição Direta e Visual**: Altere valores de células, renomeie colunas, adicione ou remova linhas e filtre dados visualmente em uma interface responsiva.
2. **Engenharia de Atributos Guiada**: Conversão inteligente de tipos, imputação de valores ausentes (`NA`), padronização, codificação de categorias e criação de novas colunas.
3. **Geração Automática de Código**: Cada clique e modificação na interface produz código R limpo (`dplyr` / `tidyverse`), totalmente comentado em português para que seu pipeline seja 100% auditável e reproduzível.
4. **Linhagem e Fluxograma (*Dataflow Tracker*)**: O objeto resultante armazena o histórico de modificações, permitindo gerar fluxogramas visuais e resumos no console com `resumo_fluxo()`.

---

## Instalação

```r
# Instalação da versão de desenvolvimento via GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("elizeumedeirosest/dataflow")
```

---

## Como Usar

### 1. Edição e Tratamento Interativo

Abra a interface gráfica diretamente no RStudio ou navegador:

```r
library(dataflow)

# Iniciar a interface interativa
dados_limpos <- dataflow(mtcars)
```

Ou acesse diretamente pelo menu **Addins** do RStudio: `dataflow -> Editor Visual de Dados`.

### 2. Resumo da Linhagem e Fluxograma

Acompanhe cada etapa realizada nos dados com o histórico de transformações:

```r
# Exibir o resumo do fluxo no console com formatação rica
resumo_fluxo(dados_limpos)

# Gerar o fluxograma visual das transformações
grafico_fluxo(dados_limpos)
```

---

## Licença

Distribuído sob a licença MIT. Veja `LICENSE` para mais informações.
