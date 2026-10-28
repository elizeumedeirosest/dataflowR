<div align="center">

<p align="center">
  <img src="man/figures/dataflowR_title.svg" alt="dataflowR" width="370" />
</p>

### Edição Visual, Engenharia de Atributos e Linhagem Reprodutível em R

[![Documentação Oficial](https://img.shields.io/badge/docs-oficial-0071e3?style=for-the-badge&logo=google-chrome&logoColor=white)](https://elizeumedeirosest.github.io/dataflowR) 
[![Licença MIT](https://img.shields.io/badge/licença-MIT-10b981?style=for-the-badge)](LICENSE) 
[![R >= 4.0](https://img.shields.io/badge/R-%3E%3D_4.0-38bdf8?style=for-the-badge&logo=R&logoColor=white)](https://cran.r-project.org/) 
[![UI Interativa](https://img.shields.io/badge/interface-Shiny%20%7C%20Handsontable-0284c7?style=for-the-badge)](https://elizeumedeirosest.github.io/dataflowR) 
[![Código Reprodutível](https://img.shields.io/badge/código-dplyr%20%7C%20tidyverse-818cf8?style=for-the-badge)](https://elizeumedeirosest.github.io/dataflowR)

<p align="center">
  <b>Uma interface visual e interativa para edição, faxina e engenharia de dados em R.</b><br>
  Edite células diretamente em planilha interativa, aplique transformações e filtros em português, copie scripts reproduzíveis com 1 clique e visualize a linhagem e o fluxograma completo de alterações.
</p>

</div>

---

## O que é o dataflowR?

O **`dataflowR`** une a simplicidade visual de uma planilha eletrônica interativa com o rigor da ciência de dados reproduzível:

1. **Edição Direta e Visual**: Altere valores de células com duplo clique, renomeie colunas, adicione ou remova linhas e filtre dados visualmente em uma interface moderna.
2. **Engenharia de Atributos Guiada**: Conversão inteligente de tipos, fragmentador automático de datas, imputação de dados ausentes (`NA`) univariada e por grupo, além de substituição de valores sentinela.
3. **Geração Automática de Código na Interface**: Cada clique e modificação produz código R limpo (`dplyr` / `tidyverse`), comentado em português. O código pode ser copiado diretamente para a área de transferência com um único clique no botão **"Copiar Código"** dentro da própria interface (estilo *esquisse*).
4. **Linhagem e Fluxograma (*Dataflow Tracker*)**: Visualize o histórico de alterações e o fluxograma visual diretamente na interface antes de salvar, ou inspecione no console via `resumo_dataflow()` e `codigo_dataflow()`.

---

## Instalação

```r
# Instalação da versão de desenvolvimento via GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("elizeumedeirosest/dataflowR")
```

---

## Como Usar

### 1. Iniciar a Interface Interativa

Abra o editor gráfico passando seu `data.frame` ou `tibble`:

```r
library(dataflowR)

# Iniciar o editor interativo
dados_limpos <- dataflow(mtcars)
```

Ou acesse diretamente pelo menu **Addins** do RStudio: `dataflowR -> Editor Visual de Dados`.

*Dentro da interface, use o botão **"Código & Linhagem"** para copiar o script `dplyr` gerado ou visualizar o fluxograma e a auditoria das etapas.*

### 2. Acesso Direto via Console (Funções Dedicadas)

Se preferir inspecionar o código e a auditoria via console após fechar a interface:

```r
# 1. Imprimir o script dplyr reproduzível gerado
codigo_dataflow(dados_limpos)

# 2. Exibir o resumo estruturado da linhagem de transformações
resumo_dataflow(dados_limpos)

# 3. Gerar o fluxograma visual interativo
grafico_dataflow(dados_limpos)
```

---

## Licença

Distribuído sob a licença MIT. Veja `LICENSE` para mais informações.
