# Gestão de risco e desastre: dados e scripts da revisão sistemática

Este repositório reúne o banco de dados, os scripts em R e as figuras corrigidas usados no manuscrito **“Gestão de risco e desastre: uma revisão sistemática da literatura acadêmica brasileira”**.

## Conteúdo

- `dados/banco_final.xlsx`: banco final da revisão sistemática, com 50 estudos e as variáveis codificadas.
- `scripts/analise_completa.R`: fluxo analítico original, adaptado para usar caminhos relativos ao repositório.
- `scripts/reproduzir_figuras_revisadas.R`: reproduz a taxonomia metodológica, o Gráfico 5 e o Gráfico 18.
- `scripts/grafico_tipos_desastre.R`: reproduz o gráfico de tipos de desastre com a categoria “Desastres”.
- `scripts/grafico_clusters_coautoria.R`: reproduz os clusters reais da rede de coautoria.
- `scripts/diagnostico_clusters.R`: documenta a diferença entre os clusters reais e o corte forçado usado na versão inicial.
- `resultados/figuras`: figuras finais empregadas na revisão do manuscrito.
- `documentacao/log_mudancas_v20.txt`: registro consolidado das últimas alterações no manuscrito.

Materiais editoriais, pareceres, carta-resposta e versões intermediárias do manuscrito não integram o repositório público.

## Requisitos

- R 4.3 ou superior. A validação local foi feita com R 4.6.1.
- Conexão com a internet apenas para instalar dependências e, na análise completa, obter o OpLexicon na primeira execução.

## Instalação

No terminal, a partir da raiz deste repositório:

```bash
Rscript scripts/instalar_dependencias.R
```

As dependências são instaladas em `.r-library`, pasta ignorada pelo Git.

## Reprodução das figuras revisadas

Execute, na ordem:

```bash
Rscript scripts/reproduzir_figuras_revisadas.R
Rscript scripts/grafico_tipos_desastre.R
Rscript scripts/grafico_clusters_coautoria.R
Rscript scripts/diagnostico_clusters.R
```

Os resultados são gravados em `resultados/figuras` e `resultados/auditoria`.

Para executar o fluxo analítico extenso que deu origem às análises iniciais:

```bash
Rscript scripts/analise_completa.R
```

Os produtos desse fluxo são gravados em `resultados/analise_completa` e não são versionados, pois podem ser regenerados.

## Verificação

```bash
Rscript scripts/verificar_repositorio.R
```

A verificação confere estrutura, leitura do banco, sintaxe dos scripts e disponibilidade das dependências.

## Observações de reprodutibilidade

- Os scripts usam caminhos relativos e podem ser executados em Windows, macOS ou Linux.
- Sementes aleatórias foram fixadas nas visualizações de rede.
- Pequenas diferenças gráficas podem ocorrer entre versões de pacotes, fontes e sistemas operacionais.
- O banco preserva os nomes de colunas originais. Os scripts normalizam esses nomes quando necessário.

## Licença

Antes da publicação pública, os autores devem definir e adicionar a licença aplicável ao código e ao banco de dados.

