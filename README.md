# Gestão de risco e desastre: dados e scripts da revisão sistemática

Este repositório reúne o banco de dados, os scripts em R e as figuras corrigidas usados no manuscrito **“Gestão de risco e desastre no Brasil: revisão sistemática da produção em português”**.

## Conteúdo

- `dados/banco_bruto.xlsx`: 831 registros exportados do Harzing’s Publish or Perish (Google Acadêmico) em fevereiro de 2025, antes da aplicação dos critérios de exclusão.
- `dados/banco_final.xlsx`: banco final da revisão sistemática, com 50 estudos e as variáveis codificadas.
- `scripts/analise_completa.R`: fluxo analítico original, adaptado para usar caminhos relativos ao repositório.
- `scripts/reproduzir_figuras_revisadas.R`: reproduz a taxonomia metodológica, o Gráfico 5 e o Gráfico 18.
- `scripts/grafico_tipos_desastre.R`: reproduz o gráfico de tipos de desastre com a categoria “Desastres”.
- `scripts/grafico_clusters_coautoria.R`: reproduz os clusters reais da rede de coautoria.
- `scripts/diagnostico_clusters.R`: documenta a diferença entre os clusters reais e o corte forçado usado na versão inicial.
- `resultados/figuras`: figuras finais empregadas na revisão do manuscrito.
- `documentacao/log_mudancas_v20.txt` e `documentacao/log_mudancas_v22.txt`: registro das alterações no manuscrito.

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

- O repositório permite reconstituir e auditar as etapas posteriores à coleta (seleção a partir dos 831 registros e análises dos 50 estudos incluídos). Não permite reproduzir de forma estrita a recuperação inicial dos resultados no Google Acadêmico, que pode variar entre buscas. A decisão de incluir ou excluir cada registro envolveu julgamento do pesquisador e não está codificada registro a registro no arquivo bruto.
- Dos 50 estudos do banco final, 49 foram localizados em `dados/banco_bruto.xlsx` (alguns com título truncado pelo Google Acadêmico). O estudo nº 46 (Nascimento, 2016, Universidade Federal de Pernambuco) foi incluído manualmente, por busca complementar, e não consta no arquivo bruto.

- Os scripts usam caminhos relativos e podem ser executados em Windows, macOS ou Linux.
- Sementes aleatórias foram fixadas nas visualizações de rede.
- Pequenas diferenças gráficas podem ocorrer entre versões de pacotes, fontes e sistemas operacionais.
- O banco preserva os nomes de colunas originais. Os scripts normalizam esses nomes quando necessário.

## Declaração de uso de inteligência artificial

Foi utilizado o Claude (Anthropic) como apoio técnico e editorial. A ferramenta auxiliou na revisão linha a linha do manuscrito, na correção de problemas no código em R que gera as figuras, na organização e na documentação deste repositório e na redação de rascunhos da carta-resposta e dos logs de alteração.

A ferramenta não coletou dados, não desenhou o protocolo de busca nem os critérios de inclusão e exclusão, não gerou referências bibliográficas e não determinou nenhuma conclusão do estudo. O banco de 50 estudos resulta da coleta e da codificação feitas pelo autor, que revisou e aprovou todo o conteúdo e assume a responsabilidade por ele. A ferramenta não é listada como autora.

## Licença

Antes da publicação pública, os autores devem definir e adicionar a licença aplicável ao código e ao banco de dados.

