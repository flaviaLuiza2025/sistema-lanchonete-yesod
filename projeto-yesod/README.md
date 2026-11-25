# Sistema de Gestão - Lanchonete (Yesod / Haskell)

## 1. Descrição do Projeto
Aplicação web monolítica desenvolvida em Haskell com o framework Yesod para o gerenciamento operacional de uma lanchonete. O sistema permite o controle completo de cardápio (categorias e produtos), cadastro de clientes com consulta automática de endereço, gerenciamento de entregadores, abertura e acompanhamento de pedidos, lançamento de itens por pedido e registro de avaliações.

## 2. Tecnologias Utilizadas
- **Linguagem:** Haskell (GHC 9.6.6)
- **Framework Web:** Yesod
- **Gerenciador de Build:** Stack (LTS 22.43)
- **Banco de Dados:** SQLite com ORM Persistent
- **Templates:** Shakespearean Templates (Hamlet)
- **Consumo de API Externa:** `http-conduit` e `aeson` (API ViaCEP)

## 3. Instruções de Execução
1. Certifique-se de ter o **Haskell Stack** instalado na máquina.
2. Clone este repositório e acesse a pasta do projeto Yesod:
   ```bash
   git clone https://github.com/flaviaLuiza2025/sistema-lanchonete-yesod.git
   cd sistema-lanchonete-yesod/projeto-yesod
   ```
3. Compile e execute (a primeira compilação baixa o GHC e as dependências):
   ```bash
   stack build
   stack exec projeto-yesod
   ```
4. Acesse http://localhost:3000. As tabelas são criadas e atualizadas automaticamente ao iniciar.

## 4. Estrutura do Banco de Dados
Todas as tabelas têm a chave primária `id`. O modelo está em `config/models.persistentmodels`.

| Tabela | Descrição | Principais campos | Chaves estrangeiras |
|---|---|---|---|
| `categoria` | Grupos do cardápio | nome, descricao | - |
| `produto` | Itens do cardápio | nome, preco | `categoria_id` |
| `cliente` | Clientes e endereços | nome, telefone, cep, endereco, complemento (opcional), cidade | - |
| `entregador` | Responsáveis pelas entregas | nome, placa_moto | - |
| `pedido` | Pedidos dos clientes | status, observacao (opcional) | `cliente_id`, `entregador_id` (opcional) |
| `item_pedido` | Produtos de cada pedido (relação N:N) | quantidade, preco_unitario | `pedido_id`, `produto_id` |
| `avaliacao` | Nota e comentário, uma por pedido | nota, comentario | `pedido_id` (única) |

As tabelas `user`, `email` e `comment` vêm do projeto base do Yesod.

## 5. Integração com API
- **API:** [ViaCEP](https://viacep.com.br/), consultada em `GET https://viacep.com.br/ws/{cep}/json/`.
- **Finalidade:** obter o endereço e a cidade do cliente a partir do CEP informado no cadastro.
- **Uso dos dados:** o logradouro e a localidade recebidos são gravados na tabela `cliente` e exibidos na listagem. Se a consulta falhar, o cadastro é salvo com um aviso no lugar do endereço, que pode ser corrigido na edição.
- **Implementação:** função `buscarEnderecoPorCep`, em `src/Handler/Lanchonete.hs`.

## 6. Integrantes
| Conta no GitHub | Principais contribuições |
|---|---|
| [flaviaLuiza2025](https://github.com/flaviaLuiza2025) | Envio da versão inicial do projeto, menu de navegação e documentação inicial |
| [ImperfectAmoeba](https://github.com/ImperfectAmoeba) | Melhorias na interface, CEP com hífen, complemento do cliente, total do pedido, regras de avaliação e exclusão, migração do banco e documentação |

## 7. Alterações Realizadas

### Interface
- Painel inicial reorganizado em cartões, nos grupos "Cadastros" e "Atendimento", com novos títulos e textos.
- Tabelas, formulários e botões estilizados com Bootstrap e Lucius.
- Correção de uma tag HTML quebrada no menu de Categorias.
- Remoção do rodapé com o texto padrão de copyright.
- Preços exibidos no formato `R$ 25,50`.

### Clientes
- O CEP é aceito com ou sem hífen na consulta à API ViaCEP.
- Novo campo opcional **Número/Complemento** no cadastro, na edição e na listagem.

### Produtos
- A listagem mostra o nome da categoria no lugar do ID.

### Pedidos e Itens
- Nova coluna **Total**, com a soma dos itens do pedido.
- Cada item guarda o **preço unitário** do momento da compra, para que reajustes não alterem pedidos antigos.
- A listagem de itens mostra preço unitário e subtotal.
- A edição do pedido mantém selecionado o status atual.

### Avaliações
- Somente pedidos com status **Entregue** podem ser avaliados, com validação também no servidor.
- Apenas uma avaliação por pedido, garantida por chave única no banco.

### Exclusões
- Excluir um pedido pede confirmação e remove também seus itens e sua avaliação, na mesma transação.
- Categorias, produtos, clientes e entregadores em uso não são excluídos; o sistema exibe um aviso no lugar do erro 500.

### Banco de dados e migração
- Novas colunas: `cliente.complemento` e `item_pedido.preco_unitario`. Nova chave única: `avaliacao.pedido_id`.
- Novo módulo `src/Migracao.hs`, que executa a migração em uma única transação, remove avaliações repetidas (mantém a primeira de cada pedido) e preenche os itens antigos com o preço atual do produto.
- A migração usa uma conexão própria, sem checagem de chaves estrangeiras, porque o Persistent recria as tabelas alteradas no SQLite. A aplicação continua com a checagem ativa.
- Nova dependência: `microlens`.
