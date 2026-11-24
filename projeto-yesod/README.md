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
   git clone [https://github.com/flaviaLuiza2025/sistema-lanchonete-yesod.git](https://github.com/flaviaLuiza2025/sistema-lanchonete-yesod.git)
   cd sistema-lanchonete-yesod/projeto-yesod
