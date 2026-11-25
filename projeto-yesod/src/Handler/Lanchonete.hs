{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Lanchonete where

import Import
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest)
import Text.Printf (printf)

-- ==========================================
-- 1. INTEGRAÇÃO COM API EXTERNA (ViaCEP)
-- ==========================================
data ViaCEPResponse = ViaCEPResponse
    { logradouro :: Maybe Text
    , localidade :: Maybe Text
    , erro       :: Maybe Bool
    } deriving (Show, Generic)

instance FromJSON ViaCEPResponse

buscarEnderecoPorCep :: Text -> IO (Text, Text)
buscarEnderecoPorCep cepDigitado = do
    let cepLimpo = filter (/= '-') (unpack cepDigitado)
        url = "GET https://viacep.com.br/ws/" ++ cepLimpo ++ "/json/"
    resultado <- try (do
        req <- parseRequest url
        res <- httpJSON req
        return (getResponseBody res :: ViaCEPResponse)
        ) :: IO (Either SomeException ViaCEPResponse)
    case resultado of
        Right dados ->
            case erro dados of
                Just True -> return ("CEP não encontrado", "Cidade não encontrada")
                _ -> return ( fromMaybe "Rua não informada" (logradouro dados)
                            , fromMaybe "Cidade não informada" (localidade dados)
                            )
        Left _ -> return ("Erro ao consultar API", "Sem cidade")

-- ==========================================
-- FUNÇÕES AUXILIARES
-- ==========================================
statusEntregue :: Text
statusEntregue = "Entregue"

statusPedidos :: [Text]
statusPedidos = ["Em Preparo", "Saiu para Entrega", statusEntregue]

-- Ex.: 25.5 -> "R$ 25,50"
formatarReais :: Double -> Text
formatarReais valor = "R$ " ++ pack (map virgula (printf "%.2f" valor))
  where
    virgula '.' = ','
    virgula c   = c

-- Usa o preço guardado no item, e não o preço atual do produto
subtotalItem :: ItemPedido -> Double
subtotalItem item = fromIntegral (itemPedidoQuantidade item) * itemPedidoPrecoUnitario item

-- Soma os subtotais dos itens que pertencem ao pedido
totalPedido :: [Entity ItemPedido] -> PedidoId -> Double
totalPedido itens pid =
    sum [subtotalItem item | Entity _ item <- itens, itemPedidoPedidoId item == pid]

-- Só exclui se nenhum outro registro depender deste; senão, mostra o aviso.
-- A contagem e a exclusão ocorrem na mesma transação.
excluirSemVinculos :: SqlPersistT Handler Int -> SqlPersistT Handler () -> Html -> Handler ()
excluirSemVinculos contarVinculos excluir aviso = do
    excluiu <- runDB $ do
        vinculos <- contarVinculos
        if vinculos > 0
            then return False
            else excluir >> return True
    setMessage $ if excluiu then "Registro excluído." else aviso

-- ==========================================
-- 2. CRUD DE CATEGORIAS
-- ==========================================
getCategoriasR :: Handler Html
getCategoriasR = do
    categorias <- runDB $ selectList [] [Asc CategoriaNome]
    defaultLayout $ do
        setTitle "Categorias - Lanchonete"
        [whamlet|
            <h1>Gerenciar Categorias
            <p><a href=@{HomeR}>Início</a> | <a href=@{CategoriasR}>Categorias</a> | <a href=@{ProdutosR}>Produtos</a> | <a href=@{ClientesR}>Clientes</a> | <a href=@{EntregadoresR}>Entregadores</a> | <a href=@{PedidosR}>Pedidos</a> | <a href=@{ItensR}>Itens</a> | <a href=@{AvaliacoesR}>Avaliações</a>
            <h3>Nova Categoria
            <form .form-crud method=post action=@{CategoriasR}>
                Nome: <input type=text name=nome required>
                Descrição: <input type=text name=descricao required>
                <button .btn .btn-primary type=submit>Cadastrar

            <h3>Lista de Categorias
            <table .table .table-striped .table-bordered .table-hover>
                <tr>
                    <th>Nome
                    <th>Descrição
                    <th>Ações
                $forall Entity cid cat <- categorias
                    <tr>
                        <td>#{categoriaNome cat}
                        <td>#{categoriaDescricao cat}
                        <td>
                            <a .btn .btn-default .btn-xs href=@{CategoriaEditarR cid}>Editar
                            <form method=post action=@{CategoriaExcluirR cid} style="display:inline;">
                                <button .btn .btn-danger .btn-xs type=submit>Excluir
        |]

postCategoriasR :: Handler Html
postCategoriasR = do
    nome <- runInputPost $ ireq textField "nome"
    desc <- runInputPost $ ireq textField "descricao"
    _ <- runDB $insert$ Categoria nome desc
    redirect CategoriasR

getCategoriaEditarR :: CategoriaId -> Handler Html
getCategoriaEditarR cid = do
    cat <- runDB $ get404 cid
    defaultLayout $ do
        setTitle "Editar Categoria"
        [whamlet|
            <h1>Editar Categoria
            <form .form-crud method=post action=@{CategoriaEditarR cid}>
                Nome: <input type=text name=nome value=#{categoriaNome cat} required>
                Descrição: <input type=text name=descricao value=#{categoriaDescricao cat} required>
                <button .btn .btn-primary type=submit>Salvar Alterações
            <p><a href=@{CategoriasR}>Cancelar
        |]

postCategoriaEditarR :: CategoriaId -> Handler Html
postCategoriaEditarR cid = do
    nome <- runInputPost $ ireq textField "nome"
    desc <- runInputPost $ ireq textField "descricao"
    runDB $ update cid [CategoriaNome =. nome, CategoriaDescricao =. desc]
    redirect CategoriasR

postCategoriaExcluirR :: CategoriaId -> Handler Html
postCategoriaExcluirR cid = do
    excluirSemVinculos (count [ProdutoCategoriaId ==. cid]) (delete cid)
        "Não é possível excluir: há produtos nesta categoria."
    redirect CategoriasR

-- ==========================================
-- 3. CRUD DE CLIENTES (COM API VIACEP)
-- ==========================================
getClientesR :: Handler Html
getClientesR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    defaultLayout $ do
        setTitle "Clientes - Lanchonete"
        [whamlet|
            <h1>Gerenciar Clientes (Com Busca Automática de CEP)
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{CategoriasR}>Categorias</a> | <a href=@{ProdutosR}>Produtos</a>

            <h3>Novo Cliente
            <form .form-crud method=post action=@{ClientesR}>
                Nome: <input type=text name=nome required>
                Telefone: <input type=text name=telefone required>
                CEP (com ou sem hífen, ex: 01001-000): <input type=text name=cep required>
                Número/Complemento (opcional): <input type=text name=complemento>
                <button .btn .btn-primary type=submit>Cadastrar e Buscar Endereço

            <h3>Clientes Cadastrados
            <table .table .table-striped .table-bordered .table-hover>
                <tr>
                    <th>Nome
                    <th>Telefone
                    <th>CEP
                    <th>Endereço (ViaCEP)
                    <th>Número/Complemento
                    <th>Cidade (ViaCEP)
                    <th>Ações
                $forall Entity clid cli <- clientes
                    <tr>
                        <td>#{clienteNome cli}
                        <td>#{clienteTelefone cli}
                        <td>#{clienteCep cli}
                        <td>#{clienteEndereco cli}
                        <td>#{fromMaybe "-" (clienteComplemento cli)}
                        <td>#{clienteCidade cli}
                        <td>
                            <a .btn .btn-default .btn-xs href=@{ClienteEditarR clid}>Editar
                            <form method=post action=@{ClienteExcluirR clid} style="display:inline;">
                                <button .btn .btn-danger .btn-xs type=submit>Excluir
        |]

postClientesR :: Handler Html
postClientesR = do
    nome <- runInputPost $ ireq textField "nome"
    tel  <- runInputPost $ ireq textField "telefone"
    cep  <- runInputPost $ ireq textField "cep"
    comp <- runInputPost $ iopt textField "complemento"
    (rua, cidade) <- liftIO $ buscarEnderecoPorCep cep
    _ <- runDB $ insert $ Cliente nome tel cep rua comp cidade
    redirect ClientesR

getClienteEditarR :: ClienteId -> Handler Html
getClienteEditarR clid = do
    cli <- runDB $ get404 clid
    defaultLayout $ do
        setTitle "Editar Cliente"
        [whamlet|
            <h1>Editar Cliente
            <form .form-crud method=post action=@{ClienteEditarR clid}>
                Nome: <input type=text name=nome value=#{clienteNome cli} required>
                Telefone: <input type=text name=telefone value=#{clienteTelefone cli} required>
                CEP: <input type=text name=cep value=#{clienteCep cli} required>
                Endereço: <input type=text name=endereco value=#{clienteEndereco cli} required>
                Número/Complemento (opcional): <input type=text name=complemento value=#{fromMaybe "" (clienteComplemento cli)}>
                Cidade: <input type=text name=cidade value=#{clienteCidade cli} required>
                <button .btn .btn-primary type=submit>Salvar
            <p><a href=@{ClientesR}>Cancelar
        |]

postClienteEditarR :: ClienteId -> Handler Html
postClienteEditarR clid = do
    nome <- runInputPost $ ireq textField "nome"
    tel  <- runInputPost $ ireq textField "telefone"
    cep  <- runInputPost $ ireq textField "cep"
    end  <- runInputPost $ ireq textField "endereco"
    comp <- runInputPost $ iopt textField "complemento"
    cid  <- runInputPost $ ireq textField "cidade"
    runDB $ update clid [ClienteNome =. nome, ClienteTelefone =. tel, ClienteCep =. cep, ClienteEndereco =. end, ClienteComplemento =. comp, ClienteCidade =. cid]
    redirect ClientesR

postClienteExcluirR :: ClienteId -> Handler Html
postClienteExcluirR clid = do
    excluirSemVinculos (count [PedidoClienteId ==. clid]) (delete clid)
        "Não é possível excluir: este cliente tem pedidos."
    redirect ClientesR

-- ==========================================
-- 4. CRUD DE PRODUTOS (RELACIONADO A CATEGORIA)
-- ==========================================
getProdutosR :: Handler Html
getProdutosR = do
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    categorias <- runDB $ selectList [] [Asc CategoriaNome]
    let nomesCategorias = [(cid, categoriaNome cat) | Entity cid cat <- categorias]
        nomeCategoria prod = fromMaybe "-" (lookup (produtoCategoriaId prod) nomesCategorias)
    defaultLayout $ do
        setTitle "Produtos - Lanchonete"
        [whamlet|
            <h1>Cardápio / Produtos
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{CategoriasR}>Categorias</a> | <a href=@{ClientesR}>Clientes</a>

            <h3>Novo Produto
            <form .form-crud method=post action=@{ProdutosR}>
                Nome: <input type=text name=nome required>
                Preço (ex: 25.50): <input type=number step=0.01 name=preco required>
                Categoria:
                <select name=categoriaId required>
                    $forall Entity cid cat <- categorias
                        <option value=#{fromSqlKey cid}>#{categoriaNome cat}
                <button .btn .btn-primary type=submit>Cadastrar Produto

            <h3>Produtos Cadastrados
            <table .table .table-striped .table-bordered .table-hover>
                <tr>
                    <th>Nome
                    <th>Preço
                    <th>Categoria
                    <th>Ações
                $forall Entity pid prod <- produtos
                    <tr>
                        <td>#{produtoNome prod}
                        <td>#{formatarReais (produtoPreco prod)}
                        <td>#{nomeCategoria prod}
                        <td>
                            <a .btn .btn-default .btn-xs href=@{ProdutoEditarR pid}>Editar
                            <form method=post action=@{ProdutoExcluirR pid} style="display:inline;">
                                <button .btn .btn-danger .btn-xs type=submit>Excluir
        |]

postProdutosR :: Handler Html
postProdutosR = do
    nome  <- runInputPost $ ireq textField "nome"
    preco <- runInputPost $ ireq doubleField "preco"
    catIdInt <- runInputPost $ ireq intField "categoriaId"
    _ <- runDB $ insert $ Produto nome preco (toSqlKey (fromIntegral catIdInt))
    redirect ProdutosR

getProdutoEditarR :: ProdutoId -> Handler Html
getProdutoEditarR pid = do
    prod <- runDB $ get404 pid
    defaultLayout $ do
        setTitle "Editar Produto"
        [whamlet|
            <h1>Editar Produto
            <form .form-crud method=post action=@{ProdutoEditarR pid}>
                Nome: <input type=text name=nome value=#{produtoNome prod} required>
                Preço: <input type=number step=0.01 name=preco value=#{produtoPreco prod} required>
                <button .btn .btn-primary type=submit>Salvar
            <p><a href=@{ProdutosR}>Cancelar
        |]

postProdutoEditarR :: ProdutoId -> Handler Html
postProdutoEditarR pid = do
    nome  <- runInputPost $ ireq textField "nome"
    preco <- runInputPost $ ireq doubleField "preco"
    runDB $ update pid [ProdutoNome =. nome, ProdutoPreco =. preco]
    redirect ProdutosR

postProdutoExcluirR :: ProdutoId -> Handler Html
postProdutoExcluirR pid = do
    excluirSemVinculos (count [ItemPedidoProdutoId ==. pid]) (delete pid)
        "Não é possível excluir: este produto está em pedidos."
    redirect ProdutosR

    -- ==========================================
-- 5. CRUD DE ENTREGADORES
-- ==========================================
getEntregadoresR :: Handler Html
getEntregadoresR = do
    entregadores <- runDB $ selectList [] [Asc EntregadorNome]
    defaultLayout $ do
        setTitle "Entregadores - Lanchonete"
        [whamlet|
            <h1>Gerenciar Entregadores
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{PedidosR}>Pedidos</a>

            <h3>Novo Entregador
            <form .form-crud method=post action=@{EntregadoresR}>
                Nome: <input type=text name=nome required>
                Placa da Moto: <input type=text name=placa required>
                <button .btn .btn-primary type=submit>Cadastrar

            <h3>Entregadores Cadastrados
            <table .table .table-striped .table-bordered .table-hover>
                <tr>
                    <th>Nome
                    <th>Placa da Moto
                    <th>Ações
                $forall Entity eid ent <- entregadores
                    <tr>
                        <td>#{entregadorNome ent}
                        <td>#{entregadorPlacaMoto ent}
                        <td>
                            <a .btn .btn-default .btn-xs href=@{EntregadorEditarR eid}>Editar
                            <form method=post action=@{EntregadorExcluirR eid} style="display:inline;">
                                <button .btn .btn-danger .btn-xs type=submit>Excluir
        |]

postEntregadoresR :: Handler Html
postEntregadoresR = do
    nome  <- runInputPost $ ireq textField "nome"
    placa <- runInputPost $ ireq textField "placa"
    _ <- runDB $ insert $ Entregador nome placa
    redirect EntregadoresR

getEntregadorEditarR :: EntregadorId -> Handler Html
getEntregadorEditarR eid = do
    ent <- runDB $ get404 eid
    defaultLayout $ do
        setTitle "Editar Entregador"
        [whamlet|
            <h1>Editar Entregador
            <form .form-crud method=post action=@{EntregadorEditarR eid}>
                Nome: <input type=text name=nome value=#{entregadorNome ent} required>
                Placa da Moto: <input type=text name=placa value=#{entregadorPlacaMoto ent} required>
                <button .btn .btn-primary type=submit>Salvar
            <p><a href=@{EntregadoresR}>Cancelar
        |]

postEntregadorEditarR :: EntregadorId -> Handler Html
postEntregadorEditarR eid = do
    nome  <- runInputPost $ ireq textField "nome"
    placa <- runInputPost $ ireq textField "placa"
    runDB $ update eid [EntregadorNome =. nome, EntregadorPlacaMoto =. placa]
    redirect EntregadoresR

postEntregadorExcluirR :: EntregadorId -> Handler Html
postEntregadorExcluirR eid = do
    excluirSemVinculos (count [PedidoEntregadorId ==. Just eid]) (delete eid)
        "Não é possível excluir: este entregador tem pedidos."
    redirect EntregadoresR

-- ==========================================
-- 6. CRUD DE PEDIDOS
-- ==========================================
getPedidosR :: Handler Html
getPedidosR = do
    pedidos      <- runDB $ selectList [] [Desc PedidoId]
    clientes     <- runDB $ selectList [] [Asc ClienteNome]
    entregadores <- runDB $ selectList [] [Asc EntregadorNome]
    itens        <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Pedidos - Lanchonete"
        [whamlet|
            <h1>Gerenciar Pedidos
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{ItensR}>Itens do Pedido</a> | <a href=@{AvaliacoesR}>Avaliações</a>

            <h3>Novo Pedido
            <form .form-crud method=post action=@{PedidosR}>
                Cliente:
                <select name=clienteId required>
                    $forall Entity clid cli <- clientes
                        <option value=#{fromSqlKey clid}>#{clienteNome cli}
                Entregador (Opcional):
                <select name=entregadorId>
                    <option value="0">-- Sem entregador ainda --
                    $forall Entity eid ent <- entregadores
                        <option value=#{fromSqlKey eid}>#{entregadorNome ent}
                Status:
                <select name=status required>
                    $forall st <- statusPedidos
                        <option value=#{st}>#{st}
                Observação: <input type=text name=observacao>
                <button .btn .btn-primary type=submit>Abrir Pedido

            <h3>Pedidos Registrados
            <table .table .table-striped .table-bordered .table-hover>
                <tr>
                    <th>ID Pedido
                    <th>ID Cliente
                    <th>ID Entregador
                    <th>Status
                    <th>Total
                    <th>Observação
                    <th>Ações
                $forall Entity pid ped <- pedidos
                    <tr>
                        <td>##{fromSqlKey pid}
                        <td>#{fromSqlKey (pedidoClienteId ped)}
                        <td>
                            $maybe eid <- pedidoEntregadorId ped
                                #{fromSqlKey eid}
                            $nothing
                                Sem entregador
                        <td>#{pedidoStatus ped}
                        <td>#{formatarReais (totalPedido itens pid)}
                        <td>#{fromMaybe "-" (pedidoObservacao ped)}
                        <td>
                            <a .btn .btn-default .btn-xs href=@{PedidoEditarR pid}>Editar Status
                            <form method=post action=@{PedidoExcluirR pid} style="display:inline;" onsubmit="return confirm('Excluir o pedido, seus itens e sua avaliação?');">
                                <button .btn .btn-danger .btn-xs type=submit>Excluir
        |]

postPedidosR :: Handler Html
postPedidosR = do
    cliIdInt <- runInputPost $ ireq intField "clienteId"
    entIdInt <- runInputPost $ ireq intField "entregadorId"
    status   <- runInputPost $ ireq textField "status"
    obs      <- runInputPost $ iopt textField "observacao"
    let mEntregador = if entIdInt == 0 then Nothing else Just (toSqlKey (fromIntegral entIdInt))
    _ <- runDB $ insert $ Pedido (toSqlKey (fromIntegral cliIdInt)) mEntregador status obs
    redirect PedidosR

getPedidoEditarR :: PedidoId -> Handler Html
getPedidoEditarR pid = do
    ped <- runDB $ get404 pid
    defaultLayout $ do
        setTitle "Editar Pedido"
        [whamlet|
            <h1>Atualizar Status do Pedido ##{fromSqlKey pid}
            <form .form-crud method=post action=@{PedidoEditarR pid}>
                Status:
                <select name=status required>
                    $forall st <- statusPedidos
                        <option value=#{st} :st == pedidoStatus ped:selected>#{st}
                Observação: <input type=text name=observacao value=#{fromMaybe "" (pedidoObservacao ped)}>
                <button .btn .btn-primary type=submit>Salvar
            <p><a href=@{PedidosR}>Cancelar
        |]

postPedidoEditarR :: PedidoId -> Handler Html
postPedidoEditarR pid = do
    status <- runInputPost $ ireq textField "status"
    obs    <- runInputPost $ iopt textField "observacao"
    runDB $ update pid [PedidoStatus =. status, PedidoObservacao =. obs]
    redirect PedidosR

postPedidoExcluirR :: PedidoId -> Handler Html
postPedidoExcluirR pid = do
    -- Itens e avaliação só existem dentro do pedido: saem junto, na mesma transação
    runDB $ do
        deleteWhere [ItemPedidoPedidoId ==. pid]
        deleteWhere [AvaliacaoPedidoId ==. pid]
        delete pid
    setMessage "Pedido excluído."
    redirect PedidosR

-- ==========================================
-- 7. CRUD DE ITENS DO PEDIDO
-- ==========================================
getItensR :: Handler Html
getItensR = do
    itens    <- runDB $ selectList [] [Desc ItemPedidoId]
    pedidos  <- runDB $ selectList [] [Desc PedidoId]
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $ do
        setTitle "Itens do Pedido - Lanchonete"
        [whamlet|
            <h1>Itens dos Pedidos
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{PedidosR}>Pedidos</a> | <a href=@{ProdutosR}>Produtos</a>

            <h3>Adicionar Item a um Pedido
            <form .form-crud method=post action=@{ItensR}>
                Pedido:
                <select name=pedidoId required>
                    $forall Entity pid ped <- pedidos
                        <option value=#{fromSqlKey pid}>Pedido ##{fromSqlKey pid} (#{pedidoStatus ped})
                Produto:
                <select name=produtoId required>
                    $forall Entity prid prod <- produtos
                        <option value=#{fromSqlKey prid}>#{produtoNome prod} - #{formatarReais (produtoPreco prod)}
                Quantidade: <input type=number name=quantidade value="1" min="1" required>
                <button .btn .btn-primary type=submit>Adicionar Item

            <h3>Itens Lançados
            <table .table .table-striped .table-bordered .table-hover>
                <tr>
                    <th>ID Pedido
                    <th>ID Produto
                    <th>Quantidade
                    <th>Preço Unitário
                    <th>Subtotal
                    <th>Ações
                $forall Entity iid item <- itens
                    <tr>
                        <td>##{fromSqlKey (itemPedidoPedidoId item)}
                        <td>#{fromSqlKey (itemPedidoProdutoId item)}
                        <td>#{itemPedidoQuantidade item}
                        <td>#{formatarReais (itemPedidoPrecoUnitario item)}
                        <td>#{formatarReais (subtotalItem item)}
                        <td>
                            <a .btn .btn-default .btn-xs href=@{ItemEditarR iid}>Editar Qtd
                            <form method=post action=@{ItemExcluirR iid} style="display:inline;">
                                <button .btn .btn-danger .btn-xs type=submit>Excluir
        |]

postItensR :: Handler Html
postItensR = do
    pedIdInt  <- runInputPost $ ireq intField "pedidoId"
    prodIdInt <- runInputPost $ ireq intField "produtoId"
    qtd       <- runInputPost $ ireq intField "quantidade"
    let prid = toSqlKey (fromIntegral prodIdInt)
    runDB $ do
        produto <- get404 prid
        -- Guarda o preço do momento: reajustes futuros não mudam este pedido
        insert_ $ ItemPedido (toSqlKey (fromIntegral pedIdInt)) prid qtd (produtoPreco produto)
    redirect ItensR

getItemEditarR :: ItemPedidoId -> Handler Html
getItemEditarR iid = do
    item <- runDB $ get404 iid
    defaultLayout $ do
        setTitle "Editar Item"
        [whamlet|
            <h1>Editar Quantidade do Item
            <form .form-crud method=post action=@{ItemEditarR iid}>
                Quantidade: <input type=number name=quantidade value=#{itemPedidoQuantidade item} min="1" required>
                <button .btn .btn-primary type=submit>Salvar
            <p><a href=@{ItensR}>Cancelar
        |]

postItemEditarR :: ItemPedidoId -> Handler Html
postItemEditarR iid = do
    qtd <- runInputPost $ ireq intField "quantidade"
    runDB $ update iid [ItemPedidoQuantidade =. qtd]
    redirect ItensR

postItemExcluirR :: ItemPedidoId -> Handler Html
postItemExcluirR iid = do
    runDB $ delete iid
    redirect ItensR

-- ==========================================
-- 8. CRUD DE AVALIAÇÕES
-- ==========================================
getAvaliacoesR :: Handler Html
getAvaliacoesR = do
    avaliacoes <- runDB $ selectList [] [Desc AvaliacaoId]
    entregues  <- runDB $ selectList [PedidoStatus ==. statusEntregue] [Desc PedidoId]
    -- Só pedidos concluídos (entregues) e ainda sem avaliação podem ser avaliados
    let avaliados = [avaliacaoPedidoId av | Entity _ av <- avaliacoes]
        pedidos   = [ped | ped <- entregues, entityKey ped `notElem` avaliados]
    defaultLayout $ do
        setTitle "Avaliações - Lanchonete"
        [whamlet|
            <h1>Avaliações dos Pedidos
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{PedidosR}>Pedidos</a>

            <h3>Registrar Avaliação
            $if null pedidos
                <p .text-muted>Nenhum pedido entregue aguardando avaliação.
            $else
                <form .form-crud method=post action=@{AvaliacoesR}>
                    Pedido:
                    <select name=pedidoId required>
                        $forall Entity pid ped <- pedidos
                            <option value=#{fromSqlKey pid}>Pedido ##{fromSqlKey pid} (#{pedidoStatus ped})
                    Nota (1 a 5): <input type=number name=nota value="5" min="1" max="5" required>
                    Comentário: <input type=text name=comentario required>
                    <button .btn .btn-primary type=submit>Enviar Avaliação

            <h3>Avaliações Recebidas
            <table .table .table-striped .table-bordered .table-hover>
                <tr>
                    <th>Pedido
                    <th>Nota
                    <th>Comentário
                    <th>Ações
                $forall Entity aid av <- avaliacoes
                    <tr>
                        <td>##{fromSqlKey (avaliacaoPedidoId av)}
                        <td>#{avaliacaoNota av} / 5
                        <td>#{avaliacaoComentario av}
                        <td>
                            <a .btn .btn-default .btn-xs href=@{AvaliacaoEditarR aid}>Editar
                            <form method=post action=@{AvaliacaoExcluirR aid} style="display:inline;">
                                <button .btn .btn-danger .btn-xs type=submit>Excluir
        |]

postAvaliacoesR :: Handler Html
postAvaliacoesR = do
    pedIdInt <- runInputPost $ ireq intField "pedidoId"
    nota     <- runInputPost $ ireq intField "nota"
    com      <- runInputPost $ ireq textField "comentario"
    let pid = toSqlKey (fromIntegral pedIdInt)
    pedido   <- runDB $ get404 pid
    -- A regra também é conferida no servidor, não só na lista do formulário
    if pedidoStatus pedido == statusEntregue
        then do
            -- insertUnique devolve Nothing se o pedido já tiver avaliação (UniqueAvaliacaoPedido)
            resultado <- runDB $ insertUnique $ Avaliacao pid nota com
            case resultado of
                Just _  -> setMessage "Avaliação registrada."
                Nothing -> setMessage "Este pedido já foi avaliado."
        else setMessage "Só pedidos entregues podem ser avaliados."
    redirect AvaliacoesR

getAvaliacaoEditarR :: AvaliacaoId -> Handler Html
getAvaliacaoEditarR aid = do
    av <- runDB $ get404 aid
    defaultLayout $ do
        setTitle "Editar Avaliação"
        [whamlet|
            <h1>Editar Avaliação
            <form .form-crud method=post action=@{AvaliacaoEditarR aid}>
                Nota (1 a 5): <input type=number name=nota value=#{avaliacaoNota av} min="1" max="5" required>
                Comentário: <input type=text name=comentario value=#{avaliacaoComentario av} required>
                <button .btn .btn-primary type=submit>Salvar
            <p><a href=@{AvaliacoesR}>Cancelar
        |]

postAvaliacaoEditarR :: AvaliacaoId -> Handler Html
postAvaliacaoEditarR aid = do
    nota <- runInputPost $ ireq intField "nota"
    com  <- runInputPost $ ireq textField "comentario"
    runDB $ update aid [AvaliacaoNota =. nota, AvaliacaoComentario =. com]
    redirect AvaliacoesR

postAvaliacaoExcluirR :: AvaliacaoId -> Handler Html
postAvaliacaoExcluirR aid = do
    runDB $ delete aid
    redirect AvaliacoesR
