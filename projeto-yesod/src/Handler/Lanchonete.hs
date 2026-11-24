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
    let url = "GET https://viacep.com.br/ws/" ++ unpack cepDigitado ++ "/json/"
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
-- 2. CRUD DE CATEGORIAS
-- ==========================================
getCategoriasR :: Handler Html
getCategoriasR = do
    categorias <- runDB $ selectList [] [Asc CategoriaNome]
    defaultLayout $ do
        setTitle "Categorias - Lanchonete"
        [whamlet|
            <h1>Gerenciar Categorias
            <p><a h<p><a href=@{HomeR}>Início</a> | <a href=@{CategoriasR}>Categorias</a> | <a href=@{ProdutosR}>Produtos</a> | <a href=@{ClientesR}>Clientes</a> | <a href=@{EntregadoresR}>Entregadores</a> | <a href=@{PedidosR}>Pedidos</a> | <a href=@{ItensR}>Itens</a> | <a href=@{AvaliacoesR}>Avaliações</a>
            <h3>Nova Categoria
            <form method=post action=@{CategoriasR}>
                Nome: <input type=text name=nome required>
                Descrição: <input type=text name=descricao required>
                <button type=submit>Cadastrar

            <h3>Lista de Categorias
            <table border=1 cellpadding=6>
                <tr>
                    <th>Nome
                    <th>Descrição
                    <th>Ações
                $forall Entity cid cat <- categorias
                    <tr>
                        <td>#{categoriaNome cat}
                        <td>#{categoriaDescricao cat}
                        <td>
                            <a href=@{CategoriaEditarR cid}>Editar
                            <form method=post action=@{CategoriaExcluirR cid} style="display:inline;">
                                <button type=submit>Excluir
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
            <form method=post action=@{CategoriaEditarR cid}>
                Nome: <input type=text name=nome value=#{categoriaNome cat} required>
                Descrição: <input type=text name=descricao value=#{categoriaDescricao cat} required>
                <button type=submit>Salvar Alterações
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
    runDB $ delete cid
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
            <form method=post action=@{ClientesR}>
                Nome: <input type=text name=nome required>
                Telefone: <input type=text name=telefone required>
                CEP (somente números, ex: 01001000): <input type=text name=cep required>
                <button type=submit>Cadastrar e Buscar Endereço

            <h3>Clientes Cadastrados
            <table border=1 cellpadding=6>
                <tr>
                    <th>Nome
                    <th>Telefone
                    <th>CEP
                    <th>Endereço (ViaCEP)
                    <th>Cidade (ViaCEP)
                    <th>Ações
                $forall Entity clid cli <- clientes
                    <tr>
                        <td>#{clienteNome cli}
                        <td>#{clienteTelefone cli}
                        <td>#{clienteCep cli}
                        <td>#{clienteEndereco cli}
                        <td>#{clienteCidade cli}
                        <td>
                            <a href=@{ClienteEditarR clid}>Editar
                            <form method=post action=@{ClienteExcluirR clid} style="display:inline;">
                                <button type=submit>Excluir
        |]

postClientesR :: Handler Html
postClientesR = do
    nome <- runInputPost $ ireq textField "nome"
    tel  <- runInputPost $ ireq textField "telefone"
    cep  <- runInputPost $ ireq textField "cep"
    (rua, cidade) <- liftIO $ buscarEnderecoPorCep cep
    _ <- runDB $insert$ Cliente nome tel cep rua cidade
    redirect ClientesR

getClienteEditarR :: ClienteId -> Handler Html
getClienteEditarR clid = do
    cli <- runDB $ get404 clid
    defaultLayout $ do
        setTitle "Editar Cliente"
        [whamlet|
            <h1>Editar Cliente
            <form method=post action=@{ClienteEditarR clid}>
                Nome: <input type=text name=nome value=#{clienteNome cli} required>
                Telefone: <input type=text name=telefone value=#{clienteTelefone cli} required>
                CEP: <input type=text name=cep value=#{clienteCep cli} required>
                Endereço: <input type=text name=endereco value=#{clienteEndereco cli} required>
                Cidade: <input type=text name=cidade value=#{clienteCidade cli} required>
                <button type=submit>Salvar
            <p><a href=@{ClientesR}>Cancelar
        |]

postClienteEditarR :: ClienteId -> Handler Html
postClienteEditarR clid = do
    nome <- runInputPost $ ireq textField "nome"
    tel  <- runInputPost $ ireq textField "telefone"
    cep  <- runInputPost $ ireq textField "cep"
    end  <- runInputPost $ ireq textField "endereco"
    cid  <- runInputPost $ ireq textField "cidade"
    runDB $ update clid [ClienteNome =. nome, ClienteTelefone =. tel, ClienteCep =. cep, ClienteEndereco =. end, ClienteCidade =. cid]
    redirect ClientesR

postClienteExcluirR :: ClienteId -> Handler Html
postClienteExcluirR clid = do
    runDB $ delete clid
    redirect ClientesR

-- ==========================================
-- 4. CRUD DE PRODUTOS (RELACIONADO A CATEGORIA)
-- ==========================================
getProdutosR :: Handler Html
getProdutosR = do
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    categorias <- runDB $ selectList [] [Asc CategoriaNome]
    defaultLayout $ do
        setTitle "Produtos - Lanchonete"
        [whamlet|
            <h1>Cardápio / Produtos
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{CategoriasR}>Categorias</a> | <a href=@{ClientesR}>Clientes</a>

            <h3>Novo Produto
            <form method=post action=@{ProdutosR}>
                Nome: <input type=text name=nome required>
                Preço (ex: 25.50): <input type=number step=0.01 name=preco required>
                Categoria:
                <select name=categoriaId required>
                    $forall Entity cid cat <- categorias
                        <option value=#{fromSqlKey cid}>#{categoriaNome cat}
                <button type=submit>Cadastrar Produto

            <h3>Produtos Cadastrados
            <table border=1 cellpadding=6>
                <tr>
                    <th>Nome
                    <th>Preço
                    <th>ID Categoria
                    <th>Ações
                $forall Entity pid prod <- produtos
                    <tr>
                        <td>#{produtoNome prod}
                        <td>R$ #{produtoPreco prod}
                        <td>#{fromSqlKey (produtoCategoriaId prod)}
                        <td>
                            <a href=@{ProdutoEditarR pid}>Editar
                            <form method=post action=@{ProdutoExcluirR pid} style="display:inline;">
                                <button type=submit>Excluir
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
            <form method=post action=@{ProdutoEditarR pid}>
                Nome: <input type=text name=nome value=#{produtoNome prod} required>
                Preço: <input type=number step=0.01 name=preco value=#{produtoPreco prod} required>
                <button type=submit>Salvar
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
    runDB $ delete pid
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
            <form method=post action=@{EntregadoresR}>
                Nome: <input type=text name=nome required>
                Placa da Moto: <input type=text name=placa required>
                <button type=submit>Cadastrar

            <h3>Entregadores Cadastrados
            <table border=1 cellpadding=6>
                <tr>
                    <th>Nome
                    <th>Placa da Moto
                    <th>Ações
                $forall Entity eid ent <- entregadores
                    <tr>
                        <td>#{entregadorNome ent}
                        <td>#{entregadorPlacaMoto ent}
                        <td>
                            <a href=@{EntregadorEditarR eid}>Editar
                            <form method=post action=@{EntregadorExcluirR eid} style="display:inline;">
                                <button type=submit>Excluir
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
            <form method=post action=@{EntregadorEditarR eid}>
                Nome: <input type=text name=nome value=#{entregadorNome ent} required>
                Placa da Moto: <input type=text name=placa value=#{entregadorPlacaMoto ent} required>
                <button type=submit>Salvar
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
    runDB $ delete eid
    redirect EntregadoresR

-- ==========================================
-- 6. CRUD DE PEDIDOS
-- ==========================================
getPedidosR :: Handler Html
getPedidosR = do
    pedidos      <- runDB $ selectList [] [Desc PedidoId]
    clientes     <- runDB $ selectList [] [Asc ClienteNome]
    entregadores <- runDB $ selectList [] [Asc EntregadorNome]
    defaultLayout $ do
        setTitle "Pedidos - Lanchonete"
        [whamlet|
            <h1>Gerenciar Pedidos
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{ItensR}>Itens do Pedido</a> | <a href=@{AvaliacoesR}>Avaliações</a>

            <h3>Novo Pedido
            <form method=post action=@{PedidosR}>
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
                    <option value="Em Preparo">Em Preparo
                    <option value="Saiu para Entrega">Saiu para Entrega
                    <option value="Entregue">Entregue
                Observação: <input type=text name=observacao>
                <button type=submit>Abrir Pedido

            <h3>Pedidos Registrados
            <table border=1 cellpadding=6>
                <tr>
                    <th>ID Pedido
                    <th>ID Cliente
                    <th>ID Entregador
                    <th>Status
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
                        <td>#{fromMaybe "-" (pedidoObservacao ped)}
                        <td>
                            <a href=@{PedidoEditarR pid}>Editar Status
                            <form method=post action=@{PedidoExcluirR pid} style="display:inline;">
                                <button type=submit>Excluir
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
            <form method=post action=@{PedidoEditarR pid}>
                Status:
                <select name=status required>
                    <option value="Em Preparo">Em Preparo
                    <option value="Saiu para Entrega">Saiu para Entrega
                    <option value="Entregue">Entregue
                Observação: <input type=text name=observacao value=#{fromMaybe "" (pedidoObservacao ped)}>
                <button type=submit>Salvar
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
    runDB $ delete pid
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
            <form method=post action=@{ItensR}>
                Pedido:
                <select name=pedidoId required>
                    $forall Entity pid ped <- pedidos
                        <option value=#{fromSqlKey pid}>Pedido ##{fromSqlKey pid} (#{pedidoStatus ped})
                Produto:
                <select name=produtoId required>
                    $forall Entity prid prod <- produtos
                        <option value=#{fromSqlKey prid}>#{produtoNome prod} - R$ #{produtoPreco prod}
                Quantidade: <input type=number name=quantidade value="1" min="1" required>
                <button type=submit>Adicionar Item

            <h3>Itens Lançados
            <table border=1 cellpadding=6>
                <tr>
                    <th>ID Pedido
                    <th>ID Produto
                    <th>Quantidade
                    <th>Ações
                $forall Entity iid item <- itens
                    <tr>
                        <td>##{fromSqlKey (itemPedidoPedidoId item)}
                        <td>#{fromSqlKey (itemPedidoProdutoId item)}
                        <td>#{itemPedidoQuantidade item}
                        <td>
                            <a href=@{ItemEditarR iid}>Editar Qtd
                            <form method=post action=@{ItemExcluirR iid} style="display:inline;">
                                <button type=submit>Excluir
        |]

postItensR :: Handler Html
postItensR = do
    pedIdInt  <- runInputPost $ ireq intField "pedidoId"
    prodIdInt <- runInputPost $ ireq intField "produtoId"
    qtd       <- runInputPost $ ireq intField "quantidade"
    _ <- runDB $ insert $ ItemPedido (toSqlKey (fromIntegral pedIdInt)) (toSqlKey (fromIntegral prodIdInt)) qtd
    redirect ItensR

getItemEditarR :: ItemPedidoId -> Handler Html
getItemEditarR iid = do
    item <- runDB $ get404 iid
    defaultLayout $ do
        setTitle "Editar Item"
        [whamlet|
            <h1>Editar Quantidade do Item
            <form method=post action=@{ItemEditarR iid}>
                Quantidade: <input type=number name=quantidade value=#{itemPedidoQuantidade item} min="1" required>
                <button type=submit>Salvar
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
    pedidos    <- runDB $ selectList [] [Desc PedidoId]
    defaultLayout $ do
        setTitle "Avaliações - Lanchonete"
        [whamlet|
            <h1>Avaliações dos Pedidos
            <p><a href=@{HomeR}>Voltar ao Início</a> | <a href=@{PedidosR}>Pedidos</a>

            <h3>Registrar Avaliação
            <form method=post action=@{AvaliacoesR}>
                Pedido:
                <select name=pedidoId required>
                    $forall Entity pid ped <- pedidos
                        <option value=#{fromSqlKey pid}>Pedido ##{fromSqlKey pid} (#{pedidoStatus ped})
                Nota (1 a 5): <input type=number name=nota value="5" min="1" max="5" required>
                Comentário: <input type=text name=comentario required>
                <button type=submit>Enviar Avaliação

            <h3>Avaliações Recebidas
            <table border=1 cellpadding=6>
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
                            <a href=@{AvaliacaoEditarR aid}>Editar
                            <form method=post action=@{AvaliacaoExcluirR aid} style="display:inline;">
                                <button type=submit>Excluir
        |]

postAvaliacoesR :: Handler Html
postAvaliacoesR = do
    pedIdInt <- runInputPost $ ireq intField "pedidoId"
    nota     <- runInputPost $ ireq intField "nota"
    com      <- runInputPost $ ireq textField "comentario"
    _ <- runDB $ insert $ Avaliacao (toSqlKey (fromIntegral pedIdInt)) nota com
    redirect AvaliacoesR

getAvaliacaoEditarR :: AvaliacaoId -> Handler Html
getAvaliacaoEditarR aid = do
    av <- runDB $ get404 aid
    defaultLayout $ do
        setTitle "Editar Avaliação"
        [whamlet|
            <h1>Editar Avaliação
            <form method=post action=@{AvaliacaoEditarR aid}>
                Nota (1 a 5): <input type=number name=nota value=#{avaliacaoNota av} min="1" max="5" required>
                Comentário: <input type=text name=comentario value=#{avaliacaoComentario av} required>
                <button type=submit>Salvar
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
