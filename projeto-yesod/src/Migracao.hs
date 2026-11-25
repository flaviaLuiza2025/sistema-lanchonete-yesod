{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Migração do banco, incluindo os ajustes de dados que o Persistent não faz.
module Migracao (migrarBanco) where

import Import.NoFoundation
import Control.Monad.Logger (logWarnN)
import Database.Persist.Sql (Single (..), rawExecute, rawExecuteCount, rawSql)

-- | Tudo roda em uma única transação: se algo falhar, nada é alterado.
migrarBanco :: (MonadUnliftIO m, MonadLogger m) => ReaderT SqlBackend m ()
migrarBanco = do
    colunasAvaliacao <- colunasDa "avaliacao"
    colunasItem      <- colunasDa "item_pedido"

    -- Antes de criar a chave única, remove as avaliações repetidas
    unless (null colunasAvaliacao) removerAvaliacoesRepetidas

    runMigration migrateAll

    -- Só na primeira vez em que a coluna é criada
    unless ("preco_unitario" `elem` colunasItem) preencherPrecosDosItens

-- | Nomes das colunas de uma tabela (lista vazia se a tabela não existe).
colunasDa :: MonadIO m => Text -> ReaderT SqlBackend m [Text]
colunasDa tabela =
    map unSingle <$> rawSql "SELECT name FROM pragma_table_info(?)" [PersistText tabela]

-- | Mantém só a primeira avaliação de cada pedido.
removerAvaliacoesRepetidas :: (MonadUnliftIO m, MonadLogger m) => ReaderT SqlBackend m ()
removerAvaliacoesRepetidas = do
    removidas <- rawExecuteCount
        "DELETE FROM avaliacao WHERE id NOT IN \
        \(SELECT MIN(id) FROM avaliacao GROUP BY pedido_id)" []
    when (removidas > 0) $
        logWarnN $ "Avaliações repetidas removidas: " ++ tshow removidas

-- | Itens antigos recebem o preço atual do produto, o único conhecido.
preencherPrecosDosItens :: MonadUnliftIO m => ReaderT SqlBackend m ()
preencherPrecosDosItens = rawExecute
    "UPDATE item_pedido SET preco_unitario = \
    \(SELECT preco FROM produto WHERE produto.id = item_pedido.produto_id)" []
