{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Treinador where

import Import
import Database.Persist.Postgresql

formTreinador :: Form Treinador 
formTreinador = renderDivs $ Treinador 
        <$> areq textField "Nome: " Nothing
        <*> areq textField "Cidade: " Nothing
        <*> areq dayField  "Dia: " Nothing

getTreinadorR :: Handler Html
getTreinadorR = do 
    (widget,enctype) <- generateFormPost formTreinador
    defaultLayout $ do 
        [whamlet|
            <form action=@{TreinadorR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="OK">
        |]


postTreinadorR :: Handler Html
postTreinadorR = do 
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formTreinador
    case res of
        FormSuccess treinador -> do 
            tid <- runDB $ insert treinador
            defaultLayout [whamlet|
                Treinador #{fromSqlKey tid} inserido(a) com sucesso!
            |]
        _ -> redirect HomeR

getPerfilR :: TreinadorId -> Handler Html
getPerfilR tid = do 
    treinador <- runDB $ get404 tid
    defaultLayout $ do 
        [whamlet|
            <h1>
                TREINADOR #{treinadorNome treinador}
            <h2>
                NASC: #{show $ treinadorDtnasc treinador}
            <h2>
                CIDADE: #{treinadorCidade treinador}  
        |]


postPerfilR :: TreinadorId -> Handler Html
postPerfilR tid = do 
    runDB $ delete tid 
    redirect ListaTreinadorR

-- SELECT * FROM TREINADOR ORDER BY NOME
getListaTreinadorR :: Handler Html
getListaTreinadorR = do
    treinadores <- runDB $ selectList [] [Asc TreinadorNome]
    defaultLayout $ do 
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <th>
                            Nome
                        
                        <th> 
                            Nasc
                        
                        <th>
                            Cidade
                        
                        <th>
                            
                
                <tbody>
                    $forall (Entity tid treinador) <- treinadores
                        <tr>
                            <td>
                                <a href=@{PerfilR tid}> 
                                    #{treinadorNome treinador}
                            
                            <td>
                                #{show $ treinadorDtnasc treinador}
                            
                            <td>
                                #{treinadorCidade treinador}
                            
                            <td>
                                <form action=@{PerfilR tid} method=post>
                                    <input type="submit" value="Apagar">
        |]