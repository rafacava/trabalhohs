{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql

formUsuario :: Form Usuario 
formUsuario = renderDivs $ Usuario 
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "Email: " Nothing
        <*> areq passwordField  "Senha: " Nothing

getCadUsuarioR :: Handler Html
getCadUsuarioR = do 
    (widget,enctype) <- generateFormPost formUsuario
    defaultLayout $ do 
        [whamlet|
            <form action=@{CadUsuarioR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="OK">
        |]

postCadUsuarioR :: Handler Html
postCadUsuarioR = do 
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usu -> do 
            uid <- runDB $ insert usu 
            redirect (PerfilUsuarioR uid)
        _ -> redirect HomeR


getPerfilUsuarioR :: UsuarioId -> Handler Html
getPerfilUsuarioR uid = do 
    usu <- runDB $ get404 uid
    defaultLayout $ do 
        [whamlet|
            <h1> 
                Nome: #{usuarioNome usu}

            <h2>
                E-mail: #{usuarioEmail usu}

            <a href=@{HomeR}> Voltar
        |]
