{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) 
        <$> areq emailField "Email: " Nothing
        <*> areq passwordField "Senha: " Nothing
        
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
        [whamlet|

                <!doctype html>
                <html lang="en">

                <head>
                <!-- Required meta tags -->
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

                <!-- Bootstrap CSS -->
                <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" integrity="sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB"
                    crossorigin="anonymous">
                    <style>
                        body {
                            background-color: black;
                            color: white;
                        }
                <body>

                <div class="container-fluid">
                    <div class="jumbotron col-sm-6 text-center col-centered">
                        <h1 style="color:black;">Cadastre um usuario
                            <br>
                            <p><h3 style="color:black;">Preencha os todos os campos:
                            <div class="container">
                                 <div class="row text-center">
                                    $maybe mensa <- msg
                                        ^{mensa}
                                    <form action=@{LoginR} method=post enctype=#{enctype}>
                                        ^{widget}
                                        <input type="submit" value="OK">
        |]

-- BD (Maybe (Entity Usuario))
autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Usuario))
autenticar email senha = runDB $ selectFirst [UsuarioEmail ==. email
                                             ,UsuarioSenha ==. senha] []

postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("root@root.com","root") -> do 
            setSession "_USR" (pack (show $ Usuario "admin" "" ""))
            redirect AdminR
        FormSuccess (email,senha) -> do 
            talvezUsuario <- autenticar email senha 
            case talvezUsuario of 
                Nothing -> do 
                    setMessage [shamlet| 
                        <h1> 
                            Usuario nao cadastrado/Senha inválida 
                    |]
                    redirect LoginR
                Just (Entity uid (Usuario n e _)) -> do 
                    setSession "_USR" (pack (show $ Usuario n e ""))
                    redirect (PerfilUsuarioR uid)
        _ -> redirect HomeR

postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_USR"
    redirect HomeR

getAdminR :: Handler Html 
getAdminR = do 
    defaultLayout [whamlet|
        <h1> BEM VINDO ADMIN!
        <form action=@{LogoutR} method=post>
            <input type="submit" value="Logout">
    |]