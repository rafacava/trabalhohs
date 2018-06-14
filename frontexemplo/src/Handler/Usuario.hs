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
                    <h1 style="color:black;">Menu de Opções
                        <br>
                        <p><h3 style="color:black;">As seguintes ações serão possiveis:
                        <div class="container">
                        <div class="row text-center">
                            <br>
                            <h1> 
                                Nome: #{usuarioNome usu}

                            <h2>
                                E-mail: #{usuarioEmail usu}

                            <a href=@{HomeR}> Voltar
             |]