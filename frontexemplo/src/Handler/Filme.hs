{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Filme where

import Import
import Database.Persist.Postgresql

formArquivo :: Form (Text,Int,Text,Int,Maybe FileInfo)
formArquivo = renderDivs $ (,,,,)
    <$> areq textField "Nome: " Nothing
    <*> areq intField "Ano: " Nothing
    <*> areq textField "Genero: " Nothing
    <*> areq intField "Duração: " Nothing
    <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Poster: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing

getFilmeR :: Handler Html
getFilmeR = do 
    (widget,enctype) <- generateFormPost formArquivo
    defaultLayout $ do
        [whamlet|
        <html>
            <head>
                <title>Cadastre um Filme - Askcine
                <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" integrity="sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB" crossorigin="anonymous">
                <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous">
                <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous">
                <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/js/bootstrap.min.js" integrity="sha384-smHYKdLADwkXOn1EmN1qk/HfnUcbVRZyYmZ4qpPea6sjB/pTJ0euyQp0Mk8ck+5T" crossorigin="anonymous">
            <body>
                <div class="container">
                    <h1>Cadastre um filme
                    <div class="row">
                        <form action=@{FilmeR} method=post enctype=#{enctype}>
                            ^{widget}                
                            <input type="submit" value="Cadastrar">
                |]


postFilmeR :: Handler Html 
postFilmeR = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess (nome,ano,genero,duracao,Just arq) -> do 
            fid <- runDB $ insert $ Filme nome ano genero duracao (Just $ (fileName arq))
            liftIO $ fileMove arq ("static/imgs/filmes" ++ (unpack $ fileName arq))
            redirect (FilmesR fid)
        FormSuccess (nome,ano,genero,duracao,Nothing) -> do 
            fid <- runDB $ insert $ Filme nome ano genero duracao Nothing
            redirect (FilmesR fid)
        _ -> redirect HomeR


getFilmesR :: FilmeId -> Handler Html
getFilmesR fid = do 
    fm <- runDB $ get404 fid
    imagem <- return $ filmeImagem fm
    staticDir <- return $ "../static/imgs/filmes"
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
                        <h1 style="color:black;">Filme Cadastrado
                            <br>
                            <p><h3 style="color:black;">
                            <div class="container">
                                 <div class="row text-center">
                         <div class="row text-center">
                <h1>
                    Nome: #{filmeNome fm}
                <h3>
                    Ano: #{show $ filmeAno fm}
                <h3>
                    Genero: #{filmeGenero fm} 
                <div class="col-sm">
                    $maybe img <- imagem 
                        <img src=#{staticDir ++ img}>
            |]

postFilmesR :: FilmeId -> Handler Html
postFilmesR tida = do 
    runDB $ delete tida 
    redirect ListaFilmeR


getListaFilmeR :: Handler Html
getListaFilmeR = do
    filmes <- runDB $ selectList [] [Asc FilmeNome]
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
                        <h1 style="color:black;">Lista de filmes
                            <br>
                            <p><h3 style="color:black;">Todos os filmes Cadastrados:
                            <div class="container">
                                 <div class="row text-center">
                                        <h1>Lista de Filmes
                                        <table>
                                            <thead>
                                                <tr>
                                                    <th>
                                                        Nome
                                                    
                                                    <th> 
                                                        Ano
                                                    
                                                    <th>
                                                        Genero
                                                    
                                                    <th>
                                                        
                                            
                                            <tbody>
                                                $forall (Entity fid filme) <- filmes
                                                    <tr>
                                                        <td>
                                                            <a href=@{FilmesR fid}> 
                                                                #{filmeNome filme}
                                                        
                                                        <td>
                                                            #{show $ filmeAno filme}
                                                        
                                                        <td>
                                                            #{filmeGenero filme}
                                                        
                                                        <td>
                                                            <form action=@{FilmesR fid} method=post>
                                                                <input type="submit" value="Apagar">
        |]