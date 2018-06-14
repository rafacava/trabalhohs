{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cinema where

import Import
import Database.Persist.Postgresql

formArquivo :: Form (Text,Int,Text,Int,Maybe FileInfo)
formArquivo = renderDivs $ (,,,,)
    <$> areq textField "Nome: " Nothing
    <*> areq intField "telefone: " Nothing
    <*> areq textField "Local: " Nothing
    <*> areq intField "Numero de Salas: " Nothing
    <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Imagem do cinema: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing

getCinemaR :: Handler Html
getCinemaR = do 
    (widget,enctype) <- generateFormPost formArquivo
    defaultLayout $ do
        [whamlet|
        <html>
        <head>
            <title>Cadastre um Cinema - Askcine
            <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" integrity="sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB" crossorigin="anonymous">
            <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous">
            <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous">
            <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/js/bootstrap.min.js" integrity="sha384-smHYKdLADwkXOn1EmN1qk/HfnUcbVRZyYmZ4qpPea6sjB/pTJ0euyQp0Mk8ck+5T" crossorigin="anonymous">
        <body>
            <div class="container">
                <h1> Cadastre um cinema
                <div class="row">
                    <form action=@{CinemaR} method=post enctype=#{enctype}>
                        ^{widget}                
                        <input type="submit" value="Cadastrar">
            |]


postCinemaR :: Handler Html 
postCinemaR = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess (nome,telefone,local,nsalas,Just arq) -> do 
            cid <- runDB $ insert $ Cinema nome telefone local nsalas (Just $ (fileName arq))
            liftIO $ fileMove arq ("static/imgs/cinemas" ++ (unpack $ fileName arq))
            redirect (CinemasR cid)
        FormSuccess (nome,telefone,local,nsalas,Nothing) -> do 
            cid <- runDB $ insert $ Cinema nome telefone local nsalas Nothing
            redirect (CinemasR cid)
        _ -> redirect HomeR


getCinemasR :: CinemaId -> Handler Html
getCinemasR cid = do 
    fm <- runDB $ get404 cid
    imagem <- return $ cinemaImagem fm
    staticDir <- return $ "../static/imgs/cinemas"
    defaultLayout $ do 
        [whamlet|
            <h1>
                Nome: #{cinemaNome fm}
            <h2>
                Telefone: #{show $ cinemaTelefone fm}
            <h2>
                Local: #{cinemaLocal fm} 
            <h2>
                $maybe img <- imagem 
                    <img src=#{staticDir ++ img}>
        |]



postCinemasR :: CinemaId -> Handler Html
postCinemasR cid = do 
    runDB $ delete cid 
    redirect ListaCinemaR


getListaCinemaR :: Handler Html
getListaCinemaR = do
    
    cinemas <- runDB $ selectList [] [Asc CinemaId]
    defaultLayout $ do 
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <th>
                            Id
                        <th>
                            Nome
                        
                        <th> 
                            Telefone
                        
                        <th>
                            Local
                        
                        <th>
                            
                
                <tbody>
                    $forall (Entity cid cinema) <- cinemas
   
                        <tr>
                            <td>
                            <button value="#{show $ cinemaTelefone cinema}">#{cinemaNome cinema}
                            
                            <td>
                                <a href=@{CinemasR cid}> 
                                    #{cinemaNome cinema}
                            
                            <td>
                                #{show $ cinemaTelefone cinema}
                            
                            <td>
                                #{cinemaLocal cinema}
                            
                            <td>
                            <form action=@{CinemasR cid} method=post>
                                <input type="submit" value="Apagar">
        |]