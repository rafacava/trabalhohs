{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sessao where

import Import
import Database.Persist.Postgresql()

formSess :: Form (Int,Maybe FileInfo)
formSess = renderDivs $ (,)
        <$> areq intField "Horario da Sessao: " Nothing
        <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Imagem de campanha: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing

getSessaoR :: CinemaId -> Handler Html
getSessaoR cid = do
    cd <- runDB $ get404 cid 
    (widget,enctype) <- generateFormPost formSess
    defaultLayout $ do 
            [whamlet|
            <html>
                <head>
                    <title>Cadastre uma Sessao
                    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" integrity="sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB" crossorigin="anonymous">
                    <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous">
                    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous">
                    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/js/bootstrap.min.js" integrity="sha384-smHYKdLADwkXOn1EmN1qk/HfnUcbVRZyYmZ4qpPea6sjB/pTJ0euyQp0Mk8ck+5T" crossorigin="anonymous">
                <body>
                    <div class="container">
                        <h1>Cadastre uma Sessao
                        <div class="row">
                            <form action=@{SessaoR cid} method=post enctype=#{enctype}>
                                ^{widget}                
                                <input type="submit" value="Cadastrar">
            |]


postSessaoR :: CinemaId -> Handler Html 
postSessaoR cid = do 
    ((res,_),_) <- runFormPost formSess
    case res of 
        FormSuccess (hora,Just arq) -> do 
            sessid <- runDB $ insert $ Sessao hora cid(Just $ (fileName arq))
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect HomeR
        FormSuccess (hora,Nothing) -> do 
            sessid <- runDB $ insert $ Sessao hora cid Nothing
            redirect HomeR
        _ -> redirect HomeR

getPaginaSessaoR :: CinemaId -> SessaoId -> Handler Html
getPaginaSessaoR cid sessid = do 
    sess <- runDB $ get404 sessid
    imagem <- return $ sessaoImagem sess
    staticDir <- return $ "../static/"
    defaultLayout $ do 
        [whamlet|
            <h1>
            Nome: #{sessaoHora sess}

            <h2>
                $maybe img <- imagem 
                    <img width="250px" height="250px" src=#{staticDir ++ img}>
        |]
