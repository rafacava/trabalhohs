{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cinema where

import Import
import Database.Persist.Postgresql

formArquivo :: Form (Text,Text,Maybe FileInfo)
formArquivo = renderDivs $ (,,,,)
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Localização: " Nothing
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
            <form action=@{CinemaR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]


postCinemaR :: Handler Html 
postCinemaR = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess (nome,local,Just arq) -> do 
            cid <- runDB $ insert $ Cinema nome local (Just $ (fileName arq))
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (CinemasR fid)
        FormSuccess (nome,local,Nothing) -> do 
            cid <- runDB $ insert $ Cinema nome local Nothing
            redirect (CinemasR fid)
        _ -> redirect HomeR


getCinemasR :: CinemaId -> Handler Html
getCinemasR fid = do 
    cm <- runDB $ get404 fid
    imagem <- return $ cinemaImagem cm
    staticDir <- return $ "../static/"
    defaultLayout $ do 
        [whamlet|
            <h1>
                Nome: #{cinemaNome cm}
            <h2>
                Ano: #{show $ cinemaAno cm}
            <h2>
                Genero: #{cinemaGenero cm} 
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
    cinemas <- runDB $ selectList [] [Asc cinemaNome]
    defaultLayout $ do 
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <th>
                            Nome
                        
                        <th> 
                            Local
                        
                        <th>
                            Imagem
                        
                        <th>
                            
                
                <tbody>
                    $forall (Entity cid cinema) <- cinemas
                        <tr>
                            <td>
                                <a href=@{CinemasR cid}> 
                                    #{cinemaNome cinema}
                            
                            
                            <td>
                                #{cinemaLocal cinema}
                            
                            <td>
                                <form action=@{CinemaR cid} method=post>
                                <input type="submit" value="Apagar">
        |]