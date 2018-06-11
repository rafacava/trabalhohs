{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Filme where

import Import
import Database.Persist.Postgresql

formArquivof :: Form (Text,Int,Text,Int,Maybe FileInfo)
formArquivof = renderDivs $ (,,,,)
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
geFilmeR = do 
    (widget,enctype) <- generateFormPost formArquivof
    defaultLayout $ do
        [whamlet|
            <form action=@{FilmeR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar Filme">
        |]

postFilmeR :: Handler Html 
postFilmeR = do 
    ((res,_),_) <- runFormPost formArquivof
    case res of 
        FormSuccess (nome,ano,gen,durac,Just arq) -> do 
            fid <- runDB $ insert $ Filme nome ano gen durac (Just $ (fileName arq))
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (FilmesR fid)
        FormSuccess (nome,ano,gen,durac,Nothing) -> do 
            fid <- runDB $ insert $ Filme nome ano gen durac Nothing
            redirect (FilmesR fid)
        _ -> redirect HomeR


getFilmesR :: FilmeId -> Handler Html
getFilmesR fid = do 
    fm <- runDB $ get404 fid
    imagem <- return $ posterFilme fm
    staticDir <- return $ "../static/"
    defaultLayout $ do 
        [whamlet|
            <h1>
                Nome: #{filmeNome fm}
            <h2>
                Ano: #{show $ filmeAno fm}
            <h2>
                Genero: #{filmeGenero fm} 
            <h2>
                $maybe img <- imagem 
                    <img src=#{staticDir ++ img}>
        |]
