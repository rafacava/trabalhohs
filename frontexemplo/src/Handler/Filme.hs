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
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (FilmesR fid)
        FormSuccess (nome,ano,genero,duracao,Nothing) -> do 
            fid <- runDB $ insert $ Filme nome ano genero duracao Nothing
            redirect (FilmesR fid)
        _ -> redirect HomeR


getFilmesR :: FilmeId -> Handler Html
getFilmesR fid = do 
    fm <- runDB $ get404 fid
    imagem <- return $ filmeImagem fm
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


getListaFilmeR :: Handler Html
getListaFilmeR = do
    filmes <- runDB $ selectList [] [Asc FilmeNome]
    defaultLayout $ do 
        [whamlet|
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
                                <form action=@{PerfilR tid} method=post>
                                <input type="submit" value="Apagar">
        |]