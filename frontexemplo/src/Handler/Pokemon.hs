{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pokemon where

import Import
import Database.Persist.Postgresql

formArquivo :: Form (Text,Int,Text,Int,Maybe FileInfo)
formArquivo = renderDivs $ (,,,,)
    <$> areq textField "Nome: " Nothing
    <*> areq intField "Geracao: " Nothing
    <*> areq textField "Tipo: " Nothing
    <*> areq intField "Nivel: " Nothing
    <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Arquivo: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing

getPokemonR :: Handler Html
getPokemonR = do 
    (widget,enctype) <- generateFormPost formArquivo
    defaultLayout $ do
        [whamlet|
            <form action=@{PokemonR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postPokemonR :: Handler Html 
postPokemonR = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess (nome,ger,tip,nivel,Just arq) -> do 
            pid <- runDB $ insert $ Pokemon nome ger tip nivel (Just $ (fileName arq))
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (PokedexR pid)
        FormSuccess (nome,ger,tip,nivel,Nothing) -> do 
            pid <- runDB $ insert $ Pokemon nome ger tip nivel Nothing
            redirect (PokedexR pid)
        _ -> redirect HomeR


getPokedexR :: PokemonId -> Handler Html
getPokedexR pid = do 
    poke <- runDB $ get404 pid
    imagem <- return $ pokemonImagem poke
    staticDir <- return $ "../static/"
    defaultLayout $ do 
        [whamlet|
            <h1>
                POKEMON #{pokemonNome poke}
            <h2>
                Geracao: #{show $ pokemonGeracao poke}
            <h2>
                Tipo: #{pokemonTipo poke} 
            <h2>
                $maybe img <- imagem 
                    <img src=#{staticDir ++ img}>
        |]
