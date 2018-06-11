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