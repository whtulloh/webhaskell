{-# language RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
             MultiParamTypeClasses, FlexibleContexts, GADTs, 
             OverloadedStrings, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeApplications, FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module WebNote where

import Prelude as PR
import Network.HTTP.Types
import Web.Spock
import Web.Spock.Config
import Web.Spock.Digestive
import Control.Monad.Logger
import Control.Monad.Trans
import Database.Persist.TH
import Database.Persist hiding (get, delete)
import Database.Persist.Sqlite hiding (get, delete)
import qualified Database.Persist as P
import Data.Text as T
import Data.Text.Lazy (toStrict)
import Text.Hamlet
import Text.Digestive
import Text.Digestive.Util
import Text.Digestive.Blaze.Html5
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Note json
  author String
  content String
  deriving Show
|]

type Server = SpockM SqlBackend () () ()
type ServerAction a = SpockAction SqlBackend () () a

note :: IO ()
note = do
    pool <- runStdoutLoggingT $ createSqlitePool "webapi.db" 5
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    spockConfig <- defaultSpockCfg () (PCPool pool) ()
    runSpock 8080 (spock spockConfig app)

app :: Server
app = do
  get "note" $ do
    (notes :: [Entity Note]) <- runSQL $ P.selectList [] [Asc NoteId]
    html $ toStrict $ renderHtml $
        H.html $ do
        H.head $ H.title "Web Haskell"
        H.body $ showNotes notes
  get "add-note" $ do
    v <- getForm "note" noteFormInput
    let view = fmap H.toHtml v
    html $ toStrict $ renderHtml $
      H.html $ do
        H.head $ H.title "Web Haskell"
        H.body $ showInputForm view
  post "input" $ do
    (v,note) <- runForm "note" noteFormInput
    case note of
      Just n -> do
        NoteKey (SqlBackendKey newId) <- runSQL $ P.insert n
        redirect $ mconcat ["/note/", T.pack $ show newId, "/view"]
      Nothing -> do
        let view = fmap H.toHtml v
        html $ toStrict $ renderHtml $
          H.html $ do
            H.head $ H.title "Web Haskell"
            H.body $ showInputForm view
  get ("note" <//> var <//> "view") $ \noteId -> do
    note <- runSQL $ P.get noteId :: ServerAction (Maybe Note)
    case note of
      Just (Note { .. }) -> do
        html $ toStrict $ renderHtml $
          H.html $ do
            H.head $ H.title "Web Haskell"
            H.body $ showNote noteAuthor noteContent
      Nothing -> do setStatus notFound404
                    html $ toStrict $ renderHtml $
                      H.html $ do
                        H.head $ H.title "Web Haskell"
                        H.body $ show404
  get ("note" <//> var <//> "delete") $ \noteId -> do
    note <- runSQL $ P.get noteId :: ServerAction (Maybe Note)
    case note of
      Just (Note { .. }) -> do
        runSQL $ P.delete (noteId :: NoteId)
        redirect "/note"
      Nothing -> do setStatus notFound404
                    html $ toStrict $ renderHtml $
                      H.html $ do
                        H.head $ H.title "Web Haskell"
                        H.body $ show404
  get ("note" <//> var <//> "edit") $ \noteId -> do
    note <- runSQL $ P.get noteId :: ServerAction (Maybe Note)
    case note of
      Just (Note { .. }) -> do
        v <- getForm "note" (noteFormEdit noteAuthor noteContent)
        let view = fmap H.toHtml v
        html $ toStrict $ renderHtml $
          H.html $ do
            H.head $ H.title "Web Haskell"
            H.body $ showEditForm view (convertText (noteId :: NoteId))
      Nothing -> do setStatus notFound404
                    html $ toStrict $ renderHtml $
                      H.html $ do
                        H.head $ H.title "Web Haskell"
                        H.body $ show404
  post ("update" <//> var) $ \noteId -> do
    (v,note) <- runForm "note" noteFormInput
    case note of
      Just n -> do
        runSQL $ P.replace (noteId :: NoteId) n
        redirect $ mconcat ["/note/", (convertText (noteId :: NoteId)), "/view"]
      Nothing -> do
        let view = fmap H.toHtml v
        html $ toStrict $ renderHtml $
          H.html $ do
            H.head $ H.title "Web Haskell"
            H.body $ showEditForm view (convertText (noteId :: NoteId))
                          

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


-- DATA FORM INPUT
noteFormInput :: Monad m => Form String m Note
noteFormInput = Note <$> "author" .: check "Input author must have minimal 5 character" checkInput (string Nothing)
                     <*> "content" .: check "Input content must have minimal 5 character" checkInput (string Nothing)


-- DATA FORM EDIT
noteFormEdit :: Monad m => String -> String -> Form String m Note
noteFormEdit a b = Note <$> "author"  .: check "Input author must have minimal 5 character" checkInput (string (Just a))
                        <*> "content" .: check "Input content must have minimal 5 character" checkInput (string (Just b))


-- FORM VALIDATION MUST HAVE >= 5 CHAR
checkInput :: String -> Bool
checkInput s = PR.length s >= 5


-- CASTING NOTEID TO TEXT
convertText :: NoteId -> Text
convertText a = T.pack $ show (fromSqlKey a)

-- HTML: DISPLAY LIST NOTES
showNotes :: [Entity Note] -> H.Html
showNotes notes = [shamlet|
  <h1>List Notes
  <a href="/add-note">
    <button type="button">Add Note
  <table>
    <tr>
      <th>Note
      <th>From
      <th>Action
    $forall Entity key val <- notes
      <tr>
        <td>#{noteContent val}
        <td>#{noteAuthor val}
        <td>
          <a href="/note/#{fromSqlKey key}/view">
            <button type="button">View
          <a href="/note/#{fromSqlKey key}/edit">
            <button type="button">Edit
          <a href="/note/#{fromSqlKey key}/delete" onclick="return confirm('Are you sure you want to delete this note?');">
            <button type="button">Delete
|]


-- HTML: DISPLAY FORM INPUT NOTES
showInputForm :: View H.Html -> H.Html
showInputForm view = do
  H.h1 "New Note"
  H.div $ do
    H.a H.! A.href "/note" $ do
      H.button "List Notes"
  H.br
  form view "/input" $ do
    H.div $ do
      label     "author"    view "From:"
      inputText "author"    view H.! A.required "required"
    H.br
    H.div $ do
      label     "content"    view "Note:"
      inputTextArea  Nothing Nothing "content" view H.! A.required "required"
    H.br
    childErrorList "" view
    H.br
    inputSubmit "Submit"


-- HTML: DISPLAY FORM INPUT NOTES
showEditForm :: View H.Html -> Text -> H.Html
showEditForm view noteId = do
  H.h1 "Edit Note"
  H.div $ do
    H.a H.! A.href "/note" $ do
      H.button "List Notes"
  H.br
  form view ("/update/" <> noteId) $ do
    H.div $ do
      label     "author"    view "From:"
      inputText "author"    view H.! A.required "required"
    H.br
    H.div $ do
      label     "content"    view "Note:"
      inputTextArea  Nothing Nothing "content" view H.! A.required "required"
    H.br
    childErrorList "" view
    H.br
    H.br
    inputSubmit "Submit"


-- HTML: DISPLAY NOTES
showNote :: String -> String -> H.Html
showNote noteAuthor noteContent = [shamlet|
  <a href="/note">
    <button type="button">List Note
  <h1>Notes:
  <h3>#{noteContent}
  <p id=descr>Note from: #{noteAuthor}
|]


-- HTML: DISPLAY RECORD NOT FOUND
show404 :: H.Html
show404 = do
  H.h1 "Record Not found!!!"
  H.a H.! A.href "/note" $ do
    H.button "List Notes"