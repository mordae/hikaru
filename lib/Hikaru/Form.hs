{-|
Module      :  Hikaru.Form
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)
-}

module Hikaru.Form
  ( FormView(..)
  , FormElement(..)
  , FormNote(..)
  , FormT
  , newForm
  , getForm
  , postForm
  , button
  , inputField
  , hiddenField
  , textField
  , selectField
  , multiSelectField
  )
where
  import BasePrelude

  import Control.Monad.Reader
  import Control.Monad.State
  import Data.Text (Text)
  import Hikaru.Action
  import Hikaru.Types


  data FormView l
    = FormView
      { formElements   :: [FormElement l]
      , formNotes      :: [FormNote l]
      }


  data FormElement l
    = HiddenField
      { elemName       :: Text
      , elemValue      :: Maybe Text
      }
    | Button
      { elemName       :: Text
      , elemLabel      :: l
      , elemAttrs      :: [(Text, Dynamic)]
      }
    | InputField
      { elemName       :: Text
      , elemLabel      :: l
      , elemValue      :: Maybe Text
      , elemNotes      :: [FormNote l]
      , elemAttrs      :: [(Text, Dynamic)]
      }
    | TextField
      { elemName       :: Text
      , elemLabel      :: l
      , elemValue      :: Maybe Text
      , elemNotes      :: [FormNote l]
      , elemAttrs      :: [(Text, Dynamic)]
      }
    | SelectField
      { elemName       :: Text
      , elemLabel      :: l
      , elemOptions    :: [(Text, l, Bool)]
      , elemNotes      :: [FormNote l]
      , elemAttrs      :: [(Text, Dynamic)]
      , elemMulti      :: Bool
      }


  data FormNote l
    = NoteError
      { noteLabel      :: l
      }
    | NoteNeutral
      { noteLabel      :: l
      }
    | NoteSuccess
      { noteLabel      :: l
      }
    deriving (Eq, Ord)


  newtype FormT l m a
    = FormT
      { unFormT        :: ReaderT Env (StateT (FormView l) m) a
      }
    deriving (Functor, Applicative, Monad)


  data Env
    = Env
      { envPrefix      :: [Text]
      , envParams      :: [(Text, Text)]
      , envFiles       :: [(Text, FileInfo FilePath)]
      , envRunChecks   :: Bool
      }

  emptyEnv :: Env
  emptyEnv = Env { envPrefix    = []
                 , envParams    = []
                 , envFiles     = []
                 , envRunChecks = False
                 }


  -- |
  -- Build an unchecked form.
  --
  newForm :: (MonadAction m) => Text -> FormT l m a -> m (FormView l)
  newForm name form = do
    (_, view) <- runForm form emptyEnv { envPrefix = [name] }
    return view


  -- |
  -- Build & process a checked form with parameters in the query string.
  --
  getForm :: (MonadAction m) => Text -> FormT l m a -> m (a, FormView l)
  getForm name form = do
    params <- getParams

    runForm form emptyEnv { envPrefix    = [name]
                          , envParams    = params
                          , envRunChecks = True
                          }


  -- |
  -- Build & process a checked form with parameters in the form fields.
  --
  postForm :: (MonadAction m) => Text -> FormT l m a -> m (a, FormView l)
  postForm name form = do
    fields <- getFields
    files  <- getFiles

    runForm form emptyEnv { envPrefix    = [name]
                          , envParams    = fields
                          , envFiles     = files
                          , envRunChecks = True
                          }


  -- |
  -- Unwrap the transformer stack and run the form.
  --
  runForm :: (MonadAction m) => FormT l m a -> Env -> m (a, FormView l)
  runForm form env = runStateT (runReaderT (unFormT form) env)
                               FormView { formElements = []
                                        , formNotes    = []
                                        }



  button :: (Monad m) => Text -> l -> FormT l m Bool
  button name label = do
    fullName <- makeName name

    appendElement
      Button { elemName  = fullName
             , elemLabel = label
             , elemAttrs = []
             }

    value <- formParamMaybe name
    case value of
      Nothing -> return False
      Just () -> return True


  inputField :: (Monad m, ToParam a, FromParam a)
             => Text -> l -> Maybe a -> FormT l m (Maybe a)
  inputField name label orig = do
    fullName  <- makeName name
    textValue <- formParamMaybe fullName

    let textOrig = toParam <$> orig
     in appendElement
          InputField { elemName  = fullName
                     , elemLabel = label
                     , elemValue = textValue <|> textOrig
                     , elemAttrs = []
                     , elemNotes = []
                     }

    formParamMaybe fullName


  hiddenField :: (Monad m, ToParam a, FromParam a)
              => Text -> Maybe a -> FormT l m (Maybe a)
  hiddenField name orig = do
    fullName  <- makeName name
    textValue <- formParamMaybe fullName

    let textOrig = toParam <$> orig
     in appendElement
          HiddenField { elemName  = fullName
                      , elemValue = textValue <|> textOrig
                      }

    formParamMaybe fullName


  textField :: (Monad m, ToParam a, FromParam a)
            => Text -> l -> Maybe a -> FormT l m (Maybe a)
  textField name label orig = do
    fullName  <- makeName name
    textValue <- formParamMaybe fullName

    let textOrig = toParam <$> orig
     in appendElement
          TextField { elemName  = fullName
                    , elemLabel = label
                    , elemValue = textValue <|> textOrig
                    , elemAttrs = []
                    , elemNotes = []
                    }

    formParamMaybe fullName


  selectField :: (Monad m, ToParam a, FromParam a, Eq a)
              => Text -> l -> (a -> l) -> [a] -> Maybe a -> FormT l m (Maybe a)
  selectField name label optlabel options orig = do
    fullName  <- makeName name
    value     <- formParamMaybe fullName

    let textOptions = [ (toParam o, optlabel o, isSel o) | o <- options ]
        isSel o     = Just o == (value <|> orig)
     in appendElement
          SelectField { elemName    = fullName
                      , elemLabel   = label
                      , elemOptions = textOptions
                      , elemAttrs   = []
                      , elemNotes   = []
                      , elemMulti   = False
                      }

    case value of
      Just v -> if v `elem` options
                   then return value
                   else return Nothing
      Nothing -> return Nothing


  multiSelectField :: (Monad m, ToParam a, FromParam a, Eq a)
                   => Text -> l -> (a -> l) -> [a] -> [a] -> FormT l m [a]
  multiSelectField name label optlabel options orig = do
    fullName     <- makeName name
    values       <- formParamList fullName

    let textOptions = [ (toParam o, optlabel o, isSel o) | o <- options ]
        isSel o     = o `elem` (values <|> orig)
     in appendElement
          SelectField { elemName     = fullName
                      , elemLabel    = label
                      , elemOptions  = textOptions
                      , elemAttrs    = []
                      , elemNotes    = []
                      , elemMulti    = True
                      }

    return $ [ v | v <- values, v `elem` options ]


  -- Form Internals ---------------------------------------------------------


  appendElement :: (Monad m) => FormElement l -> FormT l m ()
  appendElement element = FormT do
    view@FormView{formElements} <- get
    put $ view { formElements = formElements <> [element] }


  formParamMaybe :: (Monad m, FromParam a) => Text -> FormT l m (Maybe a)
  formParamMaybe name = FormT do
    Env{envParams} <- ask
    return $ fromParam =<< lookup name envParams


  formParamList :: (Monad m, FromParam a) => Text -> FormT l m [a]
  formParamList name = FormT do
    Env{envParams} <- ask
    return $ mapMaybe (fromParam . snd) $ filter ((name ==) . fst) $ envParams


  formFileMaybe :: (Monad m) => Text -> FormT l m (Maybe (FileInfo FilePath))
  formFileMaybe name = FormT do
    Env{envFiles} <- ask
    return $ lookup name envFiles


  makeName :: (Monad m) => Text -> FormT l m Text
  makeName name = FormT do
    Env{envPrefix} <- ask
    return $ mconcat $ intersperse "." $ reverse (name : envPrefix)


-- vim:set ft=haskell sw=2 ts=2 et:
