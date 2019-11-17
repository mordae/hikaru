{-|
Module      :  Hikaru.Form
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)
-}

module Hikaru.Form
  ( FormData
  , FormView(..)
  , FormElement(..)
  , FormNote(..)
  , FormT
  , newForm
  , getForm
  , postForm
  , button
  , inputField
  )
where
  import BasePrelude hiding (length)

  import Data.Text (Text)
  import Hikaru.Action
  import Hikaru.Types
  import Control.Monad.State
  import Control.Monad.Reader


  type FormData = ([(Text, Text)], [(Text, FileInfo FilePath)])


  data FormView l
    = FormView
      { formElements   :: [FormElement l]
      , formNotes      :: [FormNote l]
      }

  emptyFormView :: FormView l
  emptyFormView = FormView { formElements = []
                           , formNotes    = []
                           }


  data FormElement l
    = Button
      { elemName       :: Text
      , elemLabel      :: l
      }
    | InputField
      { elemName       :: Text
      , elemLabel      :: l
      , elemValue      :: Maybe Text
      , elemNotes      :: [FormNote l]
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
    let env = emptyEnv { envPrefix = [name] }
    (_, view) <- runForm env form
    return view


  -- |
  -- Build & process a checked form with parameters in the query string.
  --
  getForm :: (MonadAction m) => Text -> FormT l m a -> m (a, FormView l)
  getForm name form = do
    params <- getParams

    let env = emptyEnv { envPrefix    = [name]
                       , envParams    = params
                       , envRunChecks = True
                       }

    runForm env form


  -- |
  -- Build & process a checked form with parameters in the form fields.
  --
  postForm :: (MonadAction m) => Text -> FormT l m a -> m (a, FormView l)
  postForm name form = do
    fields <- getFields
    files  <- getFiles

    let env = emptyEnv { envPrefix    = [name]
                       , envParams    = fields
                       , envFiles     = files
                       , envRunChecks = True
                       }

    runForm env form


  -- |
  -- Unwrap the transformer stack and run the form.
  --
  runForm :: (MonadAction m) => Env -> FormT l m a -> m (a, FormView l)
  runForm env form = runStateT (runReaderT (unFormT form) env) emptyFormView


  button :: (Monad m) => Text -> l -> FormT l m Bool
  button name label = do
    value    <- formParamMaybe name
    fullName <- makeName name

    appendElement $ Button { elemName  = fullName
                           , elemLabel = label
                           }

    case value of
      Nothing -> return False
      Just () -> return True


  inputField :: (Monad m, ToParam a, FromParam a)
             => Text -> l -> Maybe a -> FormT l m (Maybe a)
  inputField name label value = do
    fullName <- makeName name
    value'   <- formParamMaybe fullName

    appendElement $ InputField { elemName  = fullName
                               , elemLabel = label
                               , elemValue = value' <|> fmap toParam value
                               , elemNotes = []
                               }

    formParamMaybe fullName


  -- Form Internals ---------------------------------------------------------


  appendElement :: (Monad m) => FormElement l -> FormT l m ()
  appendElement element = FormT do
    view@FormView{formElements} <- get
    put $ view { formElements = formElements <> [element] }


  formParamMaybe :: (Monad m, FromParam a) => Text -> FormT l m (Maybe a)
  formParamMaybe name = FormT do
    Env{envParams} <- ask
    return $ fromParam =<< lookup name envParams


  formFileMaybe :: (Monad m) => Text -> FormT l m (Maybe (FileInfo FilePath))
  formFileMaybe name = FormT do
    Env{envFiles} <- ask
    return $ lookup name envFiles


  makeName :: (Monad m) => Text -> FormT l m Text
  makeName name = FormT do
    Env{envPrefix} <- ask
    return $ mconcat $ intersperse "." $ reverse (name : envPrefix)


-- vim:set ft=haskell sw=2 ts=2 et:
