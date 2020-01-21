{-|
Module      :  Hikaru.Form
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides applicative form handling.
-}

module Hikaru.Form
  ( View(..)
  , FormNote(..)
  , Form
  , newForm
  , getForm
  , postForm
  , inputField
  , inputField'
  , hiddenField
  , hiddenField'
  , textArea
  , textArea'
  , selectField
  , selectField'
  , selectFieldEnum
  , selectFieldEnum'
  , multiSelectField
  , multiSelectField'
  , opt
  , req
  , addNote
  , addAttribute
  , fieldCheck
  , fieldValue
  , hasErrors
  )
where
  import BasePrelude

  import Control.Monad.Reader
  import Control.Monad.State
  import Data.Text (Text)
  import Hikaru.Action
  import Hikaru.Localize
  import Hikaru.Types
  import Lucid


  -- Form Types --------------------------------------------------------------


  data View l
    = FormFields
      { viewFields     :: [View l]
      , viewNotes      :: [FormNote l]
      }
    | HiddenField
      { viewName       :: Text
      , viewValue      :: Maybe Text
      , viewNotes      :: [FormNote l]
      , viewAttrs      :: [(Text, Dynamic)]
      }
    | InputField
      { viewName       :: Text
      , viewLabel      :: l
      , viewValue      :: Maybe Text
      , viewNotes      :: [FormNote l]
      , viewAttrs      :: [(Text, Dynamic)]
      }
    | TextArea
      { viewName       :: Text
      , viewLabel      :: l
      , viewValue      :: Maybe Text
      , viewNotes      :: [FormNote l]
      , viewAttrs      :: [(Text, Dynamic)]
      }
    | SelectField
      { viewName       :: Text
      , viewLabel      :: l
      , viewOptions    :: [(Text, l, Bool)]
      , viewNotes      :: [FormNote l]
      , viewAttrs      :: [(Text, Dynamic)]
      , viewMulti      :: Bool
      }

  instance Semigroup (View l) where
    FormFields [] [] <> view = view
    view <> FormFields [] [] = view

    FormFields fs1 ns1 <> FormFields fs2 ns2
      = FormFields (fs1 <> fs2) (ns1 <> ns2)

    FormFields fs ns <> view
      = FormFields (fs <> [view]) ns

    view <> FormFields fs ns
      = FormFields ([view] <> fs) ns

    v1 <> v2 = FormFields ([v1, v2]) []

  instance Monoid (View l) where
    mempty = FormFields [] []


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

  instance (ToHtml l) => ToHtml (FormNote l) where
    toHtml    = toHtml . noteLabel
    toHtmlRaw = toHtmlRaw . noteLabel

  instance (Localized l) => Localized (FormNote l) where
    localize lang = localize lang . noteLabel


  newtype FormT l m a
    = FormT
      { unFormT        :: ReaderT (Env l) (StateT (View l) m) a
      }
    deriving (Monad, Applicative, Functor)

  deriving instance (Monad m) => MonadReader (Env l) (FormT l m)
  deriving instance (Monad m) => MonadState (View l) (FormT l m)

  instance MonadTrans (FormT l) where
    lift = FormT . lift . lift


  newtype Form l m a
    = Form
      { unForm         :: FormT l m (Maybe a)
      }

  instance (Monad m) => Functor (Form l m) where
    fmap f Form{..} = Form do
      x <- unForm
      return $ fmap f x

  instance (Monad m) => Applicative (Form l m) where
    pure x = Form $ return $ Just x

    Form{unForm = unFormL} <*> Form{unForm = unFormR} = Form do
      l <- unFormL
      r <- unFormR
      return $ l <*> r

  instance MonadTrans (Form l) where
    lift = Form . fmap Just . lift


  data Env l
    = Env
      { envPrefix      :: [Text]
      , envParams      :: [(Text, Text)]
      , envFiles       :: [(Text, FileInfo FilePath)]
      , envCheck       :: Bool
      }


  newtype FieldT l a m b
    = FieldT
      { unFieldT       :: ReaderT (Bool, Maybe a) (StateT (View l) m) b
      }
    deriving (Monad, Applicative, Functor)

  instance MonadTrans (FieldT l a) where
    lift = FieldT . lift . lift


  -- |
  -- Build a fresh form without using any request data.
  --
  newForm :: (MonadAction m) => Text -> Form l m a -> m (View l)
  newForm name = flip execStateT view . flip runReaderT env . unFormT . unForm
    where
      view = FormFields [] []
      env  = Env { envPrefix = [name]
                 , envParams = []
                 , envFiles  = []
                 , envCheck  = False
                 }


  -- |
  -- Process the form using parameters in the query string.
  --
  getForm :: (MonadAction m) => Text -> Form l m a -> m (View l, Maybe a)
  getForm name form = do
    params <- getParams

    let view = FormFields [] []
        env  = Env { envPrefix = [name]
                   , envParams = filter (("" /=) . snd) params
                   , envFiles  = []
                   , envCheck  = True
                   }

    (value, view') <- runStateT (runReaderT (unFormT $ unForm form) env) view

    if hasErrors view'
       then return (view', Nothing)
       else return (view', value)


  -- |
  -- Process the form using parameters in the request body.
  --
  postForm :: (MonadAction m) => Text -> Form l m a -> m (View l, Maybe a)
  postForm name form = do
    fields <- getFields
    files  <- getFiles

    let view = FormFields [] []
        env  = Env { envPrefix = [name]
                   , envParams = filter (("" /=) . snd) fields
                   , envFiles  = files
                   , envCheck  = True
                   }

    (value, view') <- runStateT (runReaderT (unFormT $ unForm form) env) view

    if hasErrors view'
       then return (view', Nothing)
       else return (view', value)


  -- |
  -- TODO
  --
  hiddenField' :: (Monad m, ToParam a, FromParam a)
               => Text -> FieldT l a m b -> Form l m a
  hiddenField' name field = hiddenField name Nothing field


  -- |
  -- TODO
  --
  hiddenField :: (Monad m, ToParam a, FromParam a)
              => Text -> Maybe a -> FieldT l a m b -> Form l m a
  hiddenField name orig field = Form do
    name' <- makeName name
    value <- formParamMaybe name'

    let value' = value <|> orig
        view   = HiddenField { viewName  = name'
                             , viewValue = toParam <$> value'
                             , viewNotes = []
                             , viewAttrs = []
                             }

    view' <- runFieldT field value' view

    modify (<> view')
    return value'


  -- |
  -- TODO
  --
  inputField' :: (Monad m, ToParam a, FromParam a)
              => Text -> l -> FieldT l a m b -> Form l m a
  inputField' name label field = inputField name label Nothing field


  -- |
  -- TODO
  --
  inputField :: (Monad m, ToParam a, FromParam a)
             => Text -> l -> Maybe a -> FieldT l a m b -> Form l m a
  inputField name label orig field = Form do
    name' <- makeName name
    value <- formParamMaybe name'

    let value' = value <|> orig
        view   = InputField { viewName  = name'
                            , viewLabel = label
                            , viewValue = toParam <$> value'
                            , viewNotes = []
                            , viewAttrs = []
                            }

    view' <- runFieldT field value' view

    modify (<> view')
    return value'


  -- |
  -- TODO
  --
  textArea' :: (Monad m, ToParam a, FromParam a)
            => Text -> l -> FieldT l a m b -> Form l m a
  textArea' name label field = textArea name label Nothing field


  -- |
  -- TODO
  --
  textArea :: (Monad m, ToParam a, FromParam a)
            => Text -> l -> Maybe a -> FieldT l a m b -> Form l m a
  textArea name label orig field = Form do
    name' <- makeName name
    value <- formParamMaybe name'

    let value' = value <|> orig
        view   = TextArea { viewName  = name'
                          , viewLabel = label
                          , viewValue = toParam <$> value'
                          , viewNotes = []
                          , viewAttrs = []
                          }

    view' <- runFieldT field value' view

    modify (<> view')
    return value'


  -- |
  -- TODO
  --
  selectField' :: (Monad m, ToParam a, FromParam a, Eq a)
               => Text -> l -> (a -> l) -> [a]
               -> FieldT l a m b -> Form l m a
  selectField' name label optlabel options field
    = selectField name label optlabel options Nothing field


  -- |
  -- TODO
  --
  selectField :: (Monad m, ToParam a, FromParam a, Eq a)
              => Text -> l -> (a -> l) -> [a] -> Maybe a
              -> FieldT l a m b -> Form l m a
  selectField name label optlabel options orig field = Form do
    name' <- makeName name
    value <- formParamMaybe name'

    let value' = value <|> orig
        opts   = [ (toParam x, optlabel x, Just x == value) | x <- options ]
        view   = SelectField { viewName    = name'
                             , viewLabel   = label
                             , viewOptions = opts
                             , viewNotes   = []
                             , viewAttrs   = []
                             , viewMulti   = False
                             }

    view' <- runFieldT field value' view

    modify (<> view')
    return value'


  -- |
  -- TODO
  --
  selectFieldEnum' :: (Monad m, ToParam a, FromParam a, Eq a, Bounded a, Enum a)
                   => Text -> l -> (a -> l) -> FieldT l a m b -> Form l m a
  selectFieldEnum' name label optlabel field
    = selectFieldEnum name label optlabel Nothing field


  -- |
  -- Alternative to the 'selectField' for enumerable, bounded value types.
  --
  -- TODO: Example
  --
  selectFieldEnum :: (Monad m, ToParam a, FromParam a, Eq a, Bounded a, Enum a)
                  => Text -> l -> (a -> l) -> Maybe a
                  -> FieldT l a m b -> Form l m a
  selectFieldEnum name label optlabel orig field
    = selectField name label optlabel [minBound..maxBound] orig field


  -- |
  -- TODO
  --
  multiSelectField' :: (Monad m, ToParam a, FromParam a, Eq a)
                    => Text -> l -> (a -> l) -> [a]
                    -> FieldT l [a] m b -> Form l m [a]
  multiSelectField' name label optlabel options field
    = multiSelectField name label optlabel options Nothing field


  -- |
  -- TODO
  --
  multiSelectField :: (Monad m, ToParam a, FromParam a, Eq a)
                   => Text -> l -> (a -> l) -> [a] -> Maybe [a]
                   -> FieldT l [a] m b -> Form l m [a]
  multiSelectField name label optlabel options orig field = Form do
    name'  <- makeName name
    params <- formParams name'

    let found  = nub $ params <> fromMaybe [] orig
        opts   = [ (toParam x, optlabel x, x `elem` found) | x <- options ]
        view   = SelectField { viewName    = name'
                             , viewLabel   = label
                             , viewOptions = opts
                             , viewNotes   = []
                             , viewAttrs   = []
                             , viewMulti   = False
                             }

    view' <- runFieldT field (Just found) view

    modify (<> view')
    return $ Just found


  -- |
  -- TODO
  --
  opt :: (Monad m) => FieldT l a m ()
  opt = return ()


  -- |
  -- TODO
  --
  req :: (Monad m) => l -> FieldT l a m ()
  req label = do
    shouldCheck <- fieldCheck

    if shouldCheck
       then do
         value <- fieldValue
         case value of
           Nothing -> addNote $ NoteError label
           Just _v -> return ()

        else do
          return ()


  -- |
  -- TODO
  --
  addNote :: (Monad m) => FormNote l -> FieldT l a m ()
  addNote note = FieldT do
    modify \view -> view { viewNotes = viewNotes view <> [note] }


  -- |
  -- TODO
  --
  addAttribute :: (Monad m, Typeable v) => Text -> v -> FieldT l a m ()
  addAttribute name value = FieldT do
    modify \view ->
      view { viewAttrs = viewAttrs view <> [(name, toDyn value)] }


  -- |
  -- TODO
  --
  fieldCheck :: (Monad m) => FieldT l a m Bool
  fieldCheck = FieldT (fst <$> ask)


  -- |
  -- TODO
  --
  fieldValue :: (Monad m) => FieldT l a m (Maybe a)
  fieldValue = FieldT (snd <$> ask)


  -- |
  -- Determine whether the view has any (possibly nested) errors.
  --
  hasErrors :: View l -> Bool
  hasErrors FormFields{..} = any isErrorNote viewNotes || any hasErrors viewFields
  hasErrors view           = any isErrorNote (viewNotes view)


  -- Form Internals ---------------------------------------------------------


  runFieldT :: (Monad m) => FieldT l a m b -> Maybe a -> View l
             -> FormT l m (View l)
  runFieldT field value view = do
    Env{envCheck} <- ask
    lift $ execStateT (runReaderT (unFieldT field) (envCheck, value)) view


  isErrorNote :: FormNote l -> Bool
  isErrorNote NoteError{} = True
  isErrorNote _           = False


  formParamMaybe :: (Monad m, FromParam a) => Text -> FormT l m (Maybe a)
  formParamMaybe name = do
    Env{envParams} <- ask
    return $ fromParam =<< lookup name envParams


  formFileMaybe :: (Monad m) => Text -> FormT l m (Maybe (FileInfo FilePath))
  formFileMaybe name = do
    Env{envFiles} <- ask
    return $ lookup name envFiles


  formParams :: (Monad m, FromParam a) => Text -> FormT l m [a]
  formParams name = do
    Env{envParams} <- ask
    let match = (name ==) . fst
        conv  = fromParam . snd
     in return $ mapMaybe conv $ filter match $ envParams


  makeName :: (Monad m) => Text -> FormT l m Text
  makeName name = do
    Env{envPrefix} <- ask
    return $ mconcat $ intersperse "." $ reverse (name : envPrefix)


-- vim:set ft=haskell sw=2 ts=2 et:
