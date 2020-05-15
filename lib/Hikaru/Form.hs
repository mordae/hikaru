{-|
Module      :  Hikaru.Form
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides tools for building localized HTML forms
with server-side validation.

There are three important steps involved:

1. A 'Form' needs to be defined first. For that you need to supply the
   underlying data type as well as high-level description of the form
   elements.

2. It needs be fed some request data. You control what data by using
   'newForm', 'getForm' or 'postForm'.

3. It needs to be rendered as HTML. This module will leave you with a
   high-level form 'View', but you need to take care of the rendering
   yourself.

-}

module Hikaru.Form
  (
  -- * Using Forms
  -- |
  -- Running a form generally means turning the request data into an updated
  -- form view and when validation succeeds also the resulting object.
  --
    newForm
  , getForm
  , postForm
  , runForm

  -- * Building Forms
  -- |
  -- Forms are constructed using a simple DSL described lower.
  --
  , Form
  , FormT

  -- ** Elements
  , element
  , hidden

  -- ** Controls
  , ElementT
  , ControlT

  -- *** Input
  , input
  , input'
  , placeholder

  -- *** Select
  , select
  , options
  , optionsFromValues
  , optionsFromEnum

  -- ** Validation
  , validate
  , check
  , note

  -- ** Rendering Hints
  , hint

  -- ** Debugging
  , dumpForm
  , dumpControl

  -- * Rendering Forms
  , View(..)
  , Element(..)
  , Control(..)
  , Note(..)
  , Field(..)
  , FieldTag(..)
  , Option(..)
  , ToOption(..)
  , Selectable(..)

  -- ** Localization
  , FormMessage(..)
  , FromFormMessage(..)
  )
where
  import BasePrelude hiding (Option, Control)

  import Control.Monad.Reader (ReaderT, runReaderT, ask)
  import Control.Monad.State (StateT, runStateT, execStateT, modify, get)
  import Control.Monad.Trans (MonadTrans, lift)
  import Data.Text (Text, strip)
  import Hikaru.Action
  import Hikaru.Localize
  import Hikaru.Types


  data Env o
    = Env
      { envPrefix      :: Text
      , envParams      :: Maybe [(Text, Text)]
      , envFiles       :: Maybe [(Text, FilePath)]
      , envValue       :: Maybe o
      , envValidate    :: Bool
      }
    deriving (Show)


  -- |
  -- Root of the generated form.
  --
  -- Can be used to render the form as HTML.
  -- Roughly corresponds to the @\<form\>@ element.
  --
  data View l
    = View
      { viewElements   :: [Element l]
      , viewControls   :: [Control l]
      }
    deriving (Show)


  -- |
  -- Shortcut for a form that produces a value with the same type as was
  -- the original object. This should be a rule for all the forms.
  --
  -- Example:
  --
  -- @
  -- addItemForm :: ('MonadAction' m) => 'Form' Messages m AddItem
  -- addItemForm = do
  --   AddItem
  --     \<$\> 'element' MsgItemName do
  --           'input'' "name" itemName
  --
  --     \<*\> 'element' MsgItemType do
  --           'select' "type" itemType do
  --             'optionsFromEnum' MsgItemType
  -- @
  --
  type Form l m o = FormT l o m o

  -- |
  -- Applicative functor for form construction.
  --
  newtype FormT l o m a
    = FormT
      { runFormT       :: ReaderT (Env o) (StateT (View l) m) (Maybe a)
      }

  instance (Monad m) => Functor (FormT l o m) where
    fmap f FormT{..} = FormT do
      x <- runFormT
      return $ fmap f x
    {-# INLINE fmap #-}

  instance (Monad m) => Applicative (FormT l o m) where
    pure x = FormT $ pure $ Just x
    {-# INLINE pure #-}

    l <*> r = FormT do
      l' <- runFormT l
      r' <- runFormT r
      return $ l' <*> r'
    {-# INLINE (<*>) #-}


  -- |
  -- A form element that normally corresponds to a single row.
  --
  -- An element can be either visible or hidden and can have multiple
  -- controls. Hidden elements can have only 'input' controls.
  --
  data Element l
    = Element
      { elemLabel      :: l
      , elemControls   :: [Control l]
      }
    deriving (Show)


  -- |
  -- Element customization context.
  --
  newtype ElementT l o m a
    = ElementT
      { runElementT    :: ReaderT (Env o) (StateT (Element l) m) (Maybe a)
      }

  instance (Monad m) => Functor (ElementT l o m) where
    fmap f ElementT{..} = ElementT do
      x <- runElementT
      return $ fmap f x
    {-# INLINE fmap #-}

  instance (Monad m) => Applicative (ElementT l o m) where
    pure x = ElementT $ pure $ Just x
    {-# INLINE pure #-}

    l <*> r = ElementT do
      l' <- runElementT l
      r' <- runElementT r
      return $ l' <*> r'
    {-# INLINE (<*>) #-}


  -- |
  -- Form controls extend fields with a name, notes and rendering hints.
  --
  data Control l
    = Control
      { ctrlName       :: Text
      , ctrlField      :: Field l
      , ctrlNotes      :: [Note l]
      , ctrlHints      :: [Dynamic]
      }
    deriving (Show)


  data ControlState l v m
    = ControlState
      { csName         :: Text
      , csField        :: Field l
      , csHints        :: [Dynamic]
      , csValidators   :: [Maybe v -> m [Note l]]
      , csValue        :: Maybe v
      }

  instance (Show l, Show v) => Show (ControlState l v m) where
    show ControlState{..} =
      mconcat [ "ControlState {"
              , "csName = " <> show csName
              , ", csField = " <> show csField
              , ", csHints = " <> show csHints
              , ", csValidators = " <> show (length csValidators)
              , ", csValue = " <> show csValue
              , "}"
              ]


  -- |
  -- Control customization context.
  --
  -- Note that you can access the underlying 'Monad' using 'lift' here.
  --
  newtype ControlT (t :: FieldTag) l o v m a
    = ControlT
      { runControlT    :: ReaderT (Env o) (StateT (ControlState l v m) m) a
      }
    deriving (MonadIO, Monad, Applicative, Functor)

  instance MonadTrans (ControlT t l o v) where
    lift = ControlT . lift . lift
    {-# INLINE lift #-}

  instance (MonadAction m) => MonadAction (ControlT t l o v m) where
    getActionEnv = ControlT $ lift $ lift $ getActionEnv
    {-# INLINE getActionEnv #-}


  -- |
  -- Short text with associated severity to be presented along the
  -- form control. Used to indicate validation results.
  --
  data Note l
    = Note
      { noteSeverity   :: Severity
      , noteMessage    :: l
      }
    deriving (Show)


  -- |
  -- Form field types.
  --
  -- If you miss checkbox, radio or textarea, remember that you can
  -- signal the way to render the field using a rendering 'hint'.
  --
  data Field l
    = InputField
      { fieldType      :: Text
      , fieldPlacehold :: Maybe l
      , fieldValue     :: Text
      }
    | SelectField
      { fieldOptions   :: [Option l]
      }
    deriving (Show)


  -- |
  -- Tag used to specialize context for the individual control types.
  --
  data FieldTag
    = InputFieldTag
    | SelectFieldTag
    deriving (Show, Eq)


  -- |
  -- An option for 'select' to choose from.
  --
  data Option l
    = Option
      { optionLabel    :: l
      , optionSelected :: Bool
      , optionValue    :: Text
      }
    deriving (Show)


  class ToOption l o where
    toOption :: o -> Option l

  instance ToOption l (Option l) where
    toOption = id
    {-# INLINE toOption #-}


  -- |
  -- A class with two overlapping instances used to smoothly handle
  -- both single-select and multi-select controls.
  --
  class Selectable a where
    selectValues  :: [Text] -> Maybe a
    selectOptions :: a -> [Option l] -> [Option l]

  instance {-# OVERLAPPING #-} (Param a, Eq a) => Selectable a where
    selectValues [p] = fromParam p
    selectValues _ps = Nothing

    selectOptions p = map update
      where
        update opt@Option{..} = opt { optionSelected = match optionValue }
        match x = case fromParam x of
                    Nothing -> False
                    Just x' -> p == x'

  instance {-# OVERLAPPING #-} (Param a, Eq a) => Selectable [a] where
    selectValues = Just . mapMaybe fromParam

    selectOptions ps = map update
      where
        update opt@Option{..} = opt { optionSelected = match optionValue }
        match x = case fromParam x of
                    Nothing -> False
                    Just x' -> x' `elem` ps


  -- |
  -- Default localized messages related to form validation.
  --
  -- You might want to wrap this in your message catalog:
  --
  -- @
  -- data Messages
  --   = MsgForm 'FormMessage'
  --   | ...
  --   deriving (Show)
  --
  -- instance 'Localizable' Messages where
  --   'localize' lang (MsgForm msg) = 'localize' lang msg
  --   ...
  -- @
  --
  data FormMessage
    = FormMsgFieldRequired
    deriving (Show)

  instance Localizable FormMessage where
    -- English strings
    localize "en" FormMsgFieldRequired = Just "This field is required."

    -- No translation, caller should try a different locale.
    localize _lang _msg = Nothing


  -- |
  -- Class used to wrap the 'FormMessage' with your own message type.
  --
  -- Just point to the constructor within your own message catalog:
  --
  -- @
  -- instance 'FromFormMessage' Messages where
  --   'fromFormMessage' = MsgForm
  -- @
  --
  class FromFormMessage l where
    fromFormMessage :: FormMessage -> l


  -- Using Forms -------------------------------------------------------------


  -- |
  -- Build a fresh form without using any request data.
  --
  -- This function is used when the form is initially presented to the user.
  -- It makes no sense to validate it, since the user have not entered any
  -- input data or it has been seeded using a (most probably) valid object.
  --
  -- For example:
  --
  -- @
  -- getEditItemR :: Natural -> Action ()
  -- getEditItemR itemId = do
  --   item <- getItem itemId
  --   view <- 'newForm' "editItem" Nothing editItemForm
  --   'sendHTML' do
  --     form_ [method_ "POST"] do
  --       horizontalForm_ view
  --       horizontalFormButtons_ do
  --        submitButton_ do
  --          'lc_' MsgBtnSubmit
  -- @
  --
  newForm :: (Monad m) => Text -> Maybe o -> Form l m o -> m (View l)
  newForm name orig = flip execStateT view . flip runReaderT env . runFormT
    where
      view = View [] []
      env  = Env { envPrefix   = name
                 , envParams   = Nothing
                 , envFiles    = Nothing
                 , envValue    = orig
                 , envValidate = False
                 }


  -- |
  -- Build the form using query string parameters.
  --
  -- The values are validated and corresponding notes are generated.
  -- In case of success the resulting object is generated as well.
  --
  -- See 'postForm' for an example as they are used the same way.
  --
  getForm :: (MonadAction m) => Text -> Form l m o -> m (Maybe o, View l)
  getForm name FormT{..} = do
    params <- getParams

    let view = View [] []
        env  = Env { envPrefix   = name
                   , envParams   = Just params
                   , envFiles    = Nothing
                   , envValue    = Nothing
                   , envValidate = True
                   }

    runStateT (flip runReaderT env runFormT) view


  -- |
  -- Build the form using files and files from the submitted form.
  --
  -- The values are validated and corresponding notes are generated.
  -- In case of success the resulting object is generated as well.
  --
  -- For example:
  --
  -- @
  -- postEditItemR :: Natural -> Action ()
  -- postEditItemR itemId = do
  --   (result, view) <- 'postForm' "editItem" Nothing editItemForm
  --
  --   case result of
  --     Nothing -> do
  --       'sendHTML' do
  --         form_ [method_ "POST"] do
  --           horizontalForm_ view
  --           horizontalFormButtons_ do
  --            submitButton_ do
  --              'lc_' MsgBtnSubmit
  --
  --     Just edit -> do
  --       editItem edit
  --       'redirect' "/items/"
  -- @
  --
  postForm :: (MonadAction m) => Text -> Form l m o -> m (Maybe o, View l)
  postForm name FormT{..} = do
    fields <- getFields
    files  <- getFiles

    let view = View [] []
        env  = Env { envPrefix   = name
                   , envParams   = Just fields
                   , envFiles    = Just files
                   , envValue    = Nothing
                   , envValidate = True
                   }

    runStateT (flip runReaderT env runFormT) view


  -- |
  -- Build the form using only the supplied values.
  --
  -- The values are validated and corresponding notes are generated.
  -- In case of success the resulting object is generated as well.
  --
  -- This function is included for completeness only.
  -- You should probably use either 'newForm', 'getForm' or 'postForm'.
  --
  runForm :: (Monad m)
          => Text -> [(Text, Text)] -> [(Text, FilePath)] -> Form l m o
          -> m (Maybe o, View l)
  runForm name params files FormT{..} = do
    let view = View [] []
        env  = Env { envPrefix   = name
                   , envParams   = Just params
                   , envFiles    = Just files
                   , envValue    = Nothing
                   , envValidate = True
                   }

    runStateT (flip runReaderT env runFormT) view


  -- Building Forms ----------------------------------------------------------


  -- |
  -- Add a new form element with given label.
  --
  -- Individual controls are specified using the 'ElementT' monad.
  -- Normal elements can contain all control types.
  --
  element :: (Monad m) => l -> ElementT l o m a -> FormT l o m a
  element label body = FormT do
    env <- ask

    (res, new) <- lift $ lift do
      let base = Element label []
       in flip runStateT base $ flip runReaderT env $ runElementT body

    modify \view@View{..} ->
      view { viewElements = viewElements <> [new] }

    return res


  -- |
  -- Add a new hidden form control.
  --
  -- TODO
  --
  hidden :: (Monad m, FromFormMessage l, Param v)
         => Text
         -> (o -> v)
         -> ControlT 'InputFieldTag l o v m a
         -> FormT l o m v
  hidden name getter body = FormT do
    env@Env{..} <- ask
    (val, text) <- getParamOrig name (getter <$> envValue)

    new <- lift $ lift do
      let field = InputField "hidden" Nothing text
          state = ControlState name field [] [] val
       in flip execStateT state $ flip runReaderT env $ runControlT body

    ctrl <- lift $ lift $ buildControl env new

    modify \view@View{..} ->
      view { viewControls = viewControls <> [ctrl] }

    return $ csValue new


  -- |
  -- TODO
  --
  input :: (Monad m, FromFormMessage l, Param v)
        => Text
        -> (o -> v)
        -> ControlT 'InputFieldTag l o v m a
        -> ElementT l o m v
  input name getter body = ElementT do
    env@Env{..} <- ask
    (val, text) <- getParamOrig name (getter <$> envValue)

    new <- lift $ lift do
      let field = InputField "text" Nothing text
          state = ControlState name field [] [] val
       in flip execStateT state $ flip runReaderT env $ runControlT body

    ctrl <- lift $ lift $ buildControl env new

    modify \elt@Element{..} ->
      elt { elemControls = elemControls <> [ctrl] }

    return $ csValue new


  -- |
  -- TODO
  --
  input' :: (Monad m, FromFormMessage l, Param v)
         => Text -> (o -> v) -> ElementT l o m v
  input' name getter = input name getter $ return ()


  -- |
  -- TODO
  --
  placeholder :: (Monad m) => l -> ControlT 'InputFieldTag l o v m ()
  placeholder ph = ControlT do
    modify \s@ControlState{..} ->
      s { csField = csField { fieldPlacehold = Just ph } }


  -- |
  -- TODO
  --
  select :: (Monad m, FromFormMessage l, Param v, Selectable v)
         => Text
         -> (o -> v)
         -> ControlT 'SelectFieldTag l o v m a
         -> ElementT l o m v
  select name getter body = ElementT do
    env@Env{..} <- ask
    val <- getSelectParams name

    new <- lift $ lift do
      let val'  = val <|> (getter <$> envValue)
          field = SelectField []
          state = ControlState name field [] [] val'
       in flip execStateT state $ flip runReaderT env $ runControlT body

    ctrl <- lift $ lift $ buildControl env new

    modify \elt@Element{..} ->
      elt { elemControls = elemControls <> [ctrl] }

    return $ csValue new


  -- |
  -- Configure 'select' options directly.
  --
  -- TODO: Add an example.
  --
  options :: (Monad m, Param v, Selectable v)
          => [Option l] -> ControlT 'SelectFieldTag l o v m ()
  options opts = ControlT do
    modify \s@ControlState{..} ->
      let opts' = case csValue of
                    Nothing  -> opts
                    Just val -> selectOptions val opts
       in s { csField = csField { fieldOptions = opts' } }


  -- |
  -- Configure 'select' options using a list of values.
  --
  -- TODO: Add an example.
  --
  optionsFromValues :: (Monad m, Param v, Selectable v)
                    => (v -> l) -> [v] -> ControlT 'SelectFieldTag l o v m ()
  optionsFromValues label vals = options [ Option (label x) False (toParam x)
                                   | x <- vals
                                   ]


  -- |
  -- Configure 'select' options using enumeration.
  --
  -- TODO: Add an example.
  --
  optionsFromEnum :: (Monad m, Param v, Selectable v, Bounded v, Enum v)
                  => (v -> l) -> ControlT 'SelectFieldTag l o v m ()
  optionsFromEnum label = optionsFromValues label [minBound..maxBound]


  -- |
  -- TODO
  --
  validate :: (Monad m) => (Maybe v -> m [Note l]) -> ControlT t l o v m ()
  validate fn = ControlT do
    modify \s@ControlState{..} ->
      s { csValidators = csValidators <> [fn] }


  -- |
  -- TODO
  --
  check :: (Monad m) => (Maybe v -> [Note l]) -> ControlT t l o v m ()
  check fn = validate (return . fn)


  -- |
  -- TODO
  --
  note :: (Monad m) => Severity -> l -> ControlT t l o v m ()
  note sev msg = validate (\_ -> return [Note sev msg])


  -- |
  -- TODO
  --
  hint :: (Monad m, Typeable h) => h -> ControlT t l o v m ()
  hint x = ControlT do
    modify \s@ControlState{..} ->
      s { csHints = csHints <> [toDyn x] }


  -- Debugging ---------------------------------------------------------------


  dumpForm :: (MonadIO m, Show l, Show o) => FormT l o m ()
  dumpForm = FormT do
    env <- ask

    liftIO do
      putStr "Form Dump:\n  "
      putStrLn (show env)

    return $ Just ()


  dumpControl :: (MonadIO m, Show l, Show o, Show v) => ControlT t l o v m ()
  dumpControl = ControlT do
    state <- get

    liftIO do
      putStr "Control Dump:\n  "
      putStrLn (show state)


  -- Internal ----------------------------------------------------------------


  getParamOrig :: (Monad m, Param v)
               => Text -> Maybe v -> ReaderT (Env o) m (Maybe v, Text)
  getParamOrig name orig = do
    Env{..} <- ask

    case envParams of
      Nothing -> return (orig, maybe "" toParam orig)
      Just ps -> let param = strip <$> lookup (envPrefix <> "." <> name) ps
                  in return (fromParam =<< param, fromMaybe "" param)


  getSelectParams :: (Monad m, Param v, Selectable v)
                  => Text -> ReaderT (Env o) m (Maybe v)
  getSelectParams name = do
    Env{..} <- ask
    let name' = envPrefix <> "." <> name
    return $ selectValues . lookupList name' =<< envParams


  lookupList :: (Eq a) => a -> [(a, b)] -> [b]
  lookupList name = map snd . filter ((name ==) . fst)


  buildControl :: (Monad m, FromFormMessage l)
               => Env o
               -> ControlState l v m
               -> m (Control l)
  buildControl Env{..} ControlState{..} = do
    vres <- case envValidate of
              True  -> sequence $ map ($ csValue) (required : csValidators)
              False -> return []

    return Control { ctrlName       = envPrefix <> "." <> csName
                   , ctrlField      = csField
                   , ctrlNotes      = mconcat vres
                   , ctrlHints      = csHints
                   }


  required :: (Monad m, FromFormMessage l) => Maybe v -> m [Note l]
  required Nothing = return [Note Danger $ fromFormMessage FormMsgFieldRequired]
  required _else   = return []


-- vim:set ft=haskell sw=2 ts=2 et:
