-- |
-- Module      :  Hikaru.Form
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides tools for building localized HTML forms
-- with server-side validation.
--
-- There are three important steps involved:
--
-- 1. A 'FormT' needs to be defined first. Within it you describe
--    individual form controls (such as 'input' or 'select') along
--    with their traits (such as 'required', 'check' or 'choices').
--
-- 2. Your 'FormT' needs be fed some request data in order to produce
--    the desired object and a renderable view. You control data to use
--    by using 'newForm', 'getForm' or 'postForm' or calling the 'runForm'
--    directly.
--
-- 3. The resulting 'Form' needs to be rendered as HTML. For that you
--    might want to use "Hikaru.Widget.Render" class or render the form
--    directly.
--

module Hikaru.Form
  (
  -- * Rendering Forms
    Form(..)
  , Control(..)
  , Note(..)
  , getHintMaybe
  , getSeverity

  -- * Building Forms
  , FormT
  , runFormT
  , evalFormT
  , execFormT
  , newForm
  , getForm
  , postForm

  -- ** Controls
  , input
  , select
  , multiselect
  , textarea
  , button
  , hidden

  -- ** Traits
  , Trait

  -- *** Settings
  , format
  , choices
  , choicesAuto
  , choicesEnum
  , placeholder

  -- *** Validation
  , required
  , check
  , checkM

  -- *** Other Traits
  , addNote
  , addHint
  , adjust
  , adjustM
  )
where
  import Praha

  import Hikaru.Types
  import Hikaru.Action

  import Data.Dynamic
  import Data.List (lookup, filter, map)


  -- Rendering Forms ---------------------------------------------------------


  -- |
  -- Resulting form to be rendered.
  --
  data Form l
    = Form
      { formControls   :: [Control l]
      }


  -- |
  -- An individual form control to be rendered.
  --
  data Control l
    = Control
      { ctrlType        :: Text
        -- ^
        -- Type of the control. Defaults to the name of the function that
        -- created it, e.g. 'input', 'select', 'textarea' and so on.
        --

      , ctrlName        :: Text
        -- ^ Name of the control. Used to process request data.

      , ctrlLabel       :: Maybe l
        -- ^ Label for the control. Most types set it, 'hidden' does not.

      , ctrlPlaceholder :: Maybe l
        -- ^ An optional placeholder text for controls that support it.

      , ctrlChoices     :: [(l, Text)]
        -- ^ Possible choices for controls that support them.

      , ctrlValues      :: Maybe [Text]
        -- ^
        -- Parsed values from the request data. Usually only the first one
        -- is used, but some controls (e.g. 'multiselect') use more.

      , ctrlNotes       :: [Note l]
        -- ^ Notes resulting from validation or added manually.

      , ctrlHints       :: [(Text, Dynamic)]
        -- ^ Arbitrary rendering hints.
      }


  -- |
  -- Short text with associated severity to be presented along the
  -- form control. Used to indicate validation results.
  --
  data Note l
    = Note
      { noteSeverity   :: Severity
      , noteMessage    :: l
      }
    deriving (Show, Generic)

  instance (NFData l) => NFData (Note l)


  -- |
  -- Find and try to interpret specified hint.
  --
  -- Hints are set using the 'addHint' trait.
  --
  getHintMaybe :: (Typeable a) => Text -> Control l -> Maybe a
  getHintMaybe k Control{ctrlHints} = fromDynamic =<< lookup k ctrlHints


  -- |
  -- Calculate highest severity of all control notes.
  -- Useful to determine e.g. how to color the input field.
  --
  getSeverity :: Control l -> Severity
  getSeverity Control{ctrlNotes} = mconcat $ map (.noteSeverity) ctrlNotes


  -- Building Forms ----------------------------------------------------------


  data Env l
    = Env
      { envControls    :: [Control l]
      , envFields      :: Maybe [(Text, Text)]
      , envFiles       :: Maybe [(Text, FilePath)]
      }


  -- |
  -- Monad transformed used to define form controls.
  --
  -- Example:
  --
  -- @
  -- addCaseForm :: ('MonadAction' m) => FormT Messages m (Maybe AddCase)
  -- addCaseForm = do
  --   acName <- 'input' "name" MsgCaseName Nothing
  --               [ 'required' MsgRequired
  --               ]
  --
  --   acActive <- 'select' "active" MsgCaseEnabled Nothing
  --                 [ 'choicesEnum' MsgBool
  --                 , 'required' MsgRequired
  --                 ]
  --
  --   _submit <- 'button' "submit" MsgSubmit []
  --
  --   return $ AddCase <$> acName <*> acActive
  -- @
  --
  newtype FormT l m a
    = FormT
      { unFormT        :: StateT (Env l) m a
      }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

  instance (MonadAction m) => MonadAction (FormT l m)


  -- |
  -- Render form without any request data.
  --
  newForm :: (MonadAction m) => FormT l m a -> m (Form l)
  newForm form = execFormT Nothing form


  -- |
  -- Render form using GET query string parameters.
  --
  getForm :: (MonadAction m) => FormT l m a -> m (a, Form l)
  getForm form = do
    params <- getParams
    runFormT (Just (params, [])) form


  -- |
  -- Render form using POST form data.
  --
  postForm :: (MonadAction m) => FormT l m a -> m (a, Form l)
  postForm form = do
    formData <- getFormData
    runFormT (Just formData) form


  -- |
  -- Run form supplying the request data manually.
  --
  runFormT :: (Monad m) => Maybe FormData -> FormT l m a -> m (a, Form l)
  runFormT fdata FormT{..} = fixup <$> runStateT unFormT env0
    where
      fixup (res, env1) = (res, Form env1.envControls)
      env0 = Env { envControls = []
                 , envFields   = fmap fst fdata
                 , envFiles    = fmap snd fdata
                 }


  evalFormT :: (Monad m) => Maybe FormData -> FormT l m a -> m a
  evalFormT fdata = fmap fst . runFormT fdata


  execFormT :: (Monad m) => Maybe FormData -> FormT l m a -> m (Form l)
  execFormT fdata = fmap snd . runFormT fdata


  -- Controls ----------------------------------------------------------------

  -- |
  -- Add an input field.
  --
  -- The @ctrlType@ field value is @\"text\"@ by default, but can be modified
  -- using the 'format' trait.
  --
  -- Inputs use the first submitted value of matching name.
  --
  input :: (Monad m, Param a)
        => Text                        -- ^ Name of the control
        -> l                           -- ^ Label describing the control
        -> Maybe a                     -- ^ Optional initial value
        -> [Trait l m a]               -- ^ List of traits to apply
        -> FormT l m (Maybe a)         -- ^ Maybe the submitted value
  input name label default_ traits = do
    (texts, value) <- getMaybeTextsValue name
    let ctrl = makeControl "text" name (Just label) texts
    addControl ctrl value default_ traits


  -- |
  -- Add a select field.
  --
  -- The @ctrlType@ field value is @\"select\"@ by default.
  -- It is not advisable to modify it using the 'format' trait.
  --
  -- Selects use the first submitted value of matching name.
  --
  select :: (Monad m, Param a)
         => Text                       -- ^ Name of the control
         -> l                          -- ^ Label describing the control
         -> Maybe a                    -- ^ Optional initial value
         -> [Trait l m a]              -- ^ List of traits to apply
         -> FormT l m (Maybe a)        -- ^ Maybe the submitted value
  select name label default_ traits = do
    (texts, value) <- getMaybeTextsValue name
    let ctrl = makeControl "select" name (Just label) texts
    addControl ctrl value default_ traits


  -- |
  -- Add a multiple select field.
  --
  -- The @ctrlType@ field value is @\"multiselect\"@ by default.
  -- It is not advisable to modify it using the 'format' trait.
  --
  -- Multiselects use all submitted values of matching name and
  -- interpret them as active choices.
  --
  multiselect :: (Monad m, Param a)
              => Text                  -- ^ Name of the control
              -> l                     -- ^ Label describing the control
              -> Maybe [a]             -- ^ Optional initial choices
              -> [Trait l m [a]]       -- ^ List of traits to apply
              -> FormT l m (Maybe [a]) -- ^ Maybe the submitted choices
  multiselect name label default_ traits = do
    (texts, values) <- getMaybeTextsValues name
    let ctrl = makeControl "multiselect" name (Just label) texts
    addControl ctrl values default_ traits


  -- |
  -- Add a text area.
  --
  -- The @ctrlType@ field value is @\"textarea\"@ by default.
  -- It is not advisable to modify it using the 'format' trait.
  --
  -- Text areas use the first submitted value of matching name.
  --
  textarea :: (Monad m, Param a)
           => Text                     -- ^ Name of the control
           -> l                        -- ^ Label describing the control
           -> Maybe a                  -- ^ Optional initial value
           -> [Trait l m a]            -- ^ List of traits to apply
           -> FormT l m (Maybe a)      -- ^ Maybe the submitted value
  textarea name label default_ traits = do
    (texts, value) <- getMaybeTextsValue name
    let ctrl = makeControl "textarea" name (Just label) texts
    addControl ctrl value default_ traits


  -- |
  -- Add a button.
  --
  -- The @ctrlType@ field value is @\"button\"@ by default.
  -- It is not advisable to modify it using the 'format' trait.
  --
  -- Buttons use the first submitted value of matching name.
  --
  button :: (Monad m)
         => Text                       -- ^ Name of the button
         -> l                          -- ^ Label on the button
         -> [Trait l m Bool]           -- ^ List of traits to apply
         -> FormT l m Bool             -- ^ True when pressed
  button name label traits = do
    (texts, value) <- getMaybeTextsValue name
    let ctrl = makeControl "button" name (Just label) texts
    fromMaybe False <$> addControl ctrl value Nothing traits


  -- |
  -- Add a hidden field.
  --
  -- The @ctrlType@ field value is @\"hidden\"@ by default.
  -- It is not advisable to modify it using the 'format' trait.
  --
  -- Hidden fields use the first submitted value of matching name.
  --
  hidden :: (Monad m, Param a)
         => Text                       -- ^ Name of the hidden field
         -> Maybe a                    -- ^ Optional initial value
         -> [Trait l m a]              -- ^ List of traits to apply
         -> FormT l m (Maybe a)        -- ^ Maybe the submitted value
  hidden name default_ traits = do
    (texts, value) <- getMaybeTextsValue name
    let ctrl = makeControl "hidden" name Nothing texts
    addControl ctrl value default_ traits


  makeControl :: Text -> Text -> Maybe l -> Maybe [Text] -> Control l
  makeControl type_ name label values =
    Control { ctrlType = type_
            , ctrlName = name
            , ctrlLabel = label
            , ctrlPlaceholder = Nothing
            , ctrlChoices = []
            , ctrlValues = values
            , ctrlNotes = []
            , ctrlHints = []
            }


  addControl :: (Monad m)
             => Control l -> Maybe a -> Maybe a -> [Trait l m a]
             -> FormT l m (Maybe a)
  addControl ctrl0 value0 default_ traits = do
    (value1, ctrl1) <- lift do
      foldr (>=>) return traits (value0 <|> default_, ctrl0)

    FormT do
      modify \env -> env { envControls = env.envControls <> [ctrl1] }
      return value1


  getMaybeTextsValue :: (Monad m, Param a)
                    => Text -> FormT l m (Maybe [Text], Maybe a)
  getMaybeTextsValue name = do
    maybeFields <- FormT $ gets (.envFields)

    case maybeFields of
      Nothing     -> return (Nothing, Nothing)
      Just fields -> do
        let tvalues = lookupMany name fields
        return (Just tvalues, fromParam =<< listToMaybe tvalues)


  getMaybeTextsValues :: (Monad m, Param a)
                      => Text -> FormT l m (Maybe [Text], Maybe [a])
  getMaybeTextsValues name = do
    maybeFields <- FormT $ gets (.envFields)

    case maybeFields of
      Nothing     -> return (Nothing, Nothing)
      Just fields -> do
        let tvalues = lookupMany name fields
        return (Just tvalues, mapM fromParam tvalues)


  lookupMany :: (Eq a) => a -> [(a, b)] -> [b]
  lookupMany k = map snd . filter ((k ==) . fst)


  -- Traits ------------------------------------------------------------------


  -- |
  -- Traits are used to describe form controls more precisely.
  --
  -- They are mostly optional, but e.g. 'select' cannot operate properly
  -- without a trait describing the possible choices.
  --
  type Trait l m a = (Maybe a, Control l) -> m (Maybe a, Control l)


  -- |
  -- Change @ctrlType@ field.
  --
  -- Usually used to change 'input' format to @\"email\"@ or @\"number\"@.
  -- Check out [MDN: Form Input element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#input_types)
  -- for more information.
  --
  format :: (Monad m) => Text -> Trait l m a
  format type_ (value, ctrl0) = return (value, ctrl1)
    where
      ctrl1 = ctrl0 { ctrlType = type_ }


  -- |
  -- Change @ctrlPlaceholder@ field.
  --
  -- Used to insert a hint of what might come into the field.
  -- Usually something fictional, like a @\"42 Flower St.\" on a street field
  -- within an address form. Helps users understand the form better.
  --
  placeholder :: (Monad m) => l -> Trait l m a
  placeholder label (value, ctrl0) = return (value, ctrl1)
    where
      ctrl1 = ctrl0 { ctrlPlaceholder = Just label }


  -- |
  -- Checks that the field has been submitted.
  --
  -- Adds a @Danger@ severity note to the control if not.
  --
  required :: (Monad m) => l -> Trait l m a
  required msg (value, ctrl) = do
    if getSeverity ctrl >= Danger
       then return (value, ctrl)
       else do
         case ctrl.ctrlValues of
           Just [] -> return (value, addNote' Danger msg ctrl)
           _else   -> return (value, ctrl)


  -- |
  -- Checks that the field (if submitted) satisfies given predicate.
  --
  -- Adds a @Danger@ severity note to the control if not.
  --
  check :: (Monad m) => l -> (a -> Bool) -> Trait l m a
  check msg test = checkM msg (return . test)


  -- |
  -- Similar to 'check', but the predicate can have side effects.
  --
  -- Useful to e.g. consult the database about possible conflicts
  -- with an existing value or point out that a referenced object
  -- does no longer exist.
  --
  checkM :: (Monad m) => l -> (a -> m Bool) -> Trait l m a
  checkM msg test (value, ctrl) = do
    if getSeverity ctrl >= Danger
       then return (value, ctrl)
       else do
         case ctrl.ctrlValues of
           Nothing -> return (value, ctrl)
           Just __ -> do
             result <- case value of
                         Nothing  -> return True
                         Just val -> test val

             if result
                then return (value, ctrl)
                else return (value, addNote' Danger msg ctrl)


  -- |
  -- Set @ctrlChoices@ field.
  --
  -- Every choice takes the form of @(label, value)@.
  --
  -- Used to set possible choices manually. It is much easier to use
  -- 'choicesEnum' when the choices correspond to an @Enum@.
  --
  choices :: (Monad m, Param a) => [(l, a)] -> Trait l m a
  choices opts (value, ctrl0) = return (value, ctrl1)
    where
      ctrl1 = ctrl0 { ctrlChoices = map (fmap toParam) opts }


  -- |
  -- Set @ctrlChoices@ field.
  --
  -- Similar to 'choices', but the label for each choice is derived from
  -- the corresponding value using the supplied function.
  --
  choicesAuto :: (Monad m, Param a) => (a -> l) -> [a] -> Trait l m a
  choicesAuto label opts = choices [(label x, x) | x <- opts]


  -- |
  -- Set @ctrlChoices@ field.
  --
  -- Similar to 'choicesAuto', but generates the list of values using
  -- @[minBound..maxBound]@ of the 'Bounded' 'Enum'.
  --
  choicesEnum :: (Monad m, Param a, Enum a, Bounded a)
              => (a -> l) -> Trait l m a
  choicesEnum label = choicesAuto label [minBound..maxBound]


  -- |
  -- Add a 'Note' to the @ctrlNotes@ field.
  --
  addNote :: (Monad m) => Severity -> l -> Trait l m a
  addNote sev msg (value, ctrl) = return (value, addNote' sev msg ctrl)


  addNote' :: Severity -> l -> Control l -> Control l
  addNote' sev msg ctrl = ctrl { ctrlNotes = ctrl.ctrlNotes <> [Note sev msg] }


  -- |
  -- Add a hint to the @ctrlHints@ field.
  --
  -- Useful to communicate optional formatting hints to the code
  -- responsible for rendering the form control.
  --
  -- Hints can be retrieved using the 'getHintMaybe' function.
  --
  addHint :: (Monad m, Typeable a) => Text -> a -> Trait l m a
  addHint k v (value, ctrl0) = return (value, ctrl1)
    where
      ctrl1 = ctrl0 { ctrlHints = ctrl0.ctrlHints <> [k .= toDyn v] }


  -- |
  -- Adjust the (interpreted) submitted value.
  --
  -- Does not affect the actual submitted string.
  --
  adjust :: (Monad m) => (a -> a) -> Trait l m a
  adjust fn (value, ctrl) = return (fmap fn value, ctrl)


  -- |
  -- Similar to 'adjust', but the adjustment can have side effects.
  --
  adjustM :: (Monad m) => (a -> m a) -> Trait l m a
  adjustM fn (value0, ctrl) = do
    value1 <- traverse fn value0
    return (value1, ctrl)


-- vim:set ft=haskell sw=2 ts=2 et:
