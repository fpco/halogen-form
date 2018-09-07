-- | Form API.

module Halogen.Form
  (FormBuilder
  ,Query
  -- * Inputs
  ,text
  ,number
  ,submit
  -- * Combinators
  ,required
  ,wrap
  ,parse
  ,(<|*>)
  ,applyFields
  -- * Halogen component
  ,component)
  where

import Control.Alternative ((<|>))
import Control.Monad.State (class MonadState, State, evalState, gets, modify)
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (fromString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Prelude (class Applicative, class Apply, class Bind, class Functor, type (~>), Unit, bind, const, discard, mempty, pure, show, unit, (+), (<$>), (<*>), (<>), (==), (>>=))
import Prim.Row (class Nub, class Union)
import Record (disjointUnion)

--------------------------------------------------------------------------------
-- Component

-- | Halogen component for materializing a form.
component ::
     forall error value m.
     H.Component
       HH.HTML
       Query
       (FormBuilder error (Array (H.ComponentHTML Query)) value)
       (Either (Array error) value)
       m
component = H.component {initialState, render, eval, receiver: const Nothing}
  where
    initialState (FormBuilder builder) =
      FormState
        { form: evalState builder (BuilderState {counter: 0})
        , inputs: mempty
        , submitted: NotSubmitted
        }
    render (FormState {form: Form {html}, inputs, submitted}) =
      HH.div_ (html submitted inputs)
    eval ::
         Query
         ~> H.ComponentDSL
              (FormState error (Array (H.ComponentHTML Query)) value)
              Query
              (Either (Array error) value)
              m
    eval (Pure a) = pure a
    eval (SubmitInput a) = do
      FormState {form: Form {value}, inputs} <-
        H.modify
          (\(FormState state) -> FormState (state {submitted = Submitted}))
      H.raise (value Submitted inputs)
      pure a
    eval (SetInput i str a) = do
      FormState {form: Form {value}, inputs} <-
        H.modify
          (\(FormState {form, inputs, submitted}) ->
             FormState
               { form
               , inputs: M.insert i str inputs
               , submitted
               })
      pure a

--------------------------------------------------------------------------------
-- Inputs

-- | A text input.
text ::
     forall e a.
     Maybe String
  -> FormBuilder e (Array (HH.HTML a (Query Unit))) (Maybe String)
text def =
  FormBuilder
    (do i <- formIdent
        pure (Form {value: value i, html: html i}))
  where
    value i submitted inputs =
      pure
        (case M.lookup i inputs of
           Nothing -> def
           Just x ->
             if x == ""
               then Nothing
               else Just x)
    html i submitted inputs =
      [ HH.input
          [ HP.value
              (fromMaybe "" (joinEmptyString (M.lookup i inputs) <|> def))
          , HP.type_ HP.InputText
          , E.onValueChange (E.input (SetInput i))
          ]
      ]

-- | A number input.
number ::
     forall e a.
     Maybe Number
  -> FormBuilder e (Array (HH.HTML a (Query Unit))) (Maybe Number)
number def =
  FormBuilder
    (do i <- formIdent
        pure (Form {value: value i, html: html i}))
  where
    value i submitted inputs =
      pure (case M.lookup i inputs of
              Nothing -> def
              Just x ->
                if x == ""
                  then Nothing
                  else fromString x)
    html i submitted inputs =
      [ HH.input
          [ HP.value
              (maybe
                 ""
                 show
                 ((joinEmptyString (M.lookup i inputs) >>= fromString) <|> def))
          , HP.type_ HP.InputNumber
          , E.onValueChange (E.input (SetInput i))
          ]
      ]

--------------------------------------------------------------------------------
-- Buttons

submit :: forall h e. String -> FormBuilder e (Array (HH.HTML h (Query Unit))) Unit
submit label =
  FormBuilder
    (do i <- formIdent
        pure (Form {value, html}))
  where
    value _s _i = Right unit
    html _s _i =
      [ HH.input
          [ HP.value label
          , HP.type_ HP.InputSubmit
          , E.onClick (E.input_ SubmitInput)
          ]
      ]

--------------------------------------------------------------------------------
-- Combinators

-- | Parse the input from a form.
parse ::
     forall a b h e.
     (a -> Either (Array e) b)
  -> FormBuilder e h a
  -> FormBuilder e h b
parse parser (FormBuilder formBuilder) =
  FormBuilder
    (do Form {html, value} <- formBuilder
        pure
          (Form
             { html
             , value:
                 \submitted inputs ->
                   case value submitted inputs of
                     Right a -> parser a
                     Left e -> Left e
             }))

-- | Wrap around a form.
wrap ::
     forall e a html.
     (Maybe (Array e) -> html -> html)
  -> FormBuilder e html a
  -> FormBuilder e html a
wrap f (FormBuilder formBuilder) =
  FormBuilder
    (do Form {html: origHtml, value} <- formBuilder
        let html submitted i =
              f
                (case submitted of
                   Submitted -> either Just (const Nothing) (value submitted i)
                   NotSubmitted -> Nothing)
                (origHtml submitted i)
        pure (Form {html, value}))

-- | Make a normally optional field required.
required ::
     forall e r a html.
     {missing :: e | r}
  -> FormBuilder e html (Maybe a)
  -> FormBuilder e html a
required es =
  parse
    (\mi ->
       case mi of
         Nothing -> Left [es . missing]
         Just r -> Right r)

-- | Applicative-like combinator for combining fields.
applyFields
  :: forall f inner outer combined.
     Union inner outer combined
  => Nub combined combined
  => Apply f
  => f { | inner }
  -> f { | outer }
  -> f { | combined }
applyFields getInner getOuter =
  disjointUnion <$> getInner <*> getOuter

infixl 5 applyFields as <|*>

--------------------------------------------------------------------------------
-- Internal API

-- | Generator an identifier for a form input.
formIdent :: forall m. Bind m => MonadState BuilderState m => m Int
formIdent = do
  _ <- modify (\(BuilderState s) -> BuilderState (s {counter = s . counter + 1}))
  gets (\(BuilderState s) -> s . counter)

-- | Build a unique form.
newtype FormBuilder error html value =
  FormBuilder (State BuilderState (Form error html value))
derive instance functorBuilder :: Functor (FormBuilder error html)

instance pureForm :: Applicative (FormBuilder e (Array h)) where
  pure a =
    FormBuilder
      (pure (Form {value: const (const (Right a)), html: const (const [])}))

instance applyForm :: Apply (FormBuilder e (Array h)) where
  apply (FormBuilder getF) (FormBuilder getX) =
    FormBuilder
      (do Form {value: fValue, html: yHtml} <- getF
          Form {value: xValue, html: xHtml} <- getX
          let html i = yHtml i <> xHtml i
              value submitted i = do
                case fValue submitted i of
                  Right f ->
                    case xValue submitted i of
                      Right x -> pure (f x)
                      Left e -> Left e
                  Left e ->
                    case xValue submitted i of
                      Left e' -> Left (e <> e')
                      Right _ -> Left e
          pure (Form {html, value}))

-- | State for building forms.
data BuilderState = BuilderState
  { counter :: Int
  }

-- | Form was submitted.
data Submitted
  = Submitted
  | NotSubmitted

-- | A form itself.
data Form error html value = Form
  { value :: Submitted -> Map Int String -> Either (Array error) value
  , html :: Submitted -> Map Int String -> html
  }
derive instance functorForm :: Functor (Form error html)

-- | Internal query type.
data Query a
  = Pure a
  | SubmitInput a
  | SetInput Int
             String
             a

-- | State for the component.
data FormState error html value = FormState
  { form :: Form error html value
  , inputs :: Map Int String
  , submitted :: Submitted
  }

-- | Empty string results in a Nothing.
joinEmptyString :: Maybe String -> Maybe String
joinEmptyString mstr = do
  i <- mstr
  if i == ""
    then Nothing
    else Just i
