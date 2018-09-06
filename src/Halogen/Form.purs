-- | Form API.

module Halogen.Form
  (FormBuilder
  ,Query
  -- * Inputs
  ,textInput
  ,numberInput
  ,submitInput
  -- * Combinators
  ,wrap
  ,labelled
  ,reparse
  -- * Halogen component
  ,component)
  where

import Control.Alternative ((<|>))
import Control.Monad.State
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Prelude (class Applicative, class Apply, class Bind, class Functor, type (~>), Unit, bind, const, discard, map, mempty, pure, show, unit, (+), (<>))

-- | Build a unique form.
newtype FormBuilder error html value =
  FormBuilder (State BuilderState (Form error html value))
derive instance functorBuilder :: Functor (FormBuilder error html)

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

data Query a
  = Pure a
  | SubmitInput a
  | SetInput Int
             String
             a

data FormState error html value = FormState
  { form :: Form error html value
  , inputs :: Map Int String
  , submitted :: Submitted
  }

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

component ::
     forall error value m.
     H.Component HH.HTML Query (FormBuilder error (Array (H.ComponentHTML Query)) value) (Either (Array error) value) m
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
         Query ~> H.ComponentDSL (FormState error (Array (H.ComponentHTML Query)) value) Query (Either (Array error) value) m
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

formIdent :: forall m. Bind m => MonadState BuilderState m => m Int
formIdent = do
  _ <- modify (\(BuilderState s) -> BuilderState (s {counter = s . counter + 1}))
  gets (\(BuilderState s) -> s . counter)

reparse ::
     forall a b h e.
     (a -> Either (Array e) b)
  -> FormBuilder e h a
  -> FormBuilder e h b
reparse parser (FormBuilder formBuilder) =
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

labelled ::
     forall e x y a r.
     String
  -> (a -> Record r)
  -> (e -> HH.HTML x y)
  -> FormBuilder e (Array (HH.HTML x y)) a
  -> FormBuilder e (Array (HH.HTML x y)) (Record r)
labelled txt mk renderMsg form =
  map
    mk
    (wrap
       (\errs i ->
          [ HH.p_
              [ HH.label_
                  [ HH.text (txt <> ": ")
                  , HH.span_ i
                  , case errs of
                      Nothing -> HH.text ""
                      Just errs' -> HH.div_ (map renderMsg errs')
                  ]
              ]
          ])
       form)

wrap :: forall e a html. (Maybe (Array e) -> html -> html) -> FormBuilder e html a -> FormBuilder e html a
wrap f (FormBuilder formBuilder) =
  FormBuilder
    (do Form {html: origHtml, value} <- formBuilder
        let html submitted i =
              f (either Just (const Nothing) (value submitted i)) (origHtml submitted i)
        pure (Form {html, value}))

submitInput :: forall h e. String -> FormBuilder e (Array (HH.HTML h (Query Unit))) Unit
submitInput label =
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

textInput ::
     forall a e errors.
     {missing :: e | errors}
  -> Maybe String
  -> FormBuilder e (Array (HH.HTML a (Query Unit))) String
textInput es defaul =
  FormBuilder
    (do i <- formIdent
        pure (Form {value: value i, html: html i}))
  where
    value i submitted inputs =
      case M.lookup i inputs of
        Nothing ->
          maybe
            (Left
               (case submitted of
                  Submitted -> [es . missing]
                  NotSubmitted -> []))
            Right
            defaul
        Just v -> Right v
    html i submitted inputs =
      [ HH.input
          [ HP.value
              (case defaul <|> M.lookup i inputs of
                 Nothing -> ""
                 Just str -> str)
          , HP.type_ HP.InputText
          , E.onValueChange (E.input (SetInput i))
          ]
      ]

numberInput ::
     forall a e errors.
     {invalidNumber :: e, missing :: e | errors}
  -> Maybe Number
  -> FormBuilder e (Array (HH.HTML a (Query Unit))) Number
numberInput e mdef =
  reparse
    (\i ->
       case fromString i of
         Just n -> Right n
         Nothing -> Left [e.invalidNumber])
    (textInput e (map show mdef))
