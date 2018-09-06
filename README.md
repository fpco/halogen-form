# halogen-form

This implements formlets as in the Wadler paper
[_The Essence of Form Abstraction_](http://homepages.inf.ed.ac.uk/slindley/papers/formlets-essence.pdf)
for the halogen package.

# Introduction

## The component

`Halogen.Form.component` provides a Halogen component to put a form
into your HTML. Its type is:

```haskell
component ::
   forall error value m.
   H.Component
     HH.HTML
     Query
     (FormBuilder error (Array (H.ComponentHTML Query)) value) -- Input
     (Either (Array error) value) -- Output
     m
```

## Form builders

The input to the component is a `FormBuilder`, which looks like this:

``` haskell
newtype FormBuilder error html value
```

* The error type is user-defined. We'll see how below.
* The html type is user-defined.
* The value that the form produces in the end.

Underneath the `FormBuilder` API there is an internal type, which is
produced by a `FormBuilder`, which is the `Form` type:

``` haskell
data Form error html value = Form
  { value :: Submitted -> Map Int String -> Either (Array error) value
  , html :: Submitted -> Map Int String -> html
  }
```

A form simply has a value and a way to render it. The `Map Int String`
associates form inputs with their values, if any. A given `Form` knows
what `Int` key (provided by the `FormBuilder`) to use to pull a value
or many values from the input.

## The simplest form builder

The most basic form builder would be `textInput` which has this type:

```haskell
textInput ::
     forall a e errors.
     {missing :: e | errors}
  -> Maybe String
  -> FormBuilder e (Array (HH.HTML a (Query Unit))) String
```

The `Maybe String` is the default input, if any.

## Defining errors for your form

A text input's value may be missing, so we provide a record telling
the builder which error constructor from our error type `e` to use. It
looks like this:

``` haskell
data FormError
  = MissingInput
  | InvalidNumber
  -- Etc.

errors :: { missing :: FormError, invalidNumber :: FormError}
errors = {missing: MissingInput, invalidNumber: InvalidNumber}
```

Elsewhere in the app, you'll have a printing function:

``` haskell
printFormError msg =
  HH.strong_
    [ HH.text
        (case msg of
           InvalidNumber -> "Please enter a valid number."
           MissingInput -> "Please fill everything in."
    ]
```

Which lets you use your own way of talking to explain error messages.

## Using the form component in a slot

With our error type defined, we can use the component and build a
form:

``` haskell
data Slot = FormSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

HH.slot FormSlot Form.component (Form.textInput errors Nothing) (\value -> Nothing)
```

(`Halogen.Form` is imported as `Form`.)

This form will produce a `String` in the `value` given to the output
handler. In that output handler you can send the form value to your
`eval` function as usual.

## Combining form builders

We can combine form builders together with `Applicative`:

```haskell
HH.slot
  FormSlot
  Form.component
  (Tuple <$> Form.textInput errors Nothing
         <*> Form.numberInput errors Nothing
         <*  Form.submitInput "Submit!")
  (\value -> Nothing)
```

## Building records

With the `(<|*>)` combinator that sits in place of `<*>`, you can
build a record instead:

```haskell
HH.slot
  FormSlot
  Form.component
  (     map {name: _} (Form.textInput errors Nothing)
   <|*> map {age: _} (Form.numberInput errors Nothing)
   <*   Form.submitInput "Submit!")
  (\value -> Nothing)
```

And now your `value` will be a record of type

``` haskell
{name :: String, age :: Number}
```

E.g.

```haskell
person ::
  forall h.
    FormBuilder
      FormError
      (Array (HH.HTML h (Query Unit)))
      { name :: String, age :: Number}
person =
  map {name: _} (textInput errors Nothing) <|*>
  map {age: _} (numberInput errors Nothing) <*
  submitInput "Submit!"
```

## Validation

We can add validation to this form using the `reparse` combinator:

``` haskell
person ::
  forall h.
    FormBuilder
      FormError
      (Array (HH.HTML h (Query Unit)))
      { approved :: String }
person =
  reparse
    (\them ->
       if them . name == "Crocodile Hunter" || them . age > 70
         then Left [InsuranceApplicationFailed]
         else Right {approved: them . name})
    (map {name: _} (textInput errors Nothing) <|*>
     map {age: _}
       (reparse
          (\age ->
             if age > 18 && age < 100
               then Right age
               else Left [InvalidAge])
          (numberInput errors Nothing)) <*
     submitInput "Submit!")
```

Here I've demonstrated two things:

1. Using `reparse` on an individual form input to validate age.
2. Using `reparse` to apply a life insurance policy on multiple
   fields.

## Composability

The fact that validation, input and rendering are all coupled means I
can separate `age` into a re-usable component throughout my app:

```haskell
ageInput ::
     forall h.
     Maybe Number
  -> FormBuilder FormError (Array (HH.HTML h (Query Unit))) Number
ageInput def =
  reparse
    (\age ->
       if age > 18.0 && age < 100.0
         then Right age
         else Left [InvalidAge])
    (numberInput errors def)
```

Or make it even more generic to be used across different types of
errors:

``` haskell
ageInput ::
     forall h e errors.
     {invalidAge :: e, invalidNumber :: e, missing :: e | errors}
  -> Maybe Number
  -> FormBuilder e (Array (HH.HTML h (Query Unit))) Number
ageInput es def =
  reparse
    (\age ->
       if age > 18.0 && age < 100.0
         then Right age
         else Left [es.invalidAge])
    (numberInput es def)
```

## Wrapping up

You can wrap your own custom HTML around other form builders using
`wrap`:


``` haskell
wrap ::
     forall e a html.
     (Maybe (Array e) -> html -> html)
  -> FormBuilder e html a
  -> FormBuilder e html a
```

You can choose to print the error messages around an input, if you
like. Otherwise you can display them in e.g. a list above.
