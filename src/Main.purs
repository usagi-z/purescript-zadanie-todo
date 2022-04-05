module Main where

import Prelude

import Effect (Effect)
import Specular.Dom.Element (attr, attrs, class_, dynText, el, onClick_, text)
import Specular.Dom.Widget
import Specular.Dom.Widgets.Input
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.Ref (Ref)
import Specular.Ref as Ref
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, filterEvent, holdDyn, leftmost, never, subscribeWeakDyn_, foldDyn, filterJustEvent)
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Browser as Browser
import Unsafe.Coerce (unsafeCoerce)
import Specular.FRP.Base (subscribeEvent_, tagDyn)
import Specular.FRP (withDynamic_)
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Traversable (traverse_)
import Data.String as String

main :: Effect Unit
main = runMainWidgetInBody mainWidget

unsafeEventKey :: Browser.Event -> Effect String
unsafeEventKey event = pure (unsafeCoerce event).key

getInputOnEnterAndClear :: Browser.Node -> Browser.Event -> Effect (Maybe String)
getInputOnEnterAndClear element event = do
  if (unsafeCoerce event).key == "Enter"
  then do
    value <- getTextInputValue element
    setTextInputValue element ""
    pure $ Just value
  else pure Nothing

todoTextInput :: forall m. MonadWidget m => m (Event String)
todoTextInput = do
  Tuple element _ <- elDynAttr' "input" (pure ("autofocus" := "")) (pure unit)
  maybeInput <- domEventWithSample (getInputOnEnterAndClear element) "keypress" element
  pure $ filterJustEvent maybeInput

listItem :: String -> Widget Unit
listItem todoText = el "li" mempty $ text todoText

todosList :: Array String -> Widget Unit
todosList = traverse_ listItem

viewTODOs :: Dynamic (Array String) -> Widget Unit
viewTODOs todosD =
  el "div" mempty $
  el "ul" mempty $
  withDynamic_ todosD todosList

maybeAppendTODO :: Ref (Array String) -> Ref String -> String -> Effect Unit
maybeAppendTODO todos validation new = do
  if String.null new
    then do
      Ref.write validation "empty input not allowed"
    else do
      Ref.modify todos $ (flip Array.snoc) new
      Ref.write validation ""

mainWidget :: Widget Unit
mainWidget = do
  el "h1" mempty $ text "simplified TODO list"
  textAdded <- todoTextInput

  todos :: Ref (Array String) <- Ref.new []
  validation <- Ref.new ""
  subscribeEvent_ (maybeAppendTODO todos validation) textAdded

  el "p" [attrs ("style" := "color:red")] $ dynText $ Ref.value validation
  viewTODOs $ Ref.value todos
  
