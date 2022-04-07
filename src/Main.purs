module Main where

import Prelude

import Effect (Effect)
import Specular.Dom.Element (attrs, dynText, el, text, onClick, classWhenD, class_)
import Specular.Dom.Widget
import Specular.Dom.Widgets.Input
import Specular.Dom.Node.Class ((:=))
import Specular.Ref (Ref)
import Specular.Ref as Ref
import Specular.FRP (Dynamic, Event, filterJustEvent, withDynamic_, changed, newEvent, leftmost, switch)
import Specular.FRP.Base (subscribeEvent_, never, subscribeDyn)
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Browser as Browser
import Specular.Dom.Widgets.Button (buttonOnClick)
import Unsafe.Coerce (unsafeCoerce)
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Traversable (traverse_, traverse)
import Data.String as String
import Effect.Class (class MonadEffect)


type TodoItem = { todo :: String
                , completed :: Boolean
                }

type Todos = Ref (Array (Ref TodoItem))

type AppState =
    { todos :: Todos
    , removeTodo :: Int -> Effect Unit
    }

main :: Effect Unit
main = runMainWidgetInBody mainWidget

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

itemValidationMessage :: Dynamic (Maybe String) -> Widget Unit
itemValidationMessage mMsgD = withDynamic_ mMsgD $ \maybeMessage ->
  case maybeMessage of
    Just msg ->
      el "p" [attrs ("style" := "color:red")] $ text msg
    Nothing -> pure unit

listItem :: (Int -> Effect Unit) -> Int -> Ref TodoItem -> Widget Unit
listItem requestRemoval i todoItem = do
  editing <- Ref.new false
  localValidation :: Ref (Maybe String) <- Ref.new Nothing
  el "li" mempty $ do
    withDynamic_ (Ref.value editing) $ \editingP ->
      if editingP then
        do todoText <- Ref.read todoItem <#> _.todo
           input <- textInput { initialValue: todoText
                              , attributes: pure mempty
                              , setValue: never
                              }
           itemValidationMessage (Ref.value localValidation)
           inputEvt <- textInputValueEventOnEnter input
           (flip subscribeEvent_) inputEvt $ \newTodo ->
             if String.null newTodo
             then Ref.write localValidation $ Just "empty input not allowed"
             else do Ref.modify todoItem (_ { todo = newTodo })
                     Ref.write editing false
                     Ref.write localValidation Nothing
      else
        el "div" [class_ "todoItem"] $ do
          withDynamic_ (Ref.value todoItem <#> _.completed) $ \completed -> do
            activeD <- checkbox completed $ mempty
            (flip subscribeEvent_) (changed activeD) $ \checked ->
              Ref.modify todoItem (_ { completed = checked})
          el "div"
            [ onClick (\_ -> Ref.write editing true)
            , classWhenD (Ref.value todoItem <#> _.completed) "completed"
            ] $ do dynText $ Ref.value todoItem <#> _.todo
                   text "  "
                   removeEvt <- buttonOnClick (pure mempty) $ text "remove"
                   subscribeEvent_ (\_ -> requestRemoval i) removeEvt

viewTODOs :: AppState -> Widget Unit
viewTODOs as =
  el "div" mempty $
  el "ul" mempty $ 
  withDynamic_ (Ref.value as.todos) $ \todos ->
    (flip traverse_) (Array.mapWithIndex Tuple todos) $ \(Tuple i el) ->
      listItem as.removeTodo i el

maybeAppendTODO :: Todos -> Ref String -> TodoItem -> Effect Unit
maybeAppendTODO todos validation newTodo = do
  if String.null newTodo.todo
    then do
      Ref.write validation "empty input not allowed"
    else do
      newRef <- Ref.new newTodo
      Ref.modify todos $ (flip Array.snoc) newRef
      Ref.write validation ""


mainWidget :: Widget Unit
mainWidget = do
  el "h1" mempty $ text "simplified TODO list"
  textAdded <- todoTextInput
  
  todos :: Todos <- Ref.new []
  validation <- Ref.new ""
  { event: removeEvt :: Event Int, fire: fireRemove } <- newEvent
  let as = { todos: todos, removeTodo: fireRemove }
  numCompleted <- Ref.new 0

  subscribeEvent_ (maybeAppendTODO as.todos validation) $
    { todo: _, completed: false} <$> textAdded
  (flip subscribeEvent_) removeEvt $ \i ->
    -- TODO: check if completed and decrease numCompleted if true
    Ref.modify as.todos $ \currentTodos ->
      fromMaybe currentTodos $ Array.deleteAt i currentTodos


  -- validation
  el "p" [attrs ("style" := "color:red")] $ dynText $ Ref.value validation

  -- todo list
  viewTODOs as

  -- completed counter
  completedD :: Dynamic (Array (Event Boolean))
    <- (flip subscribeDyn) (Ref.value as.todos) $ \arrOfRefs -> do
      pure $ arrOfRefs <#> \r -> (changed $ Ref.value r) <#> _.completed
  let togglingComplete = completedD <#> leftmost
  (flip subscribeEvent_) (switch togglingComplete) $ \completed ->
    Ref.modify numCompleted (if completed then (_+1) else (_-1))
  el "p" [attrs ("style" := "color:blue")] $ do
    text "completed TODOs: "
    dynText $ Ref.value numCompleted <#> show

   --pure $ (todosArr <#> _.completed) # Array.filter identity # Array.length
