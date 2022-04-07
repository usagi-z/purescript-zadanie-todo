module Main where

import Prelude

import Effect (Effect)
import Specular.Dom.Element (attrs, dynText, el, text, onClick, classWhen, class_)
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

type Todos = Ref (Array TodoItem)

data Msg = UpdateItem Int String
         | ToggleCompleted Int
         | RemoveItem Int

type AppState =
    { todos :: Todos
    , validation :: Ref String
    , control :: Msg -> Effect Unit
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

listItem :: (Msg -> Effect Unit) -> Int -> TodoItem -> Widget Unit
listItem sendMsg i todoItem = do
  editing <- Ref.new false
  localValidation :: Ref (Maybe String) <- Ref.new Nothing
  el "li" mempty $ do
    withDynamic_ (Ref.value editing) $ \editingP ->
      if editingP then
        do input <- textInput { initialValue: todoItem.todo
                              , attributes: pure mempty
                              , setValue: never
                              }
           itemValidationMessage (Ref.value localValidation)
           inputEvt <- textInputValueEventOnEnter input
           (flip subscribeEvent_) inputEvt $ \newTodoText ->
             if String.null newTodoText
             then Ref.write localValidation $ Just "empty input not allowed"
             else
               do Ref.write editing false
                  Ref.write localValidation Nothing
                  sendMsg (UpdateItem i newTodoText)
      else
        el "div" [class_ "todoItem"] $ do
          let completed = todoItem.completed
          activeD <- checkbox completed mempty
          (flip subscribeEvent_) (changed activeD) $ \checked ->
            sendMsg (ToggleCompleted i)
          el "div"
            [ onClick (\_ -> Ref.write editing true)
            , classWhen todoItem.completed "completed"
            ] $ do text todoItem.todo
                   text "  "
                   removeEvt <- buttonOnClick (pure mempty) $ text "remove"
                   subscribeEvent_ (\_ -> sendMsg $ RemoveItem i) removeEvt

viewTODOs :: AppState -> Widget Unit
viewTODOs as =
  el "div" mempty $
  el "ul" mempty $ 
  withDynamic_ (Ref.value as.todos) $ \todos ->
    (flip traverse_) (Array.mapWithIndex Tuple todos) $ \(Tuple i el) ->
      listItem as.control i el

maybeAppendTODO :: AppState -> TodoItem -> Effect Unit
maybeAppendTODO as newTodo = do
  if String.null newTodo.todo
    then do
      Ref.write as.validation "empty input not allowed"
    else do
      Ref.modify as.todos $ (flip Array.snoc) newTodo
      Ref.write as.validation ""


mainWidget :: Widget Unit
mainWidget = do
  el "h1" mempty $ text "simplified TODO list"
  textAdded <- todoTextInput

  -- state setup
  todos :: Todos <- Ref.new []
  validation <- Ref.new ""
  { event: controlEvt, fire: sendMsg } <- newEvent
  let as = { todos: todos, control: sendMsg, validation: validation} :: AppState

  -- interaction wiring
  subscribeEvent_ (maybeAppendTODO as) $
    { todo: _, completed: false} <$> textAdded
  (flip subscribeEvent_) controlEvt $ \msg ->
    case msg of
      UpdateItem i newTodoText -> Ref.modify as.todos $ \currentTodos ->
        let changeTodoText item = item { todo = newTodoText } in
        fromMaybe currentTodos $ Array.modifyAt i changeTodoText currentTodos
      ToggleCompleted i -> Ref.modify as.todos $ \currentTodos ->
        let toggleCompleted item = item { completed = not item.completed } in
        fromMaybe currentTodos $ Array.modifyAt i toggleCompleted currentTodos
      RemoveItem i -> Ref.modify as.todos $ \currentTodos ->
        fromMaybe currentTodos $ Array.deleteAt i currentTodos

  -- validation
  el "p" [attrs ("style" := "color:red")] $ dynText $ Ref.value validation

  -- todo list
  viewTODOs as
