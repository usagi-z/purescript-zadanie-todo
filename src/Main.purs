module Main where

import Prelude

import Effect (Effect)
import Specular.Dom.Element (attrs, dynText, el, text, onClick, classWhen, class_)
import Specular.Dom.Widget
import Specular.Dom.Widgets.Input
import Specular.Dom.Node.Class ((:=))
import Specular.Ref (Ref)
import Specular.Ref as Ref
import Specular.FRP (Dynamic, Event, filterJustEvent, withDynamic_, changed, newEvent, whenD)
import Specular.FRP.Base (subscribeEvent_, never)
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Browser as Browser
import Specular.Dom.Browser (Node)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Unsafe.Coerce (unsafeCoerce)
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Data.String as String



-- ffi imports

foreign import persistTodos :: Array TodoItem -> Effect Unit
foreign import loadTodos :: Effect (Array TodoItem)
foreign import window :: Effect Node



-- types

type TodoItem = { todo :: String
                , completed :: Boolean
                }

type Todos = Ref (Array TodoItem)

data Msg = AddItem String
         | UpdateItem Int String
         | ToggleCompleted Int
         | MarkAllAsCompleted
         | RemoveItem Int
         | ClearCompleted

type App =
    { todos :: Todos
    , validation :: Ref String
    , msgEvt :: Event Msg
    , sendMsg :: Msg -> Effect Unit
    }



-- main text input widget

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




-- todo list item widget

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
          (flip subscribeEvent_) (changed activeD) $ \_ ->
            sendMsg (ToggleCompleted i)
          el "div"
            [ onClick (\_ -> Ref.write editing true)
            , classWhen todoItem.completed "completed"
            ] $ do text todoItem.todo
                   text "  "
                   removeEvt <- buttonOnClick (pure mempty) $ text "remove"
                   subscribeEvent_ (\_ -> sendMsg $ RemoveItem i) removeEvt



-- main widget

viewTODOs :: App -> Widget Unit
viewTODOs as =
  el "div" mempty $
  el "ul" mempty $ 
  withDynamic_ (Ref.value as.todos) $ \todos ->
    (flip traverse_) (Array.mapWithIndex Tuple todos) $ \(Tuple i el) ->
      listItem as.sendMsg i el

maybeAppendTODO :: App -> String -> Effect Unit
maybeAppendTODO as newTodo = do
  if String.null newTodo
    then do
      Ref.write as.validation "empty input not allowed"
    else do
      as.sendMsg (AddItem newTodo)
      Ref.write as.validation ""

mainWidget :: App -> Widget Unit
mainWidget as = do
  
   -- interaction wiring
  (flip subscribeEvent_) as.msgEvt $ \msg ->
    let changeTodoText newTodoText item = item { todo = newTodoText }
        toggleCompleted item = item { completed = not item.completed } in
    Ref.modify as.todos $ \currentTodos ->
      case msg of
        AddItem newTodoText ->
          Array.snoc currentTodos $ { todo: newTodoText, completed: false}
        UpdateItem i newTodoText ->
          fromMaybe currentTodos $ 
            Array.modifyAt i (changeTodoText newTodoText) currentTodos
        ToggleCompleted i ->
          fromMaybe currentTodos $ 
            Array.modifyAt i toggleCompleted currentTodos
        MarkAllAsCompleted -> 
          map (_ { completed = true }) currentTodos
        RemoveItem i -> 
          fromMaybe currentTodos $ 
            Array.deleteAt i currentTodos
        ClearCompleted ->
          Array.filter (not _.completed) currentTodos
  
  el "h1" mempty $ text "simplified TODO list"

  -- main input
  textAdded <- todoTextInput
  subscribeEvent_ (maybeAppendTODO as) textAdded
  
  -- validation
  el "p" [attrs ("style" := "color:red")] $ dynText $ Ref.value as.validation

  -- todo list
  viewTODOs as

  -- 'mark all as completed' button
  allCompletedClick <- buttonOnClick (pure mempty) $ text "mark all as completed"
  subscribeEvent_ (\_ -> as.sendMsg MarkAllAsCompleted) allCompletedClick

  -- 'clear completed' button
  let anyCompleted = Array.any (_.completed)
  whenD (anyCompleted <$> Ref.value as.todos) $ do 
    clearCompletedClick <- buttonOnClick (pure mempty) $ text "clear completed"
    subscribeEvent_ (\_ -> as.sendMsg ClearCompleted) clearCompletedClick
  
  -- completed counter
  let numCompleted = Ref.value as.todos <#> \ts ->
        Array.filter (_.completed) ts # Array.length
  el "p" [attrs ("style" := "color:blue")] $ do
    text "completed TODOs: "
    dynText $ numCompleted <#> show
  



-- top level

main :: Effect Unit
main = do
  
  -- state setup
  todos :: Todos <- Ref.new []
  validation <- Ref.new ""
  { event: msgEvt, fire: sendMsg } <- newEvent
  let as = { todos: todos
           , validation: validation
           , msgEvt: msgEvt
           , sendMsg: sendMsg
           } :: App

  -- persistence
  loadedTodos <- loadTodos
  Ref.write as.todos loadedTodos
    
  let saveTodos _ = do
               ts <- Ref.read as.todos
               persistTodos ts
  w <- window
  _ <- Browser.addEventListener "beforeunload" saveTodos w

  -- main widget
  runMainWidgetInBody $ mainWidget as
  
