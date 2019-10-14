{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

import Asterius.Types
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Coerce
import Data.IORef
import Data.Maybe
import ElementBuilder
import GHC.Generics
import TodoView
import WebAPI

data Todo
  = Todo
      { key, text :: String,
        editing, completed :: Bool
      }
  deriving (Generic)

instance Binary Todo

newtype TodoModel
  = TodoModel
      { todos :: [Todo]
      }
  deriving (Generic)

instance Binary TodoModel

data Route
  = All
  | Active
  | Completed

loadModel :: String -> IO (Maybe TodoModel)
loadModel k = do
  r <- localStorageGetItem k
  pure $
    case r of
      Just bs ->
        case decodeOrFail (LBS.fromStrict bs) of
          Right (_, _, model) -> Just model
          _ -> Nothing
      _ -> Nothing

saveModel :: String -> TodoModel -> IO ()
saveModel k v = localStorageSetItem k (LBS.toStrict (encode v))

modifyTodo :: TodoModel -> String -> (Todo -> Todo) -> TodoModel
modifyTodo model k f =
  model
    { todos =
        [ if key todo == k
            then f todo
            else todo
          | todo <- todos model
        ]
    }

buildTodoElement :: Route -> IORef TodoModel -> Todo -> Element
buildTodoElement route model_ref current_todo =
  emptyElement
    { className = "li",
      attributes =
        ( if editing current_todo
            then [("class", "editing")]
            else [("class", "completed") | completed current_todo]
        )
          <> [("id", key current_todo)],
      children =
        [ emptyElement
            { className = "div",
              attributes = [("class", "view")],
              children =
                [ emptyElement
                    { className = "input",
                      attributes =
                        [("checked", "true") | completed current_todo]
                          <> [("class", "toggle"), ("type", "checkbox")],
                      eventHandlers =
                        [ ( "click",
                            const $ do
                              modifyIORef' model_ref $ \model ->
                                modifyTodo model (key current_todo) $ \todo ->
                                  todo {completed = not $ completed todo}
                              render model_ref
                          )
                        ]
                    },
                  emptyElement
                    { className = "label",
                      children = [TextNode $ text current_todo],
                      eventHandlers =
                        [ ( "dblclick",
                            const $ do
                              modifyIORef' model_ref $ \model ->
                                modifyTodo model (key current_todo) $ \todo ->
                                  todo {editing = not $ editing todo}
                              render model_ref
                          )
                        ]
                    },
                  emptyElement
                    { className = "button",
                      attributes = [("class", "destroy")],
                      eventHandlers =
                        [ ( "click",
                            const $ do
                              modifyIORef' model_ref $ \model ->
                                model
                                  { todos =
                                      filter
                                        (\todo -> key todo /= key current_todo)
                                        $ todos model
                                  }
                              render model_ref
                          )
                        ]
                    }
                ]
            },
          emptyElement
            { className = "input",
              attributes =
                [ ("class", "edit"),
                  ("id", key current_todo <> "_input"),
                  ("value", text current_todo)
                ],
              eventHandlers =
                [ ( "keypress",
                    \ev -> do
                      ev_key <- fromJSString . coerce <$> indexJSObject ev "key"
                      when (ev_key == "Enter") $ do
                        input_element <-
                          getElementById $ key current_todo <> "_input"
                        new_text <-
                          trim . fromJSString . coerce
                            <$> indexJSObject (coerce input_element) "value"
                        modifyIORef' model_ref $ \model ->
                          modifyTodo model (key current_todo) $ \todo ->
                            todo {text = new_text, editing = False}
                        render model_ref
                  )
                  | editing current_todo
                ]
            }
        ],
      hidden = case route of
        All -> False
        Active -> completed current_todo
        Completed -> not $ completed current_todo
    }

buildTodoList :: Route -> IORef TodoModel -> IO Element
buildTodoList route model_ref = do
  TodoModel {..} <- readIORef model_ref
  pure
    emptyElement
      { className = "ul",
        attributes = [("class", "todo-list")],
        children = map (buildTodoElement route model_ref) todos
      }

currentRoute :: IO Route
currentRoute = do
  mode <- getURLMode
  pure $
    case mode of
      "active" -> Active
      "completed" -> Completed
      _ -> All

render :: IORef TodoModel -> IO ()
render model_ref = do
  route <- currentRoute
  new_todo_list <- buildTodoList route model_ref >>= buildElement
  old_todo_list <- todoList
  replaceWith old_todo_list new_todo_list
  model@TodoModel {..} <- readIORef model_ref
  let active_todos = length $ filter (not . completed) todos
  new_todo_count <-
    buildElement
      emptyElement
        { className = "span",
          attributes = [("class", "todo-count")],
          children =
            [ emptyElement
                { className = "strong",
                  children = [TextNode $ show active_todos]
                },
              TextNode $
                case active_todos of
                  1 -> " item"
                  _ -> " items"
            ]
        }
  old_todo_count <- todoCount
  replaceWith old_todo_count new_todo_count
  saveModel "todomvc-asterius" model

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

todoMVC :: IORef TodoModel -> IO ()
todoMVC model_ref = do
  let m = render model_ref
  addEventListener toggleAll "click"
    $ const
    $ do
      checked <- toggleAllChecked
      modifyIORef' model_ref $ \model ->
        model {todos = map (\todo -> todo {completed = checked}) $ todos model}
      m
  addEventListener newTodo "keypress" $ \ev -> do
    ev_key <- fromJSString . coerce <$> indexJSObject ev "key"
    when (ev_key == "Enter") $ do
      raw_val <-
        fromJSString . coerce <$> indexJSObject (coerce newTodo) "value"
      let val = trim raw_val
      unless (null val) $ do
        k <- randomString
        modifyIORef' model_ref $ \model ->
          model
            { todos =
                todos model
                  <> [Todo {key = k, text = val, editing = False, completed = False}]
            }
        setJSObject (coerce newTodo) "value" (coerce (toJSString ""))
        m
  makeHaskellCallback m >>= onPopstate
  addEventListener clearCompleted "click"
    $ const
    $ do
      modifyIORef' model_ref $ \model ->
        model {todos = filter (not . completed) $ todos model}
      m
  m

main :: IO ()
main = do
  init_model <- fromMaybe (TodoModel []) <$> loadModel "todomvc-asterius"
  model_ref <- newIORef init_model
  todoMVC model_ref
