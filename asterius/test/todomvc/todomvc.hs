{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import AsteriusPrim
import ElementBuilder
import TodoView
import WebAPI

data Todo = Todo
  { key, text :: String
  , completed :: Bool
  }

newtype TodoModel = TodoModel
  { todos :: [Todo]
  }

data Route
  = All
  | Active
  | Completed

buildTodoElement :: Route -> TodoModel -> Todo -> Element
buildTodoElement route _ Todo {..} =
  emptyElement
    { className = "li"
    , attributes = [("class", "completed") | completed] <> [("id", key)]
    , children =
        [ emptyElement
            { className = "div"
            , attributes = [("class", "view")]
            , children =
                [ emptyElement
                    { className = "input"
                    , attributes =
                        [("checked", "true") | completed] <>
                        [("class", "toggle"), ("type", "checkbox")]
                    }
                , emptyElement
                    { className = "label"
                    , children = [TextNode text]
                    , eventHandlers = [("click", consoleLog)]
                    }
                , emptyElement
                    {className = "button", attributes = [("class", "destroy")]}
                ]
            }
        , emptyElement
            { className = "input"
            , attributes = [("class", "edit"), ("value", "")]
            }
        ]
    , hidden =
        case route of
          All -> False
          Active -> completed
          Completed -> not completed
    }

buildTodoList :: Route -> TodoModel -> Element
buildTodoList route model@TodoModel {..} =
  emptyElement
    { className = "ul"
    , attributes = [("class", "todo-list")]
    , children = map (buildTodoElement route model) todos
    }

renderTodoList :: Route -> TodoModel -> IO ()
renderTodoList route model = do
  new_todo_list <- buildElement $ buildTodoList route model
  old_todo_list <- todoList
  replaceWith old_todo_list new_todo_list

todoApp :: Route -> TodoModel -> IO ()
todoApp route model = do
  renderTodoList route model
  route_change_callback <-
    makeHaskellCallback $ do
      mode <- getURLMode
      todoApp
        (case mode of
           "active" -> Active
           "completed" -> Completed
           _ -> All)
        model
  onPopstate route_change_callback

main :: IO ()
main =
  todoApp All $
  TodoModel
    [ Todo {key = "0", text = "0sdfghsadfasd", completed = True}
    , Todo {key = "1", text = "1wertyhdvgjdfg", completed = False}
    ]
