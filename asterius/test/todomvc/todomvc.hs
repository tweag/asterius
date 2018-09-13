{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC
  -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import AsteriusPrim
import qualified Data.Map.Strict as Map
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

buildTodoElement :: Route -> Todo -> Element
buildTodoElement route Todo {..} =
  emptyElement
    { className = "li"
    , attributes =
        Map.fromList $ [("class", "completed") | completed] <> [("id", key)]
    , children =
        [ emptyElement
            { className = "div"
            , attributes = Map.fromList [("class", "view")]
            , children =
                [ emptyElement
                    { className = "input"
                    , attributes =
                        Map.fromList $
                        [("checked", "true") | completed] <>
                        [("class", "toggle"), ("type", "checkbox")]
                    }
                , emptyElement {className = "label", children = [TextNode text]}
                , emptyElement
                    { className = "button"
                    , attributes = Map.fromList [("class", "destroy")]
                    }
                ]
            }
        , emptyElement
            { className = "input"
            , attributes = Map.fromList [("class", "edit"), ("value", "")]
            }
        ]
    , hidden =
        case route of
          All -> False
          Active -> completed
          Completed -> not completed
    }

buildTodoList :: Route -> TodoModel -> Element
buildTodoList route TodoModel {..} =
  emptyElement
    { className = "ul"
    , attributes = Map.fromList [("class", "todo-list")]
    , children = map (buildTodoElement route) todos
    }

todoApp :: TodoModel -> IO ()
todoApp model = do
  new_todo_list <- buildElement $ buildTodoList All model
  consoleLog new_todo_list
  old_todo_list <- todoList
  consoleLog old_todo_list
  replaceWith old_todo_list new_todo_list

main :: IO ()
main =
  todoApp $
  TodoModel
    [ Todo {key = "0", text = "0", completed = True}
    , Todo {key = "1", text = "1", completed = False}
    ]
