module TodoView where

import Asterius.Types

foreign import javascript "document.querySelector(\".new-todo\")"
  newTodo :: JSVal

foreign import javascript "document.querySelector(\".main\")" main' :: JSVal

foreign import javascript "document.querySelector(\".toggle-all\")"
  toggleAll :: JSVal

foreign import javascript "document.querySelector(\".toggle-all\").checked"
  toggleAllChecked :: IO Bool

foreign import javascript "document.querySelector(\".todo-list\")"
  todoList :: IO JSVal

foreign import javascript "document.querySelector(\".footer\")" footer :: JSVal

foreign import javascript "document.querySelector(\".todo-count\")"
  todoCount :: IO JSVal

foreign import javascript "document.querySelector(\".filters\")"
  filters :: JSVal

foreign import javascript "document.querySelector(\".filters > li:nth-child(1) > a\")"
  allAnchor :: JSVal

foreign import javascript "document.querySelector(\".filters > li:nth-child(2) > a\")"
  activeAnchor :: JSVal

foreign import javascript "document.querySelector(\".filters > li:nth-child(3) > a\")"
  completedAnchor :: JSVal

foreign import javascript "document.querySelector(\".clear-completed\")"
  clearCompleted :: JSVal
