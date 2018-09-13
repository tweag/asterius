{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module TodoView where

foreign import javascript "document.querySelector(\".todoapp\")" newTodo
  :: JSRef

foreign import javascript "document.querySelector(\".main\")" main' :: JSRef

foreign import javascript "document.querySelector(\".toggle-all\")" toggleAll
  :: JSRef

foreign import javascript "document.querySelector(\".todo-list\")" todoList
  :: IO JSRef

foreign import javascript "document.querySelector(\".footer\")" footer :: JSRef

foreign import javascript "document.querySelector(\".todo-count\")" todoCount
  :: IO JSRef

foreign import javascript "document.querySelector(\".filters\")" filters
  :: JSRef

foreign import javascript "document.querySelector(\".filters > li:nth-child(1) > a\")" allAnchor
  :: JSRef

foreign import javascript "document.querySelector(\".filters > li:nth-child(2) > a\")" activeAnchor
  :: JSRef

foreign import javascript "document.querySelector(\".filters > li:nth-child(3) > a\")" completedAnchor
  :: JSRef

foreign import javascript "document.querySelector(\".clear-completed\")" clearCompleted
  :: JSRef
