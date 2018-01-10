module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Concurrent
import Control.Monad
import Data.Maybe

data Node = Node Char (MVar Node) (MVar Node)
data Deque = Deque (MVar Node) (MVar Node)

nodeValue (Node value _ _) = value
nodeLeft (Node _ left _) = tryReadMVar left
nodeRight (Node _ _ right) = tryReadMVar right

mkNode x = do
  newLeft <- newEmptyMVar
  newRight <- newEmptyMVar
  let node = Node x newLeft newRight
  return node

updateNodeLeft node@(Node _ oldLeft _) newLeft = do
  oldLeftNode <- tryTakeMVar oldLeft
  putMVar oldLeft newLeft

updateNodeRight node@(Node _ _ oldRight) newRight = do
  oldRightNode <- tryTakeMVar oldRight
  putMVar oldRight newRight

clearNodeLeft node@(Node _ oldLeft _) = do
  oldNode <- tryTakeMVar oldLeft
  return ()

clearNodeRight node@(Node _ _ oldRight) = do
  oldNode <- tryTakeMVar oldRight
  return ()

mkDeque :: IO (Deque)
mkDeque = do
  newFront <- newEmptyMVar
  newBack <- newEmptyMVar
  let deque = Deque newFront newBack
  return deque
  
pop :: Deque -> IO (Maybe Char)
pop (Deque dequeFront dequeBack) = do
  back <- tryTakeMVar dequeBack
  if isNothing back
     then do return Nothing
     else do backBack <- nodeLeft (fromJust back)
             let x = nodeValue (fromJust back)
             if isNothing backBack
               then do front <- tryTakeMVar dequeFront
                       return (Just x)
               else do clearNodeRight (fromJust backBack)
                       putMVar dequeBack (fromJust backBack)
                       return (Just x)

push :: Deque -> Char -> IO ()
push (Deque dequeFront dequeBack) x = do
  back <- tryTakeMVar dequeBack
  if isNothing back
     then do newNode <- mkNode x
             putMVar dequeBack newNode
             putMVar dequeFront newNode
     else do newNode <- mkNode x
             updateNodeLeft newNode (fromJust back)
             updateNodeRight (fromJust back) newNode
             putMVar dequeBack newNode

shift :: Deque -> IO (Maybe Char)
shift (Deque dequeFront dequeBack) = do
  front <- tryTakeMVar dequeFront
  if isNothing front
     then do return Nothing
     else do frontFront <- nodeRight (fromJust front)
             let x = nodeValue (fromJust front)
             if isNothing frontFront
               then do back <- tryTakeMVar dequeBack
                       return (Just x)
               else do clearNodeLeft (fromJust frontFront)
                       putMVar dequeFront (fromJust frontFront)
                       return (Just x)

unshift :: Deque -> Char -> IO ()
unshift (Deque dequeFront dequeBack) x = do
  front <- tryTakeMVar dequeFront
  if isNothing front
     then do newNode <- mkNode x
             putMVar dequeBack newNode
             putMVar dequeFront newNode
     else do newNode <- mkNode x
             updateNodeRight newNode (fromJust front)
             updateNodeLeft (fromJust front) newNode
             putMVar dequeFront newNode
