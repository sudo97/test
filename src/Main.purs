module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign (unsafeFromForeign)
import Web.DOM.Document (createElement)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, EventType(..), target)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (toBlob, name)
import Web.File.FileList (item)
import Web.File.FileReader (fileReader, readAsText)
import Web.File.FileReader as FileReader
import Web.HTML (window)
import Web.HTML.HTMLAnchorElement (fromElement, setDownload, setTarget, toHTMLHyperlinkElementUtils)
import Web.HTML.HTMLAnchorElement as Anchor
import Web.HTML.HTMLDocument (toDocument, toParentNode)
import Web.HTML.HTMLHyperlinkElementUtils (setHref)
import Web.HTML.HTMLInputElement (fromEventTarget, files)
import Web.HTML.Window (document)
import Web.URL (fromAbsolute, toString)


main :: Effect Unit
main = do
  doc <- window >>= document
  querySelector (QuerySelector "#test") (toParentNode doc) >>= case _ of
    Just rootElt -> do
      listnr <- eventListener listener
      addEventListener (EventType "change") listnr false (Element.toEventTarget rootElt)
    Nothing -> pure unit

listener :: Event -> Effect Unit
listener evt = case target evt >>= fromEventTarget of
  Just elt -> do
    mlist <- files elt
    case mlist >>= item 0 of
      Just file -> do
         r <- fileReader
         readAsText (toBlob file) r
         listnr <- eventListener (printToOut)
         addEventListener (EventType "load") listnr false (FileReader.toEventTarget r)

         doc <-  window >>= document
         hiddenElement <- createElement "a" (toDocument doc)
         case fromElement hiddenElement of
           Just anch -> do
            case encodeURI $ "data:attachment/text," <> (name file) of
              Just uri -> do
                setHref uri (toHTMLHyperlinkElementUtils anch)
              _ -> log "failed to encode URI"
            setTarget "_blank" anch
            setDownload "myFile.csv" anch
            querySelector (QuerySelector "#link") (toParentNode doc) >>= case _ of
              Just linkElt -> do
                appendChild (Anchor.toNode anch) (Element.toNode linkElt)
              Nothing -> pure unit
           _ -> pure unit
      _ -> pure unit
  _ -> pure unit


encodeURI :: String -> Maybe String
encodeURI s = toString <$> fromAbsolute s

printToOut :: Event -> Effect Unit
printToOut evt = do
  win <- window
  doc <- document win
  querySelector (QuerySelector "#out") (toParentNode doc) >>= case _ of
    Just outElt -> do
      case target evt >>= FileReader.fromEventTarget of
        Just r -> do
          txt <- unsafeFromForeign <$> FileReader.result r
          setTextContent txt (Element.toNode outElt)
        _ -> pure unit
    _ -> pure unit