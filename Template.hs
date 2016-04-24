{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Template where
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Options.Applicative ((<>))
import qualified Options.Applicative as O
import Control.Applicative
import Eval
import Expression
import Text.XML.HXT.Core
import Control.Monad.State (State, runState)
import Control.Monad.Reader (Reader, runReader)

type State' = State Context 
type Arrow' = IOStateArrow Context XmlTree XmlTree

run rawHTML context = do
    (_,res) <- runIOSLA (processTemplate rawHTML) context undefined
    return res

processTemplate html = 
    readString [withValidate no, withParseHTML yes, withInputEncoding utf8] html
    >>> setTraceLevel 0
    >>> process 
    >>> writeDocumentToString [withIndent yes, withOutputHTML, withXmlPi no]

process :: Arrow'
process = processTopDown (
      >>> interpolateValues 
      >>> ngClass 
      >>> ngShow 
      >>> ngHide 
      >>> ngBind "ng-bind"
    )

ngClass :: String -> Arrow'
ngClass tag = 
    (
      ((\newClassNames -> 
        addAttr "class" newClassNames `when` neg (hasName "class")
        `orElse`
        processAttrl (
            changeAttrValue (\old -> mconcat [old, " ", newClassNames]) `when` hasName "class"
          )
              -- addAttr "class" classNames
      ) $< (getAttrValue tag 
            >>> this &&& getState
            >>> arr2 (\(attr, context) -> runReader (exprEvalToString attr) context)
            )
      ) >>> removeAttr tag
    ) `when` hasNgAttr tag
     
ngShow :: String -> Arrow'
ngShow tag = 
    (
      ((\boolVal -> if boolVal then this else none) 
        $< (getAttrValue tag 
            >>> this &&& getState
            >>> arr2 (\(attr, context) -> runReader (exprEvalToBool attr) context)
            )
      ) >>> removeAttr tag
    ) `when` hasNgAttr tag

ngHide :: String -> Arrow'
ngHide tag = 
    (
      ((\boolVal -> if boolVal then none else this) 
        $< (getAttrValue tag 
            >>> this &&& getState
            >>> arr2 (\(attr, context) -> runReader (exprEvalToBool attr) context)
            )
      ) >>> removeAttr tag
    ) `when` hasNgAttr tag

ngBind :: String -> Arrow'
ngBind tag = 
    (
      --txt $< (getAttrValue tag >>> arr (exprEvalToString context) )
      replaceChildren (
        (getAttrValue tag 
          >>> this &&& getState
          >>> arr2 (\(attr, context) -> runReader (exprEvalToString attr) context) 
        ) 
        >>> xread
      ) >>> removeAttr tag
    ) `when` hasNgAttr tag

interpolateValues :: Arrow'
interpolateValues = 
      ( (changeText 
            (interpolateText context)
        ) 
        `when` isText)
      >>>
      processAttrl (changeAttrValue (interpolateText context)) `when` isElem
   
interpolateText :: Context -> String -> String
interpolateText context = mconcat .  map (evalText context) . parseText

hasNgAttr :: ArrowXml a => String -> a XmlTree XmlTree
hasNgAttr attrName = isElem >>> hasAttr attrName


