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
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Control.Monad.Reader (Reader, runReader)

type State' = State Context 
type Arrow' = IOSLA (XIOState Context) XmlTree XmlTree

templateLine :: Bool -> String -> Context -> IO String
templateLine indent rawHTML context = do
    let indent' = if indent then yes else no
    (_,res) <- runIOSLA (processTemplate indent' rawHTML) (initialState context) undefined
    return $ concat res

processTemplate indent html = 
      readString [withValidate no, withParseHTML yes, withInputEncoding utf8] html
      >>> setTraceLevel 0
      >>> process 
      >>> writeDocumentToString [withIndent indent, withOutputHTML, withXmlPi no]

process :: Arrow'
process = processTopDown (
      interpolateValues 
      >>> ngClass "ng-class"
      >>> ngShow "ng-show"
      >>> ngHide "ng-hide"
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
      ) $< (getAttrValue tag &&& getUserState
            >>> arr (\(attr, context) -> runEvalToString context attr)
            )
      ) >>> removeAttr tag
    ) `when` hasElemAttr tag
     
ngShow :: String -> Arrow'
ngShow tag = 
    (
      ((\boolVal -> if boolVal then this else none) 
        $< (getAttrValue tag &&& getUserState
            >>> arr (\(attr, context) -> runEvalToBool context attr)
            )
      ) >>> removeAttr tag
    ) `when` hasElemAttr tag

ngHide :: String -> Arrow'
ngHide tag = 
    (
      ((\boolVal -> if boolVal then none else this) 
        $< (getAttrValue tag 
            >>> this &&& getUserState
            >>> arr (\(attr, context) -> runEvalToBool context attr)
            )
      ) >>> removeAttr tag
    ) `when` hasElemAttr tag

ngBind :: String -> Arrow'
ngBind tag = 
    (
      replaceChildren (
        (getAttrValue tag 
         >>> (this &&& getUserState)
         >>> arr (\(attr, context) -> runEvalToString context attr) 
        ) 
        >>> xread
      ) >>> removeAttr tag
    ) `when` hasElemAttr tag

interpolateValues :: Arrow'
interpolateValues = 
    (\context ->
      ((changeText 
            (interpolateText context)) `when` isText)
      >>>
      processAttrl (changeAttrValue (interpolateText context)) `when` isElem
    ) $< getUserState
   
interpolateText :: Context -> String -> String
interpolateText context = runEvalText context

hasElemAttr :: ArrowXml a => String -> a XmlTree XmlTree
hasElemAttr attrName = isElem >>> hasAttr attrName


