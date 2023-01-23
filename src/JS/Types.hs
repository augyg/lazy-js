module JS.Types where

type JSVal = String -- todo: Text
type Name = String 
type BaseUrl = Url
type Url = String

data JSType = JSONString String | JSNumber String | JSString String
