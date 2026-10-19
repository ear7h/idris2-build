module Build.HTML

import Data.List.Quantifiers

import Build
import Build.LazyFile

public export
ElCons : List Type -> Type
ElCons as =
  HList as -> LazyFile

export
mkEl :
  { default [] attrs : List (String, String) } ->
  (fa : All ToFragment as) =>
  String -> HList as -> LazyFile
mkEl tag body =
  let attrs' = concat $ map (\(x,y) => " \{x}=\"\{y}\"") attrs
  in ["<\{tag}\{attrs'}>" , concat body , "</\{tag}>"]


public export
SelfClosingElCons : Type
SelfClosingElCons = List (String, String) -> String

mkSelfClosingEl : String -> List (String, String) -> String
mkSelfClosingEl tag attrs =
  "<\{tag}" ++ (concat $ map (\(x,y) => " \{x}=\"\{y}\"") attrs) ++ "/>"

export
html : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
html = ("<!DOCTYPE html>"::) . mkEl { attrs } "html"

export
head : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
head = mkEl { attrs } "head"

export
title : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
title = mkEl { attrs } "title"

export
meta : SelfClosingElCons
meta = mkSelfClosingEl "meta"

export
link : SelfClosingElCons
link = mkSelfClosingEl "link"

export
body : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
body = mkEl { attrs } "body"

export
h1 : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
h1 = mkEl { attrs } "h1"

export
h2 : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
h2 = mkEl { attrs } "h2"

export
h3 : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
h3 = mkEl { attrs } "h3"

export
p : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
p = mkEl { attrs } "p"

export
div : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
div = mkEl { attrs } "div"

export
br : SelfClosingElCons
br = mkSelfClosingEl "br"

export
ol : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
ol = mkEl { attrs } "ol"

export
ul : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
ul = mkEl { attrs } "ul"

export
li : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
li = mkEl { attrs } "li"

export
olist : (prfs : All ToFragment as) => { default [] attrs : List (String, String) } -> ElCons as
olist xs =
  let ys = forget $ All.zipPropertyWith (\prf, x=> li @{[prf]} [x]) prfs xs
  in let prfs' = allStatic FLazyFile ys
  in ol $ (heterogenize ys)

export
olist' : ToFragment a => { default [] attrs : List (String, String) } -> List a -> LazyFile

export
ulist : (prfs : All ToFragment as) => { default [] attrs : List (String, String) } -> ElCons as
ulist xs =
  let ys = forget $ All.zipPropertyWith (\prf, x=> li @{[prf]} [x]) prfs xs
  in let prfs' = allStatic FLazyFile ys
  in ul $ (heterogenize ys)

export
ulist' : ToFragment a => { default [] attrs : List (String, String) } -> List a -> LazyFile

export
blockquote : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
blockquote = mkEl { attrs } "blockquote"

export
cite : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
cite = mkEl { attrs } "cite"

export
figure : All ToFragment as => { default [] attrs : List (String, String) } -> ElCons as
figure = mkEl { attrs } "figure"

export
figcaption :
  All ToFragment as =>
  { default [] attrs : List (String, String) } ->
  ElCons as
figcaption = mkEl { attrs } "figcaption"

export
a : All ToFragment as =>
  { default [] attrs : List (String, String) } ->
  ElCons as
a = mkEl { attrs } "a"

export
ahref :
  All ToFragment as =>
  { default [] attrs : List (String, String) } ->
  String -> HList as -> LazyFile
ahref link xs = a { attrs = ("href", link)::attrs } xs

export
i : All ToFragment as =>
  { default [] attrs : List (String, String) } ->
  ElCons as
i = mkEl { attrs } "i"

export
pre : All ToFragment as =>
  { default [] attrs : List (String, String) } ->
  ElCons as
pre = mkEl { attrs } "pre"

export
nav : All ToFragment as =>
  { default [] attrs : List (String, String) } ->
  ElCons as
nav = mkEl { attrs } "nav"

export
footer : All ToFragment as =>
  { default [] attrs : List (String, String) } ->
  ElCons as
footer = mkEl { attrs } "footer"
