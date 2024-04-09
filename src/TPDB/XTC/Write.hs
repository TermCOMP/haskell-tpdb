{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module TPDB.XTC.Write

( document
, writeFile, renderLBS, renderText, def
)

where

import qualified TPDB.Data as D
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Text.XML as X
import Prelude hiding (writeFile)
import Text.XML (writeFile,renderLBS,renderText)
import Text.Hamlet.XML
import Data.Default (def)

document :: D.Problem D.Identifier D.Identifier -> X.Document
document p = X.Document (X.Prologue [] Nothing []) root [] where
    root = X.Element "problem"
      (M.fromList [("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                  ,("type","termination")
                  ,("xsi:noNamespaceSchemaLocation","xtc.xsd")
                  ])
      [xml|
<trs>^{trs (D.trs p) (D.full_signature p)}
$maybe s <- D.strategy p
  <strategy>^{strategy s}
|]

strategy s = case s of
  D.Full -> [xml|FULL|]

theory f = case D.fs_theory f of
  Nothing -> [xml||]
  Just x -> [xml|
    <theory>#{T.pack $ show $ x}
  |]

replacementmap f = case D.fs_replacementmap f of
  Nothing -> [xml||]
  Just (D.Replacementmap xs) -> [xml|
    <replacementmap>
      $forall x <- xs
        <entry>#{T.pack $ show $ x}
  |]

trs :: D.TRS D.Identifier D.Identifier -> D.Signature -> [X.Node]
trs rs (D.Signature sig) = [xml|
<rules>
  $forall u <- D.strict_rules rs
    ^{rule u}
  $forall u <- D.cond_rules rs
    ^{cond_rule u}
  $if not (null (D.weak_rules rs))
    <relrules>
      $forall u <- D.weak_rules rs
        ^{rule u}
<signature>
  $forall f <- sig
    <funcsym>
      <name>#{T.pack $ show $ D.fs_name f}
      <arity>#{T.pack $ show $ D.fs_arity f}
      ^{theory f}
      ^{replacementmap f}

$if not (null (D.cond_rules rs))
  <conditiontype>ORIENTED
|]

rule (l,r) = [xml|
<rule>
  <lhs>^{term l}
  <rhs>^{term r}
|]

cond_rule (l,r,cs) = [xml|
<rule>
  <lhs>^{term l}
  <rhs>^{term r}
  <conditions>
    $forall c <- cs
      ^{cond c}
|]

cond (l,r) = [xml|
<condition>
  <lhs>^{term l}
  <rhs>^{term r}
|]

term :: D.Term D.Identifier D.Identifier -> [X.Node]
term t = case t of
  D.Var v -> [xml|
<var>#{T.pack $ show v}
|]
  D.Node f args -> [xml|
<funapp>
  <name>#{T.pack $ show f}
  $forall arg <- args
     <arg>^{term arg}
|]
