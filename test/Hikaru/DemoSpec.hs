{-|
Module      :  Hikaru.DemoSpec
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

Smoke tests coverting a simple demo site.
-}

module Hikaru.DemoSpec
  ( spec
  )
where
  import BasePrelude

  import Hikaru.Demo
  import Hikaru.Test


  spec :: Spec
  spec = do
    describe "GET /" do
      context "by default" do
        it "responds with 200 and text/html" do
          runDemo do
            resp <- get "/" []
            assertStatus 200 resp
            assertHeader hContentType "text/html; charset=utf8" resp
            assertBodyContains "<h1>Welcome" resp
            assertBodyContains " 1. " resp

      context "when asked for text/plain" do
        it "responds with 200 and text/plain" do
          runDemo do
            resp <- get "/" [(hAccept, "text/plain")]
            assertStatus 200 resp
            assertHeader hContentType "text/plain; charset=utf8" resp
            assertBodyContains "Welcome" resp
            assertBodyContains " 2. " resp

    describe "GET /404" do
      it "responds with 404" do
        runDemo do
          resp <- get "/404" []
          assertStatus 404 resp
          assertBodyContains "404" resp

    describe "GET /hello/<arg>" do
      it "greets caller" do
        runDemo do
          resp <- get "/hello/Tester" []
          assertStatus 200 resp
          assertHeader hContentType "text/plain; charset=utf8" resp
          assertHeader hCacheControl "no-store" resp
          assertBody "Hello, Tester!" resp

    describe "POST /api/echo" do
      it "echoes JSON payload" do
        runDemo do
          resp <- post "/api/echo" [(hContentType, "application/json")] "[1, 2]"
          assertStatus 200 resp
          assertBody "[1,2]" resp


  demo :: Application
  demo = unsafePerformIO makeDemo


  runDemo :: Session a -> IO a
  runDemo s = runSession s demo


-- vim:set ft=haskell sw=2 ts=2 et:
