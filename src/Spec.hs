module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "Sea un turista como Sasuke que viaja solo, entonces bajará su cansancio en 5 unidades" $ do
      irAlaPlaya (Turista (100,50) True ["Japones","Español"]) `shouldBe` Turista (95,50) True ["Japones","Español"]
    it "Sea un turista meloso que no viaja solo, entonces bajará su stress en 1 unidad" $ do
      irAlaPlaya (Turista (4,10) False ["Castellano"]) `shouldBe` Turista (4,10) False ["Castellano"]

