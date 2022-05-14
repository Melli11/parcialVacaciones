module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
type Cansancio = Number  
type Stress = Number  
type Energia = (Cansancio,Stress)

data Turista = Turista {
    energia :: (Cansancio,Stress),
    viajaSolo :: Bool,
    idiomasQueHaba :: [String]
}deriving(Show,Eq)

meloso :: Turista
meloso = Turista (4,10) False ["Castellano"]

sasuke = Turista (100,50) True ["Japones","Español"]
bajarCansancioEn' unTurista = subtract.fst.energia --ver correccion    

bajarCansancioEn :: Number -> Energia -> Energia
bajarCansancioEn  cantidad (cansancio,stress) =  (cansancio - cantidad, stress)

bajarStressEn :: Number -> Energia -> Energia
bajarStressEn cantidad (cansancio,stress) = (cansancio, stress - cantidad)

irAlaPlaya :: Turista -> Turista
irAlaPlaya unTurista 
    | viajaSolo unTurista = unTurista {energia =  bajarCansancioEn 5 (energia unTurista)}
    | otherwise = unTurista {energia = bajarStressEn 1 (energia unTurista)}



-- apreciarAlgunElementoDelPaisaje elemento unTurista =  