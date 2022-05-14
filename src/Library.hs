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
    idiomasQueHabla :: [String]
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


apreciarAlgunElementoDelPaisaje :: String -> Turista -> Turista
apreciarAlgunElementoDelPaisaje elemento unTurista = unTurista {energia = bajarStressEn (length elemento) (energia unTurista)}


salirAHablarUnIdioma :: String -> Turista -> Turista
salirAHablarUnIdioma unIdioma (Turista energia _ idiomasQueSabe) = Turista energia True (unIdioma:idiomasQueSabe)

caminar :: Number -> Turista -> Turista
caminar cantidadDeMinutos unTurista = unTurista {energia = afectarSegunTiempo cantidadDeMinutos (energia unTurista) }

afectarSegunTiempo :: Number -> (Number, Number) -> (Number, Number)
afectarSegunTiempo tiempo (cansancio,stress) =  (cansancio + tiempo/4,stress - tiempo/4)

data NiveldeLaMarea = Fuerte | Moderada | Tranquila

-- paseoEnBarco ::  NiveldeLaMarea -> Turista -> Turista
paseoEnBarco :: NiveldeLaMarea -> Turista -> Turista
paseoEnBarco Tranquila unTurista = caminar 10 . apreciarAlgunElementoDelPaisaje "mar" $ salirAHablarUnIdioma "aleman" unTurista 
paseoEnBarco Fuerte (Turista (cansancio,stress) conCompañia idioma )  = Turista (cansancio + 10 ,stress + 6) conCompañia idioma
paseoEnBarco Moderada unTurista = unTurista 


-- Crear un modelo para los turistas y crear los siguientes tres ejemplos:
-- Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
ana = Turista (0,21) True ["Español"]

-- Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15
-- unidades de cansancio y stress.
beto = Turista (15,15) False ["Aleman"]
cathi = Turista (15,15) False ["Aleman","Catalan"]

