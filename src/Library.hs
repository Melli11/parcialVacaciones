module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
type Cansancio = Number
type Stress = Number
-- type Energia = (Cansancio,Stress)

data Turista = Turista {
    cansancio :: Number,
    stress :: Number,
    viajaSolo :: Bool,
    idiomasQueHabla :: [String]
}deriving(Show,Eq)

-- FUNCIONES AUXILIARES
modificarStressEn :: Number -> Turista -> Turista
-- modificarStressEn cantidad (Turista (cansancio,stress) compania idiomas ) = Turista (cansancio,stress + cantidad) compania idiomas --Para la version con tupla energia
modificarStressEn cantidad (Turista cansancio stress compania idiomas ) = Turista cansancio (stress + cantidad) compania idiomas
modificarCansancioEn :: Number -> Turista -> Turista
-- modificarCansancioEn cantidad (Turista (cansancio,stress) compania idiomas ) = Turista (cansancio + cantidad,stress) compania idiomas --Para la version con tupla energia
modificarCansancioEn cantidad (Turista cansancio stress compania idiomas ) = Turista (cansancio + cantidad) stress  compania idiomas


-- modificarCansancioEn cantidad 

meloso :: Turista
meloso = Turista 4 10 False ["Castellano"]

sasuke = Turista 100 50 True ["Japones","Español"]


-- irAlaPlaya :: Turista -> Turista
-- irAlaPlaya unTurista
--     | viajaSolo unTurista = unTurista {energia =  bajarCansancioEn 5 (energia unTurista)}
--     | otherwise = unTurista {energia = bajarStressEn 1 (energia unTurista)}

irAlaPlaya :: Turista -> Turista
irAlaPlaya unTurista
    | viajaSolo unTurista = modificarCansancioEn (-5) unTurista
    | otherwise = modificarStressEn (-1) unTurista

apreciarAlgunElementoDelPaisaje :: String -> Turista -> Turista
-- apreciarAlgunElementoDelPaisaje elemento unTurista = unTurista {energia = bajarStressEn (length elemento) (energia unTurista)}

-- apreciarAlgunElementoDelPaisaje  = modificarStressEn.((-1)*length)
apreciarAlgunElementoDelPaisaje elemento = modificarStressEn ( (-1)* length elemento)

salirAHablarUnIdioma :: String -> Turista -> Turista
salirAHablarUnIdioma unIdioma (Turista cansancio stress _ idiomasQueSabe) = Turista cansancio stress True (unIdioma:idiomasQueSabe)

-- caminar :: Number -> Turista -> Turista
-- caminar cantidadDeMinutos unTurista = unTurista {energia = afectarSegunTiempo cantidadDeMinutos (energia unTurista) }

caminar :: Number -> Turista -> Turista
caminar cantidadDeMinutos  = modificarCansancioEn (div cantidadDeMinutos 4) . modificarStressEn (div (-cantidadDeMinutos) 4)  

-- afectarSegunTiempo :: Number -> (Number, Number) -> (Number, Number)
-- afectarSegunTiempo tiempo (cansancio,stress) =  (cansancio + tiempo/4,stress - tiempo/4)

data NiveldeLaMarea = Fuerte | Moderada | Tranquila

-- paseoEnBarco ::  NiveldeLaMarea -> Turista -> Turista
paseoEnBarco :: NiveldeLaMarea -> Turista -> Turista
paseoEnBarco Fuerte unTurista = (modificarStressEn 6 . modificarCansancioEn 10) unTurista
paseoEnBarco Tranquila unTurista = caminar 10 . apreciarAlgunElementoDelPaisaje "mar" $ salirAHablarUnIdioma "aleman" unTurista
paseoEnBarco Moderada unTurista = unTurista


-- Crear un modelo para los turistas y crear los siguientes tres ejemplos:
-- Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
ana = Turista 0 21 True ["Español"]

-- Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15
-- unidades de cansancio y stress.
beto = Turista 15 15 False ["Aleman"]
cathi = Turista 15 15 False ["Aleman","Catalan"]

-- 2. Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga
-- falta modificar las funciones existentes. Además:

type Excursion = Turista -> Turista

-- a. Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los
-- efectos propios de la excursión, reduce en un 10% su stress.


stressDelTuristaEn10Menos :: Turista -> Turista
stressDelTuristaEn10Menos  (Turista cansancio stress compania idiomas ) =  Turista cansancio (stress*0.90) compania idiomas

hacerUnaExcursion :: Turista -> Excursion -> Turista 
hacerUnaExcursion  unTurista =  stressDelTuristaEn10Menos .  aplicarExcursion unTurista 

aplicarExcursion unTurista excursion = excursion unTurista



-- b. Dada la función
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- deltaExcursionSegun indice unTurista excursion = 

-- Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión
-- determine cuánto varió dicho índice después de que el turista haya hecho la excursión.
-- Llamamos índice a cualquier función que devuelva un número a partir de un turista
