module Reposteria where

data Receta = Receta {
    cucharadasDeHarina :: Int,
    cucharadasDeAzucar :: Int,
    cucharadasDeLeche  :: Int,
    cucharadasDeAceite :: Int,
    cantidadDeHuevos   :: Int
} deriving (Show)

recetaTradicional = Receta 20 20 20 20 2
recetaPromo = Receta 10 10 10 2 5
recetaAceitosa = Receta 10 5 10 50 3

data Orden = Orden {
    nombreCliente :: String,
    receta        :: Receta,
    cantidad      :: Int,
    fueCancelada  :: Bool
} deriving (Show)

ordenPepe = Orden "Pepe" recetaTradicional 2 False
ordenJose = Orden "Jose" recetaPromo 20 True
ordenAlex = Orden "Alex" (Receta 15 5 25 9 8) 4 False
ordenLuca = Orden "Luca" (Receta 0 1 1 2 3) 5 False

data Cliente = Cliente {
    nombre              :: String,
    criterioDePaciencia :: Int
} deriving (Show)

--criterioDePaciencia :: [Orden] -> Int
{-
pepe = Cliente "Pepe" constante
jose = Cliente "Jose" mucha
alex = Cliente "Alex" (lineal 5)
luca = Cliente "Luca" (lineal 1)
-}
ordenesPasteleria = [ordenPepe, ordenJose, ordenAlex, ordenLuca]

{-  Punto 1: Saber si la receta sale bien.
    a. No es acesitoso.
        Menos de 30 cucharadas.

    b. Tiene cantidad justa de huevos.
        1 huevo cada 10 de harina.

    c. Azucar, haria y leche son proporcionados.
        Todas las cantidades son pares.
-}

recetaNoAceitosa :: Receta -> Bool
recetaNoAceitosa receta = cucharadasDeAceite receta < 30

cantidadJustaHuevos :: Receta -> Bool
cantidadJustaHuevos receta = cantidadDeHuevos receta * 10 - cucharadasDeHarina receta == 0

ingredientesProporcionales :: Receta -> Bool
ingredientesProporcionales receta = even (cucharadasDeAzucar receta) && even (cucharadasDeHarina receta) && even (cucharadasDeLeche receta)

saleBien :: Receta -> Bool
saleBien receta = recetaNoAceitosa receta && cantidadJustaHuevos receta && ingredientesProporcionales receta





