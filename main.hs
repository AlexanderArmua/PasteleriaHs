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
    criterioDePaciencia :: [Orden] -> Int
} 

pepe = Cliente "Pepe" constante
jose = Cliente "Jose" mucha
alex = Cliente "Alex" (lineal 5)
luca = Cliente "Luca" (lineal 1)

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

{- Punto 2: 
    a) Crear los criterios de paciencia. 
        constante: Espera hasta 8 horas.
        lineal: Cantidad de horas propia mas 1 hora por torta.
        mucha: <=2 tortas => 2 horas; else; 3 horas por torta.
    b) Dado un cliente saber cuanto puede tardar como maximo
-}

-- Punto a

constante :: [Orden] -> Int
constante ordenes = 8

-- Espera + LA SUMA del mapeo de ordenes a CANTIDAD
-- Transforma un array de ordenes en array de cantidades de tortas y luego los suma todos
lineal :: Int -> [Orden] -> Int
lineal esperaBase ordenes = esperaBase + sum (map cantidad ordenes)

mucha :: [Orden] -> Int
mucha [_] = 2
mucha [_,_] = 2
mucha ordenes = 3 * sum (map cantidad ordenes)

-- Punto b

-- Ademas, necesito saber cuales son las ordenes de un cliente
ordenesCliente :: Cliente -> [Orden] -> [Orden]
ordenesCliente cliente = filter ((nombre cliente ==).nombreCliente)

    -- ordenesClienteDeprecated :: Cliente -> [Orden] -> [Orden]
    -- ordenesClienteDeprecated cliente ordenes = filter ((nombre cliente ==).nombreCliente) ordenes

cuantoPuedoTardar :: Cliente -> Int
cuantoPuedoTardar cliente = criterioDePaciencia cliente (ordenesCliente cliente ordenesPasteleria)


{- Punto 3:
    a) Dado un ingrediente decir cuanto se necesita
    b) Saber si la pasteleria tiene poco trabajo
    c) Saber cuanto tarda una orden en la pasteleria
    d) Desarrollar la funcion clienteSatisfecho
-}




