--------LENGUAJES DE PROGRAMACION - PROYECTO NRO II----------
--------JUEGO CARGA LAMBDA-BURRA - HASKELL-----------------------------
--REALIZADO POR:
--
--
module LambdaBurra where
import Cards-----------------------Importando el modulo Cards que contiene la data y funciones de las cartas

data Player = Lambda | You --------Tipo de dato Player (Jugadores) Lambda/You

---------------FUNCION QUE COMPARA LA PINTA DE DOS CARTAS Y DEVUELVE TRUE SI SON IGULES O FALSE SI NO LO SON-------
compara :: Card -> Card -> Bool
compara (Card v s) (Card v1 s1) = if pinta (s) == pinta (s1) then True else False

---------------FUNCION QUE AGREGA LAS CARTAS INICIALES A LAS MANOS DE LOS JUGADORES--------------------------------
iniciar :: Int -> [Card] -> Hand -> Hand
iniciar i (card) (H(hand)) = if i == 3 then (H(hand)) else iniciar (i+1) (tomarCarta(card)) (llenarMano (card) (H(hand)))

---------------FUNCION QUE DEVOLVERA TRUE SI LA CARTA DE YOU ES MAYOR------------------------------------
mayor :: Card -> Card -> Bool
mayor x y = if ( (valor x) > (valor y) ) then True else False

---------------FUNCION QUE VERIFICA SI EN TU MANO HAY UNA CARTA CON LA PINTA DE LA MESA------------------
verificarMano :: Hand -> Card -> Bool
verificarMano (H([])) _ = False
verificarMano (H(x:xs)) y = if ( (compara x y) == True) then True else verificarMano (H(xs)) y

------------------------------------------------------PRINCIPAL------------------------------------------
main :: IO ()
main = do

  let handYou = empty ---------Crea la mano vacia para You
  let handLambda = empty ---------Crea una mano vacia para Lambda
  let mazo = baraja      ---------Crea el mazo inicial
  putStrLn"   ************WELCOME TO LAMBDA-BURRA*************   "