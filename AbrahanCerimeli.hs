import Data.Char (ord, chr)

--Primero que nada, armemos algunas funciones basicas 
--voy a hacer una funcion que me devuelva el numero m, teniendo en cuenta las condiciones  para que el numero elegido sea valido

--primero encontremos una forma de elegir 2 numeros primos distintos

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = (menorDivisorDesde n (k+1))

menorDivisor :: Integer -> Integer
menorDivisor n = (menorDivisorDesde n 2)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = (menorDivisor n) == n

minimoPrimo :: Integer -> Integer
minimoPrimo n | esPrimo n = n
                | otherwise = minimoPrimo(n+1)

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n | esPrimo n = n
                     | otherwise = minimoPrimoDesde (n+1)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo(n-1))

mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | mcd a b == 1 = True
                | otherwise = False

emcd a 0 = (a, 1, 0)
emcd a b = (g, s, t)
 where (g, sAnterior, tAnterior) = emcd b resto
       s = tAnterior
       t = sAnterior - tAnterior * q
       resto = mod a b
       q = div a b
-------------------------------------------------------------------------------

ecEquivalente :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m 

--esta funcion le meto una ecuacion de congruencia, y me devuelve la misma ecuacion con la propiedad que a y m son coprimos

solucionEcConPropAdic :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
 where (d, s, t) = emcd a m 
 
--me deuelve un par de enteros que es la clase de congruencia de la ecuacion 

solucionEc :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

--aca terminan las funciones que traigo de otras practicas
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--si hago que la funcion eliga sola dos numeros primos, siempre va a elegir los mismos dos, ya que siempre le doy un patron para elegir los numeros
--por ejemplo los dos mas chicos que cumplan que n*m >= 127,etc, lo que me limitaria mucho la funcion, no se como darle aleatoreidad a la funcion para que eliga 
--dos cualquiera y los evalue, por lo que voy a hacer una funcion en la que yo le de dos numeros cualquiera y se fija si son primos y si cumplen con 
--todas las condiciones extra

devolverNyM :: Integer -> Integer -> (Integer,Integer)
devolverNyM p q | (p /= q) && (esPrimo p) && (esPrimo q) && (p*q > 127) = (p*q,(p-1)*(q-1))
                | otherwise = undefined

--para simplificar las cosas, siempre voy a intentar alimentar a esta funcion con primos distintos, asi se que si me devuelve undefined
--es por que p*q <= 127, asi que tengo que cambiar esos, pero la funcion devolverNyM podes escribir cualquier cosa y analiza todas las caracteristicas que debe cumplir
--asi que ahora ya tengo n y m

--ahora vamos a hallar el exponente de descifrado e

devolverEDesde :: Integer -> Integer -> Integer -> Integer
devolverEDesde p q e | (sonCoprimos e m) && (e <= (m-2)) = e
                     | otherwise = devolverEDesde p q (e+1)
                     where (n,m) = devolverNyM p q
-- en mi caso elegi el primer e que cumpla que es coprimo con m y e <= (m-2)

devolverE :: Integer -> Integer -> Integer 
devolverE p q = devolverEDesde p q 2 
--me devuelve un exponente de descifrado e

devolverEyM :: Integer -> Integer -> (Integer,Integer)
devolverEyM p q = (e,m)
           where e = devolverE p q
                 (n,m) = devolverNyM p q
--ahora busquemos un exponente de cifrado d
--basicamente para encontrar d, tenemos que resolver una ecuacion de congruencia de la forma d*e ≡ 1 (mod m)

devolverD :: Integer -> Integer -> (Integer, Integer)
devolverD p q = solucionEcConPropAdic (e,1,m)
          where (e,m) = devolverEyM p q
-- con esta funcion resuelvo la ecuacion de congruencia


devolverDsolo :: Integer -> Integer -> Integer
devolverDsolo p q = d
           where (d,m) = devolverD p q
--hago una funcion que me devuelva solo el valor de d

--ahora si tenemos los elementos, n,m,e,d que son los que necesitamos


generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer)) 
generarClaves p q = ((n,d),(n,e))
          where e = devolverE p q
                d = devolverDsolo p q
                (n,m) = devolverNyM p q

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--ahora tengo unos mensajes y quiero encriptarlos

--primero tengo que armar una lista de numeros enteros que se forma, reemplazando cada caracter por un numero entre 0 y 127
--eso me lo da la funcion ord

pasarLetrasANumeros :: String -> [Integer]
pasarLetrasANumeros [] = []
pasarLetrasANumeros (c:cs) = (fromIntegral (ord c )) : (pasarLetrasANumeros cs)





--ahora escribamos una funcion que reemplaza cada numero a por el resto de a^d dividido n


pasarNumerosAEncriptacion :: [Integer] -> (Integer,Integer) -> [Integer]
pasarNumerosAEncriptacion [] _ = []
pasarNumerosAEncriptacion (a:as) (n,d) = (( a^d) `mod` n) : (pasarNumerosAEncriptacion as (n,d)) 


--ahora si, ya tenemos todo lo que necesitamos, puedo hacer la funcion 
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar (n,d) mensaje = pasarNumerosAEncriptacion (pasarLetrasANumeros mensaje) (n,d)
--esta funcion encripta palabras

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--la tercera funcion va a servir para desencriptar mensajes, seria algo asi como la inversa anterior
--asi que armemos las funciones necesarias

pasarEncriptacionANumeros :: [Integer] -> (Integer,Integer) -> [Integer]
pasarEncriptacionANumeros [] _ = []
pasarEncriptacionANumeros (b:bs) (n,e) = ((b^e) `mod` n) : pasarEncriptacionANumeros bs (n,e)
-- esta funcion nos da el mensaje encriptado, y lo pasamos a los numeros desarmados

--ahora vamos a hacer una funcion que agarra nuestros numeros desarmados y lo paso a letras
pasarNumerosALetras :: [Integer] -> String
pasarNumerosALetras [] = [] 
pasarNumerosALetras (n:ns) = (chr (fromInteger n)) : (pasarNumerosALetras ns)

--ahora si, armemos la funcion para desencriptar nuestras palabras
desencriptar :: (Integer, Integer) -> [Integer] -> String 
desencriptar (n,e) codigonumerico = pasarNumerosALetras (pasarEncriptacionANumeros codigonumerico (n,e))

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--ahora voy a intentar romper el codigo, deberia usar en este caso una clave privada y el codigo, pero me dan una clave publica, o sea en vez de
--(n,e) me dan (n,d), pero yo se la relacion entre n y e, asi que encuentro e
--tengo por ejemplo (n,d) = (100337, 60953)

--voy a factorizar a mano n = 100337 = 269 * 373 = p*q = n
--entonces m = (p-1)*(q-1) = (269-1)*(373-1) = 99696 = m
--por otro lado, como tengo la clave publica, tengo info de d
--entonces tengo n , d , m asi que con esto puedo encontrar e
--se que d*e ≡ 1 (mod m) ,entonces resuelvo esa ecuacion de congruencia con mi funcion y tengo e
--basicamente tengo una ecuacion d*X ≡ 1 (mod m)
--resolviendo me da el resultado e = 1001
--asi que ahora puedo intentar romepr el codigo con la clave privada (100337,1001)

--lo unico que queda es usar la funcion desencriptar (100337,1001) [33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800]
--[91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389]

--me devuelve la frase "Cual es tu pizza favorita?"

--ahora encriptemos, con la clave publica 
--uso la funcion : encriptar (100337, 60953) "cualestupizzafavorita?"
--me devuelve
--[74457,38913,58255,99961,23881,220,1606,38913,78982,18800,91658,91658,58255,96593,58255,438,22839,28700,18800,1606,58255,48389]

