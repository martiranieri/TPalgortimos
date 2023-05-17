{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use foldl" #-}
-- Completar con los datos del grupo
--
-- Nombre de Grupo: Undefined
-- Integrante 1: Biegun Agustina, agustinabgn@gmail.com, 1101/22
-- Integrante 2: Lara Facundo Ignacio, facuriver57@gmail.com, 647/23
-- Integrante 3: Miyasaki Camila Denise, camimiyasaki@gmail.com, 1063/22
-- Integrante 4: Ranieri Martina Belén, martubranieri@gmail.com, 1118/22


module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])


-- Funciones basicas
usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios:
-- 1.
-- Devuelve una lista con todos los nombres de todos los usuarios registrados en la red
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us, _, _) = proyectarNombres us

-- Realiza la recursión de la lista de usuarios registrados en la red para saber su nombre
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (u : us) = nombreDeUsuario u : proyectarNombres us


-- 2.
-- Devuelve lista de usuarios que están relacionados con el usuario ingresado como parámetro
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, rs, _ ) u = relacionadoCon rs u

-- Realiza la recursión sobre las relaciones de la red comparando si en alguna de las dos posiciones se encuentra
-- el usuario ingresado. Si está, agrega a la lista el nombre del usuario con el que está relacionado
relacionadoCon :: [Relacion] -> Usuario -> [Usuario]
relacionadoCon [] _ = []
relacionadoCon (r : rs) u
    | snd r == u = fst r : relacionadoCon rs u
    | fst r == u = snd r : relacionadoCon rs u
    | otherwise = relacionadoCon rs u


-- 3.
-- Devuelve el número de personas relacionadas en la red con el usuario ingresado como parámetro 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

longitud :: [t] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs


-- 4.
-- Devuelve al usuario con más relaciones en la red ingresada como parámetro
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = maximoDeAmigos (cantidadDeAmigosTodos red (usuarios red))

-- Devuelve una lista de duplas. Para cada usuario de la red devuelve, en la primer posición de la dupla,
-- cuántas relaciones tiene y, en la segunda, los datos del usuario. 
cantidadDeAmigosTodos :: RedSocial -> [Usuario] -> [(Int, Usuario)]
cantidadDeAmigosTodos _ [] = []
cantidadDeAmigosTodos red (u : us)  = (cantidadDeAmigos red u, u) : cantidadDeAmigosTodos red us

-- Compara primeros dos elementos de la lista hasta llegar a uno solo, el que tiene más relaciones
maximoDeAmigos :: [(Int, Usuario)] -> Usuario
maximoDeAmigos [x] = snd x 
maximoDeAmigos (u : us)
    | fst u >= fst (head us) = maximoDeAmigos (u : tail us)
    | otherwise = maximoDeAmigos us


-- 5.
-- Si existe al menos un usuario con un millón o más de relaciones, devuelve True. En otro caso False
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = tieneMasDeUnMillonAmigos (cantidadDeAmigosTodos red (usuarios red))

-- Recibe lista que en cada dupla tiene: (cantidad de relaciones, datos del usuario con esas relaciones)
-- si algún usuario tiene al menos diez relaciones (primer posición de cada dupla) devuelve True y sino False
tieneMasDeUnMillonAmigos :: [(Int, Usuario)]  -> Bool
tieneMasDeUnMillonAmigos [] = False
tieneMasDeUnMillonAmigos (u : us) 
    | fst u >= 10 = True
    | otherwise = tieneMasDeUnMillonAmigos us


-- 6.
-- Devuelve una lista con todas las publicaciones del usuario en la red
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, ps) u = publicacionesDeAux ps u

-- Realiza recursión dentro de todas las publicaciones de la red devolviendo en la lista únicamente con las del usuario ingresado
publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] _ = []
publicacionesDeAux (p : ps) u 
    | usuarioDePublicacion p == u = p : publicacionesDeAux ps u
    | otherwise = publicacionesDeAux ps u


-- 7.
-- Dada una red social y un usuario, devuelve una lista con las publicaciones a las que ese usuario le dio like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, ps) u = leGustaA ps u

-- Realiza recursión sobre todas las publicaciones y devuelve lista con las publicaciones que el usuario le dio like
leGustaA :: [Publicacion] -> Usuario -> [Publicacion]
leGustaA [] _ = []
leGustaA (p : ps) u
    | pertenece u (likesDePublicacion p) = p : leGustaA ps u
    | otherwise = leGustaA ps u

-- Devuelve True si el elemento está en la lista y False en otro caso
pertenece :: Eq t => t -> [t] -> Bool 
pertenece _ [] = False 
pertenece n (x : xs) 
    | n == x = True
    | otherwise = pertenece n xs

-- 8.
-- Dados dos usuarios y una red, decide si ambos usuarios le dieron like a las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

-- Decide si, dadas dos listas, estas tienen los mismos elementos (sin importar el orden)
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos x s 
    | longitud x == longitud s = incluidosEn x s
    | otherwise = False

incluidosEn :: (Eq t) => [t] -> [t] -> Bool
incluidosEn [] _ = True
incluidosEn (x : xs) s 
    | not (pertenece x s) = False
    | otherwise = incluidosEn xs s

-- 9.
-- Decide si existe entre los amigos de un usuario en una red, alguien que le haya dado me gusta a todas las publicaciones del mismo.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = aAlguienLeGustanTodasLasPub (amigosDe red u) (publicacionesDe red u)

-- Hace recursión sobre una lista de usuarios y decide si alguno le dio me gusta a todas las publicaciones de otra lista.
aAlguienLeGustanTodasLasPub :: [Usuario] -> [Publicacion] -> Bool
aAlguienLeGustanTodasLasPub [] _ = False
aAlguienLeGustanTodasLasPub (u : us) ps 
    | leGustanTodasLasPub ps u = True
    | otherwise = aAlguienLeGustanTodasLasPub us ps

-- Decide si un usuario le dio me gusta a todas las publicaciones de una lista.
leGustanTodasLasPub :: [Publicacion] -> Usuario -> Bool
leGustanTodasLasPub [] u = True
leGustanTodasLasPub (p : ps) u
    | pertenece u (likesDePublicacion p) = leGustanTodasLasPub ps u
    | otherwise = False
 
 
-- 10.
-- Dados dos usuarios dentro de una misma red, decide si es posible llegar del primero al segundo mediante una cadena de relaciones
-- Para eso, se fija si los usuarios se relacionan directamente y si no, prueba si la cadena continua con alguno de los amigos del primero
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 
    | sonAmigos u1 u2 red = True
    | amigosDe red u2 == [] = False 
    | otherwise = hayCadenaDeAmigos (eliminarRelacionYUser red u1) (amigosDe red u1) u2

-- Dada una lista y un usuario, prueba si se forma una cadena de relaciones entre algun usuario de la lista y el usuario dado
hayCadenaDeAmigos :: RedSocial -> [Usuario] -> Usuario -> Bool
hayCadenaDeAmigos _ [] _ = False
hayCadenaDeAmigos red (u : us) u2 = existeSecuenciaDeAmigos red u u2 

sonAmigos :: Usuario -> Usuario -> RedSocial -> Bool
sonAmigos u1 u2 red = pertenece u1 (amigosDe red u2)

-- Elimina a un usuario y a sus relaciones de una red social
eliminarRelacionYUser :: RedSocial -> Usuario -> RedSocial
eliminarRelacionYUser (us, rs, ps) u = (eliminar u us, eliminarTodos (relacionesDe u red) rs , ps)
                                     where red = (us, rs, ps)

-- Devuelve la lista de relaciones de una red en las que participa el usuario
relacionesDe :: Usuario -> RedSocial -> [Relacion]
relacionesDe _ (_, [], _) = []
relacionesDe u (us, r:rs, ps) 
    | fst r == u || snd r == u = r : relacionesDe u (us, rs, ps)
    | otherwise = relacionesDe u (us, rs, ps)

eliminarTodos :: Eq t => [t] -> [t] -> [t]
eliminarTodos [] xss = xss
eliminarTodos (x : xs) xss = eliminarTodos xs (eliminar x xss)

eliminar :: Eq t => t -> [t] -> [t]
eliminar _ [] = []
eliminar n (x : xs) 
    | not (pertenece n (x:xs)) = x : xs
    | n == x = xs
    | otherwise = x : eliminar n xs