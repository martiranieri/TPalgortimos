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
nombresDeUsuarios (usuarios, _, _) = proyectarNombres usuarios

-- Realiza la recursión de la lista de usuarios registrados en la red para saber su nombre
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (xs:xss) = nombreDeUsuario xs : proyectarNombres xss

-- 2.
-- Devuelve lista de usuarios que están relacionados con el usuario ingresado como parámetro
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, relaciones, _ ) usuario = amigosDeAux relaciones usuario

-- Realiza la recursión sobre las relaciones de la red comparando si en alguna de las dos posiciones se encuentra
-- el usuario ingresado. Si está, agrega a la lista el nombre del otro usuario (con el que está relacionado)
amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] _ = []
amigosDeAux (xs:xss) usuario
    | snd xs == usuario = fst xs : amigosDeAux xss usuario
    | fst xs == usuario = snd xs : amigosDeAux xss usuario
    | otherwise = amigosDeAux xss usuario


-- 3.
-- Devuelve el número de personas relacionadas en la red con el usuario ingresado como parámetro 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos redSocial usuario = longitud (amigosDe redSocial usuario)

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xss) = 1 + longitud xss


-- 4.
-- Devuelve al usuario con más relaciones en la red ingresada como parámetro
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = maximoDeAmigos (cantidadDeAmigosTodos red (usuarios red))

-- Devuelve una lista con duplas dentro. En la primer posición, cuántas relaciones tiene el usuario
-- y en la segunda los datos del usuario. Lo realiza con recusión
cantidadDeAmigosTodos :: RedSocial -> [Usuario] -> [(Int, Usuario)]
cantidadDeAmigosTodos _ [] = []
cantidadDeAmigosTodos y (xs:xss)  = (cantidadDeAmigos y xs, xs) : cantidadDeAmigosTodos y xss

-- Compara primeros dos elementos de la lista hasta llegar a uno solo, el que tiene más relaciones
maximoDeAmigos :: [(Int, Usuario)] -> Usuario
maximoDeAmigos [x] = snd x 
maximoDeAmigos (x:xs)
    | fst x >= fst (head xs) = maximoDeAmigos (x : tail xs)
    | otherwise = maximoDeAmigos xs

-- 5.
-- Si existe al menos un usuario con un millón o más de relaciones, devuelve True. En otro caso False
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = tieneMasDeUnMillonAmigos (cantidadDeAmigosTodos red (usuarios red))

-- Recibe lista que en cada dupla tiene: (cantidad de relaciones, datos del usuario con esas relaciones)
-- si algún usuario tiene al menos un millón de relaciones (primer posición de cada dupla) devuelve True y sino False
tieneMasDeUnMillonAmigos :: [(Int, Usuario)]  -> Bool
tieneMasDeUnMillonAmigos [] = False
tieneMasDeUnMillonAmigos (xs:xss) 
    | fst xs >= 10 = True
    | otherwise = tieneMasDeUnMillonAmigos xss


-- 6.
-- Dada una red social y un usuario, devuelve una lista con todas las publicaciones de ese usuario en la red ingresada
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_,_,publicaciones) usuario = publicacionesDeAux publicaciones usuario

-- Realiza recursión dentro de todas las publicaciones de la red devolviendo en la lista únicamente las del usuario ingresado
publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] _ = []
publicacionesDeAux (xs:xss) usuario 
    | usuarioDePublicacion xs == usuario = xs : publicacionesDeAux xss usuario
    | otherwise = publicacionesDeAux xss usuario


-- 7.
-- Dada una red social y un usuario, devuelve una lista con las publicaciones a las que ese usuario le dio like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, publicaciones) usuario = leGustaA publicaciones usuario

-- Realiza recursión sobre todas las publicaciones y devuelve lista con las publicaciones que el usuario le dio like
leGustaA :: [Publicacion] -> Usuario -> [Publicacion]
leGustaA [] _ = []
leGustaA (xs:xss) usuario 
    | pertenece usuario (likesDePublicacion xs) = xs : leGustaA xss usuario
    | otherwise =  leGustaA xss usuario

-- Devuelve True si el elemento está en la lista y False en otro caso
pertenece :: Eq t => t -> [t] -> Bool 
pertenece _ [] = False 
pertenece n (x : xs) 
    | n == x = True
    | otherwise = pertenece n xs


-- 8.
-- Dados dos usuarios y una red, decide si ambos usuarios le dieron like a las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2 = mismosElementos (publicacionesQueLeGustanA red us1) (publicacionesQueLeGustanA red us2)

-- Decide si, dadas dos listas, estas tienen los mismos elementos (sin importar el orden)
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] _ = True
mismosElementos (x:xs) s 
    | not (pertenece x s) = False
    | otherwise = mismosElementos xs s


-- 9.
-- Decide si existe entre los amigos de un usuario en una red, alguien que le haya dado me gusta a todas las publicaciones del mismo.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red usuario = aAlguienLeGustanTodasLasPub (amigosDe red usuario) (publicacionesDe red usuario)

-- Hace recursion sobre una lista de usuarios y decide si alguno le dio me gusta a todas las publicaciones de otra lista.
aAlguienLeGustanTodasLasPub :: [Usuario] -> [Publicacion] -> Bool
aAlguienLeGustanTodasLasPub [] _ = False
aAlguienLeGustanTodasLasPub (x:xs) publicaciones 
    | leGustanTodasLasPub publicaciones x = True
    | otherwise = aAlguienLeGustanTodasLasPub xs publicaciones

-- Decide si un usuario le dio me gusta a todas las publicaciones de una lista.
leGustanTodasLasPub :: [Publicacion] -> Usuario -> Bool
leGustanTodasLasPub [] usuario = True
leGustanTodasLasPub (xs:xss) usuario 
    | pertenece usuario (likesDePublicacion xs) = leGustanTodasLasPub xss usuario
    | otherwise = False
 
 
-- 10.
-- Dados dos usuarios dentro de la misma red nos dice si es posible con las relaciones del primer usuario llegar al segundo
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 
    | sonAmigos u1 u2 red = True
    | amigosDe red u2 == [] = False 
    | otherwise = hayCadenaDeAmigos (eliminarRelacionYUser red u1) (amigosDe red u1) u2

hayCadenaDeAmigos :: RedSocial -> [Usuario] -> Usuario -> Bool
hayCadenaDeAmigos _ [] _ = False
hayCadenaDeAmigos red (u : us) u2 = existeSecuenciaDeAmigos red u u2 


sonAmigos :: Usuario -> Usuario -> RedSocial -> Bool
sonAmigos u1 u2 red = pertenece u1 (amigosDe red u2)

eliminarRelacionYUser :: RedSocial -> Usuario -> RedSocial
eliminarRelacionYUser (us, rs, ps) u = (eliminar u us, eliminarTodos (relacionesDe u red) rs , ps)
                               where red = (us, rs, ps)

relacionesDe :: Usuario -> RedSocial -> [Relacion]
relacionesDe _ (_, [], _) = []
relacionesDe u (us, x : xs, ps) 
    | fst x == u || snd x == u = x : relacionesDe u (us, xs, ps)
    | otherwise = relacionesDe u (us, xs, ps)

eliminarTodos :: Eq t => [t] -> [t] -> [t]
eliminarTodos [] xss = xss
eliminarTodos (x : xs) xss = eliminarTodos xs (eliminar x xss)

eliminar :: Eq t => t -> [t] -> [t]
eliminar _ [] = []
eliminar n (x : xs) 
    | not (pertenece n (x:xs)) = x : xs
    | n == x = xs
    | otherwise = x : eliminar n xs
