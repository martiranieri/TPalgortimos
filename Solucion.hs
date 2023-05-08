{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solucion where

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesB = [relacion1_2, relacion2_3, relacion3_4]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-- Completar con los datos del grupo
--
-- Nombre de Grupo: Undefined
-- Integrante 1: Biegun Agustina, agustinabgn@gmail.com, 1101/22
-- Integrante 2: Lara Facundo Ignacio, facuriver57@gmail.com, 647/23
-- Integrante 3: Miyasaki Camila Denise, camimiyasaki@gmail.com, 1063/22
-- Integrante 4: Ranieri Martina Belén, martubranieri@gmail.com, 1118/22

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
-- el nombre se encuentra en la segunda posición de la dupla de datos del Usuario
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
    | snd xs ==  usuario = fst xs : amigosDeAux xss usuario
    | fst xs ==  usuario = snd xs : amigosDeAux xss usuario
    | otherwise = amigosDeAux xss usuario


-- 3.
-- Devuelve el número de personas relacionadas en la red con el usuario ingresado como parámetro 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos redSocial usuario = longitud (amigosDe redSocial usuario)

-- Devuelve el longitud de la lista ingresada
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

-- Devuelve el usuario con más relaciones en la red. Compara la primer posiciónde las duplas
-- (#relaciones en la red) con otra dupla y descarta la menor hasta obtener una única dupla
-- devolviendo la segunda posición de la dupla, los datos del usuario final
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
    | fst xs >= 1000000 = True
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
pertenece :: (Eq t) => t -> [t] -> Bool 
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
-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuarioInicio usuarioFinal = seqDeAmigosAux red (amigosDe red usuarioInicio) usuarioFinal
-- 
seqDeAmigosAux :: RedSocial -> [Usuario] -> Usuario -> Bool
seqDeAmigosAux _ [] _ = False
seqDeAmigosAux red (xs:xss) usuarioFinal 
    | xs == usuarioFinal = True
    | otherwise = seqDeAmigosAux (quitarRelacionesDeRed red xs) xss usuarioFinal 

quitarRelacionesDeRed :: RedSocial -> Usuario -> RedSocial
quitarRelacionesDeRed (usuario, (xs:xss), publicaciones) sacarUsuario 
    | not (estaUsuario sacarUsuario (xs:xss)) = (usuario, (xs:xss), publicaciones)
    | sacarUsuario == fst xs || sacarUsuario == snd xs = quitarRelacionesDeRed (usuario, xss, publicaciones) sacarUsuario
    | otherwise = quitarRelacionesDeRed (usuario, xs : xss, publicaciones) sacarUsuario

estaUsuario :: Usuario -> [Relacion] -> Bool
estaUsuario _ [] = False
estaUsuario usuario (xs:xss)
    | usuario == fst xs || usuario == snd xs = True
    | otherwise = estaUsuario usuario xss 