module Main where
import Test.HUnit
import Solucion 

main = runTestTT tests

tests = test [
    " nombresDeUsuarios" ~: testNombresDeUsuarios,

    " amigosDe" ~: testAmigosDe,

    " cantidadDeAmigos" ~: testcantidadDeAmigos,

    " usuarioConMasAmigos" ~: testusuarioConMasAmigos,

    " estaRobertoCarlos" ~: testestaRobertoCarlos,

    " publicacionesDe" ~: testpublicacionesDe,

    " publicacionesQueLeGustanA " ~: testpublicacionesQueLeGustanA,

    " lesGustanLasMismasPublicaciones" ~: testlesGustanLasMismasPublicaciones,

    " tieneUnSeguidorFiel" ~: testTieneUnSeguidorFiel,

    " existeSecuenciaDeAmigos" ~: testExisteSecuenciaDeAmigos
 ]


testNombresDeUsuarios = test [
    -- Caso red con usuarios
    nombresDeUsuarios redUno ~?= ["Cami","Agus","Facu","Martu","Pedro","Ramon","Jorge","Silvina","Agustin","Colo"], 
    nombresDeUsuarios redTres ~?= [] -- Caso red vacía
    ]

testAmigosDe = test [
    -- Caso usuario con relaciones
    amigosDe redUno usuario2 ~?= [(1,"Cami"),(3,"Facu"),(4,"Martu"),(5,"Pedro"),(6,"Ramon"),(7,"Jorge"),(8,"Silvina"),(8,"Silvina"),(10,"Colo"),(11,"Martin")], 
    amigosDe redDos usuario7 ~?= [] -- Caso usuario sin relaciones
    ]

testcantidadDeAmigos = test [
    cantidadDeAmigos redUno usuario1 ~?= 3, -- Caso con relaciones
    cantidadDeAmigos redDos usuario7 ~?= 0 -- Caso sin relaciones
    ]

testusuarioConMasAmigos = test [
    usuarioConMasAmigos redUno ~?= usuario2, -- Caso que puede devolver un único valor
    usuarioConMasAmigos redDos ~?= usuario1 -- Caso que puede devolver más de un valor (usuario1, usuario2 o usuario3)
    ]

testestaRobertoCarlos = test [
    estaRobertoCarlos redUno ~?= True, -- Caso hay usuario con más de 10 relaciones en la red
    estaRobertoCarlos redDos ~?= False -- Caso no hay usuario con más de 10 relaciones en la red
    ]

testpublicacionesDe = test [
    -- Caso usuario con publicaciones
    publicacionesDe redUno usuario2 ~?= [((2,"Agus"),"Hello World",[(4,"Martu")]),((2,"Agus"),"Good Bye World",[(1,"Cami"),(3,"Facu"),(4,"Martu"),(5,"Pedro"),(6,"Ramon"),(7,"Jorge"),(8,"Silvina"),(9,"Agustin"),(10,"Colo"),(11,"Martin")])], 
    publicacionesDe redDos usuario7 ~?= [] -- Caso usuario sin publicaciones
    ]

testpublicacionesQueLeGustanA = test [
    -- Caso publicacion con likes
    publicacionesQueLeGustanA redUno usuario7 ~?= [((2,"Agus"),"Good Bye World",[(1,"Cami"),(3,"Facu"),(4,"Martu"),(5,"Pedro"),(6,"Ramon"),(7,"Jorge"),(8,"Silvina"),(9,"Agustin"),(10,"Colo"),(11,"Martin")])],
    publicacionesQueLeGustanA redDos usuario7 ~=? [] -- Caso publicación sin likes
    ]

testlesGustanLasMismasPublicaciones = test [
    lesGustanLasMismasPublicaciones redUno usuario1 usuario2 ~?= False, -- Caso no tengan los mismos likes
    lesGustanLasMismasPublicaciones redDos usuario1 usuario2 ~?= False, -- Caso que uno de los usuarios no tenga likes
    lesGustanLasMismasPublicaciones redDos usuario2 usuario1 ~?= False, -- Caso que uno de los usuarios no tenga likes inverido
    lesGustanLasMismasPublicaciones redDos usuario6 usuario7 ~?= True, -- Caso ninguno tiene likes
    lesGustanLasMismasPublicaciones redDos usuario4 usuario8 ~?= False, -- Caso los dos usuarios tengan los mismos likes pero uno tenga más que el otro
    lesGustanLasMismasPublicaciones redDos usuario2 usuario5 ~?= True, -- Caso le gustan las mismas publicaciones
    lesGustanLasMismasPublicaciones redDos usuario5 usuario2 ~?= True -- Caso le gustan las mismas publicaciones invertido
    ]

testTieneUnSeguidorFiel = test [
    tieneUnSeguidorFiel redUno usuario2 ~?= True, -- Caso tiene un seguidor fiel
    tieneUnSeguidorFiel redUno usuario5 ~?= False -- Caso no tiene seguidor fiel
    ]


testExisteSecuenciaDeAmigos = test [
    existeSecuenciaDeAmigos redUno usuario1 usuario4 ~?= True, -- Caso ambos usuarios relacionados directamente
    existeSecuenciaDeAmigos redUno usuario1 usuario5 ~?= True, -- Caso ambos usuarios relacionados por sus amigos
    existeSecuenciaDeAmigos redUno usuario5 usuario1 ~?= True, -- Caso anterior con distinto orden
    existeSecuenciaDeAmigos redDos usuario1 usuario7 ~?= False, -- Caso uno de los usuarios sin relaciones
    existeSecuenciaDeAmigos redDos usuario7 usuario1 ~?= False -- Caso uno de los usuarios sin relaciones invertido
    ]

-- Datos:
usuario1 = (1, "Cami")
usuario2 = (2, "Agus")
usuario3 = (3, "Facu")
usuario4 = (4, "Martu")
usuario5 = (5, "Pedro")
usuario6 = (6, "Ramon")
usuario7 = (7, "Jorge")
usuario8 = (8, "Silvina")
usuario9 = (9, "Agustin")
usuario10 = (10, "Colo")
usuario11= (11, "Martin")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario3, usuario5)
relacion2_5 = (usuario2, usuario5)

relacion2_6 = (usuario2, usuario6)
relacion2_7 = (usuario2, usuario7)
relacion2_8 = (usuario2, usuario8)
relacion2_9 = (usuario2, usuario9)
relacion2_10 = (usuario2, usuario10)

relacion2_11 = (usuario2, usuario11)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4, usuario5])
publicacion1_2 = (usuario1, "No sé que postear", [usuario4, usuario8]) 
publicacion1_3 = (usuario1, "Trabajo de haskell", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Primer cuatri", [])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10,usuario11])

publicacion3_1 = (usuario3, "No", [])
publicacion3_2 = (usuario3, "Si", [usuario2, usuario5])  
publicacion3_3 = (usuario3, "Inserte frase motivacional", [usuario1, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

publicacion5_1 = (usuario5, "Holaa", [])
publicacion5_2 = (usuario5, "Qué tal", [usuario2])
publicacion5_3 = (usuario5, "Todo bien", [usuario2, usuario5])


usuariosPrimerRed = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10]
relacionesPrimerRed = [relacion1_2, relacion1_3, relacion1_4, relacion2_3, relacion2_4, relacion3_4, relacion2_5, relacion2_6, relacion2_7, relacion2_8, relacion2_8, relacion2_10, relacion2_11]
publicacionesPrimerRed = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_4, publicacion2_1, publicacion2_2,
                          publicacion3_1, publicacion3_2, publicacion3_3, publicacion4_1, publicacion4_2,
                          publicacion4_3, publicacion5_1, publicacion5_2, publicacion5_3]
redUno = (usuariosPrimerRed, relacionesPrimerRed, publicacionesPrimerRed)


usuariosSegundaRed = [usuario1, usuario2, usuario3, usuario4, usuario5] 
relacionesSegundaRed = [relacion1_2, relacion2_3, relacion1_3] 
publicacionesSegundaRed = [publicacion1_1, publicacion1_3, publicacion2_1, publicacion3_1, publicacion3_2]
redDos = (usuariosSegundaRed, relacionesSegundaRed, publicacionesSegundaRed)

usuariosRedTres = []
relacionesRedTres = []
publicacionesRedTres = []

redTres = (usuariosRedTres,relacionesRedTres,publicacionesRedTres)
