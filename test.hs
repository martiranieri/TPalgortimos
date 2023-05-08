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

    " lesGustanLasMismasPublicaciones" ~: testlesGustanLasMismasPublicaciones

    -- " tieneUnSeguidorFiel 1" ~: tieneUnSeguidorFiel redA usuario1 ~?= True,

    -- " existeSecuenciaDeAmigos 1" ~: existeSecuenciaDeAmigos redA usuario1 usuario3 ~?= True
 ]


testNombresDeUsuarios = test [
    nombresDeUsuarios redUno ~?= ["Cami","Agus","Facu","Martu","Pedro"],
    nombresDeUsuarios redDos ~?= ["Cami","Agus","Facu"]
    ]

testAmigosDe = test [
    amigosDe redUno usuario1 ~?= [(2, "Agus"), (3, "Facu"), (4, "Martu")],
    amigosDe redUno usuario2 ~?= [(1, "Cami"), (3, "Facu"),(4, "Martu"), (5, "Pedro")],
    amigosDe redUno usuario3 ~?= [(1,"Cami"),(2,"Agus"),(4,"Martu")], 
    amigosDe redUno usuario5 ~?= [(2,"Agus")]
    ]

testcantidadDeAmigos = test [
    cantidadDeAmigos redUno usuario1 ~?= 3,
    cantidadDeAmigos redUno usuario2 ~?= 4,
    cantidadDeAmigos redUno usuario5 ~?= 1,
    cantidadDeAmigos redDos usuario3 ~?= 2
    ]

testusuarioConMasAmigos = test [
    usuarioConMasAmigos redUno ~?= usuario2,
    usuarioConMasAmigos redDos ~?= usuario1 
    ]

testestaRobertoCarlos = test [
    estaRobertoCarlos redUno ~?= False,
    estaRobertoCarlos redDos ~?= False
    ]

testpublicacionesDe = test [
    publicacionesDe redUno usuario2 ~?= [((2,"Agus"),"Hello World",[(4,"Martu")]),((2,"Agus"),"Good Bye World",[(1,"Cami"),(3,"Facu"),(2,"Agus")])],
    publicacionesDe redDos usuario1 ~?= [((1,"Cami"),"Este es mi primer post",[(2,"Agus"),(4,"Martu"),(5,"Pedro")]),((1,"Cami"),"No s\233 que postear",[(4,"Martu")])],
    publicacionesDe redDos usuario2 ~?= [((2,"Agus"),"Hello World",[(4,"Martu")]),((2,"Agus"),"Good Bye World",[(1,"Cami"),(3,"Facu"),(2,"Agus")])],
    publicacionesDe redDos usuario3 ~?= [((3,"Facu"),"No",[]),((3,"Facu"),"Si",[(2,"Agus")])]
    ]

testpublicacionesQueLeGustanA = test [
    publicacionesQueLeGustanA redUno usuario3 ~?= [((2,"Agus"),"Good Bye World",[(1,"Cami"),(3,"Facu"),(2,"Agus")]),((4,"Martu"),"Just kidding, i am Mariela",[(1,"Cami"),(3,"Facu")])],
    publicacionesQueLeGustanA redUno usuario5 ~?= [((1,"Cami"),"Este es mi primer post",[(2,"Agus"),(4,"Martu"),(5,"Pedro")]),((1,"Cami"),"Trabajo de haskell",[(2,"Agus"),(5,"Pedro")]),((3,"Facu"),"Inserte frase motivacional",
                                                  [(1,"Cami"),(5,"Pedro")]),((5,"Pedro"),"Todo bien",[(2,"Agus"),(5,"Pedro")])],
    publicacionesQueLeGustanA redDos usuario1 ~?= [((2,"Agus"),"Good Bye World",[(1,"Cami"),(3,"Facu"),(2,"Agus")])],
    publicacionesQueLeGustanA redDos usuario3 ~?= [((2,"Agus"),"Good Bye World",[(1,"Cami"),(3,"Facu"),(2,"Agus")])]
    ]

testlesGustanLasMismasPublicaciones = test [
    lesGustanLasMismasPublicaciones redUno usuario1 usuario2 ~?= False,
    lesGustanLasMismasPublicaciones redDos usuario1 usuario2 ~?= True
    ]


-- Datos:
usuario1 = (1, "Cami")
usuario2 = (2, "Agus")
usuario3 = (3, "Facu")
usuario4 = (4, "Martu")
usuario5 = (5, "Pedro")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario3, usuario5)
relacion2_5 = (usuario2, usuario5)




publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4, usuario5])
publicacion1_2 = (usuario1, "No sé que postear", [usuario4])
publicacion1_3 = (usuario1, "Trabajo de haskell", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Primer cuatri", [])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario3, usuario2])

publicacion3_1 = (usuario3, "No", [])
publicacion3_2 = (usuario3, "Si", [usuario2])
publicacion3_3 = (usuario3, "Inserte frase motivacional", [usuario1, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

publicacion5_1 = (usuario5, "Holaa", [])
publicacion5_2 = (usuario5, "Qué tal", [usuario2])
publicacion5_3 = (usuario5, "Todo bien", [usuario2, usuario5])


usuariosPrimerRed = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesPrimerRed = [relacion1_2, relacion1_3, relacion1_4, relacion2_3, relacion2_4, relacion3_4, relacion2_5]
publicacionesPrimerRed = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_4, publicacion2_1, publicacion2_2,
                          publicacion3_1, publicacion3_2, publicacion3_3, publicacion4_1, publicacion4_2,
                          publicacion4_3, publicacion5_1, publicacion5_2, publicacion5_3]
redUno = (usuariosPrimerRed, relacionesPrimerRed, publicacionesPrimerRed)


usuariosSegundaRed = [usuario1, usuario2, usuario3]
relacionesSegundaRed = [relacion1_2, relacion2_3, relacion1_3]
publicacionesSegundaRed = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2]
redDos = (usuariosSegundaRed, relacionesSegundaRed, publicacionesSegundaRed)







