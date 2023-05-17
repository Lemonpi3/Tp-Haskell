import Test.HUnit
import Solucion

-- Nombre de Grupo: Grupoprogramacion
-- Integrante 1: Agustin Lehmann, 96agustin.lehmann@gmail.com, 617/23
-- Integrante 2: Joaquín Treppo, joaqokpo98@gmail.com, 52/23
-- Integrante 3: Marcos Fourcade, marcosfourca02@gmail.com, 446/23
-- Integrante 4: Tomás Kupinski, tomaskupinski@gmail.com, 267/23

main = runTestTT todosLosTest

todosLosTest = test [
    testNombresDeUsuarios, testAmigosDe, testCantidadDeAmigos, testUsuarioConMasAmigos, testEstaRobertoCarlos,
    testPublicacionesDe, testPublicacionesQueLeGustanA, testLesGustanLasMismasPublicaciones,
    testTieneUnSeguidorFiel, testExisteSecuenciaDeAmigos
    ]

testNombresDeUsuarios = test [
    "nombresDeUsuarios A" ~: nombresDeUsuarios redA ~?= ["Freddy Mercury","Linkin Park" ,"Los Fabulosos Cadillacs","Terapia","Oomph","Aerosmith","Roberto Carlos", "Celldweller"]
    ]

testAmigosDe = test [
    "amigosDe B 0" ~: amigosDe redB usuario0 ~?= [usuario2,usuario3],
    "amigosDe RC rc" ~: amigosDe redRC usuarioRc ~?= [usuario0, usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11],
    "amigosDe A 10" ~: amigosDe redA usuario10 ~?= []
    ]

testCantidadDeAmigos = test [
    "cantidadDeAmigos B 0" ~: cantidadDeAmigos redB usuario0 ~?= 2,
    "cantidadDeAmigos RC rc" ~: cantidadDeAmigos redRC usuarioRc ~?= 12,
    "cantidadDeAmigos A 10" ~: cantidadDeAmigos redA usuario10 ~?= 0,
    "cantidadDeAmigos SA 2"~: cantidadDeAmigos redSA usuario2 ~?= 0
    ]

testUsuarioConMasAmigos = test [
    "usuarioConMasAmigos RC" ~: usuarioConMasAmigos redRC ~?= usuarioRc,
    "usuarioConMasAmigos B" ~: expectAny (usuarioConMasAmigos redB) [usuario0, usuario3],
    "usuarioConMasAmigos SA" ~: expectAny (usuarioConMasAmigos redSA) (usuarios redSA)
    ]

testEstaRobertoCarlos = test [
    "estaRobertoCarlos RC" ~: estaRobertoCarlos redRC ~?= True,
    "estaRobertoCarlos A" ~: estaRobertoCarlos redA ~?= False
    ]

testPublicacionesDe = test [
    "publicacionesDe A 4" ~: publicacionesDe redA usuario4 ~?= [],
    "publicacionesDe A 1" ~: publicacionesDe redA usuario1 ~?= [publicacion1_4, publicacion1_2],
    "publicacionesDe SP 1" ~: publicacionesDe redSP usuario1 ~?= []
    ]

testPublicacionesQueLeGustanA = test [
    "publicacionesQueLeGustanA A 7" ~: publicacionesQueLeGustanA redA usuario7 ~?= [publicacion1_4, publicacion1_2],
    "publicacionesQueLeGustanA B 5" ~: publicacionesQueLeGustanA redB usuario5 ~?= [],
    "publicacionesQueLeGustanA SP 1" ~: publicacionesQueLeGustanA redSP usuario1 ~?= []
    ]

testLesGustanLasMismasPublicaciones = test [
    "lesGustanLasMismasPublicaciones A 7 9" ~: lesGustanLasMismasPublicaciones redA usuario7 usuario9 ~?= True,
    "lesGustanLasMismasPublicaciones RC 3 10" ~: lesGustanLasMismasPublicaciones redRC usuario3 usuario10 ~?= False,
    "lesGustanLasMismasPublicaciones B 7 0" ~: lesGustanLasMismasPublicaciones redB usuario7 usuario0 ~?= False,
    "lesGustanLasMismasPublicaciones SP 0 1" ~: lesGustanLasMismasPublicaciones redSP usuario0 usuario1 ~?= True
    ]

testTieneUnSeguidorFiel = test [
    "tieneUnSeguidorFiel A RC" ~: tieneUnSeguidorFiel redA usuarioRc ~?= True,
    "tieneUnSeguidorFiel B 1" ~: tieneUnSeguidorFiel redB usuario1 ~?= False,
    "tieneUnSeguidorFiel SP 1" ~: tieneUnSeguidorFiel redSP usuario1 ~?= False
    ]

testExisteSecuenciaDeAmigos = test [
    "existeSecuenciaDeAmigos B 0 6" ~: existeSecuenciaDeAmigos redB usuario0 usuario6 ~?= True,
    "existeSecuenciaDeAmigos B 2 5" ~: existeSecuenciaDeAmigos redB usuario2 usuario5 ~?= True,
    "existeSecuenciaDeAmigos RC 2 6" ~: existeSecuenciaDeAmigos redRC usuario2 usuario6 ~?= True,
    "existeSecuenciaDeAmigos A 1 5" ~: existeSecuenciaDeAmigos redA usuario1 usuario5 ~?= False,
    "existeSecuenciaDeAmigos SA 1 5" ~: existeSecuenciaDeAmigos redSA usuario1 usuario5 ~?= False
    ]

--sacado del test-catedra.hs para testUsuarioConMasAmigos
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

usuario0 = (0,"Freddy Mercury")
usuario1 = (1,"Linkin Park")
usuario2 = (2,"Ramstein")
usuario3 = (3,"Oomph")
usuario4 = (4,"Tool")
usuario5 = (5,"Michael Jackson")
usuario6 = (6,"Celldweller")
usuario7 = (7,"Terapia")
usuario8 = (8,"Soda Stereo")
usuario9 = (9,"Aerosmith")
usuario10 = (10,"Los Fabulosos Cadillacs")
usuario11 = (11,"SOAD")
usuarioRc = (12,"Roberto Carlos")

relacion_rc_0 = (usuarioRc, usuario0)
relacion_rc_1 = (usuarioRc, usuario1)
relacion_rc_2 = (usuarioRc, usuario2)
relacion_rc_3 = (usuarioRc, usuario3)
relacion_rc_4 = (usuarioRc, usuario4)
relacion_rc_5 = (usuarioRc, usuario5)
relacion_rc_6 = (usuarioRc, usuario6)
relacion_rc_7 = (usuarioRc, usuario7)
relacion_rc_8 = (usuarioRc, usuario8)
relacion_rc_9 = (usuarioRc, usuario9)
relacion_rc_10 = (usuarioRc, usuario10)
relacion_rc_11 = (usuarioRc, usuario11)

relacion_0_1 = (usuario0, usuario1)
relacion_0_2 = (usuario0, usuario2)
relacion_0_3 = (usuario0, usuario3)
relacion_0_4 = (usuario0, usuario4)
--para el 10 usuario 0 con 6 o usuario 1 con 5 o usuario 2 con 5
relacion_1_3 = (usuario1, usuario3)
relacion_4_5 = (usuario4, usuario5)
relacion_3_6 = (usuario3, usuario6)
relacion_6_5 = (usuario6, usuario5)
relacion_2_0 = (usuario2, usuario0)

--mi fiel es usuario 3 con Rc y pub n comun puede ser 7 con 9
publicacionRc_3 = (usuarioRc, "Yo quiero tener un millón de amigos", [usuario3, usuario10, usuario8])
publicacionRc_1 = (usuarioRc, "Y así más fuerte poder cantar", [usuario3])

publicacion1_4 = (usuario1, "I tried so hard",[usuario10, usuario3, usuario7, usuario9])
publicacion1_0 = (usuario1, "And got so far", [])
publicacion1_2 = (usuario1, "But in the end it doesn't even matter", [usuario7, usuario9])
publicacion2_4 = (usuario2, "Dicke Titten",[usuario0, usuario1, usuario7, usuario9])

--RedRobertoCarlos
usuariosRC = [usuario0, usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuarioRc]
relacionesRC = [relacion_rc_0, relacion_rc_1, relacion_rc_2, relacion_rc_3, relacion_rc_4, relacion_rc_5, relacion_rc_6, relacion_rc_7, relacion_rc_8, relacion_rc_9, relacion_rc_10, relacion_rc_11]
publicacionesRC = [publicacionRc_3, publicacionRc_1, publicacion1_4, publicacion1_0]
redRC = (usuariosRC, relacionesRC, publicacionesRC)

--RedA
usuariosA = [usuario0, usuario1, usuario10, usuario7, usuario3, usuario9, usuarioRc, usuario6]
relacionesA = [relacion_1_3,relacion_3_6, relacion_4_5]
publicacionesA = [publicacion1_4, publicacion1_2, publicacionRc_1, publicacionRc_3]
redA = (usuariosA,relacionesA,publicacionesA)

--RedB
usuariosB = [usuario2, usuario0, usuario1, usuario7, usuario9, usuario3, usuario5, usuario6]
relacionesB = [relacion_0_2, relacion_2_0, relacion_0_3, relacion_3_6, relacion_6_5]
publicacionesB = [publicacion1_0, publicacion1_2, publicacion2_4]
redB = (usuariosB,relacionesB,publicacionesB)

-- redSinAmigos
usuariosSA = [usuario2, usuario0, usuario1, usuario7, usuario9, usuario3, usuario5, usuario6]
relacionesSA = []
publicacionesSA = [publicacion1_0, publicacion1_2, publicacion2_4]
redSA = (usuariosSA,relacionesSA,publicacionesSA)

--redSinPublicaciones
usuariosSP = [usuario0, usuario1, usuario10, usuario7, usuario3, usuario9, usuarioRc, usuario6]
relacionesSP = [relacion_1_3,relacion_3_6, relacion_4_5]
publicacionesSP = []
redSP = (usuariosSP,relacionesSP, publicacionesSP)
