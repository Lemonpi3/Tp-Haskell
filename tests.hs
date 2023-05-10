import Test.HUnit
import Solucion

main = runTestTT todosLosTest

todosLosTest = test [testNombresDeUsuarios]

testNombresDeUsuarios = test [
    "nombresDeUsuarios RC" ~: (nombresDeUsuarios redRC) ~?= ["Freddy Mercury", "Linkin Park", "Ramstein", "Oomph", "Tool", "Michael Jackson", "Celldweller", "Terapia", "Soda Stereo", "Aerosmith", "Los Fabulosos Cadillacs", "SOAD", "Roberto Carlos"],
    "nombresDeUsuarios A" ~: (nombresDeUsuarios redA) ~?= ["Freddy Mercury","Linkin Park" ,"Los Fabulosos Cadillacs","Terapia","Oomph","Aerosmith","Roberto Carlos"]
    ]


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
usuariosA = [usuario0, usuario1, usuario10, usuario7, usuario3, usuario9, usuarioRc]
relacionesA = [relacion_1_3]
publicacionesA = [publicacion1_4, publicacion1_2, publicacionRc_1, publicacionRc_3]
redA = (usuariosA,relacionesA,publicacionesA)

--RedB
usuariosB = [usuario2, usuario0, usuario1, usuario7, usuario9, usuario3, usuario5, usuario6]
relacionesB = [relacion_0_2, relacion_2_0, relacion_0_3, relacion_3_6, relacion_6_5]
publicacionesB = [publicacion1_0, publicacion1_2, publicacion2_4]