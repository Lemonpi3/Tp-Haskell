{-# LANGUAGE CPP #-}
module Main where
#include "./iap1-tp.hs"

--Tests
main :: IO()
main = do{
    print("")
}

--Casos
--Usuarios

usuario0 = (0, "GUGUL")
usuario1 = (1, "GUGUL")
usuario2 = (2, "UTUB")
usuario3 = (3, "HELLSCRIPT")
usuario4 = (4, "GUGUL")
usuario5 = (5, "UTUB")
usuario6 = (6, "PITON")
usuario7 = (7, "GUGUL")
usuario8 = (8, "UTUB")
usuario9 = (9, "UTUB")

--Relaciones

relacion1_3 = (usuario1, usuario3)
relacion4_7 = (usuario4, usuario7)
relacion8_2 = (usuario8, usuario2)
relacion9_1 = (usuario9, usuario1)
relacion4_0 = (usuario4, usuario0)
relacion2_7 = (usuario2, usuario7)
relacion7_3 = (usuario7, usuario3)
relacion3_7 = (usuario3, usuario7)
relacion3_1 = (usuario3, usuario1)
relacion2_6 = (usuario2, usuario6)

--Publicaciones

publicacion8_4 = (usuario8, "To the moon", ["usuario7", "usuario6", "usuario4", "usuario6"])
publicacion9_2 = (usuario9, "import pandas as pd", ["usuario4", "usuario7"])
publicacion1_3 = (usuario1, "WTB For loop", ["usuario9", "usuario4", "usuario9"])
publicacion3_2 = (usuario3, "To the moon", ["usuario2", "usuario6"])
publicacion0_5 = (usuario0, "Hello world", ["usuario7", "usuario6", "usuario7", "usuario3", "usuario0"])
publicacion5_0 = (usuario5, "Deberia tomarme esto mas encerio", [])
publicacion7_2 = (usuario7, "Deberia tomarme esto mas encerio", ["usuario0", "usuario3"])
publicacion1_2 = (usuario1, "Deberia tomarme esto mas encerio", ["usuario1", "usuario2"])
publicacion8_3 = (usuario8, "import pandas as pd", ["usuario4", "usuario0", "usuario1"])
publicacion1_1 = (usuario1, "Deberia tomarme esto mas encerio", ["usuario7"])

--Listas

usuariosA = [usuario8, usuario6, usuario3, usuario8]
relacionesA = [relacion2_6, relacion4_0, relacion1_3]
publicacionesA = [publicacion9_2, publicacion7_2, publicacion5_0, publicacion1_1, publicacion9_2]

usuariosB = [usuario3, usuario2, usuario1, usuario9]
relacionesB = [relacion3_1, relacion3_7, relacion8_2, relacion3_7, relacion1_3]
publicacionesB = [publicacion7_2, publicacion1_3]

--Redes
redA = (usuariosA, relacionesA, publicacionesA)
redB = (usuariosB, relacionesB, publicacionesB)
