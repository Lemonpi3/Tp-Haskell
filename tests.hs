{-# LANGUAGE CPP #-}
module Main where
#include "./iap1-tp.hs"

-- Tests
main :: IO()
main = do{
    --el putStrLn solo esta para que salga en color el Pass o Fail en la consola (seria como un print).
    --en el caso que la funcion retorne un booleano lo comparo con ==True o ==False segun corresponda por mas que 
    --sea redundante con el fin de que la persona que este haciendo la funcion pueda encontrar el error mas rapido.
    putStrLn $ eval "nombresUsuarios A" ((nombresDeUsuarios redA) == ["UTUB","PITON","HELLSCRIPT","UTUB"]);
    putStrLn $ eval "nombresUsuarios B" ((nombresDeUsuarios redB) == ["HELLSCRIPT","UTUB","GUGUL","Roberto Carlos"]);
    putStrLn $ eval "amigosDe A usuario 3" ((amigosDe redA usuario3) == [usuario1]);
    putStrLn $ eval "amigosDe A usuario 6" ((amigosDe redA usuario6) == [usuario2]);
    putStrLn $ eval "amigosDe B usuario 3" ((amigosDe redB usuario3) == [usuario1,usuario7])
    putStrLn $ eval "amigosDe B usuario 2" ((amigosDe redB usuario2) == [usuario1,usuario8])
    putStrLn $ eval "cantidadDeAmigos A usuario 3" ((cantidadDeAmigos redA usuario3) == 1)
    putStrLn $ eval "cantidadDeAmigos A usuario 3" ((cantidadDeAmigos redA usuario6) == 1)
    putStrLn $ eval "cantidadDeAmigos B usuario 2" ((amigosDe redB usuario2) == 2)
    putStrLn $ eval "cantidadDeAmigos B usuario 3" ((cantidadDeAmigos redB usuario3) == 2)
    putStrLn $ eval "usuarioConMasAmigos A" ((usuarioConMasAmigos redA) == usuario6 || (usuarioConMasAmigos redA) == usuario3) --A como esta actualmente hay empate
    putStrLn $ eval "usuarioConMasAmigos B" ((usuarioConMasAmigos redB) == usuario3)
    putStrLn $ eval "estaRobertoCarlos A" ((estaRobertoCarlos redA) == False)
    putStrLn $ eval "estaRobertoCarlos B" ((estaRobertoCarlos redB) == True)
    putStrLn $ eval "publicacionesDe A 5" ((publicacionesDe redA usuario5) == [])
    putStrLn $ eval "publicacionesDe A 1" ((publicacionesDe redA usuario1) == [publicacion1_1, publicacion1_2])
    putStrLn $ eval "publicacionesDe B 1" ((publicacionesDe redB usuario1) == [publicacion1_3])
    putStrLn $ eval "publicacionesQueLeGustanA B 3" ((publicacionesQueLeGustanA redB usuario3)==[publicacion7_2])
    putStrLn $ eval "publicacionesQueLeGustanA A 7" ((publicacionesQueLeGustanA redA usuario7)==[publicacion1_1, publicacion9_2])
    putStrLn $ eval "publicacionesQueLeGustanA B 5" ((publicacionesQueLeGustanA redB usuario5)==[])
    putStrLn $ eval "lesGustanLasMismasPublicaciones C 2 6" ((lesGustanLasMismasPublicaciones redC usuario2 usuario6) == True)
    putStrLn $ eval "lesGustanLasMismasPublicaciones C 2 0" ((lesGustanLasMismasPublicaciones redC usuario2 usuario6) == False)
    -- putStrLn $ eval "tieneUnSeguidorFiel X Y" ((tieneUnSeguidorFiel redX usuario Y) ==True) -- Nose q se significa esta 
    -- putStrLn $ eval "tieneUnSeguidorFiel X Y" ((tieneUnSeguidorFiel redX usuario Y) ==False) -- Nose q se significa esta 
    -- putStrLn $ eval "existeSecuenciaDeAmigos X Y Z" ((existeSecuenciaDeAmigos redX usuario Y usuario Z) ==True) -- Nose q se significa esta 
    -- putStrLn $ eval "existeSecuenciaDeAmigos X Y Z" ((existeSecuenciaDeAmigos redX usuario Y usuario Z) ==True) -- Nose q se significa esta 

}

eval::String->Bool->String
--Escribe Pass o Fail en la consola dependiendo de si pasa o no el test.
--Texto verde si pasa rojo si no, el string del final es para reiniciar en caso de que se quiera seguir con un texto normal y no de eval
eval str x | x == True = str++":" ++ "\ESC[32m"++ " Pass" ++"\ESC[97m"
           | otherwise = str++":" ++ "\ESC[31m"++ " Fail"  ++"\ESC[97m"

--Casos
--Usuarios
usuario0 :: Usuario
usuario0 = (0, "GUGUL")

usuario1 :: Usuario
usuario1 = (1, "GUGUL")

usuario2 :: Usuario
usuario2 = (2, "UTUB")

usuario3 :: Usuario
usuario3 = (3, "HELLSCRIPT")

usuario4 :: Usuario
usuario4 = (4, "GUGUL")

usuario5 :: Usuario
usuario5 = (5, "UTUB")

usuario6 :: Usuario
usuario6 = (6, "PITON")

usuario7 :: Usuario
usuario7 = (7, "GUGUL")

usuario8 :: Usuario
usuario8 = (8, "UTUB")

usuario9 :: Usuario
usuario9 = (9, "Roberto Carlos")

--Relaciones
relacion1_3 :: Relacion
relacion1_3 = (usuario1, usuario3)

relacion4_7 :: Relacion
relacion4_7 = (usuario4, usuario7)

relacion8_2 :: Relacion
relacion8_2 = (usuario8, usuario2)

relacion9_1 :: Relacion
relacion9_1 = (usuario9, usuario1)

relacion4_0 :: Relacion
relacion4_0 = (usuario4, usuario0)

relacion2_7 :: Relacion
relacion2_7 = (usuario2, usuario7)

relacion7_3 :: Relacion
relacion7_3 = (usuario7, usuario3)

relacion3_7 :: Relacion
relacion3_7 = (usuario3, usuario7)

relacion3_1 :: Relacion
relacion3_1 = (usuario3, usuario1)

relacion2_6 :: Relacion
relacion2_6 = (usuario2, usuario6)


--Publicaciones

publicacion8_4 :: Publicacion
publicacion8_4 = (usuario8, "To the moon", [usuario7, usuario6, usuario4, usuario6])

publicacion9_2 :: Publicacion
publicacion9_2 = (usuario9, "import pandas as pd", [usuario4, usuario7])

publicacion1_3 :: Publicacion
publicacion1_3 = (usuario1, "WTB For loop", [usuario9, usuario4, usuario9])

publicacion3_2 :: Publicacion
publicacion3_2 = (usuario3, "To the moon", [usuario2, usuario6])

publicacion0_5 :: Publicacion
publicacion0_5 = (usuario0, "Hello world", [usuario7, usuario6, usuario7, usuario3, usuario0])

publicacion5_0 :: Publicacion
publicacion5_0 = (usuario5, "Deberia tomarme esto mas encerio", [])

publicacion7_2 :: Publicacion
publicacion7_2 = (usuario7, "Deberia tomarme esto mas encerio", [usuario0, usuario3])

publicacion1_2 :: Publicacion
publicacion1_2 = (usuario1, "Deberia tomarme esto mas encerio", [usuario1, usuario2])

publicacion8_5 :: Publicacion
publicacion8_5 = (usuario8, "import pandas as pd", [usuario4, usuario0, usuario1, usuario6, usuario2])

publicacion1_1 :: Publicacion
publicacion1_1 = (usuario1, "Deberia tomarme esto mas encerio", [usuario7])

--Listas

usuariosA = [usuario8, usuario6, usuario3, usuario8]
relacionesA = [relacion2_6, relacion4_0, relacion1_3]
publicacionesA = [publicacion9_2, publicacion7_2, publicacion5_0, publicacion1_1, publicacion1_2,publicacion1_1]

usuariosB = [usuario3, usuario2, usuario1, usuario9]
relacionesB = [relacion3_1, relacion3_7, relacion8_2, relacion3_7, relacion1_3]
publicacionesB = [publicacion7_2, publicacion1_3]

usuariosC = [usuario2, usuario6, usuario0]
relacionesC = [relacion3_1, relacion3_7, relacion8_2, relacion3_7, relacion1_3]
publicacionesC = [publicacion3_2,publicacion8_5]

--Redes
redA = (usuariosA, relacionesA, publicacionesA)
redB = (usuariosB, relacionesB, publicacionesB)
redC = (usuariosC, relacionesC, publicacionesC)
